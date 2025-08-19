#include <stdio.h>
#include <stdint.h>
#include "runtime.h"

#define HEAP_SIZE 100000
#define STACK_SIZE 100000

// initialize the heap and heap pointer
Node heap[HEAP_SIZE];
int hp = 0;

// initialize the stack and stack pointer
Node *stack[STACK_SIZE];
int sp = 0;

// program graph constructed by the compiler
extern void entry();

// TODO -- make an alloc_node function
Node *mk_int(int64_t val) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_INT;
    node->val = val;

    return node;
}

Node *mk_bool(Bool cond) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_BOOL;
    node->cond = cond;

    return node;
}

Node *mk_empty() {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_EMPTY;

    return node;
}

Node *mk_fail() {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_FAIL;

    return node;
}

Node *mk_global(int64_t arity, Node*(*code)(), char *name) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_GLOBAL;
    node->arity = arity;
    node->code = code;
    node->name = name;

    return node;
}

Node *mk_app(Node *fn, Node *arg) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_APP;
    node->fn = fn;
    node->arg = arg;

    return node;
}

Node *mk_cons(Node *e1, Node *e2) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_CONS;
    node->e1 = e1;
    node->e2 = e2;

    return node;
}

void mk_ind(Node *replace, Node *old) {
    old->tag = NODE_IND;
    old->result = replace;
}

void stack_push(Node *node) {
    stack[sp] = node;
    sp++;
}

Node *stack_pop() {
    sp--;
    return stack[sp];
}

Node *stack_peak() {
    return stack[sp-1];
}

Node *eval_I() {
    return stack_pop();
}

Node *eval_K() {
    Node *ret = stack_pop();
    stack_pop();

    return ret;
}

Node *eval_S() {
    Node *f = stack_pop();
    Node *g = stack_pop();
    Node *x = stack_pop();

    Node *g_x = mk_app(g, x);
    Node *f_x = mk_app(f, x);

    return mk_app(f_x, g_x);
}

Node *eval_add() {
    Node *int1 = unwind(stack_pop());
    Node *int2 = unwind(stack_pop());

    int64_t new_val = int1->val + int2->val;
    Node *node = mk_int(new_val);

    return node;
}

// TODO -- clean up eval_eq once type checking is working
Node *eval_eq() {
    Node *val1 = unwind(stack_pop());
    Node *val2 = unwind(stack_pop());

    NodeTag tag1 = val1->tag;
    NodeTag tag2 = val2->tag;
    if (tag1 == NODE_INT) {
        if (val1->val == val2->val) {return mk_bool(true);} else {return mk_bool(false);}
    } 
    else if (tag1 == NODE_BOOL) {
        if (val1->cond == val2->cond) {return mk_bool(true);} else {return mk_bool(false);}
    }
    else if (tag1 == NODE_EMPTY && tag2 == NODE_EMPTY) {
        return mk_bool(true);
    }
    else if (tag1 == NODE_EMPTY || tag2 == NODE_EMPTY) {
        return mk_bool(false);
    }
    else {
        return mk_empty();
    }
}

Node * eval_isempty() {
    Node *node = unwind(stack_pop());

    if (node->tag == NODE_EMPTY) {return mk_bool(true);} else {return mk_bool(false);}
}

Node * eval_iscons() {
    Node *node = unwind(stack_pop());

    if (node->tag == NODE_CONS) {return mk_bool(true);} else {return mk_bool(false);}
}

Node *eval_if() {
    Node *bool = unwind(stack_pop());
    Node *ret;
    if (bool->cond == true) {
        ret = unwind(stack_pop());
        stack_pop();
    }
    else {
        stack_pop();
        ret = unwind(stack_pop());
    }

    return ret;
}

Node *eval_cons() {
    Node *ret = mk_cons(stack_pop(), stack_pop());

    return ret;
}

Node *eval_head() {
    Node *cons = unwind(stack_pop());

    return cons->e1;
}

Node *eval_tail() {
    Node *cons = unwind(stack_pop());

    return cons->e2;
}

// TODO -- Y is definitely wrong
Node *eval_Y() {
    // Stack top is the single argument f
    Node *f = stack_pop();

    // 1) Make a placeholder node to stand in for (Y f)
    Node *hole = mk_empty();        // any fresh node works as a placeholder

    // 2) Build ret = f hole
    Node *ret = mk_app(f, hole);

    // 3) Tie the knot: hole becomes an indirection to ret
    //    So the argument to f is (Y f) == ret itself.
    mk_ind(ret, hole);

    // 4) Return f (Y f)
    return ret;
}


Node *app_global(Node *global) {
    return unwind(global->code());
}

// TODO -- add env variables to toggle on debug logs
Node *unwind(Node *node) {
    // printf("UNWIND NODE:\n");
    // print_node(node);
    // printf("\n");
    // printf("WITH STACK:\n");
    // print_stack();
    switch (node->tag) {
        case NODE_INT:
            return node;

        case NODE_BOOL:
            return node;

        case NODE_EMPTY:
            return node;

        case NODE_FAIL:
            return node;

        case NODE_CONS:
            return node;
        
        case NODE_IND:
            return unwind(node->result);

        case NODE_APP:
            // printf("PUSHING:\n");
            // print_node(node->arg, 1);
            stack_push(node->arg);
            return unwind(node->fn);

        case NODE_GLOBAL:
            if (sp >= node->arity) {
                return app_global(node);
            }
            else {
                return node;
            }
    }
}

// TODO -- programs that are just a value seg fault
Node *reduce() {
    while (1) {
        Node *root = stack_pop();
        Node *result = unwind(root);

        if (result->tag == NODE_INT || result->tag == NODE_BOOL 
            || result->tag == NODE_EMPTY || result->tag == NODE_FAIL) {
            return result;
        }

        // replace root node with an indirection node
        mk_ind(result, root);
    }
}

// TODO -- create separate utils file
void print_indent(int indent, const char *prefix) {
    for (int i = 0; i < indent; ++i) {
        printf("  ");
    }
    printf("%s", prefix);
}

void print_bool(Bool bool) {
    if (bool == true) {
        printf("True");
    }
    else if (bool == false) {
        printf("False");
    }
    else {
        printf("Attempting to print_bool a non-bool\n");
    }
}

// TODO -- update print_tree with new built-ins
void print_tree(Node *node, int indent) {
    if (node->tag == NODE_INT) {
        print_indent(indent, "");
        printf("%lld\n", node->val);
    }
    else if (node->tag == NODE_BOOL) {
        print_indent(indent, "");
        print_bool(node->cond);
    }
    else if (node->tag == NODE_IND) {
        print_indent(indent, "IND ->");
        print_tree(node->result, indent + 1);
    }
    else if (node->tag == NODE_APP) {
        print_indent(indent - 1, "APP\n");
        print_indent(indent, "├── Left:\n");
        print_tree(node->fn, indent + 1);
        print_indent(indent, "└── Right:\n");
        print_tree(node->arg, indent + 1);
    }
    else if (node->tag == NODE_GLOBAL) {
        print_indent(indent, "");
        printf("%s (arity %lld)\n", node->name, node->arity);
    }
    else {
        printf("Attempting to node_print a non-node.\n");
    }
}

void print_node(Node *node) {
    if (node->tag == NODE_INT) {
        printf("%lld", node->val);
    }
    else if (node->tag == NODE_BOOL) {
        print_bool(node->cond);
    }
    else if (node->tag == NODE_EMPTY) {
        printf("[]");
    }
    else if (node->tag == NODE_FAIL) {
        printf("Fail");
    }
    else if (node->tag == NODE_CONS) {
        printf("CONS [");
        print_node(node->e1);
        printf(", ");
        print_node(node->e2);
        printf("]");
    }
    else if (node->tag == NODE_IND) {
        print_node(node->result);
    }
    else if (node->tag == NODE_APP) {
        printf("(");
        print_node(node->fn);
        printf(" ");
        print_node(node->arg);
        printf(")");
    }
    else if (node->tag == NODE_GLOBAL) {
        printf("%s", node->name);
    }
    else {
        printf("Attempting to node_print a non-node.\n");
    }
}

void print_stack() {
    printf("____Stack_____\n");
    for (int i=0; i<sp; i++) {
        // printf("  __Node__\n");
        print_node(stack[i]);
        printf("\n");
        // printf("  __End__\n");
    }
    printf("___Stack_End___\n");
}

int main(int argc, char **argv)
{
    entry();
    // printf("INITIAL STACK:\n");
    // print_stack();
    print_node(reduce());
    // print_node(stack_pop());
    printf("\n");
    return 0;
}
