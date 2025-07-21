#include <stdio.h>
#include <stdint.h>
#include "runtime.h"

#define HEAP_SIZE 1000
#define STACK_SIZE 1000

// initialize the heap and heap pointer
Node heap[HEAP_SIZE];
int hp = 0;

// initialize the stack and stack pointer
Node *stack[STACK_SIZE];
int sp = 0;

// program graph constructed by the compiler
extern void entry();

Node *mk_int(int64_t val) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_INT;
    node->val = val;

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

Node *mk_ind(Node *result) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_IND;
    node->result = result;

    return node;
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

Node *app_global(Node *global) {
    return global->code();
}

Node *unwind(Node *node) {
    // printf("Unwinding:\n");
    // print_node(node);
    switch (node->tag) {
        case NODE_INT:
            return node;
        
        case NODE_IND:
            return unwind(node->result);

        case NODE_APP:
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


void reduce() {
    while (1) {
        Node *root = stack_peak();
        // printf("Reducing:\n");
        // print_node(root);
        Node *result = unwind(root);
        *root = *mk_ind(result);
        if (result->tag == NODE_INT) {
            return;
        }
    }
}

void print_indent(int indent, const char *prefix) {
    for (int i = 0; i < indent; ++i) {
        printf("  ");
    }
    printf("%s", prefix);
}

void print_node(Node *node, int indent) {
    if (node->tag == NODE_INT) {
        print_indent(indent, "");
        printf("%lld\n", node->val);
    }
    else if (node->tag == NODE_IND) {
        print_indent(indent, "");
        print_node(node->result, indent + 1);
    }
    else if (node->tag == NODE_APP) {
        print_indent(indent - 1, "APP\n");
        print_indent(indent, "├── Left:\n");
        print_node(node->fn, indent + 1);
        print_indent(indent, "└── Right:\n");
        print_node(node->arg, indent + 1);
    }
    else if (node->tag == NODE_GLOBAL) {
        print_indent(indent, "");
        printf("%s (arity %lld)\n", node->name, node->arity);
    }
    else {
        printf("Attempting to node_print a non-node.\n");
    }
}

void print_stack() {
    printf("____Stack_____\n");
    for (int i=0; i<sp; i++) {
        printf("  __Node__\n");
        print_node(stack[i], 1);
        printf("  __End__\n");
    }
    printf("___Stack_End___\n");
}

int main(int argc, char **argv)
{
    entry();
    printf("After entry:\n");
    print_stack();
    reduce();
    printf("After reduce:\n");
    print_stack();
    print_node(stack_pop(), 1);
    return 0;
}
