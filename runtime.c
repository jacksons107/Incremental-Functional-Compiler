#include "runtime.h"
#include "utils.h"

Bool debug_enabled = false;

#define HEAP_SIZE 100000
#define STACK_SIZE 100000

// initialize the heap and heap pointer
uint8_t heap[HEAP_SIZE];
size_t hp = 0;

// initialize the stack and stack pointer
Node *stack[STACK_SIZE];
int sp = 0;

// program graph constructed by the compiler
extern void entry();

void *heap_alloc(size_t size, size_t align) {
    size_t misalign = hp % align;
    if (misalign != 0) {
        hp += align - misalign;
    }

    // eventually this will trigger garbage collection
    if (hp + size > HEAP_SIZE) {
        fprintf(stderr, "Out of heap memory!\n");
        exit(1);
    }

    void *ptr = &heap[hp];
    hp += size;

    return ptr;
}

Node *mk_int(int64_t val) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_INT;
    node->val = val;

    return node;
}

Node *mk_bool(Bool cond) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_BOOL;
    node->cond = cond;

    return node;
}

Node *mk_empty() {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_EMPTY;

    return node;
}

Node *mk_fail() {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_FAIL;

    return node;
}

Node *mk_global(int64_t arity, Node*(*code)(), char *name) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_GLOBAL;
    node->g_arity = arity;
    node->code = code;
    node->g_name = name;

    return node;
}

Node *mk_constr(int64_t arity, char *name) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_CONSTR;
    node->c_arity = arity;
    node->c_name = name;

    return node;
}

Node *mk_struct(char *name, int64_t arity) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_STRUCT;
    node->s_name = name;
    node->s_arity = arity;
    
    Node **fields = heap_alloc(sizeof(Node*) * arity, alignof(Node*));

    node->fields = fields;

    for (int64_t i = 0; i < arity; i++) {
        fields[i] = stack_pop();
    }

    return node;
}

Node *unpack_struct(Node *struc, int64_t n) {
    if (n < 0 || n >= struc->s_arity) {
        fprintf(stderr, "unpack_struct: index out of range\n");
        exit(1);
    }

    return struc->fields[n];
}

Node *mk_app(Node *fn, Node *arg) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_APP;
    node->fn = fn;
    node->arg = arg;

    return node;
}

Node *mk_cons(Node *e1, Node *e2) {
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
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
        return mk_empty();  // probably should be error, should never get here with type checking
    }
}

Node *eval_isempty() {
    Node *node = unwind(stack_pop());

    if (node->tag == NODE_EMPTY) {return mk_bool(true);} else {return mk_bool(false);}
}

Node *eval_iscons() {
    Node *node = unwind(stack_pop());

    if (node->tag == NODE_CONS) {return mk_bool(true);} else {return mk_bool(false);}
}

Node *eval_isint() {
    Node *node = unwind(stack_pop());

    if (node->tag == NODE_INT) {return mk_bool(true);} else {return mk_bool(false);}
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

Node *eval_Y() {
    Node *f = stack_pop();

    // placeholder node
    Node *hole = mk_empty();

    Node *ret = mk_app(f, hole);
    mk_ind(ret, hole);

    return ret;
}

Node *app_global(Node *global) {
    return unwind(global->code());
}

Node *app_constr(Node *constr) {
    return mk_struct(constr->c_name, constr->c_arity);
}

Node *unwind(Node *node) {
    if (debug_enabled == true) {
        printf("UNWIND NODE:\n");
        util_print_node(node);
        printf("\n");
        printf("WITH STACK:\n");
        util_print_stack();
    }
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

        case NODE_STRUCT:
            return node;
        
        case NODE_IND:
            return unwind(node->result);

        case NODE_APP:
            // if (debug_enabled == true) {
            //     printf("PUSHING:\n");
            //     util_print_node(node->arg);
            // }
            stack_push(node->arg);
            return unwind(node->fn);

        case NODE_GLOBAL:
            if (sp >= node->g_arity) {
                return app_global(node);
            }
            else {
                return node;
            }

        case NODE_CONSTR:
            if (sp >= node->c_arity) {
                return app_constr(node);
            }
            else {
                return node;    // TODO -- should this be an error, will type checker catch it?
            }
    }
}

Node *reduce() {
    while (1) {
        Node *root = stack_pop();
        Node *result = unwind(root);

        if (result->tag == NODE_INT || result->tag == NODE_BOOL 
            || result->tag == NODE_EMPTY || result->tag == NODE_FAIL
            || result->tag == NODE_CONS || result->tag == NODE_STRUCT) {
            return result;
        }

        // replace root node with an indirection node
        mk_ind(result, root);
    }
}

void print_node(Node *node) {
    fflush(stdout);
    if (node->tag == NODE_INT) {
        printf("%lld", node->val);
    }
    else if (node->tag == NODE_BOOL) {
        util_print_bool(node->cond);
    }
    else if (node->tag == NODE_EMPTY) {
        printf("[]");
    }
    else if (node->tag == NODE_FAIL) {
        printf("Fail");
    }
    else if (node->tag == NODE_CONS) {
        printf("CONS (");
        stack_push(node->e1);
        print_node(reduce());
        printf(", ");
        stack_push(node->e2);
        print_node(reduce());
        printf(")");
    }
    else if (node->tag == NODE_IND) {
        print_node(node->result);
    }
    else if (node->tag == NODE_STRUCT) {
        int64_t arity, i;
        arity = node->s_arity;
        printf("%s (", node->s_name);
        for (i = 0; i < arity; i++) {
            Node *elt = unpack_struct(node, i);
            stack_push(elt);
            print_node(reduce());
            if (i + 1 < arity) {
                printf(", ");
            }
        }
        printf(")");
    }
    else {
        printf("Error: Result is not a value:\n");
        util_print_node(node);
    }
}

int main(int argc, char **argv)
{
    // set debug flag
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--debug") == 0) {
            debug_enabled = true;
        }
    }

    entry();

    // if (debug_enabled == true) {
    //     printf("INITIAL STACK:\n");
    //     util_print_stack();
    // }

    print_node(reduce());
    printf("\n");
    return 0;
}
