#include "runtime.h"
#include "utils.h"

Bool debug_enabled = false;

// #define HEAP_SIZE (4 * 1024 * 1024)   // 4 MB per semi-space
#define HEAP_SIZE (4 * 1024 * 10)   // 4 MB per semi-space
#define STACK_SIZE (128 * 1024)       // 128K entries ~ 1 MB

// initialize the heap and heap pointer
uint8_t heap1[HEAP_SIZE];
uint8_t heap2[HEAP_SIZE];

uint8_t *from_space = heap1;
uint8_t *to_space = heap2;

size_t hp = 0; // offset into from_space
size_t to_hp = 0; // offset into to_space

// TODO -- detect stack overflow
// initialize the stack and stack pointer
Node *stack[STACK_SIZE];
int sp = 0;


// program graph constructed by the compiler
extern void entry();


void collect_garbage() {
    printf("COLLECTING\n");
    // printf("HP: %zu\n", hp / sizeof(Node));
    // util_print_stack();
    // reset to_space heap pointer
    to_hp = 0;

    // copy root nodes on stack to to_space
    for (int i = 0; i < sp; i++) {
        stack[i] = copy_to_space(stack[i]);
    }

    // scan to_space for references
    size_t scan = 0;
    size_t size;
    while (scan < to_hp) {
        Node *node = (Node *)(to_space + scan);
        copy_refs_to_space(node);
        // scan += sizeof(Node);
        if (node->tag == NODE_STRUCT) {
            size = sizeof(Node) + node->s_arity * sizeof(Node *);
        } 
        else {
            size = sizeof(Node);
        }
        scan += size;
    }
    
    // swap from_space and to_space
    uint8_t *temp = from_space;
    from_space = to_space;
    to_space = temp;

    // move heap pointer to new heap
    hp = to_hp;
    printf("DONE\n");
    // printf("HP: %zu\n", hp / sizeof(Node));
    // util_print_stack();
}

void copy_refs_to_space(Node *node) {
    switch (node->tag) {
        case NODE_CONS:
            node->e1 = copy_to_space(node->e1);
            node->e2 = copy_to_space(node->e2);
            break;

        case NODE_STRUCT:
            for (int64_t i = 0; i < node->s_arity; i++) {
                node->fields[i] = copy_to_space(node->fields[i]);
            }
            break;
        
        case NODE_IND:
            node->result = copy_to_space(node->result);
            break;

        case NODE_APP:
            node->fn = copy_to_space(node->fn);
            node->arg = copy_to_space(node->arg);
            break;
        case FORWARDED:
            printf("Forwarded node found in to_space\n");
            exit(-1);
        default:
            break;
    }
}

Node *copy_to_space(Node *node) {
    if (!node) return NULL;
    if (node->tag == FORWARDED) return node->forwarded;

    size_t size;
    if (node->tag == NODE_STRUCT) {
        size = sizeof(Node) + node->s_arity * sizeof(Node *);
    } else {
        size = sizeof(Node);
    }

    Node *new_node = to_alloc(size, alignof(Node));
    memcpy(new_node, node, size);
    // fprintf(stderr,"COPY: %d\n", new_node->tag);

    node->tag = FORWARDED;
    node->forwarded = new_node;

    return new_node;
}

void *alloc(uint8_t *space, size_t *hp, size_t size, size_t align) {
    // align heap pointer
    size_t misalign = *hp % align;
    if (misalign != 0) {
        *hp += align - misalign;
    }

    void *ptr = &space[*hp];
    *hp += size;

    return ptr;
}

void *to_alloc(size_t size, size_t align) {
    if (to_hp + size > HEAP_SIZE) {
        printf("Out of memory\n");
        exit(-1);
    }

    return alloc(to_space, &to_hp, size, align);
}

void *heap_alloc(size_t size, size_t align) {
    if (hp + size > HEAP_SIZE) {
        collect_garbage();

        if (hp + size > HEAP_SIZE) {
            printf("Out of memory\n");
            exit(-1);
        }
    }

    return alloc(from_space, &hp, size, align);
}

Node *mk_int(int64_t val) {
    // printf("MAKING A MF INT\n");
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    // printf("INT ALLOC DONE\n");
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
    Node *node = heap_alloc(sizeof(Node) + sizeof(Node*) * arity, alignof(Node));
    node->tag = NODE_STRUCT;
    node->s_name = name;
    node->s_arity = arity;

    for (int64_t i = 0; i < arity; i++) {
        node->fields[i] = stack_pop();
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
    stack_push(fn);
    stack_push(arg);
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_APP;
    // node->fn = fn;
    // node->arg = arg;
    node->arg = stack_pop();
    node->fn = stack_pop();

    return node;
}

Node *mk_cons(Node *e1, Node *e2) {
    stack_push(e1);
    stack_push(e2);
    Node *node = heap_alloc(sizeof(Node), alignof(Node));
    node->tag = NODE_CONS;
    // node->e1 = e1;
    // node->e2 = e2;
    node->e2 = stack_pop();
    node->e1 = stack_pop();

    return node;
}

void mk_ind(Node *replace, Node *old) {
    // printf("Old Tag:\n");
    // util_print_tag(old);
    // printf("Replace Tag:\n");
    // util_print_tag(replace);å

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

Node *stack_peak(int n) {
    return stack[sp-n-1];
}

Node *eval_I() {
    // util_print_tag(stack_peak(0));
    return stack_pop();
}

Node *eval_K() {
    // util_print_stack();
    Node *ret = stack_pop();
    stack_pop();

    return ret;
}

Node *eval_S() {
    Node *f = stack_pop();
    Node *g = stack_pop();
    Node *x = stack_pop();
    stack_push(x);
    stack_push(f);

    Node *g_x = mk_app(g, x);
    f = stack_pop();
    x = stack_pop();
    stack_push(g_x);
    Node *f_x = mk_app(f, x);

    // return mk_app(f_x, g_x);
    Node *ret = mk_app(f_x, stack_pop());
    
    return ret;
}

Node *eval_add() {
    // printf("ADD\n");
    Node *int1 = unwind(stack_pop());
    Node *i2 = stack_pop();
    stack_push(int1);
    // printf("INT1\n");
    // Node *int2 = unwind(stack_pop());
    Node *int2 = unwind(i2);
    // printf("INT2\n");
    int1 = stack_pop();

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

    // this first case should be impossible with type checking
    if (tag1 != tag2) {
        return mk_bool(false);
    }
    else if (tag1 == NODE_INT && tag2 == NODE_INT) {
        // printf("%lld == %lld\n", val1->val, val2->val);
        if (val1->val == val2->val) {return mk_bool(true);} else {return mk_bool(false);}
    } 
    else if (tag1 == NODE_BOOL && tag2 == NODE_BOOL) {
        if (val1->cond == val2->cond) {return mk_bool(true);} else {return mk_bool(false);}
    }
    else if (tag1 == NODE_EMPTY && tag2 == NODE_EMPTY) {
        return mk_bool(true);
    }
    else if (tag1 == NODE_EMPTY || tag2 == NODE_EMPTY) {
        return mk_bool(false);
    }
    else {
        // return mk_empty();  // probably should be error, should never get here with type checking
        printf("YOU DONE FUCKED UP\n");
        exit(-1);
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

Node *eval_isconstr() {
    Node *exp = unwind(stack_pop());
    Node *constr = stack_pop(); // should NOT unwind, TODO -- better way to do comparison in IsConstr?

    if (exp->tag == NODE_STRUCT && strcmp(exp->s_name, constr->c_name)) {
        return mk_bool(true);
    } 
    else {
        return mk_bool(false);
    }
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

Node *eval_unpack() {
    Node *struc = unwind(stack_pop());
    Node *idx = unwind(stack_pop());

    // if (debug_enabled == true) {
    //     printf("Unpacking: ");
    //     util_print_node(struc);
    //     printf(" at idx: ");
    //     util_print_node(idx);
    //     printf("\n");
    // }

    return unpack_struct(struc, idx->val);
}

Node *eval_Y() {
    Node *f = stack_pop();
    stack_push(f);

    // placeholder node
    Node *hole = mk_empty();

    // Node *ret = mk_app(f, hole);
    Node *ret = mk_app(stack_pop(), hole);

    // hole = stack_pop();
    mk_ind(ret, hole); 

    return ret;
}

Node *app_global(Node *global) {
    // printf("APP: %s\n", global->g_name);
    return unwind(global->code());
}

Node *app_constr(Node *constr) {
    return mk_struct(constr->c_name, constr->c_arity);
}

Node *unwind(Node *node) {
    if (debug_enabled == true) {
        printf("UNWIND NODE:\n");
        util_print_tag(node);
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
            // util_print_tag(node->fn);
            return unwind(node->fn);

        case NODE_GLOBAL:
            if (sp >= node->g_arity) {
                // printf("APP GLOBAL: %s\n", node->g_name);
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
                printf("Not enough args for constr\n");
                exit(-1);
            }

        case FORWARDED:
            printf("Trying to unwind forwarded node\n");
            // util_print_stack();
            // util_print_tag(node->forwarded);
            // printf("HP: %zu\n", hp / sizeof(Node));
            exit(-1);
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
