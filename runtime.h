#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>

typedef enum {
    NODE_INT,
    NODE_GLOBAL,
    NODE_APP,
    NODE_IND,
} NodeTag;

typedef struct Node {
    NodeTag tag;
    union {
        int64_t val;               // NODE_INT
        struct {                   // NODE_APP
            struct Node *fn;
            struct Node *arg;
        };
        struct {                   // NODE_GLOBAL
            int64_t arity;
            struct Node*(*code)();
        };
        struct Node *result;       // NODE_IND
    };
} Node;

extern Node heap[];
extern int hp;

extern Node *stack[];
extern int sp;

/* makes an int node and returns a pointer to it to be pushed onto the stack */
Node *mk_int(int64_t val);

/* makes a global node and returns a pointer to it to be pushed onto the stack */
Node *mk_global(int64_t arity, Node*(*code)());

/* makes an app node and returns a pointer to it to be pushed onto the stack */
Node *mk_app(Node *fn, Node *arg);

/* creates an indirection node that can be used to replace an evaluated node */
Node *mk_ind(Node *node);

/* applies a global node by popping its arity off the stack and feeding those
   pointers to its code funtion pointer
   returns a result node that can be used to create an indirection node */
Node *app_global(Node *global);

/* function that pops two int node pointers from the stack, adds their values,
   then returns a pointer to the resulting int node
   automatically decays to a function pointer when passed to mk_global */
Node *eval_add();

/* push a node onto the stack */
void stack_push(Node *node);

/* pop a node off the stack */
Node *stack_pop();

/* peak at top node of stack */
Node *stack_peak();

/* unwind the program graph and apply the leftmost outermost redex */
Node *unwind(Node *node);

/* perform graph reduction on graph represented by current stack and heap
   push final output to the top of the stack */
void reduce();

/* prints the value of a node*/
void print_node(Node *node);


#endif