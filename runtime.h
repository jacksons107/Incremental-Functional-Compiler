#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>

typedef enum {
    true,
    false
} Bool;

typedef enum {
    NODE_INT,
    NODE_BOOL,
    NODE_GLOBAL,
    NODE_APP,
    NODE_CONS,
    NODE_EMPTY,
    NODE_IND,
} NodeTag;

typedef struct Node {
    NodeTag tag;
    union {
        int64_t val;               // NODE_INT
        Bool cond;                 // NODE_BOOL
        struct {                   // NODE_APP
            struct Node *fn;
            struct Node *arg;
        };
        struct {                   // NODE_CONS
            struct Node *e1;
            struct Node *e2;
        };
        struct {                   // NODE_GLOBAL
            int64_t arity;
            struct Node*(*code)();
            char *name;
        };
        struct Node *result;       // NODE_IND
    };                             // NODE_EMPTY (doesn't point to anything)
} Node;

extern Node heap[];
extern int hp;

extern Node *stack[];
extern int sp;

/* makes an int node and returns a pointer to it to be pushed onto the stack */
Node *mk_int(int64_t val);

/* makes a bool node and returns a pointer to it to be pushed onto the stack */
Node *mk_bool(Bool cond);

/* makes an empty node (just a tag) */
Node *mk_empty();

/* makes a global node and returns a pointer to it to be pushed onto the stack */
Node *mk_global(int64_t arity, Node*(*code)(), char *name);

/* makes an app node and returns a pointer to it to be pushed onto the stack */
Node *mk_app(Node *fn, Node *arg);

/* makes a cons node and returns a pointer to it to be pushed onto the stack */
Node *mk_cons(Node *e1, Node *e2);

/* replace the node pointed to by old with an indirection node pointing to result */
void mk_ind(Node *replace, Node *old);

/* applies a global node by calling its code pointer which pops artiy number of
   args off the stack, performs the body of the global, and returns the resulting node */
Node *app_global(Node *global);

/* pop one node from the stack and return it without unwidning it */
Node *eval_I();

/* pop two nodes from stack and return the first one without undwinding either */
Node *eval_K();

/* pop three nodes (f, g, x) from stack and return an app node representing
   f x (g x) without unwinding any of the nodes */
Node *eval_S();

/* function that pops two int node pointers from the stack, adds their values,
   then returns a pointer to the resulting int node
   automatically decays to a function pointer when passed to mk_global */
Node *eval_add();

/* pop one node off the stack and unwind it (should evaluate to a bool)
   if the bool is true then pop and unwind the next node (true branch), pop the 
   node after to burn it (false branch) and return the true branch
   if the bool is false do the opposite process */
Node *eval_if();

/* pop two nodes off the stack and put them in a cons node */
Node *eval_cons();

/* pop one node off the stack and unwind it (should evaluate to a cons)
   return the first element of the cons */
Node *eval_head();

/* pop one node off the stack and unwind it (should evaluate to a cons)
   return the second element of the cons */
Node *eval_tail();

/* pops one node off the stack, creates an app node where the fn is the popped
   node and the arg is the app node itself */
Node *eval_Y();

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

/* helper that creates indents to create the node graph in print_tree */
void print_indent(int indent, const char *prefix);

/* prints a node in s-expression form */
void print_node(Node *node);

/* prints a node in tree form*/
void print_tree(Node *node, int indent);

/* prints all the values currently on the stack */
void print_stack();


#endif