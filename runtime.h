#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdalign.h>
#include <string.h>

// TODO -- replace Bool with stdbool
typedef enum {
    true,
    false
} Bool;

typedef enum {
    NODE_INT,
    NODE_BOOL,
    NODE_STRING,
    NODE_GLOBAL,
    NODE_APP,
    NODE_CONS,
    NODE_EMPTY,
    NODE_FAIL,
    NODE_IND,
    NODE_CONSTR,
    NODE_STRUCT,
    FORWARDED,
} NodeTag;

typedef struct Node {
    NodeTag tag;
    union {
        int64_t val;                            // NODE_INT
        Bool cond;                              // NODE_BOOL
        char *str;                              // NODE_STRING
        struct Node *forwarded;                 // FORWARDED
        struct {                                // NODE_APP
            struct Node *fn;
            struct Node *arg;
        };
        struct {                                // NODE_CONS
            struct Node *e1;
            struct Node *e2;
        };
        struct {                                // NODE_GLOBAL
            int64_t g_arity;
            struct Node*(*code)();
            char *g_name;
        };
        struct Node *result;                    // NODE_IND
        struct {                                // NODE_CONSTR
            int64_t c_arity;
            char *c_name;
        };
        struct {                                // NODE_STRUCT
            char *s_name;
            int64_t s_arity;
            // struct Node **fields;
            struct Node *fields[];
        };
    };                                          // NODE_EMPTY & NODE_FAIL (don't point to anything)
} Node;

extern uint8_t heap[];
extern size_t hp;

extern Node *stack[];
extern int sp;

/* free up heap space by copying live nodes in from_space to to_space */
void collect_garbage();

/* copy references of nodes in to_space to to_space */
void copy_refs_to_space(Node *node);

/* copy node from from_space to to_space and return the pointer to the to_space node
   allocs a new node in to_space, sets all the fields to be the same as the 
   original node, then set the original node tag to forwarded and forwarded field to
   the to_space node (so that multiple copies aren't made)*/
Node *copy_to_space(Node *node);

/* allocates size worth of space  to given heap */
void *alloc(uint8_t *space, size_t *hp, size_t size, size_t align);

/* allocates an object on the to_space heap and returns a pointer to it */
void *to_alloc(size_t size, size_t align);

/* allocate an object on the heap (from_space) and return a pointer to it */
void *heap_alloc(size_t size, size_t align);

/* makes an int node and returns a pointer to it to be pushed onto the stack */
Node *mk_int(int64_t val);

/* makes a bool node and returns a pointer to it to be pushed onto the stack */
Node *mk_bool(Bool cond);

/* makes a string node and returns a pointer to it to be pushed onto the stack */
Node *mk_string(char *s);

/* makes an empty node (just a tag) */
Node *mk_empty();

/* makes a fail node (just a tag) */
Node *mk_fail();

/* makes a global node and returns a pointer to it to be pushed onto the stack */
Node *mk_global(int64_t arity, Node*(*code)(), char *name);

/* makes an app node and returns a pointer to it to be pushed onto the stack */
Node *mk_app(Node *fn, Node *arg);

/* makes a cons node and returns a pointer to it to be pushed onto the stack */
Node *mk_cons(Node *e1, Node *e2);

/* makes a constr node and returns a pointer to it to be pushed onto the stack */
Node *mk_constr(int64_t arity, char *name);

/* makes a struct node and returns a pointer to it to be pushed onto the stack */
Node *mk_struct(char *name, int64_t arity);

/* returns element of struct node at index n */
Node *unpack_struct(Node* struc, int64_t n);

/* replace the node pointed to by old with an indirection node pointing to result */
void mk_ind(Node *replace, Node *old);

/* applies a global node by calling its code pointer which pops artiy number of
   args off the stack, performs the body of the global, and returns the resulting node */
Node *app_global(Node *global);

/* applies a constr node by calling mk_struct with the node's name and arity as args */
Node *app_constr(Node *constr);

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

/* pop two nodes off the stack, return if they are equal or not (bool node) 
   type checking will have already checked if the values are the same types
   temporarily returns [] in the case of malformed expression */
Node *eval_eq();

/* pop one node off the stack, return if it is a cons node or not (bool node) */
Node *eval_iscons();

/* pop one node off the stack, return if it is a given constr node or not (bool node) */
Node *eval_isconstr();

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

/* pop two nodes off the stack, the first one will be the struct node and the 
   second will be the index (int node)
   pass the struct node and the index nodes val field to unpack_struct and return
   what it returns */
Node *eval_unpack();

/* pops one node off the stack (should be a fn), creates an empty node as a placeholder, 
   creates an app node of the function onto the empty node, replaces the empty node
   with an indirection node that points to the app node, and returns the app node */
Node *eval_Y();

/* push a node onto the stack */
void stack_push(Node *node);

/* pop a node off the stack */
Node *stack_pop();

/* peak at nth from the top node of stack */
Node *stack_peak(int n);

/* unwind the program graph and apply the leftmost outermost redex */
Node *unwind(Node *node);

/* perform graph reduction on graph represented by current stack and heap
   return final result of program as a node */
Node *reduce();

/* prints the node that is the result of the computation 
   if the node is a constructed type, drives evaluation of the inner elements and
   prints them as they are computed 
   if the result is not a value prints an error message the uses util_print_node 
   to print the node 
   flushes stdout everytime it is called */
void print_node(Node *node);

#endif