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

Node *mk_global(int64_t arity, Node*(*code)()) {
    Node *node = &heap[hp];
    hp++;
    node->tag = NODE_GLOBAL;
    node->arity = arity;
    node->code = code;

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

Node *eval_add() {
    // pop two from stack
    Node *int1 = unwind(stack_pop());
    Node *int2 = unwind(stack_pop());
    // add together
    int64_t new_val = int1->val + int2->val;
    Node *node = mk_int(new_val);

    return node;
}

Node *app_global(Node *global) {
    // call function which returns a Node back to the stack
    return global->code();
}

Node *unwind(Node *node) {
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
        Node *result = unwind(root);
        *root = *mk_ind(result);
        if (result->tag == NODE_INT) {
            return;
        }
    }
}

void print_node(Node *node) {
    if (node->tag == NODE_INT) {
        printf("%lld\n", node->val);
    }
    else if (node->tag == NODE_IND) {
        print_node(node->result);
    }
    else {
        printf("Trying to print invalid node type: ");
        if (node->tag == NODE_APP) {
            printf("APP\n");
        }
        else {
            printf("GLOBAL\n");
        }
    }
}

int main(int argc, char **argv)
{
    entry();
    reduce();
    print_node(stack_pop());
    return 0;
}
