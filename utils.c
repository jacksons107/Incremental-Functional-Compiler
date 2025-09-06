#include <stdio.h>
#include "utils.h"

void util_print_indent(int indent, const char *prefix) {
    for (int i = 0; i < indent; ++i) {
        printf("  ");
    }
    printf("%s", prefix);
}

void util_print_bool(Bool b) {
    if (b == true) {
        printf("True");
    }
    else if (b == false) {
        printf("False");
    }
    else {
        printf("Attempting to print_bool a non-bool\n");
    }
}

// TODO -- update print_tree with new built-ins
void util_print_tree(Node *node, int indent) {
    if (node->tag == NODE_INT) {
        util_print_indent(indent, "");
        printf("%lld\n", node->val);
    }
    else if (node->tag == NODE_BOOL) {
        util_print_indent(indent, "");
        util_print_bool(node->cond);
    }
    else if (node->tag == NODE_IND) {
        util_print_indent(indent, "IND ->");
        util_print_tree(node->result, indent + 1);
    }
    else if (node->tag == NODE_APP) {
        util_print_indent(indent - 1, "APP\n");
        util_print_indent(indent, "├── Left:\n");
        util_print_tree(node->fn, indent + 1);
        util_print_indent(indent, "└── Right:\n");
        util_print_tree(node->arg, indent + 1);
    }
    else if (node->tag == NODE_GLOBAL) {
        util_print_indent(indent, "");
        printf("%s (arity %lld)\n", node->g_name, node->g_arity);
    }
    else {
        printf("Attempting to node_print a non-node.\n");
    }
}

void util_print_node(Node *node) {
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
        util_print_node(node->e1);
        printf(", ");
        util_print_node(node->e2);
        printf(")");
    }
    else if (node->tag == NODE_IND) {
        util_print_node(node->result);
    }
    else if (node->tag == NODE_APP) {
        printf("(");
        util_print_node(node->fn);
        printf(" ");
        util_print_node(node->arg);
        printf(")");
    }
    else if (node->tag == NODE_GLOBAL) {
        printf("%s", node->g_name);
    }
    else if (node->tag == NODE_CONSTR) {
        printf("Constr: %s", node->c_name);
    }
    else if (node->tag == NODE_STRUCT) {
        printf("Struct: %s", node->s_name);
    }
    else {
        printf("Attempting to util_node_print a non-node or unimplimented node.\n");
    }
}

void util_print_tag(Node *node) {
    printf("Tag: %d\n", node->tag);
}

void util_print_stack() {
    printf("____Stack_____\n");
    for (int i=0; i<sp; i++) {
        // printf("  __Node__\n");
        util_print_tag(stack[i]);
        printf("\n");
        // printf("  __End__\n");
    }
    printf("___Stack_End___\n");
}