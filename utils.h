#ifndef UTILS_H
#define UTILS_H

#include "runtime.h"

/* helper that prints a bool node */
void util_print_bool(Bool b);

/* helper that creates indents to create the node graph in print_tree */
void util_print_indent(int indent, const char *prefix);

/* prints a node in s-expression form */
void util_print_node(Node *node);

/* prints a node in tree form*/
void util_print_tree(Node *node, int indent);

/* prints all the values currently on the stack */
void util_print_stack();

#endif