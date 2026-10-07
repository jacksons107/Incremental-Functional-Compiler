/* Hand-rolled test harness for runtime.c/utils.c: no external C test
   framework, just assert-style checks that print PASS/FAIL and exit
   nonzero on any failure (picked up by dune's rule as a real build
   failure). Exercises the stack, node constructors, the eval_* builtins,
   and the copying garbage collector directly, bypassing the OCaml-compiled
   graph entirely.

   runtime.c defines its own `main()` (which calls the extern `entry()`
   normally provided by OCaml-generated code) -- the dune rule compiles it
   with `-Dmain=runtime_orig_main_unused` so this file's own `main` can
   link without a symbol clash. `entry` is declared extern by runtime.c but
   the renamed original main is never called, so a trivial stub below
   satisfies the linker without ever running. */
#include "runtime.h"
#include <assert.h>
#include <string.h>

void entry(void) {}

static int failures = 0;

#define CHECK(cond, msg)                                                     \
  do {                                                                       \
    if (cond) {                                                              \
      printf("[PASS] %s\n", msg);                                            \
    } else {                                                                 \
      printf("[FAIL] %s\n", msg);                                            \
      failures++;                                                            \
    }                                                                        \
  } while (0)

static void reset_stack(void) { sp = 0; }
static void reset_globals(void) { num_globals = 0; }

static void test_stack_ops(void) {
  reset_stack();
  stack_push(mk_int(1));
  stack_push(mk_int(2));
  CHECK(sp == 2, "stack_push increments sp");
  CHECK(stack_peak(0)->val == 2, "stack_peak(0) sees the top without popping");
  CHECK(stack_peak(1)->val == 1, "stack_peak(1) sees the next item down");
  CHECK(sp == 2, "stack_peak does not itself pop");
  Node *top = stack_pop();
  CHECK(top->val == 2 && sp == 1, "stack_pop returns top and decrements sp");
}

static void test_node_constructors(void) {
  Node *i = mk_int(42);
  CHECK(i->tag == NODE_INT && i->val == 42, "mk_int sets tag and val");

  Node *b = mk_bool(true);
  CHECK(b->tag == NODE_BOOL && b->cond == true, "mk_bool sets tag and cond");

  Node *s = mk_string("hi");
  CHECK(s->tag == NODE_STRING && strcmp(s->str, "hi") == 0,
        "mk_string sets tag and str");

  Node *e = mk_empty();
  CHECK(e->tag == NODE_EMPTY, "mk_empty sets tag");

  Node *f = mk_fail();
  CHECK(f->tag == NODE_FAIL, "mk_fail sets tag");
}

static void test_eval_add(void) {
  reset_stack();
  stack_push(mk_int(3));
  stack_push(mk_int(4));
  Node *result = eval_add();
  CHECK(result->tag == NODE_INT && result->val == 7,
        "eval_add sums two int nodes");
  CHECK(sp == 0, "eval_add consumes both operands off the stack");
}

static void test_eval_eq(void) {
  reset_stack();
  stack_push(mk_int(5));
  stack_push(mk_int(5));
  CHECK(eval_eq()->cond == true, "eval_eq: equal ints -> true");

  reset_stack();
  stack_push(mk_int(5));
  stack_push(mk_int(6));
  CHECK(eval_eq()->cond == false, "eval_eq: unequal ints -> false");

  reset_stack();
  stack_push(mk_bool(true));
  stack_push(mk_bool(true));
  CHECK(eval_eq()->cond == true, "eval_eq: equal bools -> true");

  reset_stack();
  stack_push(mk_empty());
  stack_push(mk_empty());
  CHECK(eval_eq()->cond == true, "eval_eq: [] == [] -> true");

  reset_stack();
  stack_push(mk_empty());
  stack_push(mk_int(1));
  CHECK(eval_eq()->cond == false,
        "eval_eq: [] vs a non-empty value -> false");
}

/* NOTE: `mk_cons(stack_pop(), stack_pop())` inside eval_cons() has
   argument-evaluation order left unspecified by the C standard -- this
   test locks in gcc's actual observed behavior (verified empirically: the
   top-of-stack argument ends up as e1/head) as a regression baseline,
   rather than assuming a specific order from reading the source alone. */
static void test_eval_cons_head_tail(void) {
  reset_stack();
  stack_push(mk_int(111));
  stack_push(mk_int(222));
  Node *cons = eval_cons();
  CHECK(cons->tag == NODE_CONS, "eval_cons produces a NODE_CONS");
  CHECK(cons->e1->val == 222, "eval_cons: top-of-stack becomes e1 (head)");
  CHECK(cons->e2->val == 111, "eval_cons: second-from-top becomes e2 (tail)");

  reset_stack();
  stack_push(cons);
  CHECK(eval_head()->val == 222, "eval_head returns e1");

  reset_stack();
  stack_push(cons);
  CHECK(eval_tail()->val == 111, "eval_tail returns e2");
}

static void test_eval_iscons(void) {
  reset_stack();
  Node *cons = mk_cons(mk_int(1), mk_int(2));
  stack_push(cons);
  CHECK(eval_iscons()->cond == true, "eval_iscons: a cons node -> true");

  reset_stack();
  stack_push(mk_int(1));
  CHECK(eval_iscons()->cond == false, "eval_iscons: a non-cons node -> false");
}

static void test_eval_isconstr(void) {
  /* 0-arity struct: mk_struct pops `arity` fields off the stack itself, so
     arity 0 needs nothing pushed beforehand. */
  Node *pair = mk_struct("Pair", 0);

  reset_stack();
  stack_push(mk_string("Pair"));
  stack_push(pair);
  CHECK(eval_isconstr()->cond == true,
        "eval_isconstr: matching constructor name -> true");

  reset_stack();
  stack_push(mk_string("Other"));
  stack_push(pair);
  CHECK(eval_isconstr()->cond == false,
        "eval_isconstr: mismatched constructor name -> false");
}

static void test_eval_unpack(void) {
  /* mk_struct pops its fields off the stack itself, filling fields[0] from
     the top of stack -- so fields must be pushed in reverse (last field
     first) for fields[i] to end up holding what looks like the i-th
     positional argument. */
  reset_stack();
  stack_push(mk_int(20));
  stack_push(mk_int(10));
  Node *pair = mk_struct("Pair", 2);
  CHECK(pair->fields[0]->val == 10 && pair->fields[1]->val == 20,
        "mk_struct fills fields[] in argument order given reverse-pushed args");

  reset_stack();
  stack_push(mk_int(0));
  stack_push(pair);
  CHECK(eval_unpack()->val == 10, "eval_unpack: index 0 returns fields[0]");

  reset_stack();
  stack_push(mk_int(1));
  stack_push(pair);
  CHECK(eval_unpack()->val == 20, "eval_unpack: index 1 returns fields[1]");
}

static void test_eval_Y(void) {
  reset_stack();
  Node *f = mk_int(999); /* stand-in "function" -- eval_Y never inspects it */
  stack_push(f);
  Node *ret = eval_Y();

  CHECK(ret->tag == NODE_APP && ret->fn == f,
        "eval_Y wraps f in an App(f, hole) node");
  CHECK(ret->arg->tag == NODE_IND && ret->arg->result == ret,
        "eval_Y ties the knot: the hole becomes an indirection back to the "
        "App node itself");
}

static void test_gc_preserves_cons(void) {
  reset_stack();
  Node *cons = mk_cons(mk_int(10), mk_int(20));
  stack_push(cons);

  collect_garbage();

  Node *survived = stack_peak(0);
  CHECK(survived->tag == NODE_CONS, "cons cell survives collect_garbage");
  CHECK(survived->e1->val == 10 && survived->e2->val == 20,
        "cons cell's nested int values survive collect_garbage intact "
        "(exercises copy_refs_to_space's NODE_CONS case)");
  reset_stack();
}

static void test_globals_ops(void) {
  reset_globals();
  globals_push(mk_int(1));
  globals_push(mk_int(2));
  CHECK(num_globals == 2, "globals_push increments num_globals");
  CHECK(globals_get(0)->val == 1, "globals_get(0) returns the first pushed node");
  CHECK(globals_get(1)->val == 2, "globals_get(1) returns the second pushed node");
}

/* The whole point of globals[] being its own root set (scanned by
   collect_garbage independently of stack[]): a definition's node can sit
   in globals[] with nothing currently on the stack pointing to it -- the
   gap between being built and being next referenced via an ID instruction
   -- and still survive a collection that happens during that gap. */
static void test_gc_preserves_globals_with_empty_stack(void) {
  reset_stack();
  reset_globals();
  globals_push(mk_cons(mk_int(10), mk_int(20)));
  CHECK(sp == 0, "nothing on the stack points at the global");

  collect_garbage();

  Node *survived = globals_get(0);
  CHECK(survived->tag == NODE_CONS,
        "a globals[] entry survives collect_garbage with an empty stack");
  CHECK(survived->e1->val == 10 && survived->e2->val == 20,
        "the surviving global's nested values are intact");
  reset_globals();
}

static void test_gc_preserves_struct(void) {
  reset_stack();
  stack_push(mk_int(2));
  stack_push(mk_int(1));
  Node *pair = mk_struct("Pair", 2);
  stack_push(pair);

  collect_garbage();

  Node *survived = stack_peak(0);
  CHECK(survived->tag == NODE_STRUCT,
        "variable-size struct node survives collect_garbage");
  CHECK(survived->fields[0]->val == 1 && survived->fields[1]->val == 2,
        "struct's field pointers survive collect_garbage intact "
        "(exercises the variable-size NODE_STRUCT copy path)");
  reset_stack();
}

int main(void) {
  test_stack_ops();
  test_node_constructors();
  test_eval_add();
  test_eval_eq();
  test_eval_cons_head_tail();
  test_eval_iscons();
  test_eval_isconstr();
  test_eval_unpack();
  test_eval_Y();
  test_globals_ops();
  test_gc_preserves_globals_with_empty_stack();
  test_gc_preserves_cons();
  test_gc_preserves_struct();

  printf("\n%s (%d failure%s)\n", failures == 0 ? "ALL PASSED" : "SOME FAILED",
         failures, failures == 1 ? "" : "s");
  return failures == 0 ? 0 : 1;
}
