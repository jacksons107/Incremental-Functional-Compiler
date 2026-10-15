# On-disk project layout

A project root (found by `Project.root_of` via a `.oj-project` marker)
looks like this:

```
<root>/
  .oj-project          marker file, empty (Project.create)
  .oj/
    store/
      <hash>/           one directory per content-addressed hash (Store)
        j_instr         Marshal'd J_machine.j_instr list
        type            Marshal'd Ast.tyscheme
        ast             Marshal'd Ast.def, anonymized for recovery
    namespace           plain text, "name<TAB>hash" per line (Namespace)
```

- `store/<hash>/` is written once by `Store.write` and never rewritten;
  looked up by hash only, never browsed by name.
- `namespace` is an append-only log, not a rewritten snapshot -- a
  name's current hash is whichever line for it appears last. Read by
  `Namespace.read`, which folds the log down to each name's latest
  hash.
- The store is per-project; nothing is shared between projects.
