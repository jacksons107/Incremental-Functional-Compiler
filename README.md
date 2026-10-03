# Incremental-Functional-Compiler

Compiler for a functional language based in part on [The Implementation of Functional Programming Languages](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf) and [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf). 

## Development setup

Requires an OCaml 5.x toolchain via opam, plus `gcc` on your `PATH` (opam does
not manage `gcc` — install it via your system package manager).

```sh
opam switch create . 5.2.1        # first time only; creates a local switch
opam install . --deps-only --with-test
eval $(opam env)
dune build
```

## Running a program

```sh
dune exec bin/main.exe -- examples/hello.oj
```

This compiles, links, and runs the program in one shot, deriving the output
binary's name from the input file (e.g. `examples/hello.oj` → `./hello`).
Useful flags:

- `-o <path>` — write the executable to `<path>` instead of the derived name.
- `-c` / `--no-run` — build only; don't run the resulting executable.
- `-t` — type-check only; print the program's inferred type and exit (no C generated, nothing built or run).
