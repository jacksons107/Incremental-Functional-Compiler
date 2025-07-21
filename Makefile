# Paths
DUNE=dune
OCAML_MAIN=bin/main.exe
C_RUNTIME_SRCS=runtime.c generated.c
C_RUNTIME_OBJS=runtime.o generated.o
CC=gcc
CFLAGS=-O2 -Wall
DEBUG_CFLAGS=-g -O0 -Wall


# Targets
all: $(OCAML_MAIN) c_runtime

# Step 1: Compile OCaml part and generate generated.c
$(OCAML_MAIN):
	$(DUNE) build $(OCAML_MAIN)
	./_build/default/bin/main.exe

# Step 2: Compile runtime.c and generated.c to object files
runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -c runtime.c

generated.o: generated.c runtime.h
	$(CC) $(CFLAGS) -c generated.c

# Step 3: Compile C runtime and link with generated code
c_runtime: $(C_RUNTIME_OBJS)
	$(CC) -o program $(C_RUNTIME_OBJS)

# Debug build
debug: $(OCAML_MAIN)
	$(CC) $(DEBUG_CFLAGS) -c runtime.c -o runtime.o
	$(CC) $(DEBUG_CFLAGS) -c generated.c -o generated.o
	$(CC) -g -o debug_program runtime.o generated.o

# Clean up all generated artifacts
clean:
	$(DUNE) clean
	rm -f *.o program generated.c debug_program

.PHONY: all clean c_runtime debug 
