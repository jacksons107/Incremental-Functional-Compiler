###############################################################################
# Makefile – OCaml → assembly → object → exe, plus C runtime
#
#   $ make        # build everything → ./prog
#   $ make run    # build & run ./prog
#   $ make clean  # delete all generated files
###############################################################################

# ─── Toolchain ────────────────────────────────────────────────────────────────
DUNE      := dune
ASM       := as                        # macOS: /usr/bin/as
CC        := clang                    # or `gcc`
ARCH      := arm64                    # set to x86_64 on Intel Macs / Linux
CFLAGS    := -arch $(ARCH) -c
LDFLAGS   := -arch $(ARCH)

# ─── File names ──────────────────────────────────────────────────────────────
OCAML_EXE := bin/main.exe             # built by dune (relative to project root)
ASM_SRC   := entry.s                  # generated by main.exe
ASM_OBJ   := entry.o
C_SRC     := runtime.c
C_OBJ     := runtime.o
TARGET    := prog

# ─── Public targets ──────────────────────────────────────────────────────────
.PHONY: all run clean
all: $(TARGET)

run: $(TARGET)
	@./$(TARGET)

clean:
	$(DUNE) clean
	rm -f $(ASM_SRC) $(ASM_OBJ) $(C_OBJ) $(TARGET)

# ─── Build graph ─────────────────────────────────────────────────────────────
# prog  ←──  runtime.o  runtime.c
#      ↘
#       ←──  entry.o   ←──  entry.s   ←──  bin/main.exe   ←──  dune build
#                                                        ↖───────────────┘
###############################################################################

$(TARGET): $(C_OBJ) $(ASM_OBJ)
	$(CC) $(LDFLAGS) $(C_OBJ) $(ASM_OBJ) -o $@
	@echo "✅ Built $@"

$(C_OBJ): $(C_SRC)
	$(CC) $(CFLAGS) -o $@ $<

$(ASM_OBJ): $(ASM_SRC)
	$(ASM) -arch $(ARCH) -o $@ $<

# Generate entry.s by running the OCaml compiler pipeline
$(ASM_SRC): $(OCAML_EXE)
	$(DUNE) exec $(OCAML_EXE)

# Build bin/main.exe (and any other OCaml targets) with dune
$(OCAML_EXE):
	$(DUNE) build $(OCAML_EXE)
