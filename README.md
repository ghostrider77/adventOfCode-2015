# Advent of Code (2015)

**Language**: OCaml 5.3

**Packages**:
* *Yojson*
* *Str*

**Environment**

```bash
opam switch create aoc2015 ocaml-base-compiler.5.3.0
eval $(opam env --switch=aoc2015)
opam install -y utop merlin num ocaml-lsp-server
```

**Run** (in REPL):
```bash
cat input01.txt | ocaml src/day01a.ml
```

**Compile and Run**:
```bash
# day12b.ml depends on Yojson
ocamlfind ocamlopt -o prog -package yojson -linkpkg day12b.ml

cat input12.txt | ./prog
```
