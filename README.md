# Sessynth

A type-driven synthesizer for session-typed programs. You write the protocol a
process should follow, leave a hole where the code goes, and Sessynth fills it
in by proof search in intuitionistic linear logic. Every program it gives back
is well-typed by construction, and gets re-checked by the host type checker.

It is plugged into **SessInt**, a session-typed functional language based on
SILL, which parses the program, fills the holes during type checking, and
compiles the result to Go.

## What's in here

| Path | What it is |
|---|---|
| `sessynth/` | The synthesizer itself, as a library |
| `sessynth/sessynth.ml` | The proof search: inversion, focusing, recursion, wandering, and the `synth` entry point |
| `sessynth/contexts.ml` | The four contexts (Γ, Ψ, constructors, Δ) and everything that touches them |
| `sessynth/polymorphism.ml` | Type schemes: instantiation, skolemization, unification |
| `sessynth/adts.ml` | Algebraic data types: building values and matching on them |
| `sessynth/cvc5adapter.ml` | Refinement types, solved by handing a SyGuS problem to `cvc5` |
| `sessynth/choiceUtils.ml` | Helpers over the backtracking monad, including the "first *n* distinct results" consumer |
| `sessynth/flags.ml` | Search state: depth, fresh names, and the stats counters |
| `sessynth/test/` | Unit tests for polymorphism and ADTs |
| `sessint/` | The host language: lexer, parser, type checker, Go compiler, interpreter |
| `sessint/lib/sessynthAdapter.ml` | The bridge: turns a hole into a `synth` call and the answer back into SessInt |
| `sessint/test/` | Example programs with holes in them |
| `sessint/test/rules/` | Golden regression tests, one or two synthesis rules each |
| `linear_type_checker/` | An early standalone prototype, not part of the build |

`sessynth/main.ml`, `sessynth/z3adapter.ml` and the `.sl` files are old
experiments and aren't built either.

## Requirements

- OCaml 5.3 and `opam`
- The opam packages:
  ```
  opam install dune menhir choice sexplib z3 logs core core_bench core_unix
  ```
- `cvc5` 1.1.2 on your `PATH`, only needed for refinement types. Without it
  those holes just report `No valid expression for the provided type`.
- Go, to run the compiled output

## Quick start

```
dune build
dune exec ./sessint/bin/main.exe sessint/test/program2.sessint true false
go run program2.go
```

This synthesizes a recursive stream server, prints it, and writes the compiled
program to `program2.go` in whatever directory you ran it from.

## Running it

```
dune exec ./sessint/bin/main.exe FILE STRUCT TIMES [DEPTH] [MODE]
```

| Argument | What it does |
|---|---|
| `FILE` | The `.sessint` program to compile |
| `STRUCT` | `true`/`false`: whether the Go backend packs multiple sends into a struct. Usually `true` |
| `TIMES` | `true` benchmarks the compiler instead of compiling. Usually `false` |
| `DEPTH` | Optional. How deep one branch of a derivation may go. Defaults to `100`, which is usually far more than needed |
| `MODE` | Optional. `interactive` (default) lets you pick a solution; `auto` just takes the first one |

Environment variables:

- `SESSYNTH_STATS=1` prints, per hole, how many rules the search entered, how
  many branches hit the depth limit, and how many times it called `cvc5`.
- `SESSYNTH_DEBUG=1` traces every rule the search enters. Very noisy.

### Interactive mode

If a hole has more than one solution, you get them all as a numbered list.
Type some text to keep only the solutions that contain it, and repeat to
narrow further (an empty line keeps everything). Then type the number of the
one you want.

## Writing holes

A hole sits where a declaration's body would go:

```
stype IntCStream rec x. &{next: int^x, stop: @};
fib : int -> int -> {IntCStream}
? fib ?;
```

| Hole | Meaning |
|---|---|
| `? T ?` | Give me one term of type `T` |
| `? T #n ?` | Give me up to `n` distinct terms of type `T` |
| `? name ?` | Use the type `name` was declared with |

Types you can write in a hole:

| Syntax | Meaning |
|---|---|
| `int`, `bool` | Base types |
| `A -> B` | Function |
| `{x:int \| x > 0}` | Refinement type |
| `{S}` | A process offering session `S` |
| `{S <- c:S1, d:S2}` | A process offering `S` while using channels `c` and `d` |

And sessions:

| Syntax | Meaning |
|---|---|
| `A ^ S` | Send a value of type `A`, then continue as `S` |
| `A => S` | Receive a value of type `A`, then continue as `S` |
| `(S1) * S2` | Send a channel |
| `(S1) -o S2` | Receive a channel |
| `&{l1: S1, l2: S2}` | Let the other side pick a label |
| `+{l1: S1, l2: S2}` | Pick a label yourself |
| `@` | Done |
| `rec x. S` | Recursive session, unfolded once during search |
| `rec 2 x. S` | Same, but unfolded up to 2 times (more solutions, bigger search) |
| `Name` | A session type declared with `stype` |

## Tests

```
./sessint/test/rules/run.sh           # the 40 golden tests, diffed against their .expected files
./sessint/test/rules/run.sh --bless   # regenerate the .expected files from the current output
dune test                             # the polymorphism and ADT unit tests
```

`sessint/test/rules/readme.md` explains what each golden test checks and which
bug it was written against.

## Good to know

- Polymorphism and ADTs work inside the synthesizer, but SessInt has no syntax
  for them yet, so right now only the unit tests can reach them.
- Only top-level declarations can be recursive in SessInt, so a synthesized
  local recursive function is refused with an explanation instead.
- Compiled `.go` files land in the directory you run from; the ones at the repo
  root are gitignored.
- The Go backend skips top-level declarations that are just a process (no
  function arrow), so if `main` uses one, give it a parameter. Forwarding a
  channel you *received* also produces Go that won't compile.
- If a hole takes forever, try a smaller `DEPTH`. `pipeline.sessint` is
  instant at depth 20 but doesn't finish at the default 100.
