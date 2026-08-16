# Per-rule regression tests

Small synthesis problems, each aimed at one or two synthesis rules, with the
expected output pinned in a `.expected` golden file.

```
./sessint/test/rules/run.sh           # run everything, diff against the goldens
./sessint/test/rules/run.sh --bless   # regenerate the goldens from current output
```

Two things keep these tests usable:

- **Each test pins the fuel budget as the optional 4th argument to `main.exe`.** The default budget of 100 makes most
  of these intractable, because `focus_left_F` synthesizes a function's
  arguments *before* checking whether its return type can match the goal, so a
  focus that is doomed to fail still explores its whole argument space first.
  A small budget keeps that bounded and the output deterministic.
- **No external choice.** A session type containing `&{...}` sends the
  synthesizer into `synth_interactive_ext_choice`, which prompts on stdin.

Note that the declaration being synthesized is in scope for its own hole
(`check_decl` adds it to the environment before checking the body), which is
what lets a recursive process refer to itself. At a base type that shows up as
a circular solution such as `r` for `r : int`, so it appears in the goldens.

## The tests

| test | rules exercised | what it pins down |
|---|---|---|
| `arrow_left_2args` | →L | a two-argument application is built left-nested as `((add) a1) a2` |
| `arrow_left_3args` | →L | the spine is not limited to arity 2, and stays left-associated |
| `arrow_left_mixed` | →L | arguments land in the right *slots*: `pick : int -> bool -> int` must yield `((pick) r) true`, never the reverse |
| `arrow_right_lambda` | →R, →L | one lambda per arrow, each bound variable enters Ψ, and →L can then focus on it |

## What they catch

All four fail against the previous placeholder-and-substitute implementation of
→L, in distinct ways:

- `arrow_left_mixed` produced **no output at all** — the old `subst` handled
  only `Var`/`App`/`Lam` and raised on the `bool` literal argument
  (`Fail("subst pattern matching not defined for true")`).
- `arrow_left_3args` and `arrow_right_lambda` produced different argument
  assignments (`(((add3) 1) r) r` vs `(((add3) r) r) 1`), because the old rule
  synthesized arguments right-to-left and handed both premises the same Δ,
  rather than threading it left-to-right.
- `arrow_left_2args` produced fewer solutions within the same budget.
