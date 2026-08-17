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
| `intchoice_left` | ⊕L | every label is covered, and each branch continues on the *same* channel the `case` scrutinizes |
| `intchoice_left_branches` | ⊕L | that channel carries each branch's own continuation type: `a: int^@` receives before waiting, `b: @` waits straight away |
| `fwd_recursive` | fwd | `fwd t c` is allowed when `t` and the goal are the same protocol at different unfolding budgets (`𝜇¹` against `𝜇⁰`) |
| `fwd_type_mismatch` | fwd | a `bool` stream is never forwarded onto an `int` stream goal; the only forwards are of channels obtained by spawning |
| `fwd_leftover_channel` | fwd | with two input channels, both are consumed before forwarding |
| `hole_named_inputs` | goal syntax | a hole can take more than one input channel, and each is reachable under its own name |

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

Both `intchoice_left*` tests fail against the previous ⊕L rule, which bound each
branch continuation to a *fresh* channel while still emitting `case c' of ...`.
The branch bodies then referenced a channel that was never introduced, and
sessint rejected the result with `NoSuchChannelInContext: _c1`.

`hole_named_inputs` cannot even be expressed against the previous grammar:
`sessynth_tyS_list` named every input `"_"`, so a hole could carry at most one
usable input channel — with two, `List.assoc` and `consume_channel` both only
ever found the first, and the second was unreachable. That is also why the two
`intchoice_left*` goldens used to end in an `UnexpectedType`: the synthesized
process offered `_` where the declaration named `q`.

The `fwd_*` tests cover the two guards `synth_fwd` gained, but not equally:

- `fwd_type_mismatch` fails outright without the type guard — it synthesizes
  `fwd t _c0` forwarding a `bool` stream onto an `int` stream goal.
- all three fail if the guard is weakened from `tyS_equiv` to `=`, since the
  channel and the goal almost always sit at different unfolding budgets.
- none of them notice if the `delta_is_empty` guard is dropped. That is expected:
  `synth` already requires an empty Δ of a finished solution, so a leaked channel
  cannot reach the output by that route. The guard earns its place elsewhere — it
  removed five leaking solutions from `intqueue.sessint`, all of the shape
  `_c1 <- spawn IntQueue; ...; fwd _c2 _c0`, which survive the top-level check
  only because external-choice branch merging keeps just branch 0's contexts.
