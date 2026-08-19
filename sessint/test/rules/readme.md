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
- **Each test runs in auto mode** (the optional 5th argument), so the
  synthesizer takes the first solution instead of prompting. Interactive mode is
  the default when running by hand.

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
| `psi_scope_lambda` | →R, →L | a binding introduced while synthesizing one argument is *not* in scope for the next one |
| `psi_scope_tensor` | ⊗R, ⊃R | the same across the two halves of `S1 ⊗ S2`: what the sub-process receives is not in scope for the continuation |
| `psi_scope_letrec` | →R (recursive) | and again when the lambda is the recursive one, whose self-binding also enters Ψ |
| `intchoice_left` | ⊕L | every label is covered, and each branch continues on the *same* channel the `case` scrutinizes |
| `intchoice_left_branches` | ⊕L | that channel carries each branch's own continuation type: `a: int^@` receives before waiting, `b: @` waits straight away |
| `fwd_recursive` | fwd | `fwd t c` is allowed when `t` and the goal are the same protocol at different unfolding budgets (`𝜇¹` against `𝜇⁰`) |
| `fwd_type_mismatch` | fwd | a `bool` stream is never forwarded onto an `int` stream goal; the only forwards are of channels obtained by spawning |
| `fwd_leftover_channel` | fwd | with two input channels, both are consumed before forwarding |
| `hole_named_inputs` | goal syntax | a hole can take more than one input channel, and each is reachable under its own name |
| `process_ambient_capture` | process right inversion | a synthesized process value uses only the channels its own type declares, never one the caller happened to be holding |
| `stype_not_spawnable` | adapter | a session-type declaration is not offered to the search as a process it can spawn |
| `rec_inline_goal` | goal syntax | a recursive session type can be written directly in a hole, binder and occurrence both |
| `unbound_recvar_goal` | goal validation | a recursion variable with no enclosing `rec` is named in the error, not left to surface as "no valid expression" |
| `unbound_recvar_declr` | goal validation | the same holds when the goal is `? name ?`, where the offending type is reached through Ψ rather than written in the hole |
| `duplicate_input_names` | goal validation | two input channels sharing a name are rejected instead of silently hiding one |
| `extchoice_right` | &R | every label is offered, and each branch drives the input channel with the matching label |

A test whose input is meant to be *rejected* has no solution block, so `run.sh`
pins the `Synthesis error: …` line instead — the message a user actually sees,
never an OCaml backtrace, which carries file and line numbers and would churn on
every edit.

`extchoice_right` could not have existed before external choice became a plain
monadic rule. `&{…}` used to route into `synth_interactive_ext_choice`, which
enumerated each label with `Choice.run_all` and prompted on stdin, so any test
touching it hung or produced a truncated per-label listing rather than a
program.

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

`rec_inline_goal` and `unbound_recvar_goal` do not parse at all before
`sessynth_tyS` gained a production for a recursion variable *occurrence* — it
had one for the `rec x.` binder but none for the `x` referring back to it.
`unbound_recvar_declr` fails differently: it parses, but without Ψ being
validated the goal `? bad ?` is just a `TDeclr`, the offending type is never
looked at, and the search reports `No valid expression for the provided type`.

`psi_scope_lambda` is the only test where a synthesized *argument* introduces a
binding, which is what it takes to see Ψ leaking out of a subderivation — the
`arrow_left_*` tests all use `int` arguments, which bind nothing. Without
`Contexts.restore_scope` it gains three solutions, each ending in a bare `_x0`
that only the preceding lambda binds:

```
((apply) _x0 -> _x0) _x0
((apply) _x0 -> r) _x0
((apply) _x0 -> 1) _x0
```

sessint rejects all three (`NoSuchArg`), so they were never programs — they only
consumed fuel and solution slots, and had one of them come out first, auto mode
would have failed the whole compile.

`psi_scope_tensor` covers the other route to the same leak, the one that runs
through a sibling *subderivation* rather than a sibling argument. Without the fix
it gains

```
send _c0 (_c1 <- _x0 <- recv _c1; close _c1);
send _c0 _x0;
close _c0
```

where `_x0` is bound inside the process offered on `_c1` and referenced from the
continuation on `_c0`. Its golden also pins the ⊗ half of the adapter: `SendChan`
only forwards a channel already in the linear context, so `SendS` has to desugar
into a spawn followed by the send. Reverting that desugaring fails this test with
`NoSuchChannelInContext: _c1`, because the channel the sub-process offers on is
then never bound.

`stype_not_spawnable` covers the other adapter filter. `stype Stream …;` is
recorded by the parser as a *term* binding `Stream : {Stream}` so that the name
resolves later, and `check_program` puts every declaration into the environment
the adapter hands to Ψ — where it reads as a spawnable process. Without the
filter, solution 1 is `_c1 <- spawn Stream`, which is not a term at all. Type
aliases are kept, since `TDeclr` is resolved against Ψ and a goal written
`? _foo ?` needs its binding.

`process_ambient_capture` needs `wrap : {Stream} -> {Stream}`, because a process
value only picks up an ambient Δ when it is synthesized somewhere that already
holds channels — here, as the argument of a spawned function, inside a process
that declares `t`. Without the swap, solution 2 is

```
_c1 <- spawn (wrap) _c2 <- {
         send _c2 _x0;
         fwd t _c2
       };
```

where the inner process declares no inputs at all yet forwards `t`, which
belongs to the enclosing one. sessint answers `NoSuchChannelInContext: t`. At
fuel 30 that accounted for 186 of 200 solutions; with the swap, 0 of 34.

`psi_scope_letrec` exists because the two above both take the plain `TArrow`
branch of `invert_right_F`: their lambdas return a base type. Only an argument
whose type is a *recursive process generator* reaches the branch that emits
`LetRec` and puts the self-binding in Ψ alongside the parameter, so only this
test fails when that branch's `restore_scope` is the one removed. Its program
does not survive `Compiler.compile_type`, which is why `run.sh` drops OCaml
backtraces: a test can synthesize correctly and still trip the Go compiler
downstream, and that is not what these goldens pin.

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
- none of them notice if the `delta_is_empty` guard is dropped, and neither does
  anything else: dropping it leaves `intqueue.sessint` at exactly 286 complete
  programs. `synth` already requires an empty Δ of a finished solution, so that
  guard is early pruning and nothing more. (An earlier note here claimed it
  removed five leaking `spawn`-and-forward solutions from `intqueue`. That was
  measured against the old interactive external choice, where the printed
  "solutions" were per-label branch candidates rather than programs.)
