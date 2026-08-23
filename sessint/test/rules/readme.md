# Per-rule regression tests

Small synthesis problems, each aimed at one or two synthesis rules, with the
expected output pinned in a `.expected` golden file.

```
./sessint/test/rules/run.sh           # run everything, diff against the goldens
./sessint/test/rules/run.sh --bless   # regenerate the goldens from current output
```

Two things keep these tests usable:

- **Each test pins the depth budget as the optional 4th argument to `main.exe`.**
  The budget bounds how deep one branch of a derivation may go; the default of
  100 leaves the search space large enough that most of these produce hundreds of
  solutions, so a small budget keeps the output short and deterministic.
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
| `gamma_goal_named` | defn R | a goal naming a declared session type resolves it against Γ, and lands on the same program the protocol written out would |
| `gamma_goal_nested` | defn R | the same when the name sits under a send rather than at the top of the goal |
| `gamma_input_channel` | defn L | a declared name on an *input channel* is resolved under focus and written back into Δ, so the channel can then be left-inverted |
| `gamma_under_rec` | defn, μ | a declaration inside a recursive type does not stop the unfolding |
| `gamma_sync_channel` | defn L, focus | the same when what the name declares is left-*synchronous*, so focus is kept rather than released |
| `gamma_fwd_declr` | defn, fwd | `fwd` compares a channel's declared name against the goal up to Γ |
| `gamma_undeclared` | goal validation | a session type no declaration defines is named in the error |
| `refinement_arg_hypothesis` | refinement R | an argument's predicate is what may be *assumed* of it, not a second thing to prove |
| `refinement_binder_substring` | refinement R | the goal's binder is substituted at its occurrences, so a longer name containing it is left alone |
| `refinement_duplicate_binder` | refinement R | two refinements sharing a binder are rejected rather than naming one SyGuS symbol twice |

The `gamma_*` tests are the only ones where a session-type *declaration* reaches
the synthesizer at all. Everywhere else it cannot: `check_decl` runs
`expand_custom_type` before checking, so every type in Ψ and Δ arrives with its
names already expanded, and `expand_custom_exp` leaves `Synth` alone — a hole's
own goal is the one place a name survives. That is why Γ went unused for so long
without anything failing.

The three `refinement_*` tests shell out to `cvc5`, so they need it on `PATH`.
Without it the search sees an empty reply, the branch fails, and they report
`No valid expression for the provided type`.

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
consumed depth and solution slots, and had one of them come out first, auto mode
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
depth 30 that accounted for 186 of 200 solutions; with the swap, 0 of 34.

`psi_scope_letrec` exists because the two above both take the plain `TArrow`
branch of `invert_right_F`: their lambdas return a base type. Only an argument
whose type is a *recursive process generator* reaches the branch that emits
`LetRec` and puts the self-binding in Ψ alongside the parameter, so only this
test fails when that branch's `restore_scope` is the one removed. Its program
does not survive `Compiler.compile_type`, which is why `run.sh` drops OCaml
backtraces: a test can synthesize correctly and still trip the Go compiler
downstream, and that is not what these goldens pin.

The `refinement_*` tests all pin the SyGuS problem the adapter builds, which
nothing covered before. `refinement_arg_hypothesis` is the reason no refined
*function* type could be synthesized at all: an argument's predicate was emitted
as a constraint of its own, and since a `declare-var` is universally quantified,

```
(declare-var x Int)
(constraint (> x 0))
(constraint (> (y x) x))
```

claims that every integer is positive, so cvc5 answers `infeasible` for any goal
with a refined argument. It is now an antecedent, `(constraint (=> (> x 0) (> (y
x) x)))`, and the same goal synthesizes `x -> 1 + x`.

`refinement_binder_substring` covers the substitution of the goal's binder for
the application of the function being synthesized. That used to be a
`Str.global_replace` over the *finished* constraint string, so with the binder
`y` and a `yy` in scope, `(> y yy)` came out as

```
(constraint (> (y r yy) (y r yy)(y r yy)))
```

— three applications where one of them should still be a variable. Substituting
at the `RTVar` occurrence instead cannot see inside a name.

`refinement_duplicate_binder` is the one rejection of the three. `{x:int | x > 0}
-> {x:int | x > 1}` names the function `x` and its parameter `x`, so the problem
carries both `(synth-fun x ((x Int) …))` and `(declare-var x)`; cvc5 answers with
nothing at all, which reaches the search as an unparseable reply and reads as
"no valid expression". The check is over the binders down the goal's arrow spine
together with the names already in Ψ, which is exactly the set that becomes SyGuS
symbols.

Each `gamma_*` test pins one place a declaration has to be looked through, and
each fails on its own when that one is removed. `gamma_goal_named`,
`gamma_goal_nested` and `gamma_under_rec` go through the right rule; the last of
those also needs `unfold` to treat a declaration as a leaf rather than giving up
on the whole type, which is what it used to do.

`gamma_input_channel` and `gamma_sync_channel` are the two halves of the left
rule, and they differ in what the name denotes. A declaration is left-synchronous
in its own right — nothing can be inverted through a name — so both are reached by
the decide rule, and the left focus rule re-types the channel at what Γ says
before carrying on. `gamma_sync_channel` names a receive, which is still
synchronous, so focus is kept and the very next rule decomposes it;
`gamma_input_channel` names a send, which is asynchronous, so the same rule falls
through to `invert_left_S` — the ordinary release. That fall-through is why the
rewrite into Δ is not optional: without it the released judgment reads the same
name out of Δ again and loops back through decide until the depth runs out, which
is what the "delta not rewritten" line below fails on.

`gamma_fwd_declr` is the only one that reaches `tyS_equiv` with a name on one side
and the protocol on the other. Without Γ being built at all, all six produce
nothing.

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
  anything else: dropping it leaves `intqueue.sessint`'s 516 solutions
  byte-identical. `synth` already requires an empty Δ of a finished solution, so that
  guard is early pruning and nothing more. (An earlier note here claimed it
  removed five leaking `spawn`-and-forward solutions from `intqueue`. That was
  measured against the old interactive external choice, where the printed
  "solutions" were per-label branch candidates rather than programs.)
