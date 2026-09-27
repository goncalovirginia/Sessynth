#!/usr/bin/env bash
# Regression tests for individual synthesis rules.
#
#   ./sessint/test/rules/run.sh           run every test and diff against its golden file
#   ./sessint/test/rules/run.sh --bless   regenerate the golden files from current output
#
# Each test pins the synthesis depth budget (the optional 4th argument to
# main.exe) so that the search space is small, fast and deterministic; the
# default budget of 100 makes most of these intractable.
# Tests run in auto mode (the 5th argument), so the synthesizer takes the first
# solution rather than prompting; external choice is fine to use.

set -u
cd "$(dirname "$0")/../../.." || exit 1

BLESS=0
[ "${1:-}" = "--bless" ] && BLESS=1

# test name : depth budget
TESTS=(
    "arrow_left_2args:12"
    "arrow_left_3args:16"
    "arrow_left_mixed:12"
    "arrow_left_return_mismatch:12"
    "arrow_right_lambda:12"
    "psi_scope_lambda:16"
    "psi_scope_tensor:20"
    "guarded_self_spawn:20"
    "guarded_self_spawn_function:12"
    "psi_scope_letrec:25"
    "letrec_invented_name:25"
    "gamma_letrec_goal:25"
    "intchoice_left:20"
    "intchoice_left_branches:20"
    "intchoice_left_branch_names:14"
    "fwd_recursive:25"
    "fwd_type_mismatch:25"
    "fwd_leftover_channel:25"
    "hole_named_inputs:20"
    "process_ambient_capture:30"
    "stype_not_spawnable:14"
    "rec_inline_goal:25"
    "unbound_recvar_goal:15"
    "unbound_recvar_declr:15"
    "duplicate_input_names:15"
    "extchoice_right:30"
    "extchoice_branch_depth:8"
    "gamma_goal_named:14"
    "gamma_goal_nested:20"
    "gamma_input_channel:8"
    "gamma_under_rec:14"
    "gamma_sync_channel:12"
    "gamma_fwd_declr:10"
    "gamma_undeclared:6"
    "refinement_arg_hypothesis:6"
    "refinement_precedence:6"
    "refinement_binder_substring:6"
    "refinement_duplicate_binder:6"
    "unit_right:12"
    "unit_refinement:6"
)

DIR=sessint/test/rules
pass=0; fail=0

dune build 2>&1 | head -20

for entry in "${TESTS[@]}"; do
    name="${entry%%:*}"
    depth="${entry##*:}"

    raw=$(timeout 60 \
             dune exec ./sessint/bin/main.exe "$DIR/$name.sessint" true false "$depth" auto \
             </dev/null 2>&1)

    # keep only the numbered solution block; drop the interactive tail, and any
    # OCaml backtrace, which carries file and line numbers and would churn on
    # every edit. A test can synthesize fine and still trip the Go compiler
    # downstream, and that is not what these goldens are about.
    actual=$(printf '%s\n' "$raw" \
             | sed -n '/^[0-9]*:$/,$p' \
             | sed '/^Select solution: *$/,$d' \
             | sed '/^Uncaught exception:/,$d')

    # a test whose input is meant to be rejected produces no solution block, so
    # pin the message the user actually sees instead
    if [ -z "$actual" ]; then
        actual=$(printf '%s\n' "$raw" | grep '^Synthesis error: ' | head -1)
    fi

    if [ "$BLESS" = "1" ]; then
        printf '%s\n' "$actual" > "$DIR/$name.expected"
        echo "blessed  $name (depth $depth)"
        continue
    fi

    if [ ! -f "$DIR/$name.expected" ]; then
        echo "MISSING  $name has no golden file; run with --bless"
        fail=$((fail+1))
        continue
    fi

    if diff -q <(printf '%s\n' "$actual") "$DIR/$name.expected" >/dev/null; then
        echo "pass     $name (depth $depth)"
        pass=$((pass+1))
    else
        echo "FAIL     $name (depth $depth)"
        diff <(printf '%s\n' "$actual") "$DIR/$name.expected" | sed 's/^/         /'
        fail=$((fail+1))
    fi
done

[ "$BLESS" = "1" ] && exit 0
echo
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ] || exit 1
