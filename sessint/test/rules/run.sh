#!/usr/bin/env bash
# Regression tests for individual synthesis rules.
#
#   ./sessint/test/rules/run.sh           run every test and diff against its golden file
#   ./sessint/test/rules/run.sh --bless   regenerate the golden files from current output
#
# Each test pins the synthesis fuel budget (the optional 4th argument to
# main.exe) so that the search space is small, fast and deterministic; the
# default budget of 100 makes most of these intractable.
# Tests deliberately avoid session types with external choice, since those make
# the synthesizer prompt for input.

set -u
cd "$(dirname "$0")/../../.." || exit 1

BLESS=0
[ "${1:-}" = "--bless" ] && BLESS=1

# test name : fuel budget
TESTS=(
    "arrow_left_2args:12"
    "arrow_left_3args:16"
    "arrow_left_mixed:12"
    "arrow_right_lambda:12"
    "intchoice_left:20"
    "intchoice_left_branches:20"
)

DIR=sessint/test/rules
pass=0; fail=0

dune build 2>&1 | head -20

for entry in "${TESTS[@]}"; do
    name="${entry%%:*}"
    fuel="${entry##*:}"

    # keep only the numbered solution block; drop the interactive tail
    actual=$(timeout 60 \
                dune exec ./sessint/bin/main.exe "$DIR/$name.sessint" true false "$fuel" \
                </dev/null 2>&1 \
             | sed -n '/^[0-9]*:$/,$p' \
             | sed '/^Select solution: *$/,$d')

    if [ "$BLESS" = "1" ]; then
        printf '%s\n' "$actual" > "$DIR/$name.expected"
        echo "blessed  $name (fuel $fuel)"
        continue
    fi

    if [ ! -f "$DIR/$name.expected" ]; then
        echo "MISSING  $name has no golden file; run with --bless"
        fail=$((fail+1))
        continue
    fi

    if diff -q <(printf '%s\n' "$actual") "$DIR/$name.expected" >/dev/null; then
        echo "pass     $name (fuel $fuel)"
        pass=$((pass+1))
    else
        echo "FAIL     $name (fuel $fuel)"
        diff <(printf '%s\n' "$actual") "$DIR/$name.expected" | sed 's/^/         /'
        fail=$((fail+1))
    fi
done

[ "$BLESS" = "1" ] && exit 0
echo
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ] || exit 1
