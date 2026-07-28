#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
REVIEW_SCRIPT="$SCRIPT_DIR/independent-review.sh"
TEST_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/independent-review-test.XXXXXX")
trap 'rm -rf "$TEST_ROOT"' EXIT

fail() {
  printf '%s\n' "independent-review.test: $*" >&2
  exit 1
}

assert_status() {
  expected=$1
  actual=$2
  label=$3
  [ "$actual" -eq "$expected" ] ||
    fail "$label: expected exit $expected, got $actual"
}

make_repo() {
  repo=$1
  mkdir -p "$repo"
  git -C "$repo" init -q
  printf '%s\n' "before" >"$repo/tracked.txt"
  git -C "$repo" add tracked.txt
  git -C "$repo" -c user.name=Test -c user.email=test@example.invalid \
    commit -qm "Initial fixture"
  printf '%s\n' "after" >"$repo/tracked.txt"
}

make_fake_claude() {
  body=$1
  mkdir -p "$TEST_ROOT/bin"
  {
    printf '%s\n' '#!/bin/sh'
    printf '%s\n' "$body"
  } >"$TEST_ROOT/bin/claude"
  chmod +x "$TEST_ROOT/bin/claude"
}

set +e
"$REVIEW_SCRIPT" --prompt task --timeout 0 >"$TEST_ROOT/invalid.out" 2>"$TEST_ROOT/invalid.err"
status=$?
set -e
assert_status 64 "$status" "invalid timeout"
grep -q "timeout must be a positive integer" "$TEST_ROOT/invalid.err" ||
  fail "invalid timeout diagnostic missing"

make_repo "$TEST_ROOT/repo"
make_fake_claude 'printf "%s\n" "You'\''ve hit your monthly Agent SDK credit limit" >&2; exit 1'
set +e
PATH="$TEST_ROOT/bin:$PATH" REVIEW_OUTPUT_DIR="$TEST_ROOT/reviews" \
  "$REVIEW_SCRIPT" --prompt task --repo "$TEST_ROOT/repo" \
  >"$TEST_ROOT/quota.out" 2>"$TEST_ROOT/quota.err"
status=$?
set -e
assert_status 75 "$status" "usage limit"
grep -q "scripted usage is currently limited" "$TEST_ROOT/quota.err" ||
  fail "usage-limit diagnostic missing"

make_fake_claude 'trap "exit 143" TERM; while :; do sleep 1; done'
started=$(date +%s)
set +e
PATH="$TEST_ROOT/bin:$PATH" REVIEW_OUTPUT_DIR="$TEST_ROOT/reviews" \
  "$REVIEW_SCRIPT" --prompt task --repo "$TEST_ROOT/repo" --timeout 1 \
  >"$TEST_ROOT/timeout.out" 2>"$TEST_ROOT/timeout.err"
status=$?
set -e
elapsed=$(($(date +%s) - started))
assert_status 75 "$status" "timeout"
[ "$elapsed" -le 8 ] || fail "timeout took ${elapsed}s"
grep -q "timed out after 1s" "$TEST_ROOT/timeout.err" ||
  fail "timeout diagnostic missing"

make_fake_claude 'printf "%s\n" "VERDICT: clean" "Checked the change."'
PATH="$TEST_ROOT/bin:$PATH" REVIEW_OUTPUT_DIR="$TEST_ROOT/reviews" \
  "$REVIEW_SCRIPT" --prompt task --repo "$TEST_ROOT/repo" \
  >"$TEST_ROOT/success.out" 2>"$TEST_ROOT/success.err"
review_path=$(cat "$TEST_ROOT/success.out")
[ -s "$review_path" ] || fail "successful review file missing"
grep -q "^VERDICT: clean$" "$review_path" ||
  fail "successful review content missing"

printf '%s\n' "independent-review tests passed"
