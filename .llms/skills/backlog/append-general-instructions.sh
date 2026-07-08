#!/usr/bin/env bash
# Append a "# How to Implement This Task" section with universal
# implementation guidance to backlog task files.
#
# Usage:
#   append-general-instructions.sh                              # all NNN-*.md in /tmp/backlog/
#   append-general-instructions.sh /tmp/backlog/                # explicit directory
#   append-general-instructions.sh /tmp/backlog/005-foo.md      # single file
#
# Idempotent: files that already contain the marker heading are skipped.
set -euo pipefail

MARKER='# How to Implement This Task'

read -r -d '' GENERAL_INSTRUCTIONS <<'END_GENERAL' || true
# How to Implement This Task

These are the universal expectations for every backlog task. The sections
above hold task-specific extensions to these — read both before starting.

## Standard acceptance criteria

In addition to any task-specific criteria stated above, every task must
satisfy:

- [ ] Bug reproduced **before** the fix using the project's normal QA
      mechanism (browser session, CLI invocation, API request, exercising
      script — whatever fits). Evidence saved under
      `/tmp/backlog/screenshots/` or `/tmp/backlog/transcripts/`.
- [ ] Fix verified **after** the change with a second piece of evidence
      for comparison.
- [ ] The project's typecheck, lint, and test commands all pass. Look up
      the actual commands from `package.json`, `Makefile`, `CONTRIBUTING.md`,
      or the project's agent docs — do not assume `npm` if it's a Rust /
      Python / Go / etc. project.
- [ ] Targeted regression test added or updated where it would have caught
      this bug.

## Going one level deeper

Beyond the literal bug, always consider:

- Does this same problem exist in adjacent surfaces? Search for the pattern.
- Is this a symptom of an architectural issue (a missing normalization
  step, a coupling between layers, a silent error swallow)? If so, fix the
  root cause.
- Could a logging or debug-surface improvement make this class of bug
  cheaper to diagnose next time? Add it.
- Are there related issues you noticed while testing that should become
  follow-up tasks? File them as new task files in `/tmp/backlog/` using the
  same template (load the `backlog` skill again to do this).

If the task above lists task-specific deeper considerations, treat those as
additions to this list, not replacements.

## QA blocker policy

If you cannot reproduce this issue, that is a **hard blocker**, not a
reason to skip the task. Options in order of preference:

1. Re-read the problem description above to nail down the surface, then
   try again from a clean state (fresh browser session, fresh fixture,
   fresh DB).
2. Inspect the underlying state directly (DB query, RTDB curl, log file,
   internal API endpoint) to see whether the data is in the expected shape.
3. Build a temporary debug surface (URL param, debug-overlay button,
   verbose log line, dedicated CLI flag) that exposes the relevant state,
   then reproduce.
4. Ask the user for clarification only after 1-3 have failed.

Do not declare the task complete without post-fix evidence showing the
expected behavior.

## UX expectations

Quality matters even when fixing a small bug. While addressing the literal
issue:

- Look at the final result with a critical eye. For UIs, evaluate the
  rendered surface as a designer would. For CLIs, read the output as a
  first-time user would. For APIs, read the response as a consumer would.
- Adjust spacing, copy, error messages, affordances, or adjacent
  components/commands if the fix exposes an awkward result.
- Prefer changes that make the surface clearer for a first-time user over
  micro-optimizations.
END_GENERAL

append_to_file() {
  local file="$1"
  if [ ! -f "$file" ]; then
    echo "missing: $file" >&2
    return 1
  fi
  if grep -qF "$MARKER" "$file"; then
    echo "skip (already has '$MARKER'): $file"
    return 0
  fi
  # Ensure file ends with a newline before appending.
  local last_char
  last_char=$(tail -c1 "$file"; printf x)
  last_char=${last_char%x}
  if [ -n "$last_char" ] && [ "$last_char" != $'\n' ]; then
    printf '\n' >> "$file"
  fi
  printf '\n%s\n' "$GENERAL_INSTRUCTIONS" >> "$file"
  echo "appended: $file"
}

main() {
  local target="${1:-/tmp/backlog/}"
  if [ -d "$target" ]; then
    shopt -s nullglob
    local files=("${target%/}"/[0-9][0-9][0-9]-*.md)
    if [ ${#files[@]} -eq 0 ]; then
      echo "no NNN-*.md files found in $target" >&2
      exit 1
    fi
    local f
    for f in "${files[@]}"; do
      append_to_file "$f"
    done
  elif [ -f "$target" ]; then
    append_to_file "$target"
  else
    echo "not a file or directory: $target" >&2
    exit 1
  fi
}

main "$@"
