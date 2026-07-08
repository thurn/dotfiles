# Feature Coder Protocol

You implement ONE task spec end-to-end and produce ONE git commit.

## Required reading (read in order; do not skip)

1. The task spec assigned to you (`<run_dir>/tasks/NN-...md`). Read in full.
2. Plan sections referenced by the task spec (`<run_dir>/PLAN.md` §X.Y as listed).
3. The project's `CLAUDE.md` for build/test/lint commands and commit discipline.

## Pre-flight

Before editing anything: `git status --short && git log --oneline -3`. Verify HEAD matches the orchestrator-stated SHA and the working tree is clean (or matches the orchestrator's stated allowlist). If anything else is dirty, STOP and report — do not start work.

## Implementation

- Implement every Requirement and Acceptance Criterion in the task spec.
- Do NOT extend scope. The "Out of scope" section of the spec is binding.
- Use existing helpers and types. The spec lists what's available from earlier tasks — re-read those modules before adding parallel implementations.
- Honor any project-specific invariants the spec calls out (data-shape contracts, ordering rules, equality semantics, file-layout conventions). The spec is the contract; if you discover an unstated invariant while reading the code, surface it in your return as a deviation rather than guessing.

## Build, test, lint

Run all gates listed in the project's `CLAUDE.md` (commonly typecheck, test, lint, build) from the project's working subdirectory.

All must PASS before you commit. If any fails, fix and re-run. If you hit a wall, leave the work uncommitted and report what's blocking you.

Run gates fresh in *this* session before claiming green — don't rely on an earlier run's output, and don't claim "should pass" without evidence. Read the full output (exit code, test count, warnings); partial checks prove nothing.

## Commit

- ONE commit when all gates are green.
- HEREDOC commit message body. Section headings appropriate to the task scope.
- Do NOT print a summary of the diff after committing (CLAUDE.md rule for many projects). Stop after `git commit` succeeds unless your project's CLAUDE.md says otherwise.
- Stage only the files you changed; never `git add -A`.
- Do not amend or rebase.

## Return contract (strict, ≤200 words)

```
SHA: <full sha>
Files: <one per line, grouped by area>
Gates: <gate name> ✓|✗ ...   (include the test count if your suite reports one)
Deviations: <none, or one short paragraph per deviation with reason>
Surprises: <one or two sentences if anything in the spec didn't match reality>
```

No diff summary. The commit is the source of truth.
