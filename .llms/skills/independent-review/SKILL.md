---
name: independent-review
description: Get a second opinion on a nontrivial code change by running Claude Code as an independent single-pass reviewer, then verify each finding against the real code and fix only what is confirmed. Use after finishing an implementation and before declaring it done, or when explicitly asked for an independent review. Triggers on independent review, second opinion, review my change, review this branch, have Claude review, /independent-review.
---

# Independent Review

After a nontrivial code change, a fresh reviewer that has not been inside your
own reasoning catches things you cannot. This skill runs Claude Code
non-interactively over the change, then puts *you* in charge of deciding which
of its findings are real.

The reviewer is deliberately narrow: **bugs and architectural problems only** —
correctness defects, brittle code, brittle tests, missing coverage for changed
behavior, and duplication that will diverge. It is instructed not to raise
style nits, naming, aesthetic preferences, or security findings. If it raises
one anyway, drop it.

The reviewer never edits files. Every fix is yours to make and yours to justify.

## When to use it

Use it when the change is nontrivial: new behavior, a bug fix with real logic,
a refactor that moves responsibilities, anything touching state, ordering,
persistence, or a contract other code depends on. Skip it for typo fixes,
comment edits, pure data/config tweaks, and mechanical renames.

## Workflow

Do these in order. Do not skip step 1 — sending a change with failing tests to
review wastes the review on problems your own tooling already knows about.

### 1. Get the change green first

Run the project's own tests, formatter, linter, and typechecker, and make them
pass. Follow whatever the repository's instructions say; if it defines a single
aggregate check, run that. Fix what they report before continuing.

### 2. Run the review script

```bash
~/.llms/skills/independent-review/scripts/independent-review.sh --prompt "<the original task prompt>"
```

The script prints one line on stdout: the path to the review file under
`/tmp/reviews/`. Read that file.

The review can take several minutes and intentionally emits no partial
findings. Treat it as one blocking operation. When the command runner yields a
process or session handle, use the longest supported waits and, when available,
keep those waits inside one tool-orchestration call so unchanged polls do not
reinvoke the model. Use tool-level progress notifications if needed. Do not
narrate unchanged status, poll through repeated model turns, or restart a quiet
review.

**Pass the original prompt.** Supply the text of the request that created this
work — the actual task the change was supposed to accomplish, verbatim where
possible, not a summary of what you built. It is what lets the reviewer notice
that part of the task was silently dropped or that the implementation answers a
different question than the one asked. For a long prompt, write it to a file
and use `--prompt-file`.

With no `--target`, the script reviews all committed and uncommitted changes on
the current branch relative to its base branch, including untracked files. That
is the right default; pass `--target` only when you specifically want something
else:

| `--target` | Reviews |
|---|---|
| *(omitted)* | committed + uncommitted changes on the current branch |
| `src/foo.ts` | that file or directory (its diff, or the whole file if unchanged) |
| `feature-branch` | that branch against the base branch |
| `abc123..def456` | that git ref range |
| `123` or `#123` | that GitHub pull request (needs `gh`) |

For an incremental follow-up, use `--since REF`. It reviews committed, staged,
and unstaged changes since that ref, plus every current untracked file in full.
Git cannot determine when an untracked file was created, so commit the
previously reviewed state before relying on this as a strict delta. `--since`
and `--target` are mutually exclusive.

Other options: `--base BRANCH` when the base branch is not `origin/HEAD`/
`main`/`master`, `--repo DIR` to review a different checkout (a worktree, for
example), `--model` / `--effort` to change the review model, `--out FILE` to
choose the output path, `--print-prompt` to see what would be reviewed without
spending a review. `--help` documents everything.

Exit codes: `64` usage error, `65` nothing to review, `69` Claude Code missing
or unauthenticated, `70` the review run failed, `130` interrupted, and `143`
terminated. On `69`, tell the user what the stderr message says — you cannot
fix their auth for them. Do not fall back to reviewing the change yourself and
presenting it as an independent review.

### 3. Verify every finding against the real code

This is the part that matters, and it is not optional. The reviewer saw a diff
and some files; it did not run the code and it does not know the project's
history or conventions. Treat each finding as a claim to be checked, not an
instruction to be executed.

For each finding: open the cited location, read enough surrounding code to
judge it, and decide whether the described failure can actually happen. Where
it is cheap, prove it — construct the input, run the affected test, or add a
temporary assertion. A finding that survives that check is real. A finding
whose premise is wrong about how the code behaves is not, no matter how
confidently it is written.

For findings about missing or changed behavior, verify the complete observable
contract rather than inspecting one changed component in isolation. Trace all
cooperating paths and mechanisms, compare the before/after result where useful
(including timing or ordering when relevant), and check whether the requirement
is already satisfied elsewhere. The absence of one proposed implementation is
not a defect when the required behavior is present.

Reject findings that are: contradicted by the code, about behavior that is
intentional and load-bearing, outside the scope of this change, style or
aesthetic preferences, or security observations — all of which are out of
scope here regardless of merit.

### 4. Fix only confirmed issues

Fix what you verified. Do not make changes to satisfy a finding you could not
confirm, and do not perform speculative hardening "since it was mentioned". If
a finding is real but a proper fix is clearly outside this change's scope, say
so in the report rather than half-fixing it.

### 5. Rerun the affected checks

After fixing, rerun the tests, lint, and typechecks that your changes could
affect — at minimum the ones covering the files you touched. If a fix was
substantial, rerun the full check from step 1.

### 6. Report the disposition of every finding

Tell the user, per finding, one of:

- **Accepted** — what was wrong, what you changed, and what now verifies it.
- **Rejected** — the specific evidence in the code that shows the finding is
  wrong or out of scope. Cite it; "I disagree" is not a disposition.
- **Unresolved** — real, but not fixed here, with the reason (out of scope,
  needs a product decision, needs the user's input) and what it would take.

Also state plainly if the reviewer returned nothing. Include the review file
path so the user can read the raw output themselves.

## Rules

- Never ask the reviewer to fix anything. It is read-only by construction and
  by prompt; keep it that way.
- Never present the reviewer's findings to the user as established facts before
  you have verified them.
- Never suppress a confirmed finding because it is inconvenient or because the
  fix is annoying. Report it as unresolved instead.
- Run one review pass per logical change. Small reviewer fixes and user
  corrections do not require another pass. If substantial fixes materially
  change behavior or architecture and warrant a second opinion, review only the
  incremental delta with `--since REF` when the previously reviewed state is
  committed and therefore has a stable ref. Do not loop to chase an empty
  finding list.
