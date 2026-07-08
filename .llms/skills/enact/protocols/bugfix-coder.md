# Bugfix Coder Protocol

You fix bugs reproduce-first.

## Required reading

1. `<run_dir>/bugs/bug-NNN.md` — full bug report including reproduction steps and evidence.
2. The relevant source files mentioned in "Suggested area."

## Reproduce first

1. Find or write a test that captures the bug. The test MUST FAIL on the current HEAD before you fix anything. If your test passes, you haven't captured the bug — refine until it fails.
2. If a deterministic automated test isn't possible (e.g., the bug only reproduces under load, in a real browser, against a remote service), document why and proceed to manual reproduction. Capture the failing observation in detail (logs, traces, screenshots) so the fix can be verified.

The test represents the correct behavior. If your fix doesn't make the test pass, the fix is wrong — do not edit the test to match.

## Bisect or trace

If the bug is a regression, identify the introducing commit:
- Read the bug report's "Suggested area" — it usually names a candidate code path.
- `git log --oneline <last-known-good>..<current-head>` lists candidate commits.
- For bugs in shared subsystems, check whether the implicated source files are byte-identical between the candidate commits — if yes, the bug isn't in the code change you suspected; widen the search.

If you cannot reproduce the bug in any environment after multiple attempts, do NOT ship a speculative fix. Add regression tests as guardrails, document the no-repro finding in a "Resolution" section appended to the bug file, and let the orchestrator decide whether to escalate or close as transient.

## Fix

- Fix the root cause, not the symptom.
- Don't add try/catch, fallbacks, or retries that mask the underlying issue.
- The fix should be minimal — change the smallest area that resolves the issue.
- If the root cause suggests a systemic weakness (missing validation at a boundary, type system gap, misleading API), note it in your return as a deeper finding. Fix now only if scoped and directly related; otherwise flag it rather than silently expand.

## Build, test, lint

Run all gates listed in the project's `CLAUDE.md`. The new regression test must be among the passing tests.

## Commit + bug-file annotation

- ONE commit when green.
- HEREDOC body with sections: Bug (one paragraph) / Root cause (one paragraph) / Fix (one paragraph) / Regression test (one paragraph).
- Do NOT print a diff summary after committing.
- Append a "**Resolution**" section to `<run_dir>/bugs/bug-NNN.md` noting commit SHA, root cause one-liner, and the regression-test file:test-name. Do NOT delete the bug file.

## Return contract (strict, ≤300 words)

```
SHA: <full sha>
Root cause: <one sentence, or "no repro after N attempts">
Files: <one per line>
Gates: <gate name> ✓|✗ ...
Regression test: file:test-name
Bug-NNN.md annotated: yes | no
Disposition: FIXED | NO-REPRO-WITH-GUARDRAILS | ESCALATE
```
