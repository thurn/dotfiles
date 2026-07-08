# Review Feedback Coder Protocol

You address specific findings from prior reviews and produce ONE follow-up commit.

## Required reading

1. `<run_dir>/reviews/NN-conformance.md`
2. `<run_dir>/reviews/NN-quality.md`
3. The orchestrator's dispatch note tells you which findings to address (and which to skip). Honor it — don't expand scope.

## Implementation

- Address only the findings the orchestrator named. Skip the rest, even if you think they're worth fixing.
- Each fix should be minimal — touch the smallest area that resolves the issue.
- Add regression tests when the finding describes a real bug or drift risk.
- Re-read the target code fresh before each fix — earlier fixes shift line numbers and context.
- If a finding is wrong on closer inspection: for blockers, implement anyway unless the fix would introduce a real bug (then surface it in Notes); for suggestions, skip and document the reason. Never silently ignore.

## Build, test, lint

Run all four gates per the project's CLAUDE.md. All must PASS.

## Commit + cleanup

- ONE commit when green.
- HEREDOC body with one section per finding addressed.
- Do NOT print a diff summary after committing.
- Stage only files you touched.
- After committing, move both review files (`NN-conformance.md`, `NN-quality.md`) to `<run_dir>/reviews/resolved/`. `mkdir -p` the resolved directory first if needed.

## Return contract (strict, ≤200 words)

```
SHA: <full sha>
Files: <one per line>
Gates: typecheck ✓|✗ | test ✓|✗ (<count> passed) | lint ✓|✗ | build ✓|✗
Findings addressed: <list with one-word disposition each>
Reviews moved: yes | no
Notes: <one-line if anything was non-obvious; otherwise omit>
```
