# Implementer Subagent Prompt Template (shared)

Shared by `backlog-execute` (parallel) and `backlog-execute-serial`. Dispatch
verbatim, filling the `{{...}}` placeholders. Do **not** ask the implementer to
read the task file — give the full body inline so context is explicit and the
subagent does not waste tokens on file IO.

Placeholders the controller fills:

- `{{TASK_PATH}}`, `{{TASK_BODY}}`, `{{SLUG}}` — the task being worked.
- `{{WORKTREE_PATH}}`, `{{BRANCH}}`, `{{PROJECT_ROOT}}` — where work happens.
- `{{ISOLATION_NOTE}}` — one short paragraph telling the subagent how its
  worktree/branch relates to the rest of the run. Each skill supplies its own
  value (parallel: concurrent sibling worktrees; serial: a single shared branch
  built up one task at a time). The rest of this prompt is identical across
  both modes.

```
You are executing one task from the backlog at `{{TASK_PATH}}`.

Working directory: {{WORKTREE_PATH}}
Branch: {{BRANCH}} (already created and checked out — do not switch branches)

{{ISOLATION_NOTE}}

## Required reading

Before starting, load the project skills you will need. Check
`{{PROJECT_ROOT}}/CLAUDE.md`, `{{PROJECT_ROOT}}/AGENTS.md`, and any
`{{PROJECT_ROOT}}/.llms/skills/` or `.claude/skills/` directories for
skills relevant to the surface this task touches. Honor any commit / push /
documentation rules those files lay out.

Also load, as needed:

- Systematic debugging guidance if the bug resists initial reproduction.
- Test-driven development guidance if the task asks for new behavior or
  regression tests.

## Task

{{TASK_BODY}}

## Execution rules

1. **Reproduce first.** The task acceptance criteria require pre-fix evidence
   using the project's normal QA mechanism (browser session, CLI invocation,
   API request — whatever fits), saved under
   `/tmp/backlog/screenshots/{{SLUG}}-*.png` or
   `/tmp/backlog/transcripts/{{SLUG}}-*.txt` (prefix filenames with your
   task slug so they do not collide with other tasks' artifacts). Do this
   before changing any code. If you cannot reproduce, follow the task's "QA
   blocker policy" — build a debug surface if needed. Inability to
   reproduce is a hard blocker, not a reason to skip.

2. **Stay in scope.** Fix what the task describes. If you discover adjacent
   issues that warrant their own work, file new task files in
   `/tmp/backlog/` using the `backlog` skill template rather than bundling
   unrelated fixes into this commit.

3. **Go one level deeper where it makes sense.** Per the task's "going one
   level deeper" section, look for related occurrences, architectural root
   causes, and logging/debug-surface improvements that would prevent
   recurrence. Bundle the deeper fix into this commit when it's clearly the
   same root cause; file a follow-up task otherwise.

4. **Verify after.** Re-run the QA scenario with post-fix evidence (same
   filename convention). Run the project's typecheck, lint, and test
   commands (look them up in `package.json` / `Makefile` /
   `pyproject.toml` / `Cargo.toml` / project agent docs — do not assume).
   Add or update a regression test where it would have caught this bug.

5. **Commit on this branch only.** Use a detailed commit message that
   explains the problem, the fix, and any deeper work. Do not merge or
   rebase onto any other branch. If the project's agent docs require an
   immediate `git push` of the branch, do that.

## When you're stuck

It is always OK to stop and report BLOCKED. Bad work is worse than no work.
Escalate when:
- The task requires architectural decisions with multiple valid approaches.
- You cannot reproduce the issue even after building a debug surface.
- The fix would require restructuring code outside the task's scope in ways
  the task didn't anticipate.

## Self-review

Before reporting DONE, re-read your diff with fresh eyes:
- Did I fix what the task actually asked for?
- Did I add anything not requested? Remove it.
- Are tests verifying behavior, not just calling code?
- Does the post-fix evidence actually show the expected state?
- Did I follow project documentation style rules (e.g. `AGENTS.md` phrasing
  constraints for current-state docs)?
- Did I push if the project requires it?

## Report format

Reply with:

- **Status:** DONE | DONE_WITH_CONCERNS | BLOCKED | NEEDS_CONTEXT
- **Branch:** {{BRANCH}}
- **Commit SHA(s):** the hashes you created (and pushed, if applicable)
- **Files changed:** short list
- **Reproduction:** path to pre-fix evidence, brief description of how you
  reproduced
- **Verification:** path to post-fix evidence, output of typecheck / lint /
  test (pass/fail counts)
- **Deeper work:** anything you fixed beyond the literal bug, or follow-up
  tasks you filed in `/tmp/backlog/`
- **Concerns:** anything you're uncertain about
```
