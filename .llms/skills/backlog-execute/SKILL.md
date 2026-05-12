---
name: backlog-execute
description: Sequentially work through task files in /tmp/backlog/ by dispatching one implementer subagent per task, running the bundled Claude CLI review script, then committing. Lightweight variant of subagent-driven development. Use when the user says "work the backlog", "execute the backlog", "run backlog-execute", or asks to start grinding through /tmp/backlog/.
---

# Backlog Execute

Drain `/tmp/backlog/` one task at a time. For each task: implementer subagent
→ Claude CLI review script → fix loop until approved → commit + push →
archive the task file. Continue without checking in with the user between
tasks.

This is a deliberately lightweight variant of
`super-subagent-driven-development`. One implementer, one Claude review, one
commit per task.

## When to use

- The user has run the `backlog` skill (or otherwise produced standalone
  task files in `/tmp/backlog/`) and now wants them executed.
- Tasks are independent enough to be done sequentially without a shared
  branch strategy.

If `/tmp/backlog/` is empty or missing, stop and tell the user.

## The loop

```
1. List /tmp/backlog/*.md (sorted) — these are the remaining tasks.
2. Pick the lowest-numbered task. Read its full contents.
3. Dispatch IMPLEMENTER subagent with the task text inline (template below).
4. If implementer returns BLOCKED / NEEDS_CONTEXT, handle per
   super-subagent-driven-development guidance and re-dispatch.
5. Run `scripts/review_in_claude.sh` with the reviewer prompt below.
6. If Claude flags issues, re-dispatch implementer with the issue list.
   Loop steps 5-6 until Claude approves. Cap at 3 review rounds — if
   still failing, stop and surface to the user.
7. Once approved, the implementer's commit is the canonical commit. Verify
   it landed (and was pushed if the project's agent docs require it).
8. Move the task file to /tmp/backlog/done/<same-filename> so it disappears
   from the active list. Move its evidence files (screenshots, transcripts)
   too if present.
9. Goto 1.
```

Stop conditions: list empty, hard blocker the user must resolve, or three
failed review rounds on a single task.

## Dispatch

Use a `general-purpose` agent for the implementer. Run it in the foreground:
the loop is sequential and you need each result before continuing. Do not
parallelize implementers across tasks; later commits depend on earlier ones
landing.

Use the bundled script for code review:

```bash
~/.llms/skills/backlog-execute/scripts/review_in_claude.sh \
  --project-root "{{PROJECT_ROOT}}" \
  --prompt-file /tmp/backlog/review-prompt.txt
```

Write the filled reviewer prompt to a temporary file and pass it with
`--prompt-file`. The script invokes `claude` from `{{PROJECT_ROOT}}` with a
review-only prompt. Treat its output as the reviewer report: `APPROVED`
advances the task, and `CHANGES REQUESTED` drives the fix loop.

The implementer makes its own commits. Do not commit from the controller.

## Implementer prompt template

Paste this verbatim, filling in `{{PROJECT_ROOT}}`, `{{TASK_PATH}}`, and
`{{TASK_BODY}}`. Do **not** ask the implementer to read the task file
itself — give them the full text inline so context is explicit.

```
You are executing one task from the backlog at `{{TASK_PATH}}`.

Working directory: {{PROJECT_ROOT}}
Branch: whatever is currently checked out — do not switch branches.

## Required reading

Before starting, load the project skills you will need. Check
`{{PROJECT_ROOT}}/CLAUDE.md`, `{{PROJECT_ROOT}}/AGENTS.md`, and
`{{PROJECT_ROOT}}/.llms/skills/` (or `.claude/skills/`) for skills relevant
to the surface this task touches. Honor any commit / push / documentation
rules those files lay out.

Also load, as needed:

- `~/.llms/skills/super-systematic-debugging/SKILL.md` — load if the bug
  resists initial reproduction.
- `~/.llms/skills/super-test-driven-development/SKILL.md` — load if the
  task asks for new behavior or regression tests.

## Task

{{TASK_BODY}}

## Execution rules

1. **Reproduce first.** The task acceptance criteria require pre-fix
   evidence using the project's normal QA mechanism (browser session, CLI
   invocation, API request — whatever fits this project), saved under
   `/tmp/backlog/screenshots/` or `/tmp/backlog/transcripts/`. Do this
   before changing any code. If you cannot reproduce, follow the task's
   "QA blocker policy" — build a debug surface if needed. Inability to
   reproduce is a hard blocker, not a reason to skip.

2. **Stay in scope.** Fix what the task describes. If you discover
   adjacent issues that warrant their own work, **file new task files in
   `/tmp/backlog/` using the `backlog` skill template** rather than
   bundling unrelated fixes into this commit.

3. **Go one level deeper where it makes sense.** Per the task's "going
   one level deeper" section, look for related occurrences, architectural
   root causes, and logging/debug-surface improvements that would prevent
   recurrence. Bundle the deeper fix into this commit when it's clearly
   the same root cause; file a follow-up task otherwise.

4. **Verify after.** Re-run the QA scenario with post-fix evidence. Run
   the project's typecheck, lint, and test commands (look them up in
   `package.json` / `Makefile` / `pyproject.toml` / `Cargo.toml` /
   project agent docs — do not assume). Add or update a regression test
   where it would have caught this bug.

5. **Commit.** Use a detailed commit message that explains the problem,
   the fix, and any deeper work. If the project's agent docs require an
   immediate `git push`, do that too.

## When you're stuck

It is always OK to stop and report BLOCKED. Bad work is worse than no
work. Escalate when:
- The task requires architectural decisions with multiple valid
  approaches.
- You cannot reproduce the issue even after building a debug surface.
- The fix would require restructuring code outside the task's scope in
  ways the task didn't anticipate.

## Self-review

Before reporting DONE, re-read your diff with fresh eyes:
- Did I fix what the task actually asked for?
- Did I add anything not requested? Remove it.
- Are tests verifying behavior, not just calling code?
- Does the post-fix evidence actually show the expected state?
- Did I follow project documentation style rules (e.g. `AGENTS.md`
  phrasing constraints for current-state docs)?
- Did I push if the project requires it?

## Report format

Reply with:

- **Status:** DONE | DONE_WITH_CONCERNS | BLOCKED | NEEDS_CONTEXT
- **Commit SHA(s):** the hashes you created (and pushed, if applicable)
- **Files changed:** short list
- **Reproduction:** path to pre-fix evidence, brief description of how
  you reproduced
- **Verification:** path to post-fix evidence, output of typecheck /
  lint / test (pass/fail counts)
- **Deeper work:** anything you fixed beyond the literal bug, or
  follow-up tasks you filed in `/tmp/backlog/`
- **Concerns:** anything you're uncertain about
```

## Claude reviewer prompt template

Paste this into `/tmp/backlog/review-prompt.txt`, filling
`{{PROJECT_ROOT}}`, `{{TASK_PATH}}`, `{{TASK_BODY}}`,
`{{IMPLEMENTER_REPORT}}`, `{{BASE_SHA}}`, and `{{HEAD_SHA}}`, then run
`scripts/review_in_claude.sh` as shown above.

```
You are reviewing one backlog task.

Working directory: {{PROJECT_ROOT}}

## Task that was assigned

{{TASK_BODY}}

(Original task file: `{{TASK_PATH}}`)

## What the implementer reported

{{IMPLEMENTER_REPORT}}

## Commits to review

`git diff {{BASE_SHA}}..{{HEAD_SHA}}` — review every commit in this
range, not just the latest.

## Do not trust the report

Verify everything by reading the actual diff and the actual artifacts.

## Checklist

**Spec compliance**
- Does the diff implement what the task asked for?
- Did the implementer add scope that wasn't requested? (Bundling a
  legitimate deeper-root-cause fix is fine; sneaking in unrelated
  refactors is not.)
- Did they skip any acceptance criteria?

**QA evidence**
- Does pre-fix evidence exist under `/tmp/backlog/screenshots/` or
  `/tmp/backlog/transcripts/`? Open it and confirm it shows the broken
  state described in the task.
- Does post-fix evidence exist? Open it and confirm it shows the
  expected state. If pre and post artifacts look identical, that is a
  fail.
- Did the project's typecheck, lint, and test commands actually pass in
  the implementer's report? Re-run them yourself if there's any doubt.

**Code quality**
- Each touched file has a clear responsibility.
- Names are accurate.
- No dead code, no commented-out blocks, no debug `console.log` /
  `println!` / `print` statements left behind.
- Follows project documentation style rules (check `AGENTS.md` /
  `CLAUDE.md`).
- Tests verify behavior, not just structure.
- Regression test would have caught the original bug.

**Project-specific hazards**
- Re-read the project's agent docs (`AGENTS.md`, `CLAUDE.md`, project
  skill files) for any "if you change X, also update Y" rules. Verify
  the diff respects them.

**Commit hygiene**
- Was the commit pushed if the project requires it?
- Is the commit message detailed enough to explain the problem and the
  fix without reading the diff?

## Report format

Reply with one of:

- **APPROVED** — short note on what was good.
- **CHANGES REQUESTED** — bulleted list of specific issues with
  file:line references. Each item must be actionable.
```

## Re-dispatch on issues

If Claude requests changes, dispatch a follow-up implementer with:

```
You are continuing work on `{{TASK_PATH}}`.

Claude flagged the following issues with your previous commit
({{HEAD_SHA}}):

{{REVIEWER_ISSUES}}

Address each issue. If the previous commit has already been pushed and
the project's agent docs prefer new commits over amends (most do),
create a NEW commit. Push if the project requires it. Then report back
in the same format as before.
```

If Claude requests changes a second time, the third implementer
dispatch should explicitly include both prior review reports so the
implementer can see the full chain. If a third round still fails, stop
and surface to the user — something structural is wrong with the task or
the approach.

## Archiving completed tasks

After approval, move the task file out of the active queue:

```bash
mkdir -p /tmp/backlog/done
mv /tmp/backlog/NNN-<slug>.md /tmp/backlog/done/
# also move evidence files if present
[ -f /tmp/backlog/screenshots/NNN-<slug>.png ] && \
  mkdir -p /tmp/backlog/done/screenshots && \
  mv /tmp/backlog/screenshots/NNN-<slug>.png /tmp/backlog/done/screenshots/
[ -f /tmp/backlog/transcripts/NNN-<slug>.txt ] && \
  mkdir -p /tmp/backlog/done/transcripts && \
  mv /tmp/backlog/transcripts/NNN-<slug>.txt /tmp/backlog/done/transcripts/
```

## Final summary

When `/tmp/backlog/*.md` is empty (or you stopped on a blocker), print:

- count of tasks completed this session
- one-line table of `NNN — title — commit-sha`
- any tasks left in `/tmp/backlog/` and why
- any new follow-up tasks the implementers filed

Then stop.

## Anti-patterns

- Pausing between tasks to ask "should I continue?" — execute the queue.
- Parallel implementer subagents — later tasks may depend on earlier
  commits and the working tree only has one state.
- Controller-side commits — the implementer commits its own work.
- Skipping the Claude review because "the task was small."
- Skipping the pre/post evidence because "the test passes." The task
  template requires both; the Claude review enforces both.
- Treating "could not reproduce" as task complete. Per the task
  template, reproduction is a hard blocker.
- Bundling unrelated fixes into the same commit. File a new task
  instead.
- Hard-coding `npm` / `cargo` / `pytest` commands without checking what
  the project actually uses.
