---
name: backlog-execute-serial
description: Drain task files in /tmp/backlog/ strictly one at a time on a single shared git worktree and branch, running an implementer + reviewer subagent pipeline per task. Each approved task's commits stay on the branch and the next task builds on top, so tasks may depend on each other; a task that fails review is rolled back to keep the branch clean. Produces one linear branch ready to merge to master. Use when the user says "work the backlog serially", "run backlog-execute-serial", or when backlog tasks have ordering dependencies.
---

# Backlog Execute (Serial)

Drain `/tmp/backlog/` by running **one task at a time** on a **single** git
worktree and branch. Per task: implementer subagent → reviewer subagent → fix
loop until approved → the commits stay on the shared branch → archive → next
task. Because every task commits onto the same branch in order, each task
builds directly on the work of the tasks before it.

This is the sequential sibling of `backlog-execute`. There is no parallelism
and no separate integration lane: the work branch *is* the integration branch,
assembled incrementally. The final deliverable is identical — one linear
branch ready to merge to master — but the path there is simpler because nothing
is ever cherry-picked or merged.

The implementer, reviewer, and re-dispatch prompt templates are shared verbatim
with `backlog-execute` and live in `../backlog-execute-shared/`. This skill only
documents the serial orchestration around them.

## When to use

- The user has run the `backlog` skill (or otherwise produced standalone task
  files in `/tmp/backlog/`) and now wants them executed sequentially.
- **Prefer this over `backlog-execute` when:**
  - Tasks have ordering dependencies — Task B needs Task A's commit. Serial
    execution makes that the default: B starts from a branch that already
    contains A.
  - You want a low-resource, easy-to-follow run (one worktree, one branch, one
    subagent in flight) rather than maximum throughput.
- **Prefer `backlog-execute` (parallel) when** the tasks are independent and you
  want them done faster — N tasks at once across sibling worktrees.

If `/tmp/backlog/` is empty or missing, stop and tell the user.

## Configuration

Resolved from the user's invocation, with these defaults:

| Knob              | Default          | Meaning                                                                                       |
| ----------------- | ---------------- | --------------------------------------------------------------------------------------------- |
| `MAX_ROUNDS`      | `3`              | Review rounds per task before declaring it failed.                                            |
| `WORK_BRANCH`     | `backlog/serial` | The single branch all tasks commit onto, in order. Created from `BRANCH_BASE`.                |
| `BRANCH_BASE`     | current          | The branch `WORK_BRANCH` is created from.                                                      |
| `STOP_ON_FAILURE` | `false`          | If `true`, stop the whole run the first time a task fails (use when later tasks depend on it). |

There is exactly **one** worktree and **one** subagent in flight at any moment.
Unlike the parallel skill, reusing the worktree across tasks is the *intended*
design, not an anti-pattern — sequential tasks must build on the same branch.

## Harness compatibility

This skill is harness-agnostic and needs only **two** primitives (it does not
need the parallel skill's background-dispatch or concurrent-wait machinery,
because only one task is ever active).

### 1. Subagent dispatch

You must be able to spawn a fresh subagent that runs the prompt verbatim and
returns a single structured report without polluting the controller's history.
Foreground dispatch is fine and simplest here — there is nothing else to do
while one task runs.

- **Claude Code:** `Agent` tool with `subagent_type: "general-purpose"`. Run
  it in the foreground and act on the returned report. (Background +
  notification also works but buys you nothing serially.)
- **Codex:** dispatch implementers with `spawn_agent` (`agent_type: "worker"`)
  and reviewers with a fresh read-only agent, then `wait_agent` on the single
  returned id. Do not use `codex exec` or shell backgrounding.

The implementer and reviewer must be **separate** subagents — the reviewer
needs a fresh, unpolluted context.

### 2. One worktree

- **Claude Code:** if `EnterWorktree` (or any native worktree tool) is
  available, use it once at the start and its matching removal command at the
  end. Fall back to `git worktree add` only if the native tool is missing.
- **Codex / git fallback:** `git worktree add <path> -b <WORK_BRANCH> <BRANCH_BASE>`
  at the start, `git worktree remove <path>` at the end.

Create the worktree outside the main checkout. If using `git worktree add`
directly, place it under `.worktrees/` at the repo root and **verify that
`.worktrees/` is gitignored before first use** (commit a `.gitignore` entry if
not). This is the worktree-skill discipline; do not skip it. See
`super-using-git-worktrees` for full guidance.

## Worktree & branch setup

Done **once** at the start of the run, before the first task:

```bash
BRANCH_BASE=$(git branch --show-current)
BRANCH_BASE_SHA=$(git rev-parse "$BRANCH_BASE")
```

Ensure `WORK_BRANCH` is cleanly based on `BRANCH_BASE_SHA`:

- If `WORK_BRANCH` is missing, create it at `BRANCH_BASE_SHA`.
- If it exists, run `git merge-base --is-ancestor "$BRANCH_BASE_SHA" "$WORK_BRANCH"`.
  - If the check fails (the branch has diverged), preserve the old branch as
    `{{WORK_BRANCH}}-stale-<timestamp>` and recreate `WORK_BRANCH` at
    `BRANCH_BASE_SHA`. Tell the user the old branch was preserved. Do not append
    new work onto a divergent branch.
  - If the user explicitly asked to resume an existing `WORK_BRANCH`, still
    require the ancestry check to pass; otherwise stop and ask them to choose a
    different `BRANCH_BASE` or allow preservation and recreation.

Then create the single worktree checked out on `WORK_BRANCH` (native tool, or
`git worktree add .worktrees/backlog-serial -b "$WORK_BRANCH" "$BRANCH_BASE"`
if the branch is new / re-pointed). Optionally run project setup inside it
(`npm install`, `cargo build`, etc. — match what the project uses); the
implementer prompt also reminds the subagent to do this, so it is best-effort.

## The execution loop

Maintain:

- **pending**: queue of task files, sorted numerically and processed **in
  order** (task number can encode dependencies — respect it).
- **PRE_TASK_SHA**: the `WORK_BRANCH` tip before the current task's implementer
  starts. Used as the reviewer's `BASE_SHA` and as the rollback point if the
  task fails.
- **done**, **failed**: lists for the final summary.

Loop:

```
1. List /tmp/backlog/*.md (sorted). If empty, stop.
2. Resolve MAX_ROUNDS, WORK_BRANCH, BRANCH_BASE, STOP_ON_FAILURE from the
   user's invocation. Set up the worktree and branch (section above).
3. For each task file, in order:
   a. PRE_TASK_SHA = `git rev-parse WORK_BRANCH`.
   b. Dispatch the IMPLEMENTER with the full task body inline. review_rounds = 0.
   c. Handle the implementer/reviewer results per the table below, looping
      implementer ⇄ reviewer in the same worktree until APPROVED or failure.
   d. On APPROVED: archive the task file + evidence. The commits already sit on
      WORK_BRANCH — nothing to integrate. Continue to the next task.
   e. On failure: roll back WORK_BRANCH to PRE_TASK_SHA (preserving WIP on a
      branch), record it in `failed`, and either continue to the next task or,
      if STOP_ON_FAILURE, stop after the summary.
4. Remove the worktree (keep WORK_BRANCH). Emit the final summary.
```

Because the loop is sequential, there is no scheduling, no slots, and no
waiting on multiple agents — you dispatch, you get one report, you act.

### Completion handling

| Stage        | Subagent result                       | Action                                                                                                                                                            |
| ------------ | ------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| IMPLEMENTING | DONE / DONE_WITH_CONCERNS             | Set `BASE_SHA = PRE_TASK_SHA`, `HEAD_SHA = git rev-parse WORK_BRANCH`. Dispatch the REVIEWER.                                                                      |
| IMPLEMENTING | BLOCKED / NEEDS_CONTEXT (first time)  | If a small context gap, re-dispatch the implementer with the added context. Otherwise treat as a failure (roll back).                                              |
| REVIEWING    | APPROVED                              | Archive the task file + evidence. Commits stay on `WORK_BRANCH`; advance `PRE_TASK_SHA = HEAD_SHA` implicitly by moving to the next task. Record in **done**.       |
| REVIEWING    | CHANGES REQUESTED (rounds < MAX)      | Increment `review_rounds`. Dispatch the RE-IMPLEMENTER in the same worktree with the reviewer's issue list. Then re-review (new `HEAD_SHA`, same `BASE_SHA`).       |
| REVIEWING    | CHANGES REQUESTED (rounds == MAX)     | Record failure with the final reviewer report. Roll back (below).                                                                                                  |
| any          | Subagent error / timeout              | Record failure. Roll back (below).                                                                                                                                 |

**Do not stop to ask "should I continue?" between tasks** (unless
`STOP_ON_FAILURE` triggered a stop). The user asked you to drain the backlog;
drain it.

## Implementer, reviewer, and re-dispatch prompts

Read and dispatch the shared templates, filling the `{{...}}` placeholders. The
only mode-specific placeholder is `{{ISOLATION_NOTE}}`; its serial values are
below.

- **Implementer** — `../backlog-execute-shared/implementer-prompt.md`. Give the
  full task body inline.
- **Reviewer** — `../backlog-execute-shared/reviewer-prompt.md`. Dispatch a
  **fresh** subagent, never the implementer's context. `{{BASE_SHA}}` is
  `PRE_TASK_SHA` (the branch tip before this task), so the reviewer sees only
  this task's commits even though the branch carries earlier tasks' work.
  `{{HEAD_SHA}}` is the current branch tip.
- **Re-dispatch on review issues** — `../backlog-execute-shared/re-dispatch-prompt.md`.
  Used when the reviewer requests changes and `review_rounds < MAX_ROUNDS`;
  same worktree and branch.

**`{{ISOLATION_NOTE}}` for the serial implementer:**

> You are working in a dedicated git worktree on a single shared branch. Tasks
> are executed one at a time, so this branch already contains the commits from
> every previously completed task, and your work builds directly on top of them.
> Do not touch files outside this worktree and do not push to any branch other
> than this one.

**`{{ISOLATION_NOTE}}` for the serial reviewer:**

> You are reviewing inside the single dedicated git worktree. This branch
> contains commits from earlier completed tasks; review ONLY the commit range
> given below for this task, not the entire branch history.

## Rolling back a failed task

A serial branch is shared by every subsequent task, so a failed task's commits
must **not** be left on it — later tasks would build on unreviewed, broken work.
When a task fails (review exhausted, BLOCKED implementer, or subagent error):

```bash
# Preserve the work-in-progress for inspection, only if the implementer
# actually committed something past the pre-task tip.
if [ "$(git rev-parse WORK_BRANCH)" != "$PRE_TASK_SHA" ]; then
  git branch "backlog/serial-failed/NNN-<slug>" WORK_BRANCH
fi
# Reset the shared branch back to the clean tip and drop stray files.
git -C .worktrees/backlog-serial reset --hard "$PRE_TASK_SHA"
git -C .worktrees/backlog-serial clean -fd
```

Then record `{task, failed_branch, last_status, last_report}` in **failed**,
**do not** archive the task file (leave it in `/tmp/backlog/` for a re-attempt),
and move on — or stop if `STOP_ON_FAILURE` is set.

Uncommitted changes the implementer left behind are discarded by the reset; the
implementer prompt instructs it to commit its work, so committed WIP is what the
preserved `backlog/serial-failed/*` branch captures.

## Archiving completed tasks

Single stage, on APPROVED — there is no separate integration step, so the task
file moves to `done/` as soon as it is approved (its commits are already on
`WORK_BRANCH`):

```bash
mkdir -p /tmp/backlog/done
mv /tmp/backlog/NNN-<slug>.md /tmp/backlog/done/
for kind in screenshots transcripts; do
  shopt -s nullglob
  for f in /tmp/backlog/$kind/NNN-<slug>-*; do
    mkdir -p /tmp/backlog/done/$kind
    mv "$f" /tmp/backlog/done/$kind/
  done
done
```

## Final summary

When every task has been processed (or `STOP_ON_FAILURE` halted the run), print:

- **Work branch:** `{{WORK_BRANCH}}` at `<head sha>` — single linear branch
  ready to merge to master. Include a one-line
  `git log --oneline BRANCH_BASE..WORK_BRANCH` summary so the user sees the
  final shape.
- **Completed (K):** one-line table of `NNN — title — commit-sha(s)`, in the
  order they landed.
- **Failed (M):** for each, `NNN — title — preserved-branch — reason` and a
  pointer to the last reviewer or implementer report. Note that because tasks
  run serially, a failed task may have been a dependency of later ones — flag
  any later tasks that then failed for the same reason.
- **Follow-up tasks filed:** any new files implementers added to
  `/tmp/backlog/` during the run.

Then stop. Do not merge `WORK_BRANCH` into master — the user does that.

## Anti-patterns

- **Reaching for serial when tasks are independent.** If there are no ordering
  dependencies and you want speed, use `backlog-execute` (parallel) instead.
- **Leaving a failed task's commits on the shared branch.** Roll back to
  `PRE_TASK_SHA` so the next task builds on reviewed work only.
- **Reordering tasks arbitrarily.** Process in numeric order — it can encode
  dependencies. (This is the opposite of the parallel skill's FIFO-by-completion
  queue, which is safe only because parallel tasks are independent.)
- **Spawning a fresh worktree per task.** Serial reuses the one worktree on
  purpose; a new worktree per task would discard the accumulated branch state
  the next task depends on.
- **Same subagent for implementation and review.** The reviewer must have a
  fresh, unpolluted context.
- **Controller-side commits.** The implementer commits its own work.
- **Skipping the review because "the task was small."** No exceptions.
- **Skipping pre/post evidence because "the test passes."** The task template
  requires both; the review enforces both.
- **Treating "could not reproduce" as task complete.** Per the task template,
  reproduction is a hard blocker.
- **Bundling unrelated fixes into the same commit.** File a new task instead.
- **Hard-coding `npm` / `cargo` / `pytest` commands** without checking what the
  project actually uses.
- **Pausing between tasks to ask "should I continue?".** Drain the queue
  (unless `STOP_ON_FAILURE` legitimately halts it).
- **Auto-merging `WORK_BRANCH` into master.** This skill stops at producing the
  linear branch; the user merges.

## Related skills

- `backlog-execute` — the parallel sibling: N tasks at once across sibling
  worktrees with a serial integration lane. Use it when tasks are independent
  and you want throughput. Shares the prompt templates in
  `../backlog-execute-shared/`.
- `super-using-git-worktrees` — full discipline for worktree setup (gitignore
  checks, native-tool preference, baseline tests).
- `super-subagent-driven-development` — heavier-weight plan-driven version with
  explicit spec + code-quality review stages.
- `backlog` — produces the task files this skill consumes.
