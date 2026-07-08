---
name: backlog-execute
description: Drain task files in /tmp/backlog/ in parallel by running each task in its own git worktree with an implementer + reviewer subagent pipeline. Maintains a target parallelism (default 3) by immediately launching work when slots free. A serial integration lane cherry-picks and repairs approved branches onto a clean linear `backlog/integration` branch, avoiding unresolved manual cleanup whenever possible. Use when the user says "work the backlog", "execute the backlog", "run backlog-execute", or asks to start grinding through /tmp/backlog/.
---

# Backlog Execute (Parallel)

Drain `/tmp/backlog/` by running up to **N tasks at once**, each on its own git
worktree + branch. Per task: implementer subagent → reviewer subagent → fix
loop until approved → handed off to a serial integrator subagent that
cherry-picks the branch onto `backlog/integration` → archive. The orchestrator
maintains N in-flight feature tasks **plus** one in-flight integration at all
times — the **moment** any slot frees, the next pending work is launched.

This is the lightweight variant of subagent-driven development: one
implementer, one reviewer, one branch per task, plus a serial integration lane
that produces one linear branch ready to merge to master. Approved task
branches must either land on that integration branch or produce one explicit
blocked repair report; the normal outcome must not be "several approved
branches are left for manual cherry-pick."

## When to use

- The user has run the `backlog` skill (or otherwise produced standalone task
  files in `/tmp/backlog/`) and now wants them executed.
- **Critical precondition: tasks are independent.** Parallel execution means
  each task runs against the same starting commit on its own branch — Task B
  cannot rely on Task A's commit. If tasks have ordering dependencies, use
  `backlog-execute-serial` (each task builds on the previous one's commits),
  or merge the dependent tasks into one task file before starting.

If `/tmp/backlog/` is empty or missing, stop and tell the user.

## Configuration

Resolved from the user's invocation, with these defaults:

| Knob                     | Default               | Meaning                                                                                  |
| ------------------------ | --------------------- | ---------------------------------------------------------------------------------------- |
| `PARALLELISM`            | `3`                   | Max concurrent in-flight feature tasks (= max concurrent feature worktrees).             |
| `MAX_ROUNDS`             | `3`                   | Review rounds per task before declaring it failed.                                       |
| `BRANCH_BASE`            | current               | The branch feature worktrees are created from. Each task branches here.                  |
| `INTEGRATION_BRANCH`     | `backlog/integration` | Single linear branch that approved task branches are cherry-picked onto.                 |
| `INTEGRATION_MAX_ROUNDS` | `1`                   | Re-attempts per task in the integration lane before marking it integration-failed.       |
| `INTEGRATION_REPAIR_MAX_ROUNDS` | `2`            | Repair attempts after mechanical cherry-pick or verification fails.                      |

A task occupies its feature worktree from **implementer launch** through
**reviewer APPROVED** (the worktree is needed for fix iterations after review).
Once approved, the worktree is removed and the branch is enqueued for
integration; the integration lane uses its own dedicated worktree at
`.worktrees/integration` and pulls from the branch ref. Reviews of one task
run in parallel with implementers of other tasks, and integration of approved
tasks runs in parallel with both — that is what gives the pipeline its
throughput.

## Harness compatibility

This skill is harness-agnostic. Three primitives are needed; map each to what
your harness provides:

### 1. Background subagent dispatch

You must be able to spawn a fresh subagent that runs without blocking the
controller, and be notified when it finishes.

- **Claude Code:** `Agent` tool with `run_in_background: true` and
  `subagent_type: "general-purpose"`. You receive an automatic notification
  when the subagent completes — **do not poll**.
- **Codex:** use the session's agent tools, not shell-launched Codex
  processes. Dispatch implementers with `spawn_agent` using
  `agent_type: "worker"` and reviewers with a fresh read-only agent
  (`explorer` or default). Track the returned agent ids in `inflight`. Tell
  workers they are not alone in the codebase, that they own exactly one
  worktree/branch, and that they must not revert or interfere with edits in
  sibling worktrees.

The contract in either harness is identical: a fresh context that receives
the prompt verbatim, returns a single structured report, and does not pollute
the controller's history.

### 2. Worktree creation and removal

- **Claude Code:** if `EnterWorktree` (or any native worktree tool) is
  available, use it — it handles directory placement, branch creation, and
  cleanup. Fall back to `git worktree add` only if the native tool is missing.
- **Codex:** create and remove worktrees from the controller with normal git
  commands: `git worktree add <path> -b <branch> <BRANCH_BASE>` and
  `git worktree remove <path>`. Do not use `codex exec` or shell backgrounding
  as a substitute for Codex's agent tools.

Always create worktrees outside the main checkout. If using `git worktree add`
directly, place worktrees under `.worktrees/` at the repo root and verify it
is gitignored first (see `super-using-git-worktrees` for full guidance).

### 3. Concurrent waiting

The orchestrator must be able to react to "any in-flight subagent finished"
without busy-waiting.

- **Claude Code:** background subagents trigger automatic notifications — let
  them arrive, then act. Track in-flight handles in a TaskList so you can
  match notifications to slots.
- **Codex:** call `wait_agent` with the currently in-flight agent ids when
  every available slot is full or no more tasks can be launched. Use a long
  timeout; if it returns no completion, do any controller housekeeping and call
  it again. Do not poll files, manage PIDs, or use shell `wait -n`.

## Per-slot state machine

There are N concurrent **feature** slots plus **one** integration slot. Each
slot holds at most one task at a time.

Feature slot (N of these):

```
EMPTY → IMPLEMENTING → REVIEWING ─┬─APPROVED────→ enqueue(approved) → EMPTY
                       ▲          │
                       │          └─CHANGES_REQUESTED (round < MAX_ROUNDS) → IMPLEMENTING
                       │
                       └─ (BLOCKED / NEEDS_CONTEXT from implementer)
                          → record failure → EMPTY
```

Integration slot (1 of this):

```
INT_EMPTY → INTEGRATING ─┬─INTEGRATED → archive task + evidence → INT_EMPTY
                         │
                         └─INTEGRATION_FAILED ─┬─(attempts < MAX) → INTEGRATING
                                               │
                                               └─(attempts == MAX) → enqueue repair → INT_EMPTY

INT_EMPTY → REPAIRING ─┬─INTEGRATED → archive task + evidence → INT_EMPTY
                       │
                       └─REPAIR_FAILED ─┬─(attempts < REPAIR_MAX) → REPAIRING
                                        │
                                        └─(attempts == REPAIR_MAX) → record blocked repair → stop before more integrations
```

A feature slot moving to `EMPTY` triggers the next pending task launch. The
integration slot moving to `INT_EMPTY` triggers the next approved branch
launch. Both transitions are handled in the same loop iteration so freed
slots fill immediately.

## The orchestration loop

Maintain these structures:

- **pending**: queue of task files (sorted numerically), drained as tasks
  launch.
- **inflight**: `slot → {task_path, task_body, worktree_path, branch, state,
  handle, review_rounds, implementer_report?, reviewer_report?}` for each
  in-flight feature task.
- **approved**: FIFO queue of `{task_path, task_body, branch, base_sha,
  head_sha, implementer_report, reviewer_report}` awaiting integration. Order
  by completion time, not task number — see "Integration ordering" below.
- **repair_needed**: FIFO queue of approved entries whose mechanical
  integration failed and now need real conflict/test repair on a temporary
  repair branch.
- **integration_slot**: `{task_path, branch, head_sha, handle, attempts,
  repair_attempts?, repair_branch?, integration_pre_sha, mode}` or `None` if
  idle.
- **integrated**, **done**, **failed**, **blocked_repairs**: lists for the
  final summary.

Loop:

```
1. List /tmp/backlog/*.md (sorted). If empty, stop.
2. Resolve PARALLELISM, INTEGRATION_BRANCH, INTEGRATION_MAX_ROUNDS, and
   INTEGRATION_REPAIR_MAX_ROUNDS from the user's invocation. Resolve
   BRANCH_BASE = `git branch --show-current` and `BRANCH_BASE_SHA =
   git rev-parse BRANCH_BASE`.
3. Ensure the integration branch is cleanly based on `BRANCH_BASE_SHA` before
   launching feature work:
      - If INTEGRATION_BRANCH is missing, create it at `BRANCH_BASE_SHA`.
      - If it exists, run `git merge-base --is-ancestor BRANCH_BASE_SHA
        INTEGRATION_BRANCH`.
      - If that check fails, preserve the old branch as
        `{{INTEGRATION_BRANCH}}-stale-<timestamp>` and recreate
        INTEGRATION_BRANCH at `BRANCH_BASE_SHA`. Tell the user the old branch
        was preserved. Do not append new work to a divergent integration
        branch.
      - If `.worktrees/integration` already exists on the divergent branch,
        remove or detach that worktree before renaming/recreating the branch,
        then recreate the worktree on the clean INTEGRATION_BRANCH.
      - If the user explicitly requested resuming an existing integration
        branch, still require the ancestry check to pass; otherwise stop and
        ask them to choose a different `BRANCH_BASE` or allow preservation and
        recreation.
   Then ensure the integration worktree exists at `.worktrees/integration`
   checked out on INTEGRATION_BRANCH.
4. Until pending, approved, repair_needed, inflight, AND integration_slot are
   all empty, or a blocked repair stops integration:
   a. While |inflight| < PARALLELISM and pending non-empty:
        - Pop next task. Create worktree on a new branch
          `backlog/NNN-<slug>` based on BRANCH_BASE.
        - Dispatch IMPLEMENTER subagent in BACKGROUND with full task text.
        - Register slot: state=IMPLEMENTING.
   b. If integration_slot is None and (repair_needed non-empty or approved
      non-empty):
        - If repair_needed is non-empty, pop the next repair entry first.
          Record current INTEGRATION_BRANCH tip as `integration_pre_sha`,
          create/update a temporary branch
          `backlog/integration-repair/NNN-<slug>` from that tip, and dispatch
          an INTEGRATION REPAIR subagent.
        - Otherwise, pop next approved entry (FIFO).
          Record current INTEGRATION_BRANCH tip as `integration_pre_sha` so
          a failed attempt can be rolled back. Dispatch INTEGRATOR subagent
          in BACKGROUND against `.worktrees/integration` for that task's
          branch.
        - Set integration_slot.
   c. Wait for any in-flight subagent to complete (Claude Code background
      notification, Codex `wait_agent`, or the harness's equivalent agent wait
      primitive).
   d. Handle the completion based on which slot it belonged to (table below).
   e. Goto 4a (so any newly-freed slot is filled immediately, repairs get
      priority, and new approvals feed the integrator when no repair is
      waiting).
5. Emit final summary.
```

### Integration ordering

The approved queue is FIFO by completion time, not task number. The skill's
critical precondition is that tasks are independent against `BRANCH_BASE`, so
order does not matter for correctness. Sorting by task number would create
head-of-line blocking — a slow task #003 would stall integration of fast,
already-approved tasks #004–#020 — and it would not buy you anything.

### Completion handling

| Slot kind   | Slot state   | Subagent result                       | Action                                                                                                                                                                                              |
| ----------- | ------------ | ------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| feature     | IMPLEMENTING | DONE / DONE_WITH_CONCERNS             | Capture `BASE_SHA` (BRANCH_BASE tip) and `HEAD_SHA` (branch tip). Dispatch REVIEWER in BACKGROUND. State → REVIEWING.                                                                               |
| feature     | IMPLEMENTING | BLOCKED / NEEDS_CONTEXT (first time)  | If a small context gap, re-dispatch implementer with the added context. Otherwise record failure, free slot.                                                                                        |
| feature     | REVIEWING    | APPROVED                              | Remove worktree (keep branch). Enqueue `{branch, base_sha, head_sha, reports}` onto **approved**. Record in **done**. Free slot. (Task file is archived later, after successful integration.)       |
| feature     | REVIEWING    | CHANGES REQUESTED (rounds < MAX)      | Increment `review_rounds`. Dispatch RE-IMPLEMENTER in BACKGROUND with the reviewer's issue list. State → IMPLEMENTING.                                                                              |
| feature     | REVIEWING    | CHANGES REQUESTED (rounds == MAX)     | Record failure with the final reviewer report. Remove worktree (keep branch). Free slot.                                                                                                            |
| feature     | any          | Subagent error / timeout              | Record failure. Free slot.                                                                                                                                                                          |
| integration | INTEGRATING  | INTEGRATED                            | Archive task file + evidence (move to `done/`). Record `{task, branch, new_integration_head}` in **integrated**. Free integration slot.                                                             |
| integration | INTEGRATING  | INTEGRATION_FAILED (attempts < MAX)   | Reset integration worktree (`git cherry-pick --abort` if needed, then `git reset --hard $integration_pre_sha`). Increment `attempts`. Re-dispatch integrator with the failure report appended.      |
| integration | INTEGRATING  | INTEGRATION_FAILED (attempts == MAX)  | Reset integration worktree as above. Enqueue the approved entry in **repair_needed** with the last failure report. Free integration slot.                                                           |
| integration | REPAIRING    | INTEGRATED                            | Fast-forward INTEGRATION_BRANCH to the verified repair branch, archive task file + evidence, record `{task, branch, repair_branch, new_integration_head}` in **integrated**. Free integration slot. |
| integration | REPAIRING    | REPAIR_FAILED (attempts < REPAIR_MAX) | Reset integration worktree to `integration_pre_sha`, increment repair attempts, and re-dispatch repair with all prior repair reports.                                                               |
| integration | REPAIRING    | REPAIR_FAILED (attempts == REPAIR_MAX)| Reset integration worktree to `integration_pre_sha`. Record `{task, branch, repair_branch, head_sha, last_failure_report}` in **blocked_repairs**. Stop launching new feature and integration work; optionally let already-running feature agents report before the final summary. |
| integration | any          | Subagent error / timeout              | Reset integration worktree. If mechanical integration, enqueue repair; if repair, record blocked repair and stop starting new integrations.                                                         |

**Failure policy:** continue draining the backlog. Other tasks are
independent of this one through implementation and review. Integration is
different: one unresolved repair means later integration results are no
longer trustworthy on a branch that intentionally omits an approved task. Do
not create a pile of approved-but-unintegrated branches. Escalate mechanical
failures into repair, and if repair is blocked, stop integration with a
single explicit blocker.

**Do not stop to ask "should I continue?" between tasks.** The user asked you
to drain the backlog; drain it.

## Worktree setup

For each task, before launching the implementer:

```bash
slug=$(basename "$task_file" .md)                  # e.g. 042-fix-csv-export
branch="backlog/$slug"
# Claude Code path: use EnterWorktree if available — it returns the worktree path.
# Codex / git fallback:
worktree=".worktrees/$slug"
git worktree add "$worktree" -b "$branch" "$BRANCH_BASE"
```

Run any project setup inside the new worktree (`npm install`, `cargo build`,
`pip install -r requirements.txt`, `go mod download`, etc. — match what the
project uses). The implementer prompt also reminds the subagent to do this
itself if needed, so this is best-effort optimization.

**Verify `.worktrees/` is gitignored before first use** (commit a `.gitignore`
entry if not). This is the worktree-skill discipline; do not skip it.

## Implementer, reviewer, and re-dispatch prompts

These three prompt templates are shared verbatim with `backlog-execute-serial`
and live in `../backlog-execute-shared/`. Read the file and dispatch it,
filling the `{{...}}` placeholders — including `{{ISOLATION_NOTE}}`, whose
parallel value is given below.

- **Implementer** — `../backlog-execute-shared/implementer-prompt.md`. Dispatch
  in BACKGROUND with the full task body inline.
- **Reviewer** — `../backlog-execute-shared/reviewer-prompt.md`. Dispatch a
  **fresh** subagent (never the implementer's context) in BACKGROUND. It is
  read-only. `{{BASE_SHA}}` is the BRANCH_BASE tip and `{{HEAD_SHA}}` the
  branch tip.
- **Re-dispatch on review issues** — `../backlog-execute-shared/re-dispatch-prompt.md`.
  Used when the reviewer requests changes and `review_rounds < MAX_ROUNDS`;
  dispatch in BACKGROUND, same worktree and branch.

**`{{ISOLATION_NOTE}}` for the parallel implementer:**

> You are running in an isolated git worktree. Other backlog tasks are running
> concurrently in sibling worktrees. Do not touch files outside this worktree
> and do not push to any branch other than your own.

**`{{ISOLATION_NOTE}}` for the parallel reviewer:**

> You are reviewing inside an isolated git worktree. Other backlog tasks are
> running concurrently in sibling worktrees — anything you see outside this
> worktree is irrelevant to your review.

## Integrator prompt template

Dispatch a **fresh** subagent in BACKGROUND with this prompt. The integrator
is read-write but scoped to mechanical conflict resolution and re-running
tests on the integration branch — it must not redesign the task's logic.

```
You are integrating one approved backlog task branch onto the shared linear
integration branch.

Working directory: {{INTEGRATION_WORKTREE}}
Integration branch: {{INTEGRATION_BRANCH}} (already checked out — do not
switch branches, do not push to any other branch)
Task branch to integrate: {{TASK_BRANCH}}
Expected task branch tip: {{HEAD_SHA}}
Expected integration branch tip (pre-attempt): {{INTEGRATION_PRE_SHA}}

You are running in an isolated git worktree dedicated to integration. Other
backlog tasks are still being implemented and reviewed concurrently in
sibling worktrees — do not touch those.

## Task that was implemented and approved

{{TASK_BODY}}

(Original task file: `{{TASK_PATH}}`)

## Reviewer report (APPROVED)

{{REVIEWER_REPORT}}

{{#PRIOR_FAILURE_REPORT}}
## Prior integration attempt failed with

{{PRIOR_FAILURE_REPORT}}
{{/PRIOR_FAILURE_REPORT}}

## What to do

1. Verify branch tips match the expected SHAs:
       git rev-parse {{TASK_BRANCH}}
       git rev-parse HEAD
   If either has moved, STOP and report INTEGRATION_FAILED.

2. Cherry-pick every commit on the task branch that is not already on the
   integration branch:
       git cherry-pick {{INTEGRATION_BRANCH}}..{{TASK_BRANCH}}

3. **If conflicts arise:**
   - Resolve only what is needed to make the cherry-pick apply. Your job is
     mechanical conflict resolution, not redesign.
   - When two branches modify the same region for orthogonal reasons, keep
     both changes. When they overlap meaningfully, prefer the union of
     intent.
   - **Hard rule: if resolution requires changing implementation logic
     (more than a few lines of glue, or any change that alters behavior the
     task did not call out), STOP, run `git cherry-pick --abort`, and
     report INTEGRATION_FAILED with a description of the conflict. Do not
     guess.**

4. After the cherry-pick succeeds, run the project's typecheck, lint, and
   test commands in this worktree (look them up in `package.json` /
   `Makefile` / `pyproject.toml` / `Cargo.toml` / project agent docs — do
   not assume).
   - If they pass: proceed.
   - If any fail: the integration broke something. STOP,
     `git reset --hard {{INTEGRATION_PRE_SHA}}`, and report
     INTEGRATION_FAILED with the failure output. Do not try to fix tests
     by editing implementation code.

5. If the project's agent docs require pushing the integration branch,
   push it now.

## Report format

Reply with one of:

- **INTEGRATED** — include:
  - new HEAD of `{{INTEGRATION_BRANCH}}`
  - one-line summary of any conflicts you resolved (or "no conflicts")
  - pass/fail counts for typecheck / lint / tests

- **INTEGRATION_FAILED** — include:
  - the cherry-pick output or test failure (verbatim)
  - which files had conflicts
  - your assessment: is the task itself wrong, did a previously integrated
    task create an incompatibility, or is the conflict mechanical but
    beyond the "no logic changes" rule?
```

## Integration repair prompt template

Dispatch this only after the mechanical integrator has exhausted
`INTEGRATION_MAX_ROUNDS`. The repair agent is read-write and may make scoped
adapter, wiring, fixture, and test updates needed to land the approved task
on the current integration branch. It must not redesign the feature or bundle
unrelated cleanup.

Before dispatch, the controller must reset `.worktrees/integration` to
`{{INTEGRATION_PRE_SHA}}`, abort any in-progress cherry-pick, and check out
or create `{{REPAIR_BRANCH}}` at that SHA:

```bash
git cherry-pick --abort || true
git reset --hard {{INTEGRATION_PRE_SHA}}
git checkout -B {{REPAIR_BRANCH}} {{INTEGRATION_PRE_SHA}}
```

Prompt:

```
You are repairing integration for one approved backlog task branch.

Working directory: {{INTEGRATION_WORKTREE}}
Repair branch: {{REPAIR_BRANCH}} (already checked out — do not switch
branches, do not push to any branch other than this one)
Integration branch to update after approval: {{INTEGRATION_BRANCH}}
Task branch to integrate: {{TASK_BRANCH}}
Expected task branch tip: {{HEAD_SHA}}
Expected repair branch base: {{INTEGRATION_PRE_SHA}}

The mechanical integrator could not land this approved task without real
integration work. Your job is to make the smallest correct integration patch
that preserves both the approved task's behavior and the behavior already on
the integration branch.

## Task that was implemented and approved

{{TASK_BODY}}

(Original task file: `{{TASK_PATH}}`)

## Reviewer report (APPROVED)

{{REVIEWER_REPORT}}

## Mechanical integration failure

{{INTEGRATION_FAILURE_REPORT}}

{{#PRIOR_REPAIR_REPORTS}}
## Prior repair attempts

{{PRIOR_REPAIR_REPORTS}}
{{/PRIOR_REPAIR_REPORTS}}

## What to do

1. Verify branch tips match the expected SHAs:
       git rev-parse {{TASK_BRANCH}}
       git rev-parse {{INTEGRATION_BRANCH}}
       git rev-parse HEAD
   If the task branch is not {{HEAD_SHA}}, or if either {{INTEGRATION_BRANCH}}
   or HEAD is not {{INTEGRATION_PRE_SHA}}, STOP and report REPAIR_FAILED.

2. Apply the task branch onto the repair branch. Start with:
       git cherry-pick {{INTEGRATION_BRANCH}}..{{TASK_BRANCH}}

3. Resolve conflicts and follow-up failures deliberately:
   - Keep the approved task's intent and all previously integrated behavior.
   - Scoped glue is expected: prop wiring, imports, call sites, fixtures,
     tests, compatibility adapters, and equivalent integration code are in
     scope.
   - Do not redesign the feature, change product behavior outside the task's
     intent, or bundle unrelated refactors.
   - If there are multiple valid product or architecture choices, STOP and
     report REPAIR_FAILED with the decision needed.

4. Run the project's typecheck, lint, and test commands in this worktree
   (look them up; do not assume).
   - If failures are caused by integration context, fix them in scope and
     re-run.
   - If failures reveal the approved task is wrong, or require broad
     redesign outside the task, STOP and report REPAIR_FAILED.

5. Commit the repair result on {{REPAIR_BRANCH}} with a message explaining
   the task integrated and the compatibility work performed. If project docs
   require pushing branches, push only {{REPAIR_BRANCH}}.

## Report format

Reply with one of:

- **INTEGRATED** — include:
  - repair branch HEAD
  - commits created
  - one-line summary of integration repair performed
  - pass/fail counts for typecheck / lint / tests

- **REPAIR_FAILED** — include:
  - command/conflict/test output needed to understand the blocker
  - files involved
  - whether the blocker is a product decision, architectural decision,
    broken approved task, or environment/tooling failure
```

On an **INTEGRATED** repair result, the controller verifies and advances the
integration branch before archiving:

```bash
git merge-base --is-ancestor {{INTEGRATION_BRANCH}} {{REPAIR_BRANCH}}
git branch -f {{INTEGRATION_BRANCH}} {{REPAIR_BRANCH}}
git checkout {{INTEGRATION_BRANCH}}
```

If the project requires pushing the integration branch, push
`{{INTEGRATION_BRANCH}}` after the fast-forward.

## Archiving completed tasks

Archiving happens in two stages.

**Stage 1 — on APPROVED (feature slot freed):** remove the feature worktree
but keep the branch. The branch is still needed for the integrator to
cherry-pick from. The task file stays in `/tmp/backlog/` until integration
succeeds.

```bash
# Remove the worktree but keep the branch ref
git worktree remove "$worktree_path"
```

**Stage 2 — on INTEGRATED (integration slot freed):** move the task file and
its evidence into `done/`.

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

If you used `EnterWorktree` (or equivalent native tool), use its matching
removal command — do **not** mix native creation with `git worktree remove`.

The feature branch is preserved through both stages. After the full run, the
user has `INTEGRATION_BRANCH` (single linear branch ready to merge to master)
plus all the per-task `backlog/NNN-*` branches still available for
inspection or PRing individually if they prefer.

## Failed tasks

**Feature-stage failures** (3 review rounds without approval, BLOCKED
implementer, or subagent error):

- Do **not** archive the task file. Leave it in `/tmp/backlog/` so the user
  can re-attempt it later.
- Remove the worktree (`git worktree remove <path>`). Leave the branch
  intact so the user can inspect the work-in-progress.
- Record `{task, branch, last_status, last_report}` in **failed** for the
  final summary.
- Free the feature slot and launch the next pending task.

**Mechanical integration failures** (INTEGRATION_FAILED after
`INTEGRATION_MAX_ROUNDS` attempts, or subagent error in mechanical
integration):

- Do **not** archive the task file.
- Reset the integration worktree to `integration_pre_sha`.
- Enqueue the approved task in **repair_needed** with the failure report.
- Free the integration slot. The next integration-lane action must process
  `repair_needed` before later approved branches.

**Repair failures** (REPAIR_FAILED after `INTEGRATION_REPAIR_MAX_ROUNDS`
attempts, or subagent error during repair):

- Do **not** archive the task file.
- Reset the integration worktree to `integration_pre_sha`.
- Leave both the feature branch and any repair branch intact for inspection.
- Record `{task, branch, repair_branch, head_sha, last_failure_report}` in
  **blocked_repairs** for the final summary.
- Stop starting new integration work. You may let already-running feature
  implementers/reviewers finish if the harness makes that cheaper than
  interruption, but do not integrate further approved branches on top of an
  integration branch that omits this approved task.

## Final summary

When `pending`, `approved`, `repair_needed`, `inflight`, and
`integration_slot` are all empty, or a blocked repair stops integration,
print:

- **Integration branch:** `{{INTEGRATION_BRANCH}}` at `<head sha>` — single
  linear branch ready to merge to master. Include a one-line
  `git log --oneline BRANCH_BASE..INTEGRATION_BRANCH` summary so the user
  sees the final shape.
- **Integrated (K):** one-line table of `NNN — title — task-branch —
  integrated-as-sha`. Include `repair-branch` when integration required
  repair.
- **Blocked repair (0 or 1 expected):** if repair failed, print
  `NNN — title — task-branch — repair-branch — head-sha — blocker` and say
  integration stopped there to avoid creating a manual cleanup pile.
- **Approved but not yet integrated:** only list tasks left here when a
  blocked repair stopped the run before they could be attempted.
- **Review-failed (M):** for each, `NNN — title — branch — reason` and a
  pointer to the last reviewer or implementer report.
- **Follow-up tasks filed:** any new files implementers added to
  `/tmp/backlog/` during the run.
- **Per-task branches preserved:** the full list of `backlog/NNN-*`
  branches, in case the user wants to inspect or PR any individually.

Then stop.

## Anti-patterns

- **Wave scheduling.** Do not wait for all N in-flight tasks to finish
  before launching the next batch. Launch the next pending task the
  *instant* any slot frees.
- **Foreground subagents.** Dispatching implementer or reviewer in the
  foreground serializes the pipeline and defeats parallelism. Always
  background.
- **Sleep-loops to wait for subagents.** Use the harness's notification or
  agent wait primitive. In Codex, use `wait_agent`; do not sleep-loop, poll
  files, manage PIDs, or use shell job control.
- **Sharing a branch across tasks.** Each task gets its own
  `backlog/NNN-slug` branch off `BRANCH_BASE`. Never have two implementers
  committing to the same branch.
- **Reusing a worktree across tasks.** Each task gets a fresh worktree;
  remove it on archival or failure. Reuse leaks state between tasks.
- **Same subagent for implementation and review.** Reviewer must have a
  fresh, unpolluted context.
- **Controller-side commits.** The implementer commits its own work.
- **Skipping the review because "the task was small."** No exceptions.
- **Skipping pre/post evidence because "the test passes."** The task
  template requires both; the review enforces both.
- **Treating "could not reproduce" as task complete.** Per the task
  template, reproduction is a hard blocker.
- **Bundling unrelated fixes into the same commit.** File a new task
  instead.
- **Hard-coding `npm` / `cargo` / `pytest` commands** without checking
  what the project actually uses.
- **Pausing between tasks to ask "should I continue?".** Drain the queue.
- **Treating a mechanical integration failure as final.** Enqueue repair and
  make a scoped integration patch instead of handing approved branches back
  to the user for manual cherry-pick.
- **Continuing integration after a repair is blocked.** Once repair fails
  after its allowed rounds, stop starting new integrations. Later approved
  branches may depend on the missing approved task, and skipping it creates a
  misleading integration branch.
- **Parallelizing integration.** Only one integrator runs at a time;
  two would race on the integration branch tip and clobber each other.
- **Sorting the approved queue by task number.** It's FIFO by completion.
  Reordering would create head-of-line blocking with no correctness
  benefit (tasks are independent by precondition).
- **Letting the integrator rewrite implementation logic to make conflicts
  go away.** The integrator does mechanical conflict resolution and runs
  tests — anything more is an escalation, not a fix.
- **Letting the mechanical integrator "fix" failing post-cherry-pick tests by
  editing code.** A test failure after integration means the task needs the
  repair lane. The repair agent may make scoped compatibility fixes with full
  verification.
- **Auto-merging the integration branch into master.** This skill stops
  at producing the linear branch; the user merges.

## Related skills

- `backlog-execute-serial` — the one-at-a-time sibling: a single worktree and
  branch, each approved task's commits left in place for the next task to build
  on. Use it instead of this skill when tasks have ordering dependencies or you
  want a lower-resource run. Shares the implementer/reviewer/re-dispatch prompts
  in `../backlog-execute-shared/`.
- `super-using-git-worktrees` — full discipline for worktree setup
  (gitignore checks, native-tool preference, baseline tests).
- `super-subagent-driven-development` — heavier-weight plan-driven version
  with explicit spec + code-quality review stages.
- `backlog` — produces the task files this skill consumes.
