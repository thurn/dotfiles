---
name: wtbatch
description: Take a batch of work items (bugs, features, chores — related or not), analyze their size and interdependencies, plan an execution strategy, run subagents in isolated git worktrees to complete them, and end with one consolidated promote prompt covering all items. Use when explicitly requested by name with a list of tasks.
---

# Worktree Batch

You are the **planner**. You do not implement anything yourself — you triage
the batch, decide the execution strategy, dispatch subagents that each work in
their own isolated git worktree, monitor them, and consolidate everything into
a single promote decision at the end. Your context is for coordination;
implementation detail lives in the subagents.

The unit of work is a **group**: one or more work items assigned to one
subagent in one worktree. The whole skill is: triage → group → dispatch →
collect → one consolidated promote prompt → serial promotion → cleanup.

## 1. Triage the batch (before touching any code)

Parse the user's list into discrete work items. For each item record:

- **A stable ID** (`item-1`, `item-2`, …) and a short kebab-case slug.
- **Size estimate** — S / M / L:
  - **S** — mechanical, well-specified, likely 1–2 files (copy change, small
    style fix, config tweak, obvious one-function bug).
  - **M** — a normal feature or bug fix: some investigation, a few files,
    needs verification.
  - **L** — open-ended, multi-file, needs design judgment, or the description
    is vague enough that discovery is most of the work.
- **Touched area** — best guess at the subsystem/files involved. A quick
  `grep`/`glob` against the repo is fine for this; deep code reading is not —
  that's the subagents' job, done inside their worktrees.
- **Dependencies** — does this item build on, conflict with, or share files
  with another item in the batch?

If an item is too ambiguous to size or scope at all, ask the user about it
now, in one batched `AskUserQuestion` covering every ambiguous item — not one
interrupt per item, and never mid-execution.

## 2. Decide the execution strategy

Grouping rules, in priority order:

1. **Dependent items share a group — but not necessarily a subagent.** If B
   builds on A, they belong to the same group (same worktree, same branch, in
   order) so B is implemented on top of A's commits. Never split a dependency
   chain across parallel worktrees. Whether one subagent handles the whole
   chain is a context-budget question: small items (bug fixes, small
   features) can share one subagent, but if the chain's combined work would
   not fit comfortably within ~200,000 tokens of context, run **sequential
   subagents in the same worktree** — dispatch a fresh subagent per item (or
   per chunk), each building on the previous one's commits, with your
   dispatch prompt carrying forward the interfaces and decisions the next
   item needs.
2. **Items that touch the same files share a group.** Two parallel worktrees
   editing the same file guarantee a promotion conflict; put them in one
   group instead, applying the same context-budget test to decide between one
   subagent and sequential subagents.
3. **Independent items get independent groups** and run in parallel.
4. **Batch small items, bounded by context.** The governing question for how
   much work one subagent gets is: *does this set of tasks fit comfortably
   within ~200,000 tokens of context, including the investigation and
   verification it will take?* In practice that means up to ~3 S items or
   2 S + 1 M per subagent when they're thematically adjacent (same screen,
   same subsystem) — one worktree setup amortized over several cheap fixes —
   while an L item always gets a subagent to itself.
5. **Cap concurrency at ~4 groups at once.** More groups than that: run in
   waves, dispatching the next group as one finishes.

Model selection per group (always specify the model explicitly when
dispatching): S-only groups → a fast/cheap model; M groups → the standard
model (Sonnet); L groups → the most capable model available (Opus-tier).

Before dispatching, post the plan to the user as a short table — item, size,
group, parallel/sequential, model — as a status update. **Do not wait for
approval**; proceed immediately unless the user interjects. This is a
flexible skill: if the batch's shape calls for a different structure (one big
sequential chain, a pipeline, a single group), use it — the rules above are
defaults, not law.

## 3. Create worktrees and dispatch subagents

**You** create each group's worktree (so slugs and branches stay coordinated),
then hand the path to the subagent. From the repo root:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
SLUG="<group-slug>"
BRANCH="wt/$SLUG"
WORKTREE="$REPO_ROOT/.worktrees/$SLUG"
git -C "$REPO_ROOT" worktree add -b "$BRANCH" "$WORKTREE" master
```

Create all worktrees for a wave first, then dispatch all of that wave's
subagents **in a single message** so they run concurrently.

Each subagent gets a self-contained dispatch prompt — it inherits none of your
context. Include:

1. **Where to work:** the absolute `$WORKTREE` path. All reading, searching,
   editing, building, and testing happens inside it. It must never read from
   or touch the primary working tree or any other group's worktree.
2. **The work items**, verbatim from the user plus your triage notes, in
   execution order, each with its item ID.
3. **Repo conventions:** follow the repository's own CLAUDE.md/AGENTS.md and
   verification steps (lint, typecheck, tests). In a fresh worktree, run the
   project's setup first (e.g. `npm install` when `node_modules` is not
   committed).
4. **Commit contract:** commit per item (or finer), detailed messages, each
   message naming the item ID it completes. Do **not** push, do not touch
   `master`, do not create extra branches.
5. **Evidence contract:** match evidence to risk. Behavioral claims require
   interactions, state or log assertions, DOM measurements, and captured error
   buffers. Visual changes require screenshots in `$WORKTREE/screenshots/` at
   **2× device scale**. Use at most one representative desktop, one mobile, and
   one changed interaction state by default; add captures only for distinct
   responsive, safe-area, or rendering risks. Use a full-screen image for
   holistic composition and a tight selector crop only when the localized
   detail would otherwise be unreadable. Verify file dimensions with `file`
   after capture. With `agent-browser`: `set viewport W H 2`; use a unique
   `--session <group-slug>` and a unique dev-server port per group, and kill
   only its own server (by port/PID) when done. Capture and inspect one
   representative state before an expensive final matrix so a wrong visual
   direction is corrected early.
6. **Report contract:** the final message must be a structured report — per
   item: `DONE`/`BLOCKED` + one-line summary + commit hashes + screenshot
   paths (absolute); then overall verification results (commands run and
   outcomes) and any concerns. No prose beyond that.

## 4. Monitor and collect

As group reports arrive:

- **DONE:** record the commits, screenshot paths, and verification evidence
  in your ledger. Dispatch the next wave's group into a freed slot.
- **BLOCKED / failed:** diagnose from the report. Missing context →
  re-dispatch the same group with the context added. Underpowered →
  re-dispatch on a more capable model. Genuinely stuck or the item is wrong →
  mark that item blocked and carry it, with the reason, into the consolidated
  prompt; don't let one blocked item stall the other groups.
- A report claiming DONE **without verification evidence** (no test/lint
  output, no screenshots for a visual change) is not done — send it back.

Keep a ledger file at `$REPO_ROOT/.worktrees/wtbatch-ledger.md` with one line
per item: status, group, branch, commits, screenshot paths. If the session is
compacted, the ledger plus `git log` on the `wt/*` branches is your recovery
map — trust it over your recollection, and never re-dispatch a group the
ledger marks complete.

## 5. One consolidated promote prompt

When every group is DONE or blocked, present **one** consolidated review, not
per-group prompts.

First, for each group with visual changes, start its demo server as a
background process from inside its own worktree, each on its **own
non-default port** (e.g. 5174, 5175, …) so they collide with neither each
other nor anything the user runs in the primary tree. Verify each URL
actually responds before handing it over.

Then send a plain-text message organized by item:

- Item ID and title, one-line summary of what was done, which branch holds it.
- Each screenshot path on its own line as **bare plain text** — no backticks,
  no markdown links or image syntax, no quotes; anything else suppresses the
  inline render.
- The group's demo URL on its own line, with any route/`?goto=` hint needed
  to land on the changed screen.
- Blocked items listed last with the reason.

Then call `AskUserQuestion` with **multiSelect: true**, one option per group
("Promote <slug>: item-1, item-3"), so the user can promote any subset. Keep
the question text concise and refer to "the screenshots and demo URLs above."
Do not proceed without explicit approval.

## 6. Promote approved groups — serially

**Never delete, `git clean`, `git checkout --`, or `git stash` untracked or
modified files in the primary working tree.** Runtime data, saves, and local
config there are the user's, not yours. Only ever fast-forward `master`; a
spotless `git status` is not a goal of promotion.

Promote one group at a time. All conflict resolution happens inside that
group's worktree — the primary tree only ever fast-forwards:

```bash
# For each approved group, in dependency-safe order:
git -C "$WORKTREE" rebase master          # replay the group's commits onto current master
git -C "$REPO_ROOT" checkout master
git -C "$REPO_ROOT" merge --ff-only "$BRANCH"
```

Because each promotion advances `master`, every subsequent group must be
rebased onto the **new** master before its own fast-forward — that is why
promotion is serial. If a rebase conflicts, resolve it in that worktree
(dispatch a fix subagent into the worktree if the conflict is substantial),
keep the commits separate, and never resolve anything on `master` itself. If
the `--ff-only` merge fails, `master` moved again — re-run the worktree
rebase and retry; never fall back to conflict resolution on `master`.

Do NOT run `git rebase "$BRANCH"` with `master` checked out in the primary
tree — that rewrites master's own published commits and forces a divergent
history.

After all approved groups land, confirm with `git log --oneline` that
`master` is linear — the old master commits (original hashes intact) followed
by each group's commits in promotion order — and has not diverged from
`origin/master`. Then push if the repository's conventions say to push.

## 7. Clean up

For each **promoted** group: kill its demo server (by its specific port/PID
only — never a broad pkill pattern), confirm the port is free, then:

```bash
git -C "$REPO_ROOT" worktree remove "$WORKTREE"
git -C "$REPO_ROOT" branch -D "$BRANCH"
```

Groups the user **declined** keep their worktree, branch, and running demo
server until the user says they're done reviewing. Blocked items' worktrees
also stay, so their partial work is recoverable. Remove the ledger file only
when every group is either promoted-and-cleaned or explicitly abandoned.

## 8. Follow-ups stay in worktrees

Any follow-up — a fix to a promoted item, a retry of a blocked one, a new
item the review surfaced — is itself worktree work. Re-enter this cycle (or
plain `wt` for a single item) with a fresh worktree; never edit the primary
tree's `master` in place.

## Red flags

- Implementing anything yourself in the planner context, or reading deeply
  into the codebase before worktrees exist.
- Two parallel worktrees whose items touch the same files.
- A dependency chain split across parallel groups.
- Dispatching a subagent without an explicit model, or without the report
  contract.
- Accepting DONE without verification evidence.
- Per-group promote prompts instead of one consolidated prompt.
- Screenshot paths wrapped in backticks/markdown, or full-screen captures.
- Parallel promotion, conflict resolution on `master`, or any cleanup of the
  primary tree's untracked/modified files.
- Re-dispatching a group the ledger already marks complete.
