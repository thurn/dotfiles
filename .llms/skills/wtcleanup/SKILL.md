---
name: wtcleanup
description: Replay the current worktree branch's unique commits onto master as linear history, then remove the worktree and its branch. Use when explicitly requested by name to promote and clean up a worktree task.
disable-model-invocation: true
---

# Worktree Cleanup

Promote the current worktree's commits onto `master` as linear history, then
remove the worktree and its branch. This is the companion cleanup step for the
`wt` skill.

Take the commits unique to the current worktree's branch and replay them on top
of `master`, preserving commit order and keeping them as separate commits. The
final result is that `master` points to the new linear history: the old master
commits followed by these worktree commits. Resolve conflicts as needed. Do not
leave the result on a feature branch, do not merge, do not squash, and do not
create a new permanent branch.

## 1. Identify the worktree and branch

From inside the worktree (or by inspecting `git worktree list`), determine the
repository root, the worktree path, and its branch:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"   # run from the primary tree
BRANCH="<the worktree's branch>"
WORKTREE="<the worktree's path>"
```

## 2. Replay commits onto master

**Never delete, `git clean`, `git checkout --`, or `git stash` untracked or
modified files in the primary working tree.** This step runs against the user's
primary checkout, which may contain files created or changed by running
processes while you work — for example runtime data such as player saves under
`saved-quests/`, local config, or scratch output. These are not build junk and
are not yours to remove. If `git status` shows untracked or modified files
before or after `checkout`/`merge`/`cherry-pick`, leave them exactly as they
are. A spotless working tree is **not** a goal of promotion; preserving the
user's files is. Only ever `cherry-pick`/`merge` the worktree branch's commits —
do not "tidy up" anything else in the primary tree to make `git status` clean.

First try a fast-forward only. This succeeds when master has not moved since the
worktree was created, and otherwise fails without touching anything:

```bash
git -C "$REPO_ROOT" checkout master
git -C "$REPO_ROOT" merge --ff-only "$BRANCH"
```

Do NOT use `git rebase "$BRANCH"` here. When master has advanced, `git rebase
"$BRANCH"` (run while on master) replays *master's* own commits on top of the
branch — silently reordering and rewriting already-published commits, which then
diverge from `origin/master` and require a history-rewriting force-push.
`merge --ff-only` cannot do this: it only fast-forwards or fails.

If the `--ff-only` merge fails because master has advanced, replay the branch's
unique commits onto the current master, preserving order:

```bash
git -C "$REPO_ROOT" cherry-pick master.."$BRANCH"
```

Resolve any conflicts as they arise, keeping each commit separate. After the
replay, confirm `master` is a linear history of the old master commits followed
by the worktree commits (`git log --oneline`), and that it has not diverged from
`origin/master` (the existing master commits keep their original hashes).

## 3. Clean up (only after a successful promotion)

Remove the worktree and delete its now-redundant branch:

```bash
git -C "$REPO_ROOT" worktree remove "$WORKTREE"
git -C "$REPO_ROOT" branch -D "$BRANCH"
```

Do not delete the worktree or branch if promotion did not complete cleanly.
