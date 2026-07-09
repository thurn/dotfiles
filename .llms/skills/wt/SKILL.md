---
name: wt
description: Implement a task in an isolated git worktree, then optionally replay its commits onto master as linear history. Use when explicitly requested by name for sandboxed task work that may be promoted to master.
disable-model-invocation: true
---

# Worktree Task

Implement the requested task inside a standalone git worktree, then offer to
replay the resulting commits onto `master`.

## 1. Create the worktree first — before any analysis

This is the very first thing to do on startup, **before** reading code,
searching the repository, or otherwise analyzing the task. The user's primary
working tree may be modified while you work, so anything you read from it can
become stale or invalid. Create the worktree immediately and perform *all*
code-based analysis — reading files, searching, running tools — against the
worktree, never the primary tree.

From the current repository root, create an isolated worktree under
`.worktrees/`:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
SLUG="<short-kebab-case-name-for-the-task>"
BRANCH="wt/$SLUG"
WORKTREE="$REPO_ROOT/.worktrees/$SLUG"
git -C "$REPO_ROOT" worktree add -b "$BRANCH" "$WORKTREE" master
```

Pick `SLUG` from the task description. If `.worktrees/` is not already
git-ignored, that is fine — the worktree directory itself is registered with git
and not treated as untracked content.

## 2. Implement the task

`cd` into `$WORKTREE` and do all work there, including all analysis and
investigation. Follow the repository's own conventions and verification steps
(lint, typecheck, tests). Commit your work with detailed commit messages, using
separate commits where it makes sense — the commit boundaries are preserved when
promoting to master.

Do not read from, analyze, or touch the user's primary working tree or its
checked-out branch — it may change underneath you and give invalid information.

Keep a small runtime ledger for every long-lived process or session you start
while working: dev/demo servers, emulators, browser automation controllers, and
headless browser sessions. For each item record what it is, where it was started
from, its port or session name, and its PID or process group when available.
Anything added to this ledger must have an explicit cleanup step before the task
is considered finished, unless the user has deliberately asked to keep it
running.

## 3. Prompt before promoting

When the task is complete, stop and ask the user whether to move the commits
from this worktree's branch onto `master`. Ask using the `AskUserQuestion` tool
with two explicit options: "Yes" (promote the commits onto `master`) and "No"
(leave them on the worktree branch). Do not proceed without explicit approval.

If you were working on a visual change, provide the complete file paths to one
or more screenshots showing your work.

### Leave a running demo server so the user can interact with the work

In addition to the screenshots, before you prompt for promotion, start the
project's dev/demo server **from inside `$WORKTREE`** and leave it running so the
user can click through the change themselves. The screenshots show a frozen
moment; the live server lets the user exercise the actual behaviour.

- Run the server as a **background** process so it keeps running across turns —
  it must stay up while the user reviews and until they answer the promotion
  prompt. Do not block the session waiting on it.
- Record the exact server command, port, PID, and process group in the runtime
  ledger when starting it. If the server command spawns children, such as Vite,
  Firebase emulators, Java processes, or other watchers, the cleanup step must
  account for the whole process tree rather than only the first PID.
- Start it on a **non-default port** so it never collides with a server the user
  is already running in their primary tree (for example, if the project's
  default dev port is `5173`, start the demo on `5174` or another free port).
- Serve from the worktree, not the primary tree, so the running instance
  reflects the code under review.
- Once it is up, verify it actually responds (e.g. curl the URL or hit it with
  the browser tool) before telling the user it is ready — do not hand over a URL
  you have not confirmed loads.

Then, in the same plain-text message that carries the screenshot paths, give the
user the **demo URL** on its own line (e.g. `http://localhost:5174/`), along with
any `?goto=`/route hint needed to land directly on the changed screen. Keep the
`AskUserQuestion` prompt itself concise and refer to "the demo URL and
screenshots above." Note the demo server will be shut down when the change is
promoted (or if promotion is declined and the user is done reviewing).

**Prefer full-screen screenshots.** Capture the full browser viewport so the
user can evaluate the changed UI in its real page context, including nearby
layout, spacing, and controls. If multiple viewport sizes matter, provide one
full-screen screenshot per viewport.

Every `agent-browser` run must use a unique session name for this task, and that
session name must be recorded in the runtime ledger. Reuse the same session for
all screenshots and browser QA for this task. When browser QA is complete, close
or stop that exact session and verify its controller and headless Chrome
processes are gone. Do not leave `agent-browser` controllers or
`agent-browser-chrome-*` profiles running after the task is complete.

**Capture screenshots at high pixel density so they stay legible.** Set a
**2× device scale** *before* taking each screenshot so the output has enough
detail to read during QA. With `agent-browser` the device-scale argument is the
third value of the viewport command — do not omit it:

```bash
agent-browser set viewport 1920 1080 2   # the trailing "2" is the 2x scale — required
```

**Always verify the result after capturing** — do not assume the viewport
setting took effect. Run `file <path>` to confirm the dimensions match the full
viewport at the intended device scale and that the image is crisp and readable.
If it is low-detail or the wrong viewport size, recapture before presenting the
screenshot to the user.

The client renders a local screenshot inline **only when its path appears as
bare plain text** in a normal assistant message. Two things break that
rendering, so avoid both:

- **Do not put the paths inside the `AskUserQuestion` call.** That tool renders
  its `question` and option text as plain text and never renders images, so a
  path placed there shows as a literal string the user cannot view. Keep the
  question concise and just refer to "the screenshots above."
- **Do not wrap the path in any formatting.** No backticks/inline code, no
  fenced code block, no markdown link (`[...](...)`) or image (`![...]`)
  syntax, no surrounding quotes. Any of these suppress the inline render and
  leave the user with unclickable text. Write the path on its own line as
  raw text, nothing else — e.g. an `open ...` command in a code block will NOT
  render; the bare path on its own line WILL.

So: in the normal assistant message you send *before* calling
`AskUserQuestion`, put each screenshot path on its own line as bare plain text.
Save the screenshots to a stable, easy-to-reach location rather than a scratch
temp dir — a good default is a `screenshots/` folder inside the worktree (e.g.
`$WORKTREE/screenshots/`), which is removed with the worktree on cleanup; offer
to copy them to the user's Desktop if they prefer.

## 4. Replay commits onto master (only after approval)

**Never delete, `git clean`, `git checkout --`, or `git stash` untracked or
modified files in the primary working tree.** Promotion runs against the user's
primary checkout, which may contain files created or changed by running
processes while you work — for example runtime data such as player saves under
`saved-quests/`, local config, or scratch output. These are not build junk and
are not yours to remove. If `git status` shows untracked or modified files
before or after `checkout`/`merge`/`cherry-pick`, leave them exactly as they
are. A spotless working tree is **not** a goal of promotion; preserving the
user's files is. Only ever `cherry-pick`/`merge` the worktree branch's commits —
do not "tidy up" anything else in the primary tree to make `git status` clean.

Take the commits unique to the worktree's branch and replay them on top of
`master`, preserving commit order and keeping them as separate commits. The
final result is that `master` points to the new linear history: the old master
commits followed by these worktree commits. Do not merge, do not squash, and do
not leave the result on a feature branch.

**Reconcile in the worktree first, so the primary tree only ever
fast-forwards.** All conflict resolution must happen on the worktree branch,
inside the sandbox — never on `master` in the primary checkout. With the branch
checked out in the worktree, rebase it onto the current `master`:

```bash
git -C "$WORKTREE" rebase master
```

This replays *the branch's* own commits on top of `master`, resolving any
conflicts here in the isolated worktree. It rewrites only the branch commits
(throwaway anyway); `master`'s commits are untouched and keep their original
hashes. If `master` has not moved since the worktree was created, this is a
no-op. Resolve any conflicts as they arise, keeping each commit separate.

Do NOT instead run `git rebase "$BRANCH"` while `master` is checked out in the
primary tree. That is the dangerous inverse: it replays *master's* own commits
on top of the branch — silently reordering and rewriting already-published
commits, which then diverge from `origin/master` and require a history-rewriting
force-push. The safe command above (`git -C "$WORKTREE" rebase master`, run with
the *branch* checked out) rebases the branch onto master, which is what we want.

Once the branch sits cleanly on top of `master`, fast-forward `master` onto it
in the primary tree. Because the branch is already based on the current master,
this can only fast-forward — it can never produce a conflict on `master`:

```bash
# From the repo root (primary working tree), with master checked out there:
git -C "$REPO_ROOT" checkout master
git -C "$REPO_ROOT" merge --ff-only "$BRANCH"
```

If this `--ff-only` merge fails, `master` advanced again between the rebase and
the merge. Do not fall back to resolving conflicts on `master`; re-run the
worktree rebase (`git -C "$WORKTREE" rebase master`) and retry the fast-forward.

After the merge, confirm `master` is a linear history of the old master commits
followed by the worktree commits (`git log --oneline`), and that it has not
diverged from `origin/master` (the existing master commits keep their original
hashes).

## 5. Clean up (only after a successful promotion)

First, clean up every item in the runtime ledger. Shut down the demo server you
started in step 3 and any related child processes. Kill only the recorded
processes, process group, session, or specific port for this task — never with a
broad pattern that could also kill a server the user is running in their primary
tree. Confirm the demo port is free, browser automation sessions are closed, and
no worktree-rooted emulator or watcher processes remain before continuing.

Then remove the worktree and delete its now-redundant branch:

```bash
git -C "$REPO_ROOT" worktree remove "$WORKTREE"
git -C "$REPO_ROOT" branch -D "$BRANCH"
```

Because the demo server was serving out of `$WORKTREE`, it must be stopped before
`worktree remove` so the directory can be cleanly removed.

Do not delete the worktree or branch if promotion was declined or did not
complete cleanly. If promotion was declined, leave the worktree and branch in
place, then ask whether the user wants to keep the demo server running. If they
are done reviewing, or if they ask to stop, clean up the runtime ledger even
though the branch remains. If they deliberately keep the demo server running,
state exactly which URL/port is still live and remind them that it should be
stopped when review is finished.

Before ending the task, run a final resource check scoped to the recorded
ledger: verify the demo port, browser session, and worktree-rooted server or
emulator processes are either stopped or explicitly being left alive at the
user's request. Report any intentionally retained runtime resources in the final
message.

## 6. Follow-up requests stay in a worktree

Any follow-up request that builds on a `/wt` task — refinements, fixes,
adjustments to what was just implemented — is itself worktree work. Do **not**
implement it directly on the primary tree's `master`. Start a fresh worktree
(go back to step 1 with a new slug) and run the same isolate → implement →
prompt → promote → clean-up cycle. This holds even after a prior promotion has
already landed on `master`: the next change gets its own worktree, not an
in-place edit of the primary checkout.
