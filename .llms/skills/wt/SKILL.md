---
name: wt
description: Implement a task in an isolated git worktree, then optionally replay its commits onto master as linear history. Use when explicitly requested by name for sandboxed task work that may be promoted to master.
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

**Always create a fresh worktree for a new task.** An existing worktree with a
similar name, branch, subject, or apparent prior progress is not an invitation
to reuse it. Do not inspect that worktree's status, log, diff, or files to decide
whether it is relevant; it may belong to the user or another agent. Finding an
existing worktree and continuing there is prohibited.

The only exceptions are:

- the user explicitly tells you in the current request to continue in a
  specific existing worktree or branch; or
- this task is a follow-up to an unpromoted worktree that you created earlier
  in the same conversation, as defined in section 6.

Filesystem discovery alone never establishes either exception. If your chosen
slug or branch already exists, choose a new slug and create a new worktree; do
not open or reuse the existing one.

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
or more screenshots showing your work. Use the smallest evidence set that
demonstrates the distinct visual risks: normally one representative desktop,
one representative mobile, and one changed interaction state when each is
relevant. Add a viewport or state only when it exercises a different layout,
safe-area, or rendering risk. Before running an expensive final matrix, inspect
one representative capture early enough to correct the visual direction.

### Leave a running demo server so the user can interact with the work

In addition to the screenshots, before you prompt for promotion, start the
project's dev/demo server **from inside `$WORKTREE`** and leave it running so the
user can click through the change themselves. The screenshots show a frozen
moment; the live server lets the user exercise the actual behaviour.

Do not use a long-lived `exec_command` PTY, its returned session id, a trailing
`&`, or `nohup` alone as the server's lifetime boundary. Those processes can
remain children of Codex's turn-scoped command runner and be reaped after the
final response. Hand the server to an operating-system service manager, close
the launching command, and use the service identity as the cleanup handle.

On macOS in the Codex desktop app, use a transient `launchd` service. Choose a
free non-default port, then run:

```bash
DEMO_PORT=5174
DEMO_LABEL="codex.wt.$SLUG.$DEMO_PORT"
DEMO_LOG="/tmp/$DEMO_LABEL.log"

lsof -iTCP:"$DEMO_PORT" -sTCP:LISTEN -n -P

launchctl submit -l "$DEMO_LABEL" -o "$DEMO_LOG" -e "$DEMO_LOG" -- \
  /bin/zsh -lc 'cd "$1" && shift && exec "$@"' demo-server \
  "$WORKTREE" npm run dev -- --port "$DEMO_PORT"
```

The preflight `lsof` output must be empty; otherwise choose another port rather
than stopping a process that is not in this task's runtime ledger.

This transfers ownership to `launchd`; the command used to submit it should
return immediately. Record `DEMO_LABEL`, `DEMO_LOG`, `DEMO_PORT`, `$WORKTREE`,
and the exact server command in the runtime ledger. Treat the label—not a Codex
tool session id—as the primary lifecycle handle. Inspect startup and verify both
the service and review URL before handoff:

```bash
launchctl print "gui/$(id -u)/$DEMO_LABEL"
tail -n 80 "$DEMO_LOG"
curl -fsS "http://localhost:$DEMO_PORT/path/to/review" >/dev/null
```

If startup is still in progress, poll the URL in bounded intervals while
continuing to communicate; do not assume a successful `launchctl submit` means
the application is ready. To restart the demo, unload the exact label, confirm
the port is free, and submit the same service again. To stop it during cleanup:

```bash
launchctl bootout "gui/$(id -u)/$DEMO_LABEL"
lsof -iTCP:"$DEMO_PORT" -sTCP:LISTEN -n -P
```

The final `lsof` output must be empty. If the project spawns children such as
Vite, Firebase emulators, Java processes, or other watchers, also verify no
process rooted in `$WORKTREE` remains. Never use a broad `pkill` pattern.

When `launchctl` is unavailable, use the host's actual user service manager
(for example, a named `systemd-run --user` unit) with the same properties: a
stable unit identity, detached logs, bounded readiness checks, and exact-unit
cleanup. If no service manager is available, state that persistence across
turns cannot be guaranteed; do not describe `&`, `nohup`, or an exec session as
a durable demo server.

- Start it on a **non-default port** so it never collides with a server the user
  is already running in their primary tree (for example, if the project's
  default dev port is `5173`, start the demo on `5174` or another free port).
- Serve from the worktree, not the primary tree, so the running instance
  reflects the code under review.
- Once it is up, verify it actually responds (e.g. curl the URL or hit it with
  the browser tool) before telling the user it is ready — do not hand over a URL
  you have not confirmed loads.

Then, in the same normal assistant message that carries the screenshots, give
the user the **demo URL** on its own line. The demo URL must be a review URL, not
merely the server root. It must land directly on the changed screen/state with
the changed UI visible, using the real route plus any required query parameters
(`?goto=...`, `?game=...`, feature flags, seed values, etc.). A URL such as
`http://localhost:5174/` is acceptable only when the changed UI is visible at
that exact URL after a fresh open. For stateful apps, create or preserve the
local state needed for review (for example a QA room in the local emulator) and
include that direct URL, such as
`http://localhost:5174/dreamscape/0-firstlight-meadow?game=<roomId>`.

Before handing over the demo URL, open that exact URL in a fresh browser tab or
isolated browser session and assert both:

- `location.href` is the URL you will give the user.
- A selector/text assertion proves the changed UI is visible immediately.

Keep the `AskUserQuestion` prompt itself concise and refer to "the demo URL and
screenshots above." Note the demo server will be shut down when the change is
promoted (or if promotion is declined and the user is done reviewing).

When providing a mobile demo URL intended for Safari in an iOS Simulator, also
put a directly copyable command immediately after the URL, using the same exact
verified review URL:

```bash
xcrun simctl openurl booted 'http://localhost:5174/path/to/review?goto=scene'
```

Keep the URL shell-quoted so query parameters are passed intact.

**Prefer full-screen screenshots.** Capture the full browser viewport so the
user can evaluate the changed UI in its real page context, including nearby
layout, spacing, and controls. The default evidence budget is one desktop, one
mobile, and one changed interaction state when relevant; it is a maximum, not a
quota. Add a full-screen viewport only when it demonstrates a distinct risk.
Use DOM geometry and state assertions for objective behavioral or layout claims
rather than multiplying screenshots.

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

### Codex app review artifacts: inline images and real demo URLs

When running in the Codex desktop app, local screenshots must be displayed
inline as part of the promotion request turn itself. Do not send the demo URL
and screenshots in an earlier progress update and then ask for promotion later;
the user must receive the review artifacts and the promote/leave-on-branch
choice together. Use Markdown image syntax with an absolute filesystem path:

```md
![Short description](/absolute/path/to/worktree/screenshots/changed-region.png)
```

Rules for Codex app screenshot delivery:

- Use absolute filesystem paths only. Relative paths do not render reliably.
- Put each screenshot image on its own line or paragraph so the app has room to
  render it.
- Do not put screenshot paths or Markdown images inside `AskUserQuestion`; that
  tool renders plain text and will not display images inline. Instead, send the
  Codex app artifact message and the `AskUserQuestion` call as one immediate
  review/promotion handoff, with no intervening work or status update.
- Do not provide only file paths when the environment supports inline images.
  The user should see the screenshots in the conversation without asking for a
  second message.
- Keep the local image files in a stable worktree path, preferably
  `$WORKTREE/screenshots/`, until promotion/cleanup is complete.

The Codex app review/promotion handoff should include, in this order:

1. The direct demo URL on its own line.
2. Any short note needed to explain the state it opens, such as the viewport or
   persisted local room.
3. Each screenshot rendered inline using Markdown image syntax.
4. A brief note that the server will remain running until the user answers.
5. The promotion question with explicit "Yes" and "No" options, either via
   `AskUserQuestion` immediately after the artifact message or, when that tool
   is unavailable, in the same assistant message.

Example:

```md
Demo URL:

http://localhost:5174/dreamscape/0-firstlight-meadow?game=abc123

This opens directly on the changed starting deck modal in the local emulator
room used for QA.

![Starting deck modal](/Users/name/repo/.worktrees/task/screenshots/modal.png)

![Hover info cards](/Users/name/repo/.worktrees/task/screenshots/hover-info.png)

The demo server is still running from the worktree on port 5174 while you
review.

Promote the committed worktree changes onto master?

- Yes: replay the worktree commit onto master, then clean up the demo server,
  worktree, local branch, and pushed review branch.
- No: leave the commit on the worktree branch and keep master unchanged.
```

If `AskUserQuestion` is not available in the current Codex mode, ask the same
promotion question as plain assistant text in the same message as the artifacts,
with explicit "Yes" and "No" options, and do not promote until the user answers.

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

Then remove the worktree and delete its now-redundant local branch:

```bash
git -C "$REPO_ROOT" worktree remove "$WORKTREE"
git -C "$REPO_ROOT" branch -D "$BRANCH"
```

Because the demo server was serving out of `$WORKTREE`, it must be stopped before
`worktree remove` so the directory can be cleanly removed.

If the review branch was pushed to a remote, delete that remote branch too after
`master` has been pushed successfully. The remote review branch is a promotion
artifact, not a retained archive, once its commits are on `master`:

```bash
git -C "$REPO_ROOT" push origin --delete "$BRANCH"
```

If the remote branch does not exist, note that and continue. Do not let a
missing remote branch turn a successful promotion into a failure.

Do not delete the worktree, local branch, or remote branch if promotion was
declined or did not complete cleanly. If promotion was declined, leave the
worktree and branch in place, then ask whether the user wants to keep the demo
server running. If they are done reviewing, or if they ask to stop, clean up the
runtime ledger even though the branch remains. If they deliberately keep the
demo server running, state exactly which URL/port is still live and remind them
that it should be stopped when review is finished.

Before ending the task, run a final resource check scoped to the recorded
ledger: verify the demo port, browser session, and worktree-rooted server or
emulator processes are either stopped or explicitly being left alive at the
user's request. Also verify the worktree path, local branch, and pushed review
branch are gone after a successful promotion. Report any intentionally retained
runtime resources or retained review branches in the final message.

## 6. Follow-up requests stay with the active review worktree

Any follow-up request that builds on an unpromoted `/wt` task — refinements,
fixes, adjustments to what was just implemented, or feedback from the live demo
and screenshots — must continue in the **same existing worktree and branch**
until the user either approves promotion or declines it. Do **not** create a new
worktree while the previous `/wt` review is still active. The user is reviewing
one coherent branch; keep subsequent commits on that branch, update the running
demo from that same worktree, recapture screenshots, push the branch again, and
then ask the promote/leave-on-branch question again with the updated artifacts.

An "active review worktree" exists only when you created it earlier in this
same conversation and handed its artifacts to the user for review, or when the
user explicitly identifies the worktree or branch to continue. A matching entry
from `git worktree list`, a suggestive branch name, nearby commits, or
uncommitted changes does not make a worktree active for your task. Never infer
ownership or continuity from repository state.

Do **not** implement the follow-up directly on the primary tree's `master`.
The active review worktree remains the isolation boundary until promotion is
resolved.

Start a fresh worktree only when there is no active unpromoted review worktree
for the task, for example:

- the previous `/wt` task was already promoted and cleaned up;
- promotion was declined and the user is asking for a new, separate attempt;
- the follow-up is unrelated to the active review branch.

After a prior promotion has landed on `master` and cleanup is complete, the next
change gets its own worktree rather than an in-place edit of the primary
checkout.
