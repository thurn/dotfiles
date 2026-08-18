---
name: wt
description: Implement and commit a task in an isolated git worktree based on Tollgate's local release branch, immediately submit it as a non-promotable candidate, then obtain one explicit promotion mandate and drive in-scope CI repairs, certified promotion, and remote synchronization to completion without repeated approval prompts.
---

# Worktree Task

Implement and commit the requested task inside a standalone git worktree,
immediately submit the immutable commit to Tollgate for speculative validation,
then offer one scope-bound promotion mandate. After approval, authorize exact
Tollgate candidates and keep repairing and resubmitting in-scope CI failures
until certified promotion onto local `release` and, when configured, remote
`master` succeeds. The user's local
`master` branch remains an ordinary user-owned checkout and is never Tollgate's
promotion target.

## 1. Create the worktree before writing code

It is fine to begin with a research, discussion, or planning phase without
creating a worktree. During that phase, read-only repository inspection may be
performed in the user's primary working tree. If the task remains advisory and
no repository files need to change, no worktree is required at all.

Treat findings from the primary tree as provisional because the user may
modify it while the discussion is in progress. As soon as implementation is
about to begin, create a fresh worktree **before the first repository
modification** and revalidate any repository-specific assumptions that matter
to the implementation there. Do not edit files, run generators or dependency
operations that mutate the checkout, or start task-specific runtime processes
in the primary tree. Once the worktree exists, perform all implementation,
code-based investigation, and verification against the worktree.

**When implementation begins, always create a fresh worktree for a new task.**
An existing worktree with a similar name, branch, subject, or apparent prior
progress is not an invitation to reuse it. Do not inspect that worktree's
status, log, diff, or files to decide whether it is relevant; it may belong to
the user or another agent. Finding an existing worktree and continuing there is
prohibited.

Do not append the worktree name to routine progress updates or every turn. State
the worktree name in bold in the review/promotion handoff that asks the user the
explicit Yes/No promotion question.

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
git -C "$REPO_ROOT" worktree add -b "$BRANCH" "$WORKTREE" release
```

Pick `SLUG` from the task description. If `.worktrees/` is not already
git-ignored, that is fine — the worktree directory itself is registered with git
and not treated as untracked content.

After creating the worktree, confirm that `tg` is installed, the repository is
registered, and Tollgate is healthy. Run these checks from `$WORKTREE` so
repository auto-selection uses the task's own checkout:

```bash
command -v tg
tg --no-launch status
tg --no-launch doctor
```

If Tollgate is unavailable, the repository is not registered, or the doctor
reports a blocking problem, stop and report the setup problem. Do not silently
fall back to direct Git promotion: that would discard the speculative evidence
and exact-promotion guarantees this workflow exists to provide.

## 2. Implement the task

Before making the first repository change, `cd` into `$WORKTREE` and do all
implementation work there, including implementation-time analysis and
investigation. Follow the repository's own conventions and verification steps
(lint, typecheck, tests). Stage every intended deliverable with `git add` so the
index is the exact artifact being frozen for review. Do not push the worktree
branch. Worktree branches are local-only throughout their lifetime.

Once implementation begins, do not read from, analyze, or touch the user's
primary working tree or its checked-out branch — it may change underneath you
and give invalid information. Recheck any relevant conclusions from the earlier
read-only research phase inside the worktree before relying on them.

Keep a small runtime ledger for every long-lived process or session you start
while working: dev/demo servers, emulators, browser automation controllers, and
headless browser sessions. For each item record what it is, where it was started
from, its port or session name, and its PID or process group when available.
Anything added to this ledger must have an explicit cleanup step before the task
is considered finished, unless the user has deliberately asked to keep it
running.

### Finish local review evidence before freezing the candidate

Complete all proportionate local verification, browser QA, screenshots, and
demo preparation before freezing the source commit. The candidate submission
is deliberately the final work action before the review/promotion handoff; do
not leave screenshots, browser walkthroughs, documentation checks, or other
ordinary task work until after submission.

### Freeze, schedule, and hand off immediately

As soon as implementation and proportionate local verification are complete,
inspect the staged diff, confirm it is the exact intended deliverable, and
create one detailed local commit. This commit is not promotion: it is the
immutable source object Tollgate needs in order to run useful speculative CI.

```bash
git -C "$WORKTREE" diff --cached --check
git -C "$WORKTREE" commit -m "<detailed description>"
git -C "$WORKTREE" status --short
```

The worktree must be clean after the commit. Immediately submit it without
promotion authority; do not wait until the user answers the promotion prompt:

```bash
tg --no-launch --json candidate HEAD
```

Record the returned candidate ID, source OID, tested OID, and queue revision as
part of the task state. Confirm that the returned source OID is exactly
`git -C "$WORKTREE" rev-parse HEAD`. The candidate ID, rather than a mutable
branch name, is the handle for status, logs, cancellation, and later approval.
Submission must not move local `release`, local `master`, or remote `master`.

After successful submission, immediately send the review artifacts and ask the
promotion question. Do **not** run `tg wait`, poll `tg status`, inspect candidate
logs, or delay the handoff to see whether speculative CI passes. The expected
handoff state is commonly `queued`, `preparing`, or `running`; report it as
"Tollgate validation scheduled" rather than waiting to report a certificate.
The whole point of speculative submission is to overlap CI with the user's
review time.

Do not run `tg check` for this purpose: independent-check evidence has no
promotion authority and cannot be reused by a later approval. Only a
synchronous candidate-submission error blocks the initial handoff. An
asynchronous validation result is handled when the user answers: approval waits
for valid evidence and promotion, while a declined candidate is canceled.

### Generate a clickable worktree review link

After submitting the candidate and before presenting the review/promotion
handoff, construct a link that opens this exact worktree in the Worktree Review
VS Code extension. The direct extension URI is:

```text
vscode://dthurn.worktree-review/review?worktree=<URL-encoded-absolute-worktree-path>&base=release
```

Do not present that custom-scheme URI directly in ChatGPT or Codex. The desktop
app does not open it reliably. URL-encode the complete extension URI again as
the `url` parameter of the VS Code HTTPS redirector:

```text
https://vscode.dev/redirect?url=<URL-encoded-complete-vscode-URI>
```

Present the result as a Markdown link named **Open worktree review**. Always use
the captured absolute `$WORKTREE` path and `base=release`; do not substitute the
worktree slug, branch name, a relative path, or `master`. Preserve both encoding
layers: the worktree path and base belong to the inner URI, while the entire
inner URI belongs to the outer redirect URL.

Include this link in the initial review/promotion handoff for every implemented
task, whether or not the task has visual artifacts. Put it in a normal Codex app
message where Markdown links are clickable, not only inside `AskUserQuestion`.
The link is a review artifact and must appear before the explicit Yes/No
promotion choice. Do not repeat it after successful promotion and cleanup,
because Tollgate may have removed the worktree by then.

## 3. Prompt before authorizing promotion

When the task is complete, the review artifacts are ready, and the exact
reviewed commit has just been submitted as a non-promotable candidate, stop and
ask the user whether to grant a promotion mandate for the reviewed task onto
local `release` and, under the repository's push policy, remote `master`.
There must be no candidate-status polling or unrelated work between successful
submission and this handoff. Ask using the `AskUserQuestion` tool with two
explicit options:

- "Yes" — grant a promotion mandate for the reviewed task. Authorize the
  displayed exact candidate, and if CI exposes failures within the approved
  scope, repair them in the same worktree, submit and authorize exact replacement
  candidates, and continue until Tollgate certifies, promotes, and pushes the
  result. Do not ask again merely because CI required another repair iteration.
- "No" — cancel and dequeue the non-promotable candidate while leaving its
  local commit and worktree intact.

Do not authorize promotion without explicit approval. Include the candidate ID
and full source OID in the review handoff so the mandate starts from a clearly
identified reviewed artifact. Describe validation as scheduled; do not wait to
learn or report its later state. The user may approve while it runs, and
Tollgate will promote only an exact candidate after it earns valid evidence.
Also state the worktree name in bold in this handoff. This is the required
worktree-name disclosure; it does not need to be repeated in other turns.

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
the relevant review context visible, using the real route plus any required
query parameters (`?goto=...`, `?game=...`, feature flags, seed values, etc.). A
URL such as `http://localhost:5174/` is acceptable only when the relevant review
context is visible at that exact URL after a fresh open. For stateful apps,
create or preserve the local state needed for review (for example a QA room in
the local emulator) and include that direct URL, such as
`http://localhost:5174/dreamscape/0-firstlight-meadow?game=<roomId>`.

Before handing over the demo URL, open that exact URL in a fresh browser tab or
isolated browser session and assert both:

- `location.href` is the URL you will give the user.
- A selector/text assertion proves the intended starting context and the first
  documented control are visible immediately.

### Present each demo as a reproducible walkthrough

Take the extra effort to prepare a clean, deterministic demo environment that
shows the change in its natural context. With every demo, give the user short
numbered instructions that say how to reproduce the visual result, including
what to open, what to do, and what to observe.

For animations and transitions, stage the review URL one or two interactions
before the changed effect and make the triggering interaction part of the
numbered walkthrough. Prefer “Open this link, then click **Delve** to see the
animation” over a link that immediately autoplays the animation. Verify the
entire walkthrough from a fresh browser session before handing it over.

Keep the `AskUserQuestion` prompt itself concise and refer to "the demo URL and
screenshots above." Note the demo server will be shut down before promotion is
authorized so Tollgate can safely clean up the worktree (or, if promotion is
declined, when the user is done reviewing).

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

Use the globally configured Playwright MCP tools for screenshots and browser
QA. Each MCP client receives its own isolated BrowserContext from the singleton
HTTP service, so reuse this task's MCP client for the full walkthrough. Record
the browser context in the runtime ledger and close it with the MCP browser
close tool when QA is complete. Leave the shared Playwright MCP launchd service
running for other tasks; it owns the single shared Chromium process.

**Capture screenshots at high pixel density so they stay legible.** The shared
Playwright MCP service creates contexts at 2× device scale. Set the CSS viewport
with the MCP browser resize tool before each screenshot.

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

1. The **Open worktree review** Markdown link generated from the absolute
   worktree path.
2. The direct demo URL on its own line.
3. Numbered instructions for reproducing the visual result from the clean state
   opened by that URL.
4. Any short note needed to explain the state it opens, such as the viewport or
   persisted local room.
5. Each screenshot rendered inline using Markdown image syntax.
6. A brief note that the server will remain running until the user answers.
7. The worktree name in bold.
8. The authorization question with explicit "Yes" and "No" options, either via
   `AskUserQuestion` immediately after the artifact message or, when that tool
   is unavailable, in the same assistant message.

Example:

```md
Worktree review:

[Open worktree review](https://vscode.dev/redirect?url=<encoded-vscode-URI>)

Demo URL:

http://localhost:5174/dreamscape/0-firstlight-meadow?game=abc123

This opens a clean local-emulator room one step before the changed Delve
animation.

1. Open the demo URL above.
2. Click **Delve**.
3. Observe the new transition into the starting deck modal.

![Starting deck modal](/Users/name/repo/.worktrees/task/screenshots/modal.png)

![Hover info cards](/Users/name/repo/.worktrees/task/screenshots/hover-info.png)

The demo server is still running from the worktree on port 5174 while you
review.

Candidate `019…` for source commit `012345…` has been scheduled for speculative
validation without promotion authority. Tollgate may still be running.

**Worktree: `task`**

Grant a promotion mandate for this reviewed task through local release to remote master?

- Y: stop the demo, authorize exact candidate `019…`, repair and resubmit any
  in-scope CI failures without another approval prompt, promote local release,
  push certified remote master, then clean up the worktree and local branch.
- N: cancel candidate `019…` and leave its local commit in the worktree with
  release and master unchanged.
```

If `AskUserQuestion` is not available in the current Codex mode, ask the same
authorization question as plain assistant text in the same message as the
artifacts, with explicit "Y" and "N" options, and do not authorize promotion
until the user answers.

## 4. Authorize Tollgate promotion (only after approval)

The user's approval grants a durable promotion mandate for the reviewed task,
initially anchored to the displayed candidate and source OID. Each Tollgate
authorization remains bound to one exact candidate and source OID, but the
user's mandate persists across replacement candidates created solely to repair
CI failures within the approved task scope. Do not ask for approval again for
those repair iterations.

The mandate does not authorize unrelated work, a material change to the
reviewed behavior or user-visible intent, a new product or design decision, or
bypassing Tollgate when evidence is stale. Stop and request renewed review and
approval only when a repair would cross one of those boundaries, or when the
user declines or revokes the mandate.

Before authorization, re-read the candidate and worktree state:

```bash
tg --no-launch --json status <candidate-id>
git -C "$WORKTREE" status --short
git -C "$WORKTREE" rev-parse HEAD
```

Require all of the following:

- the candidate is still active and lacks promotion authority;
- its retained source OID equals the exact clean worktree `HEAD` submitted for
  this iteration (and, for the initial candidate, the commit shown to the user);
- `$WORKTREE` is still clean and `HEAD` equals that source OID; and
- no edit has occurred since this exact candidate was submitted.

This is the first time after initial submission that the workflow should poll
or wait for candidate state. If validation is still queued or running, that is
normal: authorization grants authority immediately and the `--wait` below then
waits for Tollgate to finish validation and promotion. If validation already
finished, Tollgate reuses its sealed evidence when the generation is still
exact.

Stop the task's demo server and any other worktree-rooted runtime processes
before authorizing. Tollgate may automatically remove the clean source worktree
and branch immediately after successful promotion and push; no process should
still depend on that directory.

Then authorize the candidate and wait for the complete result:

```bash
tg --no-launch approve <candidate-id> --wait
```

Candidate authorization is also a scheduling-priority decision. Tollgate keeps
already-authorized work in order, moves this exact candidate and its active hard
dependencies ahead of unrelated candidates that are still awaiting promotion
authority, and automatically rebuilds only the speculative suffix whose prefix
changed. A later candidate must therefore not remain blocked merely because an
earlier independent review has not received a user decision. Do not run
`tg reorder` merely to bypass unrelated unauthorized candidates; reserve manual
reordering for an explicit user-requested order that differs from authorization
order.

Tollgate owns queue serialization, speculative-prefix reconstruction, evidence
reuse, exact-parent verification, local `release` compare-and-swap, and any
configured leased push. Do not run the legacy `promote.sh`, manually rebase the
candidate, check out or move `release`, fast-forward it yourself, cherry-pick the source,
or push the feature branch. Those operations would replace the exact tested
object or race Tollgate's authoritative state.

### Queue-revision conflicts are refreshed, not bypassed

Authorization uses a queue-revision compare-and-swap. If it loses a race to
another queue mutation, fetch fresh candidate and queue state. The existing
user mandate may be retried against the new revision only when the candidate ID
remains active, its retained source OID still equals the worktree's exact
submitted commit, the worktree remains unchanged, and the candidate has not
already gained promotion authority. This retry is transparent because each
authorization is bound to the immutable source, while Tollgate decides whether
the current validation generation can reuse evidence or must rerun.

Do not retry blindly when the candidate is canceled, superseded, terminal for a
source failure, or refers to a different source OID. Resolve the state explicitly.
If CI requires editing the source commit, use the repair loop below.

### Continue through in-scope CI repair iterations

Approval means keep going until CI passes and Tollgate completes promotion; it
is not a prompt to request approval after every failed candidate. When CI fails:

Run `tg --no-launch diagnose <candidate-id>` first to replay exact evidence and attribute the failure, adding `--verify-repair` when Tollgate reports one unambiguous structured repair worth validating.

1. Inspect the failed checks and logs and determine whether the necessary repair
   stays within the reviewed task scope and intent.
2. In the same worktree, cancel or supersede the failed candidate as Tollgate
   requires, make the repair, rerun proportionate local verification, and amend
   the single task commit.
3. Submit the amended `HEAD` as a new immutable candidate. Verify that the new
   candidate's retained source OID exactly equals the clean worktree `HEAD`.
4. Under the still-active user mandate, authorize that exact replacement and
   wait for validation and promotion. Do not ask the user to approve it again.
5. Repeat for further in-scope CI failures until a candidate passes and is
   promoted, or until an external blocker or a scope boundary genuinely requires
   user input.

An amended source always requires a new exact Tollgate candidate. It requires
renewed user approval only when the amendment materially changes the approved
scope, behavior, review artifact, or intent rather than merely repairing CI.

### Completion and push

If repository policy enables remote push, wait for Tollgate to finish both
local promotion and its exact leased push. If automatic push is disabled, run
`tg --no-launch push` after local promotion so only Tollgate-certified contiguous
commits are sent. Never substitute a raw `git push` for this step.

Finally confirm:

- Tollgate reports the candidate promoted and the repository healthy;
- local `release` contains the promoted tested OID;
- configured remote `master` equals local `release`; and
- no unrelated commit was rewritten or reordered.

Do not update the user's local `master` checkout as part of this workflow. It
may remain behind after certified promotion and can later be synchronized by
the user with an ordinary `git pull --ff-only` in the primary checkout. Report
that state when relevant; never treat a stale user-owned `master` as failed
promotion.

If validation, promotion, or push fails, do not bypass the gate or clean up the
worktree. Inspect the candidate status and logs, preserve all evidence, and
repair only within the task worktree when the source itself must change.

## 5. Clean up (only after a successful promotion)

Runtime-ledger cleanup should already have happened immediately before
authorization. Recheck it after promotion: kill only the recorded processes,
process group, browser session, or exact task port — never with a broad pattern
that could stop another worktree's server. Confirm the demo port is free,
browser automation sessions are closed, and no worktree-rooted emulator or
watcher remains.

Tollgate normally removes an eligible clean source worktree and its branch after
successful promotion and remote synchronization. Verify whether `$WORKTREE` and
`$BRANCH` still exist. If Tollgate reports cleanup `needs-attention`, inspect the
exact reason and use `tg worktree remove "$WORKTREE"` when it is safe. Do not
silently force-remove a worktree whose identity, cleanliness, or source OID no
longer matches the captured candidate.

Do not delete the worktree or branch if promotion did not complete cleanly.
If promotion is explicitly declined, cancel the unauthorized candidate with
`tg cancel <candidate-id>` so it cannot block later queue promotion, leave the
local commit and worktree intact, and ask whether the user wants to keep the
demo server running. If they are done reviewing, or if they ask to stop, clean
up the runtime ledger even though the committed branch remains. A future attempt
may resubmit that same clean commit as a new candidate.

Before ending the task, run a final resource check scoped to the recorded
ledger: verify the demo port, browser session, and worktree-rooted server or
emulator processes are either stopped or explicitly being left alive at the
user's request. Also verify the candidate's terminal state and that the
worktree path and local branch are gone after successful promotion. Report any
intentionally retained runtime resources or worktrees in the final message.

## 6. Follow-up requests stay with the active review worktree

Any follow-up request that builds on an unpromoted `/wt` task — refinements,
fixes, adjustments to what was just implemented, or feedback from the live demo
and screenshots — must continue in the **same existing worktree and branch**.
Do **not** create a new worktree while the previous `/wt` review is still active
or while an approved promotion mandate is being completed. The user is
reviewing and promoting one coherent branch.

Before editing a candidate already submitted to Tollgate, cancel its exact
candidate ID and confirm it has left the active queue. Then make the follow-up
changes in the same worktree, stage them, and amend the single local source
commit rather than stacking another task commit:

```bash
tg --no-launch cancel <old-candidate-id>
git -C "$WORKTREE" commit --amend
tg --no-launch --json candidate HEAD
```

Run the appropriate focused verification again, update the running demo from
that worktree, recapture only affected screenshots, and record the replacement
candidate ID and source OID. Before the user has granted a promotion mandate, a
user-requested refinement changes the review artifact and the replacement needs
explicit approval. After the user has granted the mandate, an in-scope CI repair
inherits that mandate and its exact replacement candidate should be authorized
without another prompt. A material change to approved scope, behavior, or
user-visible intent still requires renewed review and approval. Do not push the
worktree branch.

An "active review worktree" exists only when you created it earlier in this
same conversation and handed its artifacts to the user for review, or when the
user explicitly identifies the worktree or branch to continue. A matching entry
from `git worktree list`, a suggestive branch name, nearby commits, or
uncommitted changes does not make a worktree active for your task. Never infer
ownership or continuity from repository state. Track the active Tollgate
candidate ID alongside the worktree identity; filesystem discovery alone is
not enough to rediscover promotion authority.

Do **not** implement the follow-up directly on the primary tree's user-owned
`master`.
The active review worktree remains the isolation boundary until promotion is
resolved.

Start a fresh worktree only when there is no active unpromoted review worktree
for the task, for example:

- the previous `/wt` task was already promoted and cleaned up;
- promotion was declined and the user is asking for a new, separate attempt;
- the follow-up is unrelated to the active review branch.

After a prior promotion has landed on local `release` (and remote `master` when
configured) and cleanup is complete, the next
change gets its own worktree rather than an in-place edit of the primary
checkout.
