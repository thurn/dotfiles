---
name: wt-sequence
description: Execute a discrete implementation plan as a rolling stack of independently valid task commits in one retained Git worktree, overlapping review of task N with speculative implementation and Tollgate validation of task N+1. Use when the user explicitly invokes wt-sequence or asks to implement a task-by-task plan while reviewing and promoting each preceding task separately.
---

# Worktree Sequence

Implement a plan with exactly one task under review and at most one later task executing
speculatively. Promote and push every approved task independently while retaining the editable
worktree until the final task.

## Inherit the worktree task contract

Before acting, read `~/.llms/skills/wt/SKILL.md` completely. Apply all of its repository
isolation, verification, candidate, promotion-mandate, CI-repair, remote-push, demo, runtime,
and safety rules except where this skill explicitly replaces them.

This skill replaces these `wt` assumptions:

- The sequence worktree contains a linear stack of one independently valid commit per plan task.
- The reviewed candidate may be a first-parent ancestor of clean worktree `HEAD`; it need not be
  `HEAD` while one speculative descendant exists.
- Intermediate candidates use retained-worktree cleanup policy.
- A pinned review link ends at the reviewed commit rather than the live worktree.
- Intermediate promotion preserves the sequence worktree and branch. Normal Tollgate cleanup
  runs only for the final task.
- The review handoff is a commentary checkpoint while speculative implementation continues. If
  speculation finishes first, repeat the complete handoff in the final response and wait.

Never use this skill for a one-task request; use `wt`. Never place two tasks under review or work
more than one task beyond the reviewed task.

## Preflight

1. Parse the supplied plan into ordered task IDs, titles, dependencies, and acceptance criteria.
   Every task must leave the repository valid and independently pass its required checks. Publish
   the interpreted task ledger in commentary and proceed without another approval when clear.
2. Stop before implementation if a boundary is ambiguous, dependencies contradict the order, or
   adjacent tasks must be combined to produce a valid commit.
3. Require explicit user identity to resume an existing sequence: a sequence name, worktree, or
   branch. Filesystem discovery alone never grants ownership or continuity.
4. Verify required tooling:

   - `tg candidate --help` and `tg approve --help` contain `--retain-worktree`.
   - `code --list-extensions --show-versions` reports `dthurn.worktree-review` version 0.3.0 or
     newer.
   - Tollgate is registered and `tg --no-launch status` plus `tg --no-launch doctor` have no
     blocking result.

5. Create one fresh editable worktree from local `release`, following `wt` naming and collision
   rules. Use one local-only branch for the whole sequence.
6. Initialize the private ledger with `scripts/ledger.py`. Store it under the linked worktree's
   private Git metadata, never in the checkout. Record every task commit OID, candidate ID and
   source OID, review state, temporary stash, and runtime resource as it changes. Validate ledger
   claims against Git and Tollgate before acting on them.

## Freeze each task

For task N:

1. Recheck its assumptions inside the sequence worktree. Implement only that task's acceptance
   criteria and logging requirements.
2. Complete proportionate focused checks and the repository's required local review at the
   cumulative task-N snapshot.
3. Stage exactly task N and create one detailed commit. Keep the worktree clean at the freeze
   boundary. The commit's sole first-parent predecessor is task N-1 or the captured `release` base.
4. Submit the exact commit immediately:

   - If later plan tasks exist, run `tg --no-launch --json candidate --retain-worktree <full-oid>`.
   - If this is the final task, run `tg --no-launch --json candidate <full-oid>`.

5. Verify the returned source OID equals the task commit. Record the candidate before doing more
   work. Do not poll speculative validation.

An unreviewed task N+1 is still submitted as a non-promotable dependent candidate immediately
after it is complete. Its existence grants no promotion authority.

## Present a pinned review and continue

Construct the nested HTTPS redirect described by `wt`, adding the exact reviewed commit as the
inner URI's `head` query parameter:

```text
vscode://dthurn.worktree-review/review?worktree=<encoded-absolute-path>&base=release&head=<full-oid>
```

The **Open worktree review** link must therefore exclude the speculative descendant, index,
working tree, and untracked files.

For a code-only task, send a concise commentary checkpoint containing:

- task ID/title;
- **Open worktree review**;
- candidate ID and full source OID;
- “Tollgate validation scheduled”;
- the canonical instruction to reply `y` to approve, or send feedback.

Then immediately begin task N+1 in the same active turn. Treat any user message as steering at the
next safe command boundary. `y` is the canonical approval, and other unambiguous approval language
also counts. A message combining changes with approval is feedback first unless it unambiguously
authorizes promotion after those changes.

If task N+1 finishes first, submit its retained candidate, then stop. Repeat task N's complete
review handoff in the final response so the user does not need collapsed commentary.

## Visual review snapshot

The editable worktree may already contain task N+1, so never serve task N's demo from its live
checkout. When task N needs visual or behavioral review:

1. Create an ephemeral detached review worktree at task N's exact OID. Never edit or commit there.
2. Run the inherited `wt` demo, browser QA, walkthrough, screenshot, and error-buffer workflow
   from that detached snapshot.
3. Record its path, demo service, port, logs, and browser context in the private ledger.
4. Include the pinned code link, exact demo URL, walkthrough, and inline screenshots in the same
   commentary review checkpoint, then continue task N+1 in the editable worktree.
5. Remove only this detached review worktree and its recorded runtime resources after task N is
   promoted, rejected, or replaced by an amended snapshot.

## Interrupt for feedback

Feedback on task N takes priority over speculative work.

1. Stop at a safe command boundary. If task N+1 has tracked or untracked work, preserve it in a
   uniquely named `git stash push --include-untracked` entry and record the exact stash OID.
2. Cancel task N's exact candidate and every submitted speculative descendant candidate. Confirm
   each has left the active queue before rewriting history.
3. If a committed task N+1 exists, capture both OIDs, detach at task N, apply feedback, amend task
   N, switch back to the sequence branch, then restack descendants with
   `git rebase --onto <amended-N> <old-N> <sequence-branch>`. Resolve conflicts in favor of each
   task's accepted contract. Do not use destructive reset.
4. Restore any speculative stash and audit the entire task N+1 diff. Apply the same correction or
   changed assumption wherever relevant. Ask only when feedback changes task N+1's intended
   behavior, acceptance criteria, or position in the plan.
5. Reverify task N and affected cumulative state. Submit exact replacement candidates, retained
   when later tasks remain. Issue a new pinned link for task N's new OID and require fresh approval.

Never amend a promoted commit. Represent corrections after promotion as a new plan task.

## Promote the reviewed task

On approval of task N, interrupt speculation immediately:

1. Preserve task N+1 work as above and stop task N's detached demo resources.
2. Read candidate status and verify:

   - it is active and lacks promotion authority;
   - retained source OID equals the reviewed task-N commit;
   - that commit is on the sequence branch's first-parent ancestry;
   - the ledger and pinned review name that exact OID;
   - intermediate candidates report cleanup policy `retain-worktree`; the final candidate reports
     automatic cleanup.

3. Run `tg --no-launch approve <candidate-id> --wait`. The approval covers only task N and
   in-scope CI repairs. Wait for certified local promotion and configured remote push.
4. If CI fails, use the inherited `wt` diagnose/repair loop. Amend task N, audit and restack task
   N+1, replace stale candidates, and authorize task N's replacement without another prompt when
   behavior remains inside the approved intent. Material behavior or review-evidence changes
   require renewed review.
5. Verify local `release` and configured remote equal the promoted tested OID.
6. If the promoted tested OID differs from task N's source OID, re-anchor the sequence branch with
   `git rebase --onto <promoted-oid> <old-task-N-oid> <sequence-branch>`. Restore speculative WIP,
   audit it, update the ledger's promoted task OID (and first promoted parent as its new base), and
   cancel/resubmit any task N+1 candidate whose commit OID changed.
7. Remove task N's detached review resources. Keep the editable worktree for intermediate tasks.
8. Make completed task N+1 the sole review task after revalidation and exact candidate submission,
   then begin task N+2. If task N+1 is incomplete, finish it first.

Approval of task N never authorizes task N+1.

## Plan changes, rejection, and blockers

- Future unstarted tasks may be added, removed, reordered, or rewritten in the ledger.
- Change speculative task N+1 freely: cancel its candidate, amend or rebuild its one commit, and
  resubmit. It has not been reviewed.
- A scope change to the review task requires replacement review artifacts and fresh approval.
- If more tasks are added while the apparent final task is unapproved, cancel its automatic-cleanup
  candidate and resubmit the same exact source with `--retain-worktree` before continuing.
- Once final-task approval begins, finish its promotion, push, and cleanup. Added work starts a new
  sequence from updated `release`.
- Explicit non-promotion cancels the review candidate and all speculative descendants, halts
  implementation, stops review runtimes, and preserves the editable worktree and commits. Remove
  them only on a separate explicit instruction.
- A blocker in task N+1 does not block approval or promotion of completed task N. Report the
  blocker and keep task N's review actionable.

## Finish the sequence

The final task uses normal automatic cleanup and the inherited `wt` completion checks. Before
authorization, ensure its candidate source equals clean worktree `HEAD` and stop every recorded
runtime. After promotion and remote synchronization, verify Tollgate removed the sequence
worktree and branch, the private ledger disappeared with its Git metadata, detached review
worktrees are absent, and recorded ports and processes are stopped.

Do not print a change summary when repository instructions prohibit one. Report only promotion,
remote synchronization, cleanup outcome, and any retained resource or concrete blocker.

## Ledger helper

Use `scripts/ledger.py --help` for commands. It performs atomic updates and refuses mismatched
worktree identities. Run `verify` after resumption and before every review or promotion handoff.
