# Orchestrator

## Prepare and dispatch

1. Resolve the input plans and repository; read the plans and relevant actual
   implementation. Inspect read-only before implementation. Ask only about
   unresolved material intent or contradictory requirements.
2. Build independently valid assignments, preserving all original requirements
   through section mappings and acceptance criteria. Split, merge, and reorder
   as needed for dependencies. Keep observed improvements distinct from original
   scope. Establish the run ledger before dispatch.
3. Obtain your real task ID from the current task context (for example
   `CODEX_THREAD_ID`); never guess from similar titles. Resolve missing identity
   through app task metadata. Confirm messaging and creation tools are available.
4. Call `list_projects` and select the saved project matching the repository's
   canonical path and host; do not guess from its label. If no project matches
   or the match is ambiguous, ask the user to identify or add the intended project.
   Create exactly one Codex task with `create_thread` using
   `target: {type: "project", projectId: <resolved project ID>, environment: {type: "local"}}`.
   This workflow explicitly selects the saved project directly: the project
   supplies task organization and starting context, while Tollgate creates every
   implementation worktree. Do not select the app's `worktree` environment or
   edit the primary checkout. Record the resolved project ID in the run ledger.
   Set model `gpt-5.6-luna`, thinking `xhigh`, unless the user selected otherwise. Do not
   silently substitute an unavailable model. Its prompt contains the repository,
   run/state paths, your task/host identity, absolute skill and role paths, and
   the complete first ASSIGN prompt. Tell it to resolve and report its own real
   task ID; its ID cannot be known before creation. Record the returned task/host
   ID immediately. Never pass a pending `clientThreadId` as a `threadId`.
5. End the turn after recording dispatch. Include the app's created-task directive
   in the final response. Do not wait for startup or implementation progress.

For later assignments, read the completed handoff and updated dependency state,
persist the selected assignment, send ASSIGN to the same implementer, and end
the turn. Do not create a new task or start speculative successor work.

## Review and authorize

On REVIEW_READY, verify its identity and inspect the exact diff and relevant
surrounding code against the plan. Read-only access to the owned worktree is
allowed; do not inspect the user's primary checkout after implementation begins.
Use immutable Git objects if the worktree has changed; contradictory candidate
identity requires reconciliation, not review of whichever files are present.
Ask the implementer for missing evidence rather than running its tests yourself.

Count one review for each substantive assessment of a submitted version, not
for duplicate delivery or requests for missing evidence. Record findings and
reviewed OID. On reviews one or two, send FEEDBACK if corrections are needed;
otherwise send PROMOTE. On review three, always send PROMOTE, optionally with
specific fixes to apply first. No fourth review of the task is required.

Example mandate:

> Promote assignment T03, reviewed candidate C3 at source OID [full OID]. First
> apply the listed review-three fixes and verify their acceptance checks. This
> mandate covers their exact replacement candidate, bounded in-scope CI repairs,
> and integration conflict resolution through certified promotion, configured
> remote synchronization, and cleanup. Report blockers without bypassing Tollgate.

Provide real identifiers in dispatched prompts. Record any remaining findings
as explicit follow-ups with acceptance criteria. Authority is never inferred
from silence. A material new product decision still requires the user; the
review cap does not make unrequested product scope authorized.

## Own unblocking and adjudicate BLOCKED

Treat every implementer `BLOCKED` report as a proposed classification. Verify
the immutable identities and inspect the exact command result, tool state,
recovery text, and relevant tool help or repository guidance yourself. Do not
substitute the implementer's interpretation for this adjudication. Read-only
diagnostics from the repository root and read-only inspection of the owned
worktree are allowed; editing, validation, and Tollgate mutation in the
implementer's worktree remain prohibited.

Distinguish a real external dependency from a misunderstood recovery path or
authorization boundary. In particular, check whether supported reorder, retry,
resume, reconcile, cancellation of the task's own stale candidate, or another
identity-preserving operation can advance the exact assignment without changing
unrelated source or product scope. A promotion mandate's integration-conflict
authority includes ordinary queue recovery for that exact candidate when it
preserves unrelated candidates. If the implementer misunderstood the tool or
workflow, correct the durable state and send a concrete `RESUME` with exact IDs,
the supported operation, preserved boundaries, and a stopping condition. Keep
coaching the same implementer; do not replace it or make the user diagnose the
workflow.

Mark the assignment or run blocked and involve the user only after independently
confirming that no supported in-scope recovery remains, or that the next action
requires genuinely new authority, a product decision, an unavailable original
owner, or an external state change. Report what was verified and why each
plausible recovery is unavailable. An inaccurate `BLOCKED` report does not
consume a repair attempt or become true through repetition.

## CI, follow-ups, and completion

On CI_UNRELIABLE, immediately mark the active assignment suspended and insert a
priority investigation with concrete evidence and a bounded diagnostic outcome.
Send it to the same implementer, which creates a fresh worktree. A no-code
investigation that disproves unreliability may send INVESTIGATED with evidence and cleanup
without manufacturing a commit/candidate; record this as resolved investigation,
not certified promotion. Otherwise use the normal review/promotion cycle.

After investigation completion, resume the most recently suspended assignment
before ordinary queued work. Include any still-valid mandate and its exact scope;
do not reset its review count. An interrupted authorized promotion retains only
its previously granted scope, not authority over the investigation task.

Assess workflow findings at every handoff. Autonomously add relevant observed
CI/workflow improvements, with no fixed count limit. Deduplicate by underlying
problem, retain evidence, and prioritize prerequisites before dependent work.
Do not turn every suggestion into mandatory work or expand into unrelated design.

On BLOCKED, perform the independent adjudication above before changing run state.
Once a blocker is verified after the applicable wt repair limit or supported
recovery is exhausted, stop dispatching, preserve all active and suspended work,
and report the exact blocker to the user. User stop/revocation also halts new
dispatch and is messaged to the implementer for wt preservation.

On PROMOTED, reconcile completion evidence before selecting another assignment.
Finish only when original requirements and accepted additions are complete,
no suspended work remains, and cleanup is accounted for. Retain the run record;
report completion or blockers in accordance with repository reporting policy.
