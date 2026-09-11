# Message and state contract

## Durable record

Use `~/.codex/implement-plan/<unique-run-id>/`, outside repository checkouts.
The orchestrator exclusively owns `state.json`; write updates atomically using
a temporary sibling and rename. The implementer writes immutable, uniquely named
Markdown reports under `handoffs/`. Neither role overwrites an existing report.
Keep original plan documents unchanged. This record is workflow state, not
maintained repository guidance.

Record repository root, absolute plan paths and their content fingerprints,
both Codex task IDs and host IDs, model/reasoning choice, and run status.
For each assignment record its stable ID, original plan section mapping, scope,
acceptance criteria, dependencies, priority, state, review count, and report paths.
Record worktree path/branch/creating task, base OID, candidate ID, full source and
tested OIDs, queue revision, promotion mandate and its scope, runtime-ledger
reference, suspension/resumption details, blockers, and workflow findings.
Retain superseded identities and completed assignments for audit and deduplication.

Use states: queued, implementing, reviewing, fixing, promoting, suspended,
blocked, completed. Track a stack of suspended assignments for nested CI issues.
A repeated investigation of the same unresolved boundary is a blocker, not a
new task with a reset repair budget.

## Delivery

Use the Codex app `send_message_to_thread` tool to the recorded peer task and
host. Persist state/report and outgoing message identity before sending. After
successful delivery, end the turn without checking peer progress. A task message
wakes the recipient; no polling loop is needed. Do not use task creation again
to work around an idle or blocked peer.

Each message carries a unique message ID, run ID, assignment ID, type, sender
and recipient task IDs, applicable candidate ID/full source OID, and report or
state path. Use clear prose, not an opaque payload. Use these message types:

| Type | Required substance |
| --- | --- |
| ASSIGN | Outcome, plan sections, boundaries, dependencies, acceptance criteria, grounded code pointers, validation, expected evidence, reply destination. |
| REVIEW_READY | Worktree/branch/base, candidate/source/tested identity and queue revision, scheduled validation, verification evidence, review link, relevant artifacts, workflow report. |
| FEEDBACK | Review number, reviewed candidate/OID, prioritized concrete findings, requested corrections, acceptance checks; no promotion authority. |
| PROMOTE | Reviewed candidate/OID, review number, explicit mandate, exact requested fixes if any, permitted replacement scope, unresolved follow-ups. |
| CI_UNRELIABLE | Failure handle, evidence/hypothesis, canceled candidate state, preserved work, runtime cleanup, investigation need. |
| INVESTIGATED | No-code investigation outcome, retained evidence, reason no source fix is justified, candidate-free worktree cleanup, workflow report, request to resume suspended work. |
| RESUME | Suspended assignment and original owned worktree, investigation outcome, dependencies now satisfied, mandate status, validation needed. |
| PROMOTED | Source and promoted tested OIDs, candidate terminal state, release/remote verification, evidence reused or new CI required, cleanup, workflow report, request next assignment. |
| BLOCKED | Proposed blocker for orchestrator adjudication: exact boundary, raw tool state and recovery text, attempted and untried supported recoveries, retained evidence, preserved work/resources, and claimed decision or external change required. |

Every implementer handoff includes time-consuming steps, observed tool/build/CI
problems, and concrete improvements, or says none were observed. Distinguish
measured durations from estimates; do not invent timing data.

## Reconciliation

Before acting, verify run, sender, assignment, expected state, and immutable
candidate identity. Handle each message once. A replay does not increment a
review counter, dispatch another task, or authorize another candidate. A changed
candidate must explain its predecessor and replacement reason. Never apply
feedback or authority to an unrelated candidate simply because it is latest.

A `BLOCKED` message never automatically makes the assignment or run blocked.
The orchestrator independently verifies the claimed boundary and supported
recovery paths. If the implementer misunderstood the tool, queue semantics, or
mandate, record the correction and send `RESUME` with concrete recovery guidance;
escalate only a blocker the orchestrator has substantiated.

On context resumption, read state and referenced reports, then reconcile with
the recorded task's own Git/Tollgate evidence as the role permits. Read task
history once if needed to resolve a delivery ambiguity; do not repeatedly read
status. Retry uncertain delivery using the same message ID so the recipient can
deduplicate. If task creation has an uncertain outcome, resolve that creation
from app results/history rather than creating a second implementer.

The implementer keeps its own durable record of processed message IDs and
assignment ownership alongside reports; the orchestrator records its processed
IDs in state. A missing or contradictory identity blocks mutation until resolved.
Do not transfer ownership to a replacement Codex task. Report an unavailable
original owner rather than silently recreating it.
