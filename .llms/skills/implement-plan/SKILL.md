---
name: implement-plan
description: Implement one or more task-based plans through two persistent Codex tasks, with an orchestrator reviewing and authorizing an implementer using a fresh wt worktree per task. Use only when the user explicitly invokes $implement-plan.
---

# Implement Plan

Use this skill only when the user explicitly invokes `$implement-plan` by name.
Do not infer invocation from a request to implement a task, execute a plan, or
work from a plan document.

Run the supplied plans through exactly two persistent Codex tasks: the receiving
task is the orchestrator; one separate task is the implementer. Execute one
assignment at a time, promoting it before starting its successor, except for
the CI investigation interruption below. Do not substitute subagents for these
two tasks or create an implementer per assignment.

Read [wt](../wt/SKILL.md) completely before execution. Both roles read
[the message and state contract](references/messages.md), then their applicable
role: [orchestrator](references/orchestrator.md) or
[implementer](references/implementer.md). Follow repository instructions.

## Explicit wt adaptations

- The implementer owns every task worktree. The orchestrator may inspect those
  worktrees read-only for review, but cannot edit, run validation, or operate
  Tollgate there. No other cross-owner access is granted.
- Unblocking the implementer is a core orchestrator responsibility. Treat a
  `BLOCKED` handoff as an evidence-bearing claim to adjudicate, not a workflow
  verdict to relay. Independently reconcile the exact tool state, documented
  recovery paths, scope, and authority; correct misunderstandings and send the
  same implementer concrete recovery instructions whenever an in-scope path
  exists. Escalate only after verifying that supported recovery is exhausted or
  that progress genuinely requires new user authority or an external state change.
- The orchestrator's explicit task promotion mandate replaces wt's human
  Yes/No question. Invoking this workflow delegates that decision, including
  relevant observed CI/workflow follow-ups. Do not ask the user again per task.
- Review at most three submitted versions per task. When satisfied, or on the
  third review regardless of remaining findings, send a promotion mandate.
  The third review may include specific fixes with that mandate: the implementer
  applies them, validates, replaces the candidate, and promotes without review four.
- The mandate covers those explicit fixes, in-scope CI repair, and integration
  conflict resolution. It never bypasses certification. Material scope decisions
  outside the mandate return to the orchestrator; user product decisions remain
  with the user. Unresolved findings become traceable follow-up tasks.
- The implementer sends wt's complete evidence and exact candidate identity to
  the orchestrator immediately after submission, then ends its turn. The
  orchestrator also ends its turn immediately after dispatch. No polling,
  `wait_threads`, monitor, heartbeat, or scheduled wakeup substitutes for messages.
  This does not prohibit the implementer's wt-required promotion wait after authority.
- Suspected CI unreliability suspends the active task for a fresh investigation
  worktree. Resume the suspended task only in its original implementer-owned
  worktree. Ordinary regressions stay in the task's bounded wt repair loop.

All other wt rules remain: fresh Tollgate worktrees, local-only branches,
proportionate evidence, exact immutable candidates, bounded repairs, certified
promotion and remote synchronization, runtime cleanup, and preserved blocked work.
Never run a plan merely because its document is an example supplied while
designing or testing this skill.
