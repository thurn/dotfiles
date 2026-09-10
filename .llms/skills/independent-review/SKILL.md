---
name: independent-review
description: Get one second opinion on a nontrivial code change from a fresh gpt-5.6-sol subagent, then verify each finding against the real code and fix only what is confirmed. Run at most one review per session unless the user explicitly requests additional reviews. Use after finishing an implementation and before declaring it done, or when explicitly asked for an independent review. Triggers on independent review, second opinion, review my change, review this branch, Sol review, /independent-review.
---

# Independent Sol Review

After a nontrivial code change, a fresh reviewer that has not been inside your
own reasoning catches things you cannot. This skill delegates a read-only pass
to a fresh `gpt-5.6-sol` subagent, then puts *you* in charge of deciding which
of its findings are real.

The reviewer is deliberately narrow: **bugs and architectural problems only** —
correctness defects, brittle code, brittle tests, missing coverage for changed
behavior, and duplication that will diverge. It is instructed not to raise
style nits, naming, aesthetic preferences, or security findings. If it raises
one anyway, drop it.

The reviewer never edits files. Every fix is yours to make and yours to justify.

## When to use it

Use it when the change is nontrivial: new behavior, a bug fix with real logic,
a refactor that moves responsibilities, anything touching state, ordering,
persistence, or a contract other code depends on. Skip it for typo fixes,
comment edits, pure data/config tweaks, and mechanical renames.

## Workflow

Do these in order. The pre-review check is deliberately focused and cheap:
catch obvious failures before spending reviewer attention, but do not spend the
project's expensive final-validation budget on code the reviewer may change.
For workflow sequencing, assume the independent review will require source
changes. Historically it has done so on every use; a no-change review is not a
reasonable basis for running final validation early.

Run this workflow at most once per session, including follow-up tasks in the
same session. Findings, fixes made in response to findings, later user
corrections, and substantial follow-up changes do not trigger another review.
Run an additional review only when the user explicitly requests another pass.
A run that fails before producing a review may be retried after its failure is
resolved.

### 1. Make the change review-ready with focused checks

Run the cheapest focused checks that exercise the changed behavior and make
them pass. Include a formatter, targeted linter or typecheck only when it is
cheap or directly relevant. The goal is to catch syntax errors, broken focused
tests, and other obvious defects before asking for review.

Do **not** run a full, aggregate, repository-selected, release, or otherwise
expensive final validation before the independent review merely to make the
change "green." Even when repository policy ultimately requires that suite,
defer it until reviewer findings are resolved so it normally runs once against
the final source. Do not speculate that the reviewer might return no changes;
sequence the work on the assumption that pre-review aggregate evidence will be
invalidated. If no focused check exists, use a cheap smoke check or proceed
to review and treat the missing focused coverage as explicit context; do not
substitute the expensive aggregate suite.

### 2. Run one independent Sol subagent

Spawn a fresh subagent with the `gpt-5.6-sol` model. Use `fork_turns: "none"`
so it does not inherit your reasoning, suspected problems, proposed fixes, or
prior conclusions. Give it only the evidence needed to review the change:

- the original user request, verbatim where practical;
- the absolute repository or worktree path;
- the base ref and exact review scope; and
- relevant repository instruction-file paths.

Tell the subagent to inspect the requested diff and surrounding code, remain
strictly read-only, and return only actionable bugs, architectural problems,
brittle tests, missing coverage for changed behavior, or duplication likely to
diverge. It must cite exact file locations and explain a concrete failure mode.
It must not edit files, run destructive commands, or report style, naming,
aesthetic, or security observations.

Use a prompt shaped like this:

```text
Independently review the completed change for the original request below.
Work read-only: do not edit files or mutate repository state. Inspect the diff
against <base-ref> in <absolute-worktree-path> and read enough surrounding code
to validate each claim. Report only actionable correctness or architecture
issues, brittle tests, missing coverage for changed behavior, or duplication
likely to diverge. For every finding, cite the exact file and line, explain the
concrete failure mode, and state why it belongs to this change. If there are no
findings, say so plainly.

Original request:
<original user request>
```

Wait for that exact subagent to finish. Do not send it follow-up hints that
would compromise independence. If it cannot access the change or complete the
review, report the failure plainly; do not substitute your own review and call
it independent.

### 3. Verify every finding against the real code

This is the part that matters, and it is not optional. The reviewer saw a diff
and some files; it did not run the code and it does not know the project's
history or conventions. Treat each finding as a claim to be checked, not an
instruction to be executed.

For each finding: open the cited location, read enough surrounding code to
judge it, and decide whether the described failure can actually happen. Where
it is cheap, prove it — construct the input, run the affected test, or add a
temporary assertion. A finding that survives that check is real. A finding
whose premise is wrong about how the code behaves is not, no matter how
confidently it is written.

For findings about missing or changed behavior, verify the complete observable
contract rather than inspecting one changed component in isolation. Trace all
cooperating paths and mechanisms, compare the before/after result where useful
(including timing or ordering when relevant), and check whether the requirement
is already satisfied elsewhere. The absence of one proposed implementation is
not a defect when the required behavior is present.

Reject findings that are: contradicted by the code, about behavior that is
intentional and load-bearing, outside the scope of this change, style or
aesthetic preferences, or security observations — all of which are out of
scope here regardless of merit.

### 4. Fix only confirmed issues

Fix what you verified. Do not make changes to satisfy a finding you could not
confirm, and do not perform speculative hardening "since it was mentioned". If
a finding is real but a proper fix is clearly outside this change's scope, say
so in the report rather than half-fixing it.

### 5. Validate the final source

After resolving the review, rerun the focused checks affected by accepted
findings. Then run the project's required repository-selected or aggregate
validation against the final source. This is the normal point for expensive
validation, whether or not the reviewer requested changes.

Avoid duplicate aggregate runs. Reuse genuinely valid aggregate evidence only
when it already covers the exact unchanged final source; otherwise run the
required suite once after review. If that final suite fails, diagnose and repair
it normally, then rerun only what repository policy and the repair require.

### 6. Report the disposition of every finding

Tell the user, per finding, one of:

- **Accepted** — what was wrong, what you changed, and what now verifies it.
- **Rejected** — the specific evidence in the code that shows the finding is
  wrong or out of scope. Cite it; "I disagree" is not a disposition.
- **Unresolved** — real, but not fixed here, with the reason (out of scope,
  needs a product decision, needs the user's input) and what it would take.

Also state plainly if the reviewer returned nothing.

## Rules

- Always use a fresh `gpt-5.6-sol` subagent with `fork_turns: "none"` for the
  review. Do not substitute a different model or inherit the parent context.
- Never ask the reviewer to fix anything. Keep it read-only by prompt and task
  scope.
- Never present the reviewer's findings to the user as established facts before
  you have verified them.
- Never suppress a confirmed finding because it is inconvenient or because the
  fix is annoying. Report it as unresolved instead.
- Never make an expensive aggregate or final-validation run a prerequisite for
  requesting the independent review. Focused green checks are the prerequisite;
  final validation belongs after review findings are resolved.
- Run at most one completed review per session unless the user explicitly
  requests more. Follow-up tasks in the same session, reviewer fixes, user
  corrections, substantial follow-up changes, and an empty finding list do not
  justify another pass. `--since REF` changes review scope; it does not justify
  another review.
