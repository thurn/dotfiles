---
name: scenario-design-docs
description: Use when the user explicitly invokes `scenario-design-docs` or asks for an interview-led technical design process that turns a short prompt into a standalone technical design document plus a QA-ready scenario appendix. This skill combines brainstorming-style questioning with technical-design-docs-style documentation, with special emphasis on scenario testing, simulated behavior, acceptance criteria, and one-question-at-a-time design validation before implementation.
disable-model-invocation: true
---

# Scenario Design Docs

Turn a short prompt into a fully specified technical design document through a
relentless, one-question-at-a-time interview. Use scenario testing as the core
method for finding ambiguities: walk through realistic ways the system will be
used, simulate expected behavior as precisely as possible, and resolve each
decision before writing the final documents.

This is a design skill, not an implementation skill. Do not write production
code, scaffold projects, or start implementation while this skill is active.
The output describes what to build and how it must behave, not how to sequence
the work.

## Source Skills

Use these local skills as the foundation for the process:

- `../brainstorming/SKILL.md` for project-context exploration, one-question
  interviewing, approach comparison, and approval gates.
- `../technical-design-docs/SKILL.md` for standalone technical design document
  standards, prominent links, concrete acceptance criteria, and reader testing.

Do not paste those skills into the working context unless needed. Read the
specific sections that affect the current task, then apply the integrated
workflow below.


## Core Rules

- Ask exactly one substantive question per message during the interview.
- For every question, provide a recommended answer and explain the reason.
- Prefer multiple-choice questions when practical.
- Label multiple-choice answers `A`, `B`, `C`, and so on.
- Mark exactly one multiple-choice answer as `[SUGGESTED]`.
- Resolve dependencies between decisions in order. Ask upstream questions
  before questions whose answer depends on them.
- Use scenario testing throughout the interview, not only at the end.
- Present scenarios one at a time. Do not batch many scenarios into one prompt.
- Simulate expected behavior at the highest fidelity the project allows.
- Preserve a running list of confirmed decisions, assumptions, rejected options,
  open questions, and accepted scenarios.
- Resolve open questions before writing the documents. If anything remains
  unresolved intentionally, report it separately instead of adding an `Open
  Questions` section to the design document.
- Do not break the task into milestones, phases, implementation steps, MVPs, or
  later enhancements as part of this process.
- Fully specify the agreed target behavior, including details that might be
  deferred by a later implementation plan. Do not omit behavior because it seems
  outside a theoretical minimum viable product.
- Stop after producing the design document and scenario appendix unless the user
  explicitly asks for implementation planning.

## Workflow

Follow these steps in order.

### 1. Explore Context

Inspect only the context needed to understand the project:

- Existing files, docs, tickets, specs, or source modules the prompt references.
- Relevant patterns and constraints in the current repository.
- Recent commits when they clarify active direction or unresolved work.

If the prompt describes a project too broad for one design, pause the interview
and ask the user to choose the first independently implementable slice.

### 2. Establish Design Tree

Identify the main branches of the design:

- Users and use cases.
- Inputs, commands, data, or entry points.
- Outputs, state changes, side effects, and integrations.
- Error handling and edge cases.
- Compatibility, migration constraints, and observability.
- Testing and manual QA expectations.

Do not present this as a full outline for approval. Use it internally to choose
the next highest-leverage question.

### 3. Interview One Decision At A Time

Ask questions in dependency order. Each question should include:

- The decision being resolved.
- Why it matters.
- A recommended answer.
- Either a concise open-ended prompt or multiple-choice answers.

For multiple-choice questions, use this shape:

```text
Question: Which behavior should the tool use when the input file is missing?

A. Print a concise error and exit non-zero. [SUGGESTED]
B. Create an empty input file and continue.
C. Prompt interactively for a new path.

Recommendation: A, because missing input is usually a caller error and should
be deterministic in scripts and CI.
```

Ask only one question. Wait for the user's answer before continuing.

### 4. Scenario-Test The Design

After enough upstream decisions are known, switch from abstract questions to
scenario prompts. Each scenario should describe one realistic usage path and
ask the user to confirm or adjust the expected behavior.

For concrete tools, simulate exact behavior:

- CLI command line, flags, environment, working directory, and fixtures.
- Standard output and standard error, including ordering and wording.
- Exit status.
- Files created, modified, deleted, or left unchanged.
- Logs, metrics, network calls, database writes, or other side effects.

For APIs and services, simulate:

- Request shape, auth context, pre-existing state, and relevant headers.
- Response status, body semantics, errors, idempotency, and persistence effects.
- Retries, partial failures, race conditions, and compatibility behavior.

For user interfaces, simulate:

- Starting state, user action, visible UI state, validation messages, disabled
  controls, loading states, and navigation.
- Accessibility-relevant behavior such as focus, keyboard flow, labels, and
  announcements when it materially affects acceptance.

For abstract systems, use descriptive scenarios:

- Actor, goal, preconditions, trigger, expected decision path, observable
  outcome, and acceptance signal.

Each scenario prompt should include:

- A short scenario name.
- Preconditions and inputs.
- The expected result at empirical verification level.
- The recommended expected behavior.
- A single confirmation question.

### 5. Compare Approaches

Once the problem shape and key scenarios are clear, propose two or three viable
approaches. Lead with the recommended option. Include only the practical
consequences that matter to choosing a path.

This is the only point where grouped options are expected. After the user picks
or amends the approach, return to one-question-at-a-time clarification for any
remaining gaps.

### 6. Present Final Design Understanding

Before writing files, present a compact summary of:

- The chosen approach.
- Confirmed user-visible behavior.
- Core components and ownership boundaries.
- Important non-goals.
- The scenario set that will become the appendix.
- Any agreed behavior that may be expensive or non-minimal but still belongs in
  the complete target design.
- Remaining assumptions, if any.

Ask for approval to write the documents. If the user changes anything, update
the understanding and continue the interview until the design is stable.

### 7. Write The Documents

Create two markdown files unless the user specifies a different destination:

- `/tmp/docs/YYYY-MM-DD-<topic>-design.md`
- `/tmp/docs/YYYY-MM-DD-<topic>-scenario-appendix.md`

The primary design document must prominently link to the appendix near the top.
The appendix must link back to the design document.

Follow the `technical-design-docs` style for the primary document:

- Make it standalone.
- Put related links near the top.
- Emphasize goals, constraints, decisions, required behavior, and acceptance
  criteria.
- Specify complete target behavior, not an MVP slice or delivery sequence.
- Avoid code blocks, pseudocode blocks, diagrams, and unnecessary internal file
  structure.
- Specify critical external contracts in prose or bullets when needed.
- Do not include generic design-process sections such as `Alternatives
  Considered`, `Risks and Tradeoffs`, or `Open Questions` unless the user
  explicitly requests them for this document.
- Do not include milestone, phase, implementation-step, roadmap, or MVP
  breakdowns. If sequencing is needed later, leave it for implementation
  planning outside this skill.

The appendix is allowed to be more empirical and example-heavy than the primary
document. It should contain scenario records detailed enough for manual QA or
acceptance review. Include exact commands, outputs, UI states, requests,
responses, data fixtures, or side effects when the project makes that possible.

### 8. Self-Review

Review both documents before finishing:

- No unresolved `TODO`, `TBD`, placeholders, or hidden-context references.
- No contradictions between the design and scenarios.
- Every acceptance criterion has at least one scenario or explicit rationale.
- Every scenario has clear preconditions, action, expected result, and
  verification method.
- The primary document links to the appendix.
- The appendix is usable as manual QA input.
- The design remains scoped to one implementable project.

Fix issues inline.

### 9. Reader Test

Apply the fresh-reader test from `technical-design-docs` to both files. If
subagents are available and explicitly authorized by the user, use one with only
the documents as input. Otherwise do the check yourself from a cold-reader
perspective.

Confirm that a reader can understand:

- What problem is being solved.
- What behavior is expected.
- Which scenarios prove the expected behavior.
- What remains out of scope or unresolved.

Fix any comprehension gaps before reporting completion.

## Document Expectations

The primary design document should usually include:

- Title.
- Summary.
- Related Information, including the scenario appendix link.
- Problem and Context.
- Goals.
- Non-Goals.
- Constraints and Requirements.
- Proposed Design.
- Critical Interfaces or API Surfaces, if relevant.
- Expected Behavior and Acceptance Criteria.
- Migration or Compatibility Requirements, if relevant.
- Operational Considerations, if relevant.

Do not add `Alternatives Considered`, `Risks and Tradeoffs`, `Open Questions`,
or similar generic proposal sections by default. If information from those
categories is essential, incorporate it into the concrete requirements,
behavior, migration constraints, operational requirements, or acceptance
criteria instead.

The scenario appendix should usually include:

- Title.
- Link back to the primary design.
- How to use the appendix for review.
- Scenario index.
- Scenario records with preconditions, inputs, action, expected behavior,
  side effects, verification method, and related acceptance criteria.
- Negative, edge, compatibility, and failure scenarios when relevant.

Do not group scenarios by implementation phase, MVP boundary, or delivery
milestone. Group them by behavior, actor, interface, or verification area.

## Completion

When finished, report:

- The primary design document path.
- The scenario appendix path.
- Whether both documents passed self-review.
- Any intentionally unresolved open questions.

Do not proceed to implementation or implementation planning unless the user asks
for that next step.
