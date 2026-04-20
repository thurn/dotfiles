---
name: technical-design-docs
description: Write standalone technical design documents, RFCs, decision docs,
  and architecture proposals. Use when the user wants a substantial technical
  spec or design write-up. This skill gathers enough scope to understand the
  problem, then writes the full document in one pass, with prominent links to
  related information, strong context for readers, and careful tradeoff
  framing. Avoid code blocks, pseudocode, diagrams, and unnecessary
  implementation detail.
---

# Technical Design Docs

Use this skill when the user wants a technical design document, RFC, decision
document, architecture proposal, migration plan, or similar structured
technical write-up.

The default output is a complete standalone markdown document. The document
should help a reader understand the problem, goals, constraints, proposed
direction, tradeoffs, and rollout implications without needing the original
prompt or surrounding conversation.

## Core Requirements

Follow these rules unless the user explicitly asks for something different:

- Default to roughly 500 lines, hard-wrapped at 80 characters. If the scope is
  genuinely smaller, keep the document complete rather than padding it.
- Write the full document in one pass once the scope is understood.
- Do not ask for approval of the outline, individual sections, or final draft.
- Make the document standalone. Do not assume access to the prompt,
  conversation, chat history, or unstated local context.
- Link to all other relevant information very prominently near the top of the
  document.
- Emphasize goals, constraints, decisions, risks, and acceptance criteria more
  than detailed implementation mechanics.
- Do not include diagrams.
- Do not include code blocks or pseudocode blocks.
- Do not specify file or directory structure in the design document.
- Trust the implementation team to choose specific algorithms, data
  structures, helper abstractions, and internal organization details.

Critical API surfaces are allowed when they matter to interoperability,
ownership boundaries, rollout safety, or external contracts. Specify them in
prose or compact bullets, not as code or pseudocode blocks.

## Workflow

### 1. Understand the scope quickly

Gather only the context needed to write a strong document:

- What problem is being solved
- Who the audience is
- What constraints or requirements are fixed
- What existing systems, docs, or decisions are relevant
- What outcome or decision the document should drive

Read any referenced files, docs, tickets, or notes that directly affect the
problem statement, constraints, prior decisions, interfaces, or rollout plan.
Do not broaden this into open-ended research unless the user asks for that.

Ask clarifying questions only when a missing answer would materially change the
document or create a serious risk of misunderstanding. A question is material
when its answer would change the goals, scope, constraints, rollout, or
acceptance criteria. Keep those questions short and front-loaded. Do not turn
the process into section-by-section approval or collaborative outlining unless
the user explicitly asks for that.

If required context remains unavailable after concise clarification, continue
with explicit assumptions and call them out in the document's summary, risks,
or open questions as appropriate.

### 2. Decide the output target

If the user named a destination file, use it. Otherwise create a descriptive
markdown filename in the working directory.

Unless the user specified a different length or format:

- Target roughly 500 lines
- Hard-wrap prose at 80 characters
- Prefer headings, short paragraphs, and flat bullets

### 3. Write the full document in one pass

After the scope is understood, write the complete draft immediately. Do not
stop to ask whether the outline looks good. Do not ask whether each section is
correct before continuing. The user asked for a design document, so produce
the design document.

### 4. Reader-test the document with a fresh sub-agent

If the environment supports sub-agents, validate the document with a fresh
sub-agent that has no access to the earlier conversation context.

Give the sub-agent only the document and a short task such as:

- Summarize the problem, proposal, and expected benefits
- List the major constraints and risks
- Identify any knowledge the document assumes but does not explain
- Identify ambiguous terms, unclear decisions, or missing rollout details
- State whether the document is understandable as a standalone artifact

Treat the result as a reader-comprehension test, not as a co-authoring pass.
Fix any gaps the sub-agent finds. If the first pass finds meaningful issues,
run one more fresh-reader pass after revising.

If sub-agents are unavailable, do the same check yourself by pretending you
only have the document and none of the conversation context. Use the same
reader questions listed above.

## Document Shape

Adapt the exact structure to the task, but most technical design docs should
contain some version of these sections:

- Title
- Summary
- Related Information
- Problem and Context
- Goals
- Non-Goals
- Constraints and Requirements
- Proposed Design
- Critical Interfaces or API Surfaces, if relevant
- Alternatives Considered
- Risks and Tradeoffs
- Rollout and Migration
- Operational Considerations
- Open Questions

Do not force every section into every document. If a section adds no value,
omit it. If the user provided a required template, follow it while preserving
the rest of this skill's guidance.

## Writing Guidance

### Related information must be prominent

Place a `Related Information` section near the top of the document, typically
immediately after the summary. Link all relevant material there:

- Requirements docs
- Product briefs
- Prior design docs
- Incident reports
- Tracking issues
- Dashboards
- Prototypes
- External specifications

For each link, include a short note explaining why it matters. If no links are
available, say so explicitly rather than implying the document is complete on
its own.

### Make the document standalone

Write for a reader who has not seen the original request.

- Expand or define unfamiliar acronyms on first use
- Explain system-specific terminology when it first appears
- Pull critical assumptions into the document itself
- Include enough background for a reader to understand why the change exists
- Never refer to "the prompt", "the request above", "the discussion", or
  similar hidden context

### Specify the right things

Bias toward describing:

- User-visible goals
- Business or technical constraints
- Ownership boundaries
- Required behaviors and invariants
- Safety properties
- Compatibility expectations
- Rollout gates
- Testing expectations
- Observability needs
- Failure modes

Avoid over-specifying:

- Exact algorithms unless they are the point of the decision
- Internal helper structure
- Any file or directory structure
- File-by-file implementation plans
- Placeholder code
- Pseudocode flows
- Incidental naming choices that can be resolved during implementation

Trust implementation teams to make sound local choices within the stated
constraints.

### No code or diagrams

Do not include:

- Code blocks
- Pseudocode blocks
- ASCII diagrams
- Mermaid diagrams
- Architecture images

If an interface contract is important, describe it in prose or concise bullets.

### Keep the writing concrete

Avoid generic filler. Every section should help a reader decide, implement,
review, or operate the change. Prefer concrete constraints and explicit
tradeoffs over vague optimism.

## Editing Existing Documents

If the user asks to revise an existing design doc, preserve the author's
intent while applying this skill's standards:

- Fill in missing standalone context
- Promote related links near the top
- Remove code, pseudocode, or diagrams if the user wants this style
- Reduce unnecessary implementation prescription
- Tighten goals, constraints, rollout, and risk framing

## Completion

Before finishing:

- Confirm the document is complete and self-contained
- Confirm related links are prominent
- Confirm there are no code or pseudocode blocks
- Confirm there are no diagrams
- Run the fresh-reader test and fix any comprehension gaps

Then return the document path and a brief note about any important unresolved
questions that remain in the document.
