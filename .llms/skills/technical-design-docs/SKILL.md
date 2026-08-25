---
name: technical-design-docs
description: Write standalone technical design documents, RFCs, decision docs,
  and architecture proposals. Use when the user wants a substantial technical
  spec or design write-up. This skill gathers enough scope to understand the
  problem, then writes the full document in one pass, with prominent links to
  related information, strong context for readers, and concrete validation
  guidance. Use concise examples and enough specific technical detail to guide
  implementation, while avoiding diagrams.
disable-model-invocation: true
---

# Technical Design Docs

Use this skill when the user wants a technical design document, RFC, decision
document, architecture proposal, migration plan, or similar structured
technical write-up.

The default output is a complete standalone markdown document. The document
should help a reader understand the problem, goals, constraints, proposed
direction, validation approach, and required behavior without needing the
original prompt or surrounding conversation. It specifies what to build, not how
to schedule or sequence the work.

## Core Requirements

Follow these rules for all documents generated with this skill:

- Hard-wrap prose at 80 characters. Let the scope determine the document's
  length, and include all content needed for clarity and completeness.
- Write the full document in one pass once the scope is understood.
- Do not ask for approval of the outline, individual sections, or final draft.
- Make the document standalone. Do not assume access to the prompt,
  conversation, chat history, or unstated local context.
- Use plain, established language. Do not invent jargon, abbreviations, branded
  labels, internal shorthand, or new names for concepts when standard terms or
  a longer plain-language explanation communicate the same meaning.
- Assume the intended reader has the knowledge expected of a computer science
  graduate with basic familiarity with the problem domain. Do not define
  standard languages, formats, frameworks, protocols, identifiers, or domain
  concepts that such a reader would know, including terms such as C#, JSON,
  UUID, and UI Toolkit. Define only project-specific, newly introduced, or
  genuinely ambiguous terminology. Bold each necessary term and define it in
  the same sentence or paragraph as its first use.
- When a concept needs more than a short inline definition, give the simple
  definition first and add a forward link such as "see the
  [Request Routing](#request-routing) section below" to the fuller explanation.
- Never include a `Glossary`, `Terminology`, `Definitions`, or equivalent
  section. Define unfamiliar terms where readers first encounter them.
- Link to all other relevant information very prominently near the top of the
  document.
- Include goals, constraints, decisions, acceptance criteria, and validation
  guidance when they carry information that affects the design. Do not add
  generic high-level sections merely to make the document look complete.
- Specify concrete behavior, interfaces, data flow, state changes, dependencies,
  failure handling, and implementation constraints whenever they are needed to
  make the design actionable.
- Fully specify the agreed target behavior, including requirements that might be
  deferred by a later implementation plan. Do not omit behavior because it seems
  outside a theoretical minimum viable product.
- Do not break the task into milestones, phases, implementation steps, roadmap
  stages, MVP slices, or later-enhancement buckets.
- Do not include diagrams.
- Use concrete examples of behavior, APIs, payloads, configuration, syntax, and
  user flows when they materially improve understanding.
- Do not specify file or directory structure in the design document.
- Do not include `Open Questions`, `Risks and Tradeoffs`, or `Non-Goals`
  sections.
- Always include a short `Manual QA` section as the final section of the
  document.
- Leave incidental local choices to the implementation team, but do not omit a
  mechanism, algorithm, data structure, or internal boundary when it is
  important to the design or needed to remove ambiguity.

Critical API surfaces are allowed when they matter to interoperability,
ownership boundaries, migration safety, or external contracts. Specify them in
prose, compact bullets, inline code, or concise fenced code samples, whichever
makes the contract easiest to understand.

## Workflow

### 1. Understand the scope quickly

Gather only the context needed to write a strong document:

- What problem is being solved
- Who the audience is
- What constraints or requirements are fixed
- What existing systems, docs, or decisions are relevant
- What outcome or decision the document should drive

Read any referenced files, docs, tickets, or notes that directly affect the
problem statement, constraints, prior decisions, interfaces, or migration
requirements.
Do not broaden this into open-ended research unless the user asks for that.

Ask clarifying questions before writing when a missing answer would materially
change the document or create a serious misunderstanding. A question is material
when its answer would change the goals, scope, constraints, migration or
compatibility requirements, or acceptance criteria. Keep those questions short
and front-loaded. Do not turn the process into section-by-section approval or
collaborative outlining unless the user explicitly asks for that.

If required context remains unavailable after concise clarification, continue
only when the remaining assumptions are narrow enough that they do not change
the direction. Call those assumptions out in the summary or relevant body
section. Do not include an open-questions section in the final document.

### 2. Decide the output target

If the user named a destination file, use it. Otherwise create a descriptive
markdown filename in the working directory.

Unless the user specified a different format, hard-wrap prose at 80 characters
and prefer headings, short paragraphs, and flat bullets. Use as much space as
the design needs to remain complete and readable.

### 3. Write the full document in one pass

After the scope is understood, write the complete draft immediately. Do not
stop to ask whether the outline looks good. Do not ask whether each section is
correct before continuing. The user asked for a design document, so produce
the design document.

### 4. Reader-test the document with a fresh sub-agent

If the environment supports sub-agents, validate the document with a fresh
sub-agent that has no access to the earlier conversation context. This
**cold reader** is a reader who sees only the document and brings no context
from its creation.

Give the sub-agent only the document and a short task such as:

- Summarize the problem, proposal, and expected benefits
- List the major constraints and validation expectations
- Identify any knowledge the document assumes beyond a computer science
  graduate's basic familiarity with the problem domain
- Perform a "what the fuck are you talking about" pass: flag invented terms,
  abbreviations, ambiguous or branded labels, internal shorthand, and
  specialized language that exceeds that baseline. Do not flag standard
  technical or domain terminology merely because it is not defined.
- Read from top to bottom and flag every place where understanding depends on a
  definition, fact, or concept that appears only later or never appears
- Flag sections or paragraphs that present details before introductory context,
  or examples before explaining what they illustrate
- Identify places where a concise example would make system behavior, an API,
  payload, configuration, syntax, or user flow substantially clearer
- Identify unclear decisions or missing migration or compatibility requirements
- State whether the document is understandable as a standalone artifact

Treat the result as a reader-comprehension test, not as a co-authoring pass.
Replace unnecessary jargon with plain language. Define only project-specific,
newly introduced, or genuinely ambiguous terms that a reader with the expected
background would not know. Bold and define each such term on first use. If it
needs a fuller explanation, add a simple definition followed by a forward link
to the relevant section below. Reorder the document whenever a reader needs
information before the document supplies it. Add concise examples where they
resolve a comprehension gap. Fix every comprehension gap before finishing. If
the first pass finds meaningful issues, run one more fresh-reader pass after
revising.

If sub-agents are unavailable, do the same check yourself by pretending you
only have the document and none of the conversation context. Use the same
reader questions listed above.

## Document Shape

Adapt the structure to the task. A typical technical design document should
reach the proposed design quickly:

- Title
- Introduction
- Related Information, when relevant
- Proposed Design
- Concrete Mechanics, such as interfaces, data models, data flow, state changes,
  failure handling, and dependencies
- Migration and Compatibility, when relevant
- Operational Considerations, when relevant
- Alternatives Considered, only when they explain a meaningful design choice
- Manual QA

By default, use one concise introductory paragraph instead of separate
`Summary`, `Problem and Context`, `Goals`, `Constraints`, or `Requirements`
sections. Give the problem and context, goals, and meaningful constraints about
one sentence each, then state the proposed direction. Expand any of them only
when the additional information is necessary to understand the design, and put
detailed requirements beside the design mechanics they constrain.

Every section must earn its place by helping a reader understand, implement,
review, validate, or operate the design. Omit empty framing sections and generic
content added only to satisfy a template. If the user provided a required
template, follow it while preserving the rest of this skill's guidance.
`Manual QA` is required and must be the final section. `Open Questions`, `Risks
and Tradeoffs`, and `Non-Goals` must be omitted even if they would normally
appear in a generic design-doc template.

## Writing Guidance

### Related information must be prominent

Place a `Related Information` section near the top of the document, typically
immediately after the introduction. Link all relevant material there:

- Requirements docs
- Product briefs
- Prior design docs
- Incident reports
- Tracking issues
- Dashboards
- Prototypes
- External specifications

For each link, include a short note explaining why it matters. If no relevant
links are available, omit the section.

### Make the document standalone

Write for a reader who has not seen the original request.

- Prefer familiar words and standard domain terms over coined names,
  abbreviations, branded labels, internal shorthand, or jargon. Do not give an
  ordinary concept a special name merely to make the proposal sound more formal
  or distinctive. Use a longer plain-language explanation when it is clearer.
- Assume readers have the knowledge expected of a computer science graduate
  with basic familiarity with the problem domain. Do not expand or define
  standard terms and acronyms that audience would know, such as C#, JSON, UUID,
  HTTP, API, or the names of the domain's established frameworks and systems.
- On first use, bold and briefly define only project-specific, newly introduced,
  or genuinely ambiguous terms. The definition must appear in the same sentence
  or paragraph.
- Never add a `Glossary`, `Terminology`, `Definitions`, or equivalent section. A
  reader must not need to leave the point of first use to understand a term.
- If a concept requires more detail, start with a simple inline definition and
  then link forward by section name, for example: "see the
  [Request Routing](#request-routing) section below."
- Introduce information in dependency order. Explain the problem and the
  concepts needed to understand a decision before presenting that decision.
  Do not rely on a later section to make an earlier section intelligible.
- Preserve that top-to-bottom order at every level. Begin the document with the
  context and prerequisites for its decisions. Begin each section with its
  purpose or main idea before examples and supporting details. Within each
  paragraph, establish the subject before adding qualifications, consequences,
  or exceptions.
- Pull critical assumptions into the document itself
- Include enough background for a reader to understand why the change exists
- Never refer to "the prompt", "the request above", "the discussion", or
  similar hidden context

### Make the design specific and actionable

Prefer concrete technical detail over abstract statements of intent. Describe
the actual system mechanics a reader needs to implement or review the design,
including relevant:

- Components, responsibilities, and ownership boundaries
- Interfaces, request and response shapes, and data models
- Data flow, state transitions, and persistence behavior
- Required behaviors, invariants, and safety properties
- Dependencies and integration points
- Concurrency, ordering, caching, and consistency behavior
- Failure modes, recovery behavior, and observability
- Compatibility and migration mechanics
- Validation flows and any debug surfaces needed to exercise them

Include goals, constraints, and decisions only when they explain why the design
exists, rule out a plausible alternative, or establish a requirement the system
must satisfy. Omit generic statements that could apply to any project.

When a particular algorithm, data structure, helper boundary, or internal
organization is essential to correctness or central to the proposal, specify it.
When several local implementations would satisfy the design equally well, leave
that choice to the implementation team. When uncertain, err on the side of
including the concrete detail needed to eliminate ambiguity.

Do not turn the design document into a delivery plan. Omit milestones, phases,
MVP slices, delivery order, file-by-file instructions, placeholder code, and
incidental names that do not affect the design.

Do not use the document to decide what can be cut from a minimum viable product.
If a behavior has been agreed, include it in the target design even when a later
implementation plan may choose to deliver it after other work.

### Use examples to clarify the design

Include concrete examples when they make the proposed system, behavior, API,
payload, configuration, syntax, or user flow easier to understand. Examples are
not mandatory in every section, but many designs benefit from representative
inputs, outputs, interactions, and edge cases.

- Introduce each example with enough context to explain what it demonstrates.
- Place examples after the relevant concept is introduced and before deeper
  details, qualifications, or consequences.
- Use inline code for short names and expressions. Use concise fenced code
  blocks when formatting is important to understanding an API, type, payload,
  configuration, command, or syntax.
- Prefer realistic examples over abstract placeholders. Keep only the details
  needed to illustrate the design decision.
- Explain the implication of an example after presenting it when that
  implication is not self-evident.
- Do not include full implementation bodies or let examples prescribe
  incidental internal organization.
- Avoid lengthy pseudocode. Use it only when it is the clearest concise way to
  communicate behavior and real syntax would distract from the design.

Do not include ASCII diagrams, Mermaid diagrams, architecture images, or other
diagrams.

### End with manual QA

Every document must end with a short `Manual QA` section. This section explains
how a reviewer, implementer, or QA partner should interactively validate that
the completed work is correct.

Include:

- The main end-to-end flows to exercise manually
- Important edge cases, compatibility checks, or failure states to trigger
- Expected visible behavior, system state changes, or observable signals
- Any debug surfaces, fixtures, toggles, seed data, admin actions, or inspection
  views needed to put the system into the required initial states

Design those debug surfaces as part of the proposed work when they are needed
for reliable QA. Keep this section practical and focused on interactive
validation, not automated test implementation.

### Keep the writing concrete

Avoid generic filler. Every section should help a reader decide, implement,
review, or operate the change. Prefer concrete constraints and explicit
behavior over vague optimism.

## Editing Existing Documents

If the user asks to revise an existing design doc, preserve the author's
intent while applying this skill's standards:

- Fill in missing standalone context
- Replace invented jargon, abbreviations, branded labels, internal shorthand,
  and unnecessary terminology with standard terms or plain language
- Bold and define unavoidable project-specific or genuinely ambiguous terms
  inline on first use; never add a `Glossary`, `Terminology`, `Definitions`, or
  equivalent section or define baseline technical and domain terminology
- Reorder documents, sections, and paragraphs when they depend on concepts that
  have not yet been introduced
- Promote related links near the top
- Add concise, contextualized examples where they materially clarify behavior,
  APIs, payloads, configuration, syntax, or user flows
- Remove diagrams; remove code or pseudocode only when it is lengthy,
  distracting, or too implementation-specific
- Remove `Open Questions`, `Risks and Tradeoffs`, and `Non-Goals` sections
- Remove generic goals, constraints, or principles that do not affect the
  design
- Compress short problem, context, goal, and constraint sections into one
  introductory paragraph unless their detail is necessary to understand the
  design
- Add missing technical mechanics needed to make the design actionable; remove
  implementation details only when they are incidental to the design
- Tighten substantive requirements, migration mechanics, and acceptance
  criteria
- Add or revise the final `Manual QA` section

## Completion

Before finishing:

- Confirm the document is complete and self-contained
- Confirm no invented jargon, abbreviations, branded labels, internal
  shorthand, or unnecessary terminology remains
- Confirm every unavoidable project-specific or genuinely ambiguous term is
  bold and defined inline on first use, with no `Glossary`, `Terminology`,
  `Definitions`, or equivalent section and no definitions of terminology
  covered by the expected reader baseline
- Confirm concepts needing more explanation have a simple definition followed
  by a forward link to the relevant section below
- Confirm the document reads coherently from top to bottom without relying on
  definitions or context introduced later
- Confirm each section and paragraph introduces its subject before examples,
  details, qualifications, or consequences
- Confirm useful examples are present where they materially improve
  understanding, and that each example is concise, contextualized, and followed
  by any necessary explanation
- Confirm the introduction states the problem and context, goals, meaningful
  constraints, and proposed direction concisely
- Confirm problem, context, goal, constraint, or requirement sections exist only
  when their additional detail is necessary to understand the design
- Confirm the document specifies enough concrete mechanics, interfaces, data
  flow, state changes, and failure behavior to guide implementation without
  guesswork
- Confirm related links are prominent
- Confirm there are no diagrams
- Confirm code samples do not contain full implementations and pseudocode is
  used only when it is concise and clearer than real syntax
- Confirm there are no `Open Questions`, `Risks and Tradeoffs`, or `Non-Goals`
  sections
- Confirm the final section is `Manual QA`
- Run the fresh-reader test and fix any comprehension gaps

Then return the document path and a brief note about the final validation
coverage.
