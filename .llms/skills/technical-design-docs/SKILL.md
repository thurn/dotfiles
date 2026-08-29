---
name: technical-design-docs
description: Write standalone technical design documents, RFCs, decision docs,
  and architecture proposals. Use when the user wants a substantial technical
  spec or design write-up. This skill gathers enough scope to understand the
  problem, then writes the full document in one pass, with prominent links to
  related information, strong context for readers, and concrete validation
  guidance. Write for an ADHD reader with short paragraphs, liberal use of
  bulleted lists, and frequent examples, especially short code samples. Include
  enough specific technical detail to guide implementation while avoiding
  diagrams.
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

- Hard-wrap prose at 80 characters. This is a formatting rule, not a content
  limit.
- Let the agent judge the appropriate length of the document and all of its
  sections, paragraphs, lists, and examples. Include all content needed for
  clarity and completeness; do not impose word, page, or paragraph limits.
- Assume the reader has ADHD. Make the document easy to scan, resume after an
  interruption, and understand without holding a large amount of context in
  working memory.
- Prefer short paragraphs and use bulleted lists liberally. Begin each
  substantial section with a short paragraph that orients the reader, then move
  into bullets or a fenced code block when either form communicates the content
  clearly. Interleave additional short paragraphs as needed to introduce,
  connect, or explain those lists and examples.
- Avoid walls of prose. Use bullets for sets of requirements, behaviors,
  constraints, decisions, invariants, failure cases, and validation criteria.
  Keep each bullet focused on one distinct point so readers can scan the list
  without reconstructing an argument spread across several bullets.
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
- When a concept needs more explanation than an inline definition can provide,
  give the simple definition first and add a forward link such as "see the
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
- Use concrete examples liberally to illustrate behavior, APIs, payloads,
  configuration, syntax, user flows, algorithms, state changes, and failure
  handling.
- For every section, ask: "Could we put a fenced code block here which
  illustrates this concept?" Add one whenever it would make the section easier
  to understand. Prefer code samples under 10 lines, but treat that as guidance,
  not a hard limit. Do not split or chain multiple samples merely to evade it.
- Do not specify file or directory structure in the design document.
- Do not include `Open Questions`, `Risks and Tradeoffs`, or `Non-Goals`
  sections.
- Always include a `Manual QA` section as the final section of the document.
- Leave incidental local choices to the implementation team, but do not omit a
  mechanism, algorithm, data structure, or internal boundary when it is
  important to the design or needed to remove ambiguity.

Critical API surfaces are allowed when they matter to interoperability,
ownership boundaries, migration safety, or external contracts. Specify them in
prose, bullets, inline code, or fenced code samples, whichever makes the
contract easiest to understand.

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
compatibility requirements, or acceptance criteria. Keep those questions
front-loaded. Do not turn the process into section-by-section approval or
collaborative outlining unless the user explicitly asks for that.

If required context remains unavailable after clarification, continue
only when the remaining assumptions are narrow enough that they do not change
the direction. Call those assumptions out in the summary or relevant body
section. Do not include an open-questions section in the final document.

### 2. Decide the output target

If the user named a destination file, use it. Otherwise create a descriptive
markdown filename in the working directory.

Unless the user specified a different format, hard-wrap prose at 80 characters.
Choose all other formatting and content lengths according to what makes the
design complete, clear, and readable.

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

Give the sub-agent only the document and a task such as:

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
- Flag walls of prose, sections that lack a short orienting paragraph, and
  material that would be easier to scan as a bulleted list or fenced code block
- Check whether a reader can resume at each heading after an interruption
  without rereading earlier sections merely to recover local context
- For every section, ask whether a fenced code block could illustrate the
  concept and identify where examples would make system behavior, an API,
  payload, configuration, syntax, user flow, algorithm, state change, or failure
  mode clearer
- Identify unclear decisions or missing migration or compatibility requirements
- State whether the document is understandable as a standalone artifact

Treat the result as a reader-comprehension test, not as a co-authoring pass.
Replace unnecessary jargon with plain language. Define only project-specific,
newly introduced, or genuinely ambiguous terms that a reader with the expected
background would not know. Bold and define each such term on first use. If it
needs a fuller explanation, add a simple definition followed by a forward link
to the relevant section below. Reorder the document whenever a reader needs
information before the document supplies it. Add examples wherever they improve
comprehension. Fix every comprehension gap before finishing. If
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

Choose the introduction's length and structure according to the design. It may
combine or separate the summary, problem and context, goals, constraints, and
requirements. State the proposed direction early, and put detailed requirements
beside the design mechanics they constrain when that improves comprehension.

Every section must earn its place by helping a reader understand, implement,
review, validate, or operate the design. Omit empty framing sections and generic
content added only to satisfy a template. If the user provided a required
template, follow it while preserving the rest of this skill's guidance.
`Manual QA` is required and must be the final section. `Open Questions`, `Risks
and Tradeoffs`, and `Non-Goals` must be omitted even if they would normally
appear in a generic design-doc template.

## Writing Guidance

### Optimize for an ADHD reader

Structure the document for scanning, limited working memory, and easy re-entry
after interruption. A reader should be able to jump to a heading, read the
opening paragraph, and quickly recognize the section's important facts,
decisions, and examples.

Use this rhythm throughout the document:

- Start a substantial section with a short paragraph that states its purpose or
  main idea.
- Follow with a bulleted list when the section contains several related facts,
  requirements, behaviors, choices, consequences, or validation checks.
- Use a fenced code block instead when structure, syntax, or an example conveys
  the idea more directly.
- Add another short paragraph after a list or code block when the reader needs
  an implication, transition, qualification, or explanation.
- Continue alternating among short paragraphs, lists, and code blocks as the
  subject requires. Do not let a section collapse into an uninterrupted wall of
  prose.

Use bulleted lists liberally, including for:

- Component responsibilities and ownership boundaries
- Requirements, constraints, decisions, and invariants
- Request, response, event, and data-field semantics
- State transitions and the conditions that trigger them
- Failure modes, recovery behavior, and observable signals
- Compatibility rules, migration behavior, and operational checks
- Acceptance criteria and manual QA scenarios

Make every bullet understandable on its own within the section. Lead with the
distinguishing fact or behavior, keep parallel items grammatically parallel, and
avoid burying the important difference at the end. Use prose where reasoning is
inherently sequential or where splitting it into bullets would obscure a causal
argument.

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

For each link, include a note explaining why it matters. If no relevant
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
- On first use, bold and define only project-specific, newly introduced,
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

### Use examples liberally to clarify the design

Actively look for opportunities to include concrete examples that make the
proposed system, behavior, API, payload, configuration, syntax, user flow,
algorithm, state change, or failure mode easier to understand. Representative
inputs, outputs, interactions, edge cases, and short code samples often explain
a design more directly than prose alone.

For every section, ask: "Could we put a fenced code block here which illustrates
this concept?" Add one when the answer is yes and it improves the document. A
fenced block can show real code, pseudocode, a type, a payload, configuration, a
command, a query, a state transition, or another text format suited to the
concept.

- Introduce each example with enough context to explain what it demonstrates.
- Place examples after the relevant concept is introduced and before deeper
  details, qualifications, or consequences.
- Use inline code for names and expressions. Use fenced code blocks whenever
  their formatting or structure helps explain an API, type, payload,
  configuration, command, syntax, behavior, or decision.
- Prefer realistic examples over abstract placeholders. Include the detail that
  helps the reader understand the design decision.
- Explain the implication of an example after presenting it when that
  implication is not self-evident.
- Prefer each code sample to be fewer than 10 lines. This is guidance rather
  than a hard limit: use the length that best explains the concept. Do not glue
  together or sequence multiple code blocks to work around the guidance.
- Do not let examples prescribe incidental internal organization.
- Use pseudocode when it communicates behavior more clearly than real syntax.

Do not include ASCII diagrams, Mermaid diagrams, architecture images, or other
diagrams.

### End with manual QA

Every document must end with a `Manual QA` section. This section explains
how a reviewer, implementer, or QA partner should interactively validate that
the completed work is correct.

Include:

- The main end-to-end flows to exercise manually
- Important edge cases, compatibility checks, or failure states to trigger
- Expected visible behavior, system state changes, or observable signals
- Any debug surfaces, fixtures, toggles, seed data, admin actions, or inspection
  views needed to put the system into the required initial states

Design those debug surfaces as part of the proposed work when they are needed
for reliable QA. Keep this section focused on interactive validation, not
automated test implementation, and let the agent judge how much detail it needs.

### Keep the writing concrete

Avoid generic filler. Every section should help a reader decide, implement,
review, or operate the change. Prefer concrete constraints and explicit
behavior over vague optimism.

## Editing Existing Documents

If the user asks to revise an existing design doc, preserve the author's
intent while applying this skill's standards:

- Fill in missing standalone context
- Hard-wrap prose at 80 characters unless the user specified another format
- Rewrite walls of prose into short orienting paragraphs followed by bulleted
  lists or fenced code blocks, interspersed with additional short paragraphs
- Use bulleted lists liberally for related requirements, behaviors, decisions,
  invariants, failure cases, compatibility rules, and validation criteria
- Make each substantial section easy to resume after interruption without
  requiring the reader to recover excessive context from earlier sections
- Replace invented jargon, abbreviations, branded labels, internal shorthand,
  and unnecessary terminology with standard terms or plain language
- Bold and define unavoidable project-specific or genuinely ambiguous terms
  inline on first use; never add a `Glossary`, `Terminology`, `Definitions`, or
  equivalent section or define baseline technical and domain terminology
- Reorder documents, sections, and paragraphs when they depend on concepts that
  have not yet been introduced
- Promote related links near the top
- Add contextualized examples liberally where they clarify behavior, APIs,
  payloads, configuration, syntax, user flows, algorithms, state changes, or
  failure handling
- For every section, ask whether a fenced code block could illustrate the
  concept, and add one when useful
- Prefer code samples under 10 lines without splitting or chaining samples to
  evade that guidance; use a different length when it better serves clarity
- Remove diagrams; remove code or pseudocode only when it is distracting or too
  implementation-specific
- Remove `Open Questions`, `Risks and Tradeoffs`, and `Non-Goals` sections
- Remove generic goals, constraints, or principles that do not affect the
  design
- Choose whether problem, context, goal, and constraint material belongs in one
  introduction or separate sections based on what best explains the design
- Add missing technical mechanics needed to make the design actionable; remove
  implementation details only when they are incidental to the design
- Tighten substantive requirements, migration mechanics, and acceptance
  criteria
- Add or revise the final `Manual QA` section

## Completion

Before finishing:

- Confirm the document is complete and self-contained
- Confirm prose is hard-wrapped at 80 characters unless the user specified
  another format
- Confirm the document is easy for an ADHD reader to scan and resume after an
  interruption
- Confirm substantial sections begin with a short orienting paragraph and then
  use bulleted lists or fenced code blocks wherever those forms improve clarity
- Confirm bulleted lists are used liberally, each bullet focuses on a distinct
  point, and additional short paragraphs connect or explain the lists and code
  blocks where needed
- Confirm no section contains an avoidable wall of prose
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
- Confirm every section was evaluated by asking, "Could we put a fenced code
  block here which illustrates this concept?", and that useful examples are
  present wherever they improve understanding
- Confirm code samples are preferably under 10 lines, have not been split or
  chained to evade that guidance, and use another length only when it better
  explains the concept
- Confirm the introduction states the problem and context, goals, meaningful
  constraints, and proposed direction clearly
- Confirm problem, context, goal, constraint, or requirement sections exist only
  when their additional detail is necessary to understand the design
- Confirm the document specifies enough concrete mechanics, interfaces, data
  flow, state changes, and failure behavior to guide implementation without
  guesswork
- Confirm related links are prominent
- Confirm there are no diagrams
- Confirm pseudocode is used when it communicates behavior more clearly than
  real syntax
- Confirm there are no `Open Questions`, `Risks and Tradeoffs`, or `Non-Goals`
  sections
- Confirm the final section is `Manual QA`
- Run the fresh-reader test and fix any comprehension gaps

Then return the document path and a note about the final validation
coverage.
