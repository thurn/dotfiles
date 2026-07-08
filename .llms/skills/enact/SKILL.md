---
name: enact
description: Use only when explicitly requested by name for orchestrating a large multi-step project from a design document using coordinated subagents. Triggers on /enact.
disable-model-invocation: true
---

# Enact: Multi-Agent Orchestration

Enact turns a design document into a fully executed project using coordinated
subagents. The orchestration planner (you, right now) assesses scope, asks
clarifying questions, writes an execution plan, locks it in, and immediately
begins execution. The orchestrator coordinates subagents and should not read
project context files itself -- subagents do all the reading and writing.

**Core assumption:** Almost all work is done by subagents. The orchestrator is a
coordinator, not a coder.

**Lifecycle:** You write an orchestration plan to `<run_dir>/ENACT_PLAN.md` in
the enact project directory for this run, perform the standalone check, lock the
plan in, then immediately initialize execution from that plan in the same
session unless the user explicitly requested planning only.
`<run_dir>` means `/tmp/enact/<run-name>/`, not the target repo, not
`~/.llms/skills/enact`, not the skill directory, and not the planner's current
working directory unless that truly is the enact run directory. The target
implementation project directory is still recorded inside the plan, but
`ENACT_PLAN.md` itself lives in `<run_dir>` and is intentionally not version
controlled. The orchestrator follows the plan mechanically, spawning subagents
for all real work.

## Inputs

The user provides a design document (inline, as a file, or as a URL). Before
planning, you MUST understand:

1. What is being built and why
2. How to manually QA the result (CLI harness, web interface, etc.)
3. Whether QA requires project-specific debug surfaces, fixtures, seed data,
   feature flags, admin routes, scripted setup helpers, or other tooling to
   create the application states that must be tested

**If the manual QA strategy is unclear, ask. Do not proceed without one.**
Manual QA feasibility is a planning requirement, not a task-level afterthought.
If user-observable behavior depends on hard-to-create state, include explicit
work to build the debug or setup surface needed for QA. Building such debug
tools is a core part of the Enact pipeline.

## Orchestration Planning

The orchestration planner (you) writes the orchestration plan to
`<run_dir>/ENACT_PLAN.md`, where `<run_dir>` is the enact project directory for
this run and `<project_dir>` is the repository root or working project
directory for the implementation target. Resolve both directories first, then
write the file at that exact `<run_dir>` path on disk. Do not place
`ENACT_PLAN.md` in the target repo, the skill directory, your home directory,
or some other incidental cwd. The plan describes the project context, scope
assessment, agent roster, per-task execution policy, and QA strategy. The
orchestrator follows the Enact protocol mechanically — the plan does NOT need
to spell out every subagent prompt since the protocol defines those.

Assume future execution may need to resume from `ENACT_PLAN.md` with **no other
context from this conversation**. The plan must stand on its own.

Use `AskUserQuestion` freely to resolve ambiguity before writing the plan. The
goal is a plan that the orchestrator can execute mechanically, immediately
after the plan is locked in.

### Assessing Scope

Read the design document. Assess complexity per the Scaling Guidelines below
to decide how much planning ceremony is needed. The plan should state the
assessed complexity level and the selected roster for both:

- Project-level agents: whether to use surveyor(s), how many researchers,
  whether to synthesize research, whether to run a plan refiner, whether to run
  a task refiner, and whether to run a QA scenario generator.
- Task-level agents: which reviewers and QA runners run for ordinary tasks,
  which tasks need manual QA, what amount of code review is useful, whether QA
  can run in parallel with code review, and what fix loop applies when review
  and QA both find issues.

Default to running both code review and manual QA for every task with
user-observable behavior. Scale down only when the task is purely internal and
has strong automated verification.

### QA Planning

The orchestration plan must define a practical QA strategy before execution
starts. For every user-observable surface, decide:

- The exact command, URL, harness, browser flow, CLI entry point, fixture, or
  setup path that lets a QA Runner exercise it.
- The application states that must exist before testing, and how those states
  will be created repeatably.
- Whether the project needs temporary or permanent debug surfaces to create
  those states. If yes, the task generator must create implementation tasks for
  those surfaces before the dependent feature QA runs.
- Which tasks require per-task QA and which scenarios apply after each task.
- What successful behavior looks like, with exact expected output, visible
  text, response bodies, records, screenshots, or command output documented in
  advance.

QA scenarios should present fully simulated behavior, not vague intent. They
must document the precise setup, action sequence, and expected result before a
QA Runner executes them.

### Parallel Review and QA Feasibility

For most projects, per-task manual QA can run at the same time as code review.
After the Feature Coder commits a task, the Conformance Reviewer, Quality
Reviewer, and QA Runner can all inspect the same HEAD independently: reviewers
read the diff and task contract, while QA executes prewritten scenarios against
the running system. The orchestrator should dispatch them in parallel by
default for tasks with manual QA.

When review and QA run in parallel, collect all findings before dispatching
fixes. Prefer one combined fix round that addresses actionable review findings
and filed QA bugs together, then rerun only the checks that could have been
affected: failed QA scenarios, targeted reviewer follow-up, or both.

### Writing the Orchestration Plan

Write the orchestration plan to `<run_dir>/ENACT_PLAN.md` in the enact project
directory itself. If needed, state the resolved absolute `<run_dir>` and
`<project_dir>` in your own working notes before writing so you do not place
the file in the wrong location. `ENACT_PLAN.md` lives in `<run_dir>`, not in
the target repo, and should not be added to version control.

This orchestration plan is distinct from `<run_dir>/PLAN.md`, which is the
technical project plan created later by the execution-time Planner subagent.

`ENACT_PLAN.md` must be fully understandable to a fresh agent that has not seen
the prior conversation. Do not rely on conversational context, inline design
doc text that is not restated, or phrases like "as discussed above."

`ENACT_PLAN.md` must also act as a document hub. A fresh implementation agent
should be able to find every relevant source artifact from links in the plan
itself.

`ENACT_PLAN.md` MUST begin with a link to this skill file:
e.g. `**Skill reference:** ~/.llms/skills/enact/SKILL.md`

The plan should contain:

1. **Original request** — The original user prompt quoted verbatim in a fenced
   block. If later user messages changed scope, constraints, or acceptance
   criteria, include a separate **Relevant follow-up constraints** section with
   those instructions quoted verbatim too.
2. **Relevant documents** — A linked list of every source artifact a fresh
   implementation agent may need: design docs, specs, issue links, PR links,
   screenshots, local files, external docs, or notes. Use exact file paths or
   URLs. Do not mention a document without linking it.
3. **Execution context** — Project directory, design doc location or source,
   and any required entry points, URLs, or commands needed to begin work
4. **Context** — What's being built and why, key constraints
5. **Scope assessment** — Complexity level and summary rationale
6. **Run name** — A short memorable kebab-case name for this run, chosen by
   the planner, such as `crisp-checkout` or `harbor-search`. Do not use an
   integer id.
7. **Project-level agent plan** — Which top-level agents run or are skipped,
   including surveyor, researchers, synthesizer, planner, plan refiner, task
   generator, QA scenario generator, and task refiner. Include the rationale
   for any skipped refinement step.
8. **Per-task agent policy** — Which task-level agents run for ordinary tasks,
   how much code review is useful, when manual QA is required, whether QA runs
   in parallel with review, and how combined review/QA findings are fixed.
9. **QA strategy** — How to manually test changes after each task. Include
   the exact tool/commands to use, the URL or entry point of the system under
   test, the required setup state, the debug surfaces or fixtures needed to
   create that state, and what a successful test looks like. This is critical
   because QA runs after every task with user-observable changes.
10. **QA scenario expectations** — The expected simulated behavior that
   scenarios must encode: setup, actions, exact expected output or visible
   state, and failure signals.
11. **Verification** — How to confirm the project is complete (build, test,
   lint commands, end-to-end checks)
12. **Resolved assumptions** — Decisions needed to execute the work without
   going back to the prior conversation. If something is still unknown and
   risky, ask the user before writing the plan.

The plan should NOT contain:
- Specific research assignments (the surveyor decides these)
- A proposed task breakdown (the task generator decides this)
- Detailed subagent prompts (the orchestrator writes these from context)
- References to prior conversation state such as "as discussed", "above",
  "the user's earlier note", or "the attached doc" without restating the
  necessary content or file path
- Unlinked mentions of documents, tickets, screenshots, or specs that the
  fresh implementation agent may need

Before finishing, perform a **standalone check**: read `ENACT_PLAN.md` as if
you were a fresh implementation agent seeing only that file. If anything needed
to begin execution immediately is missing, unclear, or only present in conversation
history, fix the plan before handing it off.

As part of that standalone check, verify:
- The original user request is present verbatim
- Every relevant follow-up constraint is either quoted verbatim or fully
  resolved in the plan
- Every document the agent may need is linked with an exact path or URL
- The project-level agent plan and per-task agent policy are explicit
- Manual QA strategy is executable, including any required debug/setup surfaces
- The file you wrote is actually located at `<run_dir>/ENACT_PLAN.md`
- No instruction depends on unstated conversation context

After writing `ENACT_PLAN.md`, lock it in and proceed directly to the
Orchestrator Execution Protocol. Do not stop after planning unless the user
explicitly requested a plan-only run.

## File-Based Communication

All subagent communication flows through a named run directory chosen by the
orchestration planner and recorded in the plan:

`<run_dir> = /tmp/enact/<run-name>/`

The orchestration planner chooses `<run-name>` as a short memorable kebab-case
name tied to the project, not an integer id. If that directory already exists,
the orchestrator appends a short suffix.

```
/tmp/enact/<run-name>/
  ENACT_PLAN.md        # Orchestration plan and execution policy
  STATE.md             # Ground-truth orchestrator state
  HANDOFF.md           # Optional resume note written before long pauses or context resets
  survey.md            # Research assignments
  research/
    topic-name.md      # Individual research findings
  RESEARCH.md          # Synthesized research findings
  PLAN.md              # Technical project plan
  tasks/
    01-task-name.md    # Individual task specs
    02-task-name.md
  reviews/
    01-conformance.md  # Code review notes per task
    01-quality.md
    resolved/          # Resolved review files (audit trail)
  qa/
    scenarios.md       # QA scenarios
    results/
      01-results.md    # QA results per task
  bugs/
    bug-001.md         # Bug reports from QA
  META.md              # Final retrospective
```

### Artifact Boundaries

Subagents must keep their artifacts within their assigned purpose:

- `<run_dir>/RESEARCH.md` provides context on the current project and problem
  space. It must not propose the solution, define a file structure, prescribe
  algorithms, decompose implementation tasks, or otherwise do planner work. In
  a greenfield or already-understood project, it may be very short or empty.
- `<run_dir>/PLAN.md` defines what to build and the acceptance-level product or
  technical shape. It must not contain a detailed task breakdown, detailed code
  snippets, or step-by-step implementation instructions. Task generation owns
  decomposition and implementable task specs.
- `<run_dir>/META.md` captures generic workflow lessons about Enact itself. It
  must not contain project-specific advice, scenario ideas, command examples,
  file paths, language-specific prescriptions, or notes that only make sense
  for the completed project. When a run exposes a project-specific failure,
  go one level deeper: identify the generic software engineering or
  orchestration practice that would have prevented the failure.

**Context overflow prevention:** The orchestrator reads only index files and
summaries from `<run_dir>`. It may read and write `STATE.md` directly and write
`META.md` at the end. It never reads full source code. Each subagent receives
only the files relevant to its task. If interrupted or uncertain, re-read
`STATE.md` before doing anything else.

### What the Orchestrator Passes to Subagents

Each subagent prompt includes:
1. The specific assignment (what to do)
2. Relevant `<run_dir>` file paths to read (not the content — let the
   subagent read them)
3. Output file path to write results to
4. Any relevant CLAUDE.md instructions for the project (especially
   build/test/lint commands)

The orchestrator does NOT paste file contents into prompts. It gives paths.

### STATE.md

`<run_dir>/STATE.md` is the orchestrator's durable ground truth. Update it
after every project-level subagent and after every task-level step.

Include:
- Run name, run directory, current phase, and next step
- Completed project-level steps
- Task statuses
- QA status by task or scenario (`pending`, `PASS`, `FAIL`, `N/A`)
- Parallel review/QA dispatches currently in flight
- Review, QA, or bugfix round counts for the current task
- Any escalation points waiting on the user

If the run is interrupted or the orchestrator loses track of progress, trust
`STATE.md` over memory and reconstruct the next step from it.

### HANDOFF.md (optional)

When the orchestrator anticipates a context reset (approaching context limits,
user-initiated clear, session handoff), write `<run_dir>/HANDOFF.md` with:
current phase, the exact next dispatch, any in-flight subagent and its
expected return, and any orchestrator-level notes that haven't yet made it
into `STATE.md`. On resume, the new orchestrator reads `HANDOFF.md` and
`STATE.md` together; `STATE.md` is authoritative if they disagree.

## Two Phases of Execution

Enact has two execution phases after `ENACT_PLAN.md` is locked in.
**Project-level** work produces shared artifacts that inform all subsequent
work. **Task-level** work implements, reviews, and validates individual units of
change serially.

### Project-Level Phase

These subagents run once (or a small number of times) to establish the shared
knowledge base and plan. Their outputs live at the top level of `<run_dir>` and
are read by all task-level subagents.

**Sequence:** Run the project-level agent roster selected in
`ENACT_PLAN.md`: Surveyor(s) if needed -> Researcher(s) -> Synthesizer if
needed -> Planner -> Plan Refiner if selected -> Task Generator -> QA Scenario
Generator -> Task Refiner if selected.

### Task-Level Phase

For **each task**, the orchestrator runs this cycle. Tasks themselves remain
serial: do not start Task N+1 until Task N has passed its required review and
QA gates.

1. **Feature Coder** -- implements the task
2. **Code Conformance Reviewer** + **Code Quality Reviewer** + **QA Runner**
   in parallel when the per-task policy allows it. QA executes only scenarios
   relevant to this task; the orchestrator records results in `STATE.md`.
3. **Fix round** if review or QA found actionable issues. Collect all review
   findings and QA bugs first, then dispatch the smallest set of fix coders
   that can address them coherently. Prefer one combined fix round before
   rerunning checks.
4. **Targeted recheck** after fixes: rerun failed QA scenarios, targeted
   reviewer follow-up, or both, depending on what changed. Cap QA loops at 3
   rounds per task, then escalate to the user.

After all tasks complete:
5. **Integration Reviewer** (once, project-level)
6. **Technical Writer** (once, project-level)
7. **Retrospective** (write `<run_dir>/META.md`)

## Orchestrator Context Discipline

The orchestrator's job is coordination, not knowledge. Every byte the orchestrator reads is a byte that won't fit when the run is half-finished and you have to keep the next dispatch coherent. Read narrowly.

**The orchestrator should ONLY read:**

- `<run_dir>/STATE.md` (always — it's the ground truth)
- `<run_dir>/reviews/NN-*.md` AFTER a reviewer returns — to decide which findings to include in the fix round
- `<run_dir>/qa/results/NN-*.md` AFTER QA returns — to decide which QA failures to include in the fix round
- `<run_dir>/bugs/bug-NNN.md` when triaging a fresh bug or preparing a combined fix round
- `git status` / `git log --oneline -3` for sanity checks
- The skill file itself if you forgot the protocol
- The protocol files in `~/.claude/skills/enact/protocols/<role>.md` if you forgot a return contract

**The orchestrator MUST NOT read** (subagents read these themselves):

- `<run_dir>/tasks/NN-*.md` task specs — the feature coder, conformance reviewer, and quality reviewer all read the spec themselves; the orchestrator only needs the title and `Blocked by` line, both of which are in `STATE.md`
- `<run_dir>/PLAN.md` sections — subagents read the sections referenced by their task spec
- `<run_dir>/qa/scenarios.md` scenario bodies — the QA Runner reads them
- The full diff of any commit — reviewers read it
- Source files in the target project — coders and reviewers read them

If you find yourself reading any of the "MUST NOT" files to construct a dispatch prompt, stop — the dispatch should be 3-5 lines pointing at the protocol and the inputs, not a re-explanation of what the spec already says.

## Standard Dispatch Pattern

Every task-level subagent dispatch follows the same shape. Don't restate the protocol; point at it. Don't restate the spec; point at it.

```
You are the <role> for <task identifier> in the <run-name> Enact run.

Follow ~/.claude/skills/enact/protocols/<role>.md.

Inputs:
- Task spec: <run_dir>/tasks/NN-...md
- Plan: <run_dir>/PLAN.md (sections referenced by the spec)
- HEAD: <expected SHA>
- <any role-specific input — e.g., commit SHA to review, scenarios to run>

Orchestrator notes (only if needed):
- <one-line drift carry-over>
- <one-line bug to verify>
- <one-line skip-this-finding instruction>
```

A typical dispatch prompt should be under 300 tokens. If yours runs longer, you're either re-explaining the protocol (delete that — point at it) or re-explaining the task spec (delete that — the subagent reads it).

The protocol files specify the strict return contract. Do not ask for additional fields in the dispatch prompt; do not accept verbose returns.

## Subagent Catalog

### Project-Level Subagents

#### Surveyor
**Purpose:** Breadth-first scan of the problem domain.
**Input:** Design document path.
**Output:** Research assignments in `<run_dir>/survey.md` -- a list of specific
questions that need answering, grouped by topic.
**When to use:** Large/unfamiliar projects. Skip for small, well-understood
changes.

#### Researcher
**Purpose:** Deep-dive into a specific research assignment.
**Input:** One research assignment from the survey, plus relevant file paths.
**Output:** Findings in `<run_dir>/research/topic-name.md`.
**Parallelism:** Run multiple researchers concurrently. Each gets an independent
topic.

#### Synthesizer
**Purpose:** Combine research findings into a single document.
**Input:** Paths to all `<run_dir>/research/*.md` files.
**Output:** `<run_dir>/RESEARCH.md` -- cohesive summary of the current state of
the world relevant to the project.
**Boundary:** Context only. Do not propose the solution, file layout,
algorithms, or implementation tasks.
**When to skip:** Small projects with 1-2 researchers -- the researcher output
can serve directly as `RESEARCH.md`.

#### Planner
**Purpose:** Write a technical project plan.
**Input:** Design document path + `<run_dir>/RESEARCH.md`.
**Output:** `<run_dir>/PLAN.md` -- standalone technical plan focused on what to
build and why. The planner reads code directly to fill knowledge gaps.
**Boundary:** Do not include a detailed task breakdown, detailed code snippets,
or step-by-step implementation instructions. The Task Generator owns
decomposition.

#### Plan Refiner
**Purpose:** Audit the plan from a fresh perspective. The plan must stand alone
— if it references project-specific concepts without defining them, that is a
gap. The refiner catches logical holes, missing definitions, and unstated
assumptions precisely because it has no other context. When the refiner finds
ambiguities or missing decisions, it should resolve them with reasonable
defaults and document the choice — not defer them as open questions.
**Input:** `<run_dir>/PLAN.md` only (no other context — this is intentional).
**Output:** Updated `<run_dir>/PLAN.md` with gaps filled and decisions made.
**Signature verification:** For every helper, function, type, or exported
symbol the plan invokes by name, the refiner MUST read that symbol at its
declared location and pin its actual signature, return shape, and export
status into the plan section that depends on it. A plan that names an
unverified symbol is not ready for task generation; if a symbol can't be
located, the plan refiner flags it rather than letting the coder discover
the gap.

#### Task Generator
**Purpose:** Decompose the plan into implementable tasks.
**Input:** `<run_dir>/PLAN.md`.
**Output:** Individual task files in `<run_dir>/tasks/NN-task-name.md` (the
source of truth that feature coders read). Also creates matching Claude Code
tasks via `TaskCreate` for progress tracking — these mirror task titles only.
Each task file follows the template below.
**QA dependency:** If QA requires debug surfaces, fixtures, seed data, feature
flags, or scripted setup helpers, create explicit prerequisite tasks for those
tools before tasks whose QA depends on them.

#### QA Scenario Generator
**Purpose:** Generate manual QA scenarios from the plan and tasks.
**Input:** `<run_dir>/PLAN.md` + all `<run_dir>/tasks/*.md`.
**Output:** `<run_dir>/qa/scenarios.md` -- numbered scenarios with setup,
steps, and exact expected outcomes. Focuses on the QA strategy selected in
`ENACT_PLAN.md`, including any debug surfaces or setup helpers.
**Required:** Every scenario must begin with a **target verification step**
that asserts something unique about the system under test (e.g., page title,
visible heading, expected UI element) so the runner can confirm it is testing
the correct application. Specify tool invocations by exact command name —
never use generic descriptions like "open the app."
**Simulation requirement:** Scenarios must document fully simulated behavior in
advance: initial state, actions, exact expected output or visible state, and
failure signals. Do not write placeholders like "verify it works."

#### Task Refiner
**Purpose:** Spot-check that tasks are self-contained and implementable by a
coder who has never seen the project. Like the plan refiner, the lack of
surrounding context is the point — if the task can't be understood in
isolation, it needs more detail.
**Input:** A batch of `<run_dir>/tasks/*.md` files (no other context —
intentional). One refiner reviews all tasks, not one refiner per task.
**Output:** Updated task files, or flagged issues.

### Task-Level Subagents

These run once per task, in the cycle described above. Each one's full briefing — required reading, what to do, build commands, commit discipline, return contract — lives in its protocol file. The orchestrator dispatches by pointing at the protocol; do not restate it.

| Subagent | Purpose | Protocol |
|---|---|---|
| Feature Coder | Implement one task end-to-end; produce ONE commit. Serial — one at a time, no worktrees. | `~/.claude/skills/enact/protocols/feature-coder.md` |
| Conformance Reviewer | Verify the implementation satisfies every Requirement and Acceptance Criterion. Output: `<run_dir>/reviews/NN-conformance.md`. | `~/.claude/skills/enact/protocols/conformance-reviewer.md` |
| Quality Reviewer | Review structure, encapsulation, tests, API design, drift risk. Skip linter-style. Output: `<run_dir>/reviews/NN-quality.md`. | `~/.claude/skills/enact/protocols/quality-reviewer.md` |
| Review Feedback Coder | Address only the specific findings the orchestrator names. Move resolved reviews to `<run_dir>/reviews/resolved/`. | `~/.claude/skills/enact/protocols/review-feedback-coder.md` |
| QA Runner | Execute the named scenarios against the running system. Output: `<run_dir>/qa/results/NN-results.md`. File bugs to `<run_dir>/bugs/bug-NNN.md`. | `~/.claude/skills/enact/protocols/qa-runner.md` |
| Bugfix Coder | Reproduce-first bug fix. If no repro after multiple attempts, add regression tests as guardrails and report NO-REPRO — do not ship a speculative fix. | `~/.claude/skills/enact/protocols/bugfix-coder.md` |

Run reviewers and eligible QA in parallel (one tool turn with the applicable
Agent calls). Run fix coders only if findings are worth addressing — skip nits,
skip judgment calls. When review findings and QA bugs both exist, wait for all
parallel checks to return before dispatching fixes so one fix round can address
the full set where practical.

### Post-Task Subagents and Artifacts

These run once, or are written once, after all tasks are complete.

| Subagent | Purpose | Protocol |
|---|---|---|
| Integration Reviewer | Final end-to-end audit; cross-task coherence; go/no-go verdict. Output: `<run_dir>/reviews/integration-review.md`. | `~/.claude/skills/enact/protocols/integration-reviewer.md` |
| Technical Writer | Light docs sweep: spot-check existing docs for accuracy; check top-level index / README for needed updates; cross-link related docs. | `~/.claude/skills/enact/protocols/technical-writer.md` |

#### Retrospective
**Purpose:** Capture what the run taught Enact without a full metacognition
pipeline.
**Author:** The orchestrator writes `<run_dir>/META.md` after post-task work.
**Contents:** Generic workflow lessons only: what worked, what failed, QA or
review friction, and prompt or process changes worth considering next time.
Keep it short and evidence-based. Do not write project-specific scenario ideas,
commands, file paths, or language-specific guidance. If a project-specific
issue exposed a weakness, describe the generic practice Enact should follow to
avoid that class of failure.

## Task File Template

Each `<run_dir>/tasks/NN-task-name.md` follows this structure:

```markdown
# Task NN: [Title]

## Context
[Project background + where this task fits. Domain terms defined.]

## Objective
[What this task accomplishes in 1-2 sentences.]

## Key Files
- `path/to/file.ext` - [why to read it]

## Requirements
1. [Specific change]
2. [Another change]

## Acceptance Criteria
- [ ] [Verifiable criterion]
- [ ] Code compiles and passes linting
- [ ] Changes committed

## Dependencies
Blocked by: [task numbers, or "none"]
```

## Orchestrator Execution Protocol

When the orchestrator implements this plan:

1. Create `<run_dir>` if it does not already exist and initialize `STATE.md`
2. **Project-level phase:** Run the project-level agents selected in
   `ENACT_PLAN.md` in sequence, with parallelism where noted in the catalog.
   Respect explicit skip decisions in the plan.
3. **Task-level phase:** For each task serially, run the task-level cycle from
   the per-task agent policy. Default cycle: feature coder -> conformance
   reviewer + quality reviewer + QA runner in parallel -> combined fix round if
   needed -> targeted rechecks. Update `STATE.md` after every step.
4. If QA finds bugs, file bug reports, include them in the combined fix round
   when practical, then rerun only the failed scenarios for that task. Cap this
   QA loop at 3 rounds per task, then escalate to the user.
5. **Post-task phase:** Integration reviewer, technical writer, then write
   `META.md`
6. If interrupted or uncertain, re-read `STATE.md`, inspect the relevant files
   under `<run_dir>`, and resume from the recorded next step
7. Present final summary to the user

### Handling Failures

- **Subagent produces poor output:** Re-run with a more specific prompt. Do not
  attempt to fix it yourself.
- **QA finds bugs:** Create bug reports in `<run_dir>/bugs/`, update `STATE.md`,
  include the bugs in the next fix round, and rerun only the failed scenarios.
  Max 3 QA rounds per task; after that, escalate to the user.
- **Integration reviewer flags issues:** Create follow-up tasks or escalate.
- **Context getting large:** The orchestrator's own context should stay small
  because it reads summaries, not source. If approaching limits, summarize
  `<run_dir>` state and continue.

## Scaling Guidelines

Scale planning effort to **codebase complexity**, not just file count. A 60-file
app of straightforward components is simpler than a 20-file system with deep
interdependencies. Consider how many files you need to *understand* (read and
reason about), not just how many you'll *change*.

| Complexity | Surveyors | Researchers | Planners | Feature Tasks |
|------------|-----------|-------------|----------|---------------|
| Low (isolated changes, familiar patterns) | 0 | 1-2 | 1 | 1-3 |
| Medium (cross-cutting changes, some unknowns) | 0-1 | 2-3 | 1 | 4-10 |
| High (deep interdependencies, unfamiliar domain) | 1 | 3-5 | 1 | 10-30 |

Low-complexity projects skip surveyors and the synthesizer, going researcher ->
planner -> tasks directly. The orchestration planner decides the right level of
ceremony.
