---
name: enact-audit
description: Use only when explicitly requested by name for blind review of a completed project against its design document, with parallel code review, manual QA, and remediation via coordinated subagents. Triggers on /enact-audit.
disable-model-invocation: true
---

# Enact-Audit: Blind Project Verification with Remediation

Enact-Audit takes a *completed* project and a project design document, then
independently verifies the implementation, fixes problems it finds, and reports
genuine spec ambiguities. The orchestrator (you) coordinates subagents
serially; subagents do all reading, reviewing, testing, and fixing.

**Core assumption:** Almost all work is done by subagents. The orchestrator is
a coordinator, not an auditor or coder.

**Critical: this is a BLIND review.** Subagents read only the project source
tree and the provided design document. They MUST NOT read planning or
intermediate artifacts from any prior build process — including Enact run
directories (`/tmp/enact/<run-name>/` and its `PLAN.md`, `RESEARCH.md`,
`tasks/`, `qa/scenarios.md`, etc.), prior PR descriptions, or any other
"how it was built" notes. The whole point is an unbiased check of the
implementation against the spec.

**Critical: manual QA is mandatory and a hard blocker.** If the orchestrator
finds it cannot run manual QA — no harness exists, the system can't be driven
headlessly, no debug surface is reachable, fixtures are missing, the UI can't
be exercised programmatically — that is NOT permission to fall back to
code-review-only. It is a problem to be *solved* before verification can
proceed. The orchestrator MUST actively design and build the QA enabling work
needed: debug endpoints, headless modes, seed fixtures, deterministic
harnesses, snapshot tooling, scriptable shells, etc. No user approval is
required for this enabling work — just do it. Code review cannot substitute
for executing the system. The only acceptable escalation is when the QA
enabling work itself is genuinely impossible (e.g., requires hardware the
agent cannot access); in that case, escalate to the user and pause. Do not
ship a code-review-only audit and call it done.

**Critical: fixing is part of the audit, not an opt-in step.** Enact-Audit's
job is to verify *and remediate*. Once bugs are filed, the orchestrator MUST
run the Fixer -> Verifier cycle for every non-escalated bug without asking
the user whether to proceed. Do NOT stop to ask "should I fix all of these
or only the blockers?" — fix everything the audit found that isn't
escalated (bugs hitting the 3-round cap). The user invoked `/enact-audit`
to get a verified, remediated worktree; pausing to ask for scope is the
wrong default and wastes their time. Escalate only when a bug genuinely
can't be fixed (3 rounds exhausted, or a spec ambiguity blocks direction).
Surface the final list of fixes applied and any escalations in `REPORT.md`
at the end.

**Lifecycle:** Enact-Audit runs end-to-end in a single session. The
orchestration planner (you, at the start) writes an audit plan to
`<run_dir>/AUDIT_PLAN.md`, then *immediately* executes that plan via subagents
in the same session. There is no clear-context handoff like Enact has.

`<run_dir>` means `/tmp/enact-audit/<run-name>/`, not the target repo, not the
skill directory, and not the orchestrator's incidental cwd.

## Inputs

The user provides:
1. The path or URL of a completed project (a repo or working directory)
2. A project design document (inline, file, or URL) describing what should
   have been built

Before planning, you MUST understand:
1. What the project was supposed to do
2. How to manually QA the result (CLI harness, web interface, etc.)
3. How to run build/test/lint locally

**If the manual QA path is unclear, ask. Do not proceed without one.** A
vague answer like "the user can click around in the browser" is not enough —
you need a concrete way for a subagent to drive the system, observe its
behavior, and produce evidence. If no such path exists yet, the QA
Strategist's first job will be to design and build one. Plan for that work
explicitly rather than skipping QA.

## Forbidden Inputs (Blind Review)

The orchestrator and every subagent MUST NOT read:
- `/tmp/enact/**` or any other Enact run directory
- `ENACT_PLAN.md`, `PLAN.md`, `RESEARCH.md`, `tasks/`, `STATE.md`, `META.md`,
  `qa/scenarios.md`, `bugs/`, `reviews/` files from a prior build run
- PR descriptions, commit messages from build PRs, or implementation notes
- Any document that describes *how* the project was built rather than *what*
  it should do

The orchestrator's plan must explicitly enumerate this prohibition for every
subagent prompt.

If the user-provided "design document" itself contains build-process notes
(task breakdowns, agent assignments), the orchestration planner extracts only
the *spec* portions into a clean `<run_dir>/SPEC_INPUT.md` and instructs
subagents to read that file rather than the original.

## Audit Planning

The orchestration planner (you) writes the audit plan to
`<run_dir>/AUDIT_PLAN.md`. Resolve `<run_dir>` and `<project_dir>` first
(`<run_dir>` is `/tmp/enact-audit/<run-name>/`), then write the file at that
exact `<run_dir>` path on disk. Do not place `AUDIT_PLAN.md` in the target
repo, the skill directory, your home directory, or any incidental cwd.

Use the deferred `AskUserQuestion` tool freely (load via `ToolSearch` first)
to resolve ambiguity before writing the plan.

### Assessing Scope

Read the design document. Assess complexity per the Scaling Guidelines below to
decide how many feature decomposers, reviewers, and QA runners to use. The plan
should state the assessed complexity level.

### Writing the Audit Plan

`AUDIT_PLAN.md` MUST begin with a link to this skill file:
e.g. `**Skill reference:** ~/.llms/skills/enact-audit/SKILL.md`

The plan should contain:

1. **Original request** — The original user prompt quoted verbatim in a fenced
   block. If later user messages changed scope, constraints, or acceptance
   criteria, include a separate **Relevant follow-up constraints** section
   with those instructions quoted verbatim.
2. **Relevant documents** — A linked list of every source artifact a subagent
   may legitimately need: the design document, target project directory,
   external specs the design references. Use exact file paths or URLs. Do NOT
   list any forbidden inputs (Enact artifacts, build PRs, etc.).
3. **Forbidden inputs** — Restate the blind-review prohibition with concrete
   paths/globs to avoid (e.g., `/tmp/enact/**`, any `ENACT_PLAN.md`).
4. **Execution context** — Project directory, design doc location, build/
   test/lint commands, how to run the system under test (URL, entry point,
   CLI command).
5. **Context** — What's being audited and why (1-3 sentences).
6. **Scope assessment** — Complexity level and which subagents to use
   (e.g., "Medium complexity — 2 feature decomposers, 3 parallel reviewers,
   1 QA runner").
7. **Run name** — A short memorable kebab-case name for this run, chosen by
   the planner, such as `lucid-ledger` or `keen-compass`. Do not use an
   integer id.
8. **QA strategy seed** — Initial thoughts on how to manually test: tools,
   commands, URLs, entry points. The QA Strategist subagent will refine this.
   If no QA path exists yet, name the gap explicitly (e.g., "no headless
   harness — Strategist must design one") and budget Discovery time for
   building it. Do not paper over a missing QA path.
9. **QA enabling work plan** — Concrete sketch of what test-only fixtures,
   debug endpoints, headless flags, or seed scripts the audit will add to
   unblock QA. The orchestrator does NOT need user approval to add these —
   they are part of the audit's job — but the plan should name them so the
   user knows what's coming.
10. **Verification commands** — Build, test, lint commands to run after fixes.
11. **Resolved assumptions** — Decisions needed to execute without further
    clarification. If something is still unknown and risky, ask the user
    before writing the plan.

The plan should NOT contain:
- Detailed feature lists (the Spec Decomposer produces these)
- Specific review or QA assignments (downstream subagents decide these)
- Detailed subagent prompts (the orchestrator writes these from context)
- Any reference to Enact artifacts or other forbidden inputs

After writing `AUDIT_PLAN.md`, **immediately proceed to execution in the same
session.** Do not instruct the user to clear context or restart. Tell them
the plan is written and you are starting execution, then begin.

## File-Based Communication

All subagent communication flows through a named run directory:

`<run_dir> = /tmp/enact-audit/<run-name>/`

The orchestration planner chooses `<run-name>`. If that directory already
exists, append a short suffix.

```
/tmp/enact-audit/<run-name>/
  AUDIT_PLAN.md          # Orchestration plan (this run)
  SPEC_INPUT.md          # Cleaned spec (if original doc had build notes)
  STATE.md               # Ground-truth orchestrator state
  SPEC.md                # Decomposed expected features
  VERIFICATION.md        # Verification matrix: features -> checks
  qa/
    strategy.md          # QA strategy + fixture/surface plan
    scenarios.md         # QA scenarios
    fixtures/            # Notes on any test surfaces built for QA
    results/
      NN-results.md      # QA results per scenario batch
  reviews/
    feature-NN-conformance.md
    feature-NN-quality.md
    resolved/            # Resolved review files (audit trail)
  bugs/
    bug-NNN.md           # Bugs found by review or QA
  fixes/
    fix-NNN.md           # Fix records (which bug, what changed, verification)
  ambiguities/
    NNN-ambiguity.md     # Genuine spec ambiguities to surface to user
  REPORT.md              # Final audit report (for the user)
  META.md                # Retrospective
```

**Context overflow prevention:** The orchestrator reads only index files and
summaries from `<run_dir>`. It may read and write `STATE.md` directly and
write `REPORT.md` and `META.md` at the end. It never reads full source code.
Each subagent receives only the files relevant to its task. If interrupted or
uncertain, re-read `STATE.md` before doing anything else.

### What the Orchestrator Passes to Subagents

Each subagent prompt includes:
1. The specific assignment (what to do)
2. Relevant `<run_dir>` file paths to read (paths, not contents)
3. The target project directory and the design doc path
4. Output file path to write results to
5. **The blind-review prohibition** with concrete forbidden paths
6. Relevant CLAUDE.md instructions for the project (build/test/lint commands)

The orchestrator does NOT paste file contents into prompts. It gives paths.

### STATE.md

`<run_dir>/STATE.md` is the orchestrator's durable ground truth. Update it
after every subagent run.

Include:
- Run name, run directory, target project directory, current phase, next step
- Completed phases
- Feature statuses (decomposed, reviewed, QA'd, fixed, verified)
- Open bugs and their fix status
- QA results by scenario (`pending`, `PASS`, `FAIL`, `N/A`)
- Fix-and-verify round counts per bug (cap at 3, then escalate)
- Open ambiguities awaiting user input

If interrupted, trust `STATE.md` over memory and reconstruct the next step
from it.

## Two Phases of Execution

Enact-Audit has two distinct phases. **Discovery** produces shared artifacts
that inform all subsequent verification. **Verification & Remediation** runs
checks, fixes issues, and confirms fixes serially.

### Discovery Phase

These subagents run once (or a small number of times) to establish what
should exist and how to check it.

**Sequence:** Codebase Surveyor -> Spec Decomposer -> Verification Planner ->
QA Strategist -> Fixture Builder (only skipped if no enabling work is needed)

The orchestrator MUST NOT exit Discovery until QA is actually executable.
If the QA Strategist cannot identify a workable approach, the orchestrator
re-runs the Strategist with a more aggressive brief — explicitly directing
it to design new debug endpoints, headless modes, seed scripts, snapshot
harnesses, or scriptable shells as needed. The Fixture Builder then
implements them. No user approval is required to add this enabling work.
The audit only escalates if the enabling work itself is genuinely
impossible (e.g., the system requires hardware the agent cannot access).

### Verification & Remediation Phase

For **each verification batch** (typically grouped by feature), the
orchestrator runs this cycle serially:

1. **Code Reviewers** (parallel: conformance + quality, plus security if the
   spec involves security-sensitive surface)
2. **QA Runner** (executes scenarios for the feature batch)
3. **Triage** (orchestrator reviews findings, files bugs, identifies
   ambiguities)
4. **Fixer** (addresses each bug — reproduce-first methodology)
5. **Verifier** (re-runs the specific reviews and QA scenarios that
   originally failed; max 3 fix-verify rounds per bug, then escalate)

After all features are processed:
6. **Cross-Feature Reviewer** (once, end-to-end consistency check)
7. **Report Author** (writes `<run_dir>/REPORT.md`)
8. **Retrospective** (write `<run_dir>/META.md`)

## Subagent Catalog

### Discovery Subagents

#### Codebase Surveyor
**Purpose:** Breadth-first scan of the implementation. Produces a map of what
exists in the codebase: top-level structure, entry points, key modules,
test layout, build commands actually used.
**Input:** Target project directory + CLAUDE.md path if present.
**Output:** `<run_dir>/CODEBASE_MAP.md` — a concise map (under ~300 lines).
**Forbidden:** Must not read any planning artifacts (see Forbidden Inputs).
**When to skip:** Trivial projects with one obvious entry point.

#### Spec Decomposer
**Purpose:** Read the design document and decompose it into a list of
*expected features* and *expected behaviors* the implementation should
exhibit. Each item is a checkable claim about the system.
**Input:** Design document path (or `<run_dir>/SPEC_INPUT.md` if cleaned) +
`<run_dir>/CODEBASE_MAP.md`.
**Output:** `<run_dir>/SPEC.md` — numbered list of expected features and
behaviors, grouped by area. Each item has a stable id (e.g., `F-12`).
**Critical:** When the spec is genuinely ambiguous (multiple reasonable
interpretations, missing detail), record the ambiguity in
`<run_dir>/ambiguities/NNN-ambiguity.md` with the spec passage quoted, the
possible interpretations, and a recommended default for verification. Do not
silently pick one interpretation.

#### Verification Planner
**Purpose:** For each feature in `SPEC.md`, decide which checks will verify
it: static code review (conformance, quality, security), manual QA scenario,
or both. Group checks into batches that can be run together.
**Input:** `<run_dir>/SPEC.md` + `<run_dir>/CODEBASE_MAP.md`.
**Output:** `<run_dir>/VERIFICATION.md` — matrix of features -> checks, with
batch grouping.

#### QA Strategist
**Purpose:** Design the manual QA approach. Decide what tools are used to
exercise the system (CLI, web UI driver, headless browser, REPL). Identify
*test surfaces* needed: fixtures, seed data, debug endpoints, mock services,
deterministic seeds, snapshot harnesses. If new surfaces are needed, write
a fixture-build plan — do not abandon QA because surfaces are missing.
**Input:** `<run_dir>/VERIFICATION.md` + `<run_dir>/CODEBASE_MAP.md` + design
doc + execution context from `AUDIT_PLAN.md`.
**Output:** `<run_dir>/qa/strategy.md` (overall strategy) and
`<run_dir>/qa/scenarios.md` (numbered scenarios with setup, steps, expected
outcomes, and a target verification step that asserts the runner is hitting
the right system). If fixtures are needed, also write
`<run_dir>/qa/fixtures/plan.md`.
**Required:** Every scenario must begin with a **target verification step**
asserting something unique about the system under test (page title, visible
heading, expected CLI banner). Specify tool invocations by exact command —
never use generic descriptions like "open the app."
**Hard rule:** "Cannot QA this feature" is not an acceptable strategy. If a
feature is not directly observable, the Strategist designs a debug surface
or test harness that makes it observable, and adds it to the fixture plan.
Substituting a code review for a missing QA path is forbidden.

#### Fixture Builder
**Purpose:** Build the test surfaces needed for QA: fixtures, seed scripts,
headless harnesses, deterministic configs, debug endpoints, scriptable
entry points. Required whenever the Strategist's plan identifies any
missing surface — only skipped when QA can be executed against the
out-of-the-box system.
**Input:** `<run_dir>/qa/fixtures/plan.md`.
**Output:** Fixture code applied directly to the working tree of the target
project. Updates `<run_dir>/qa/fixtures/plan.md` with what was built and how
QA Runners invoke it.
**Critical:** Fixtures must not change product behavior under normal
operation. Prefer additive, test-only surfaces gated behind explicit flags
or test-only entry points. Building enabling work needs no user approval.

### Verification & Remediation Subagents

These run per feature batch, in the cycle described above.

#### Code Conformance Reviewer
**Purpose:** Verify the implementation matches the spec for one feature
batch. Flags missing functionality, incorrect behavior, and spec deviations.
**Input:** Relevant entries from `<run_dir>/SPEC.md` and
`<run_dir>/VERIFICATION.md` + paths to relevant source files (the reviewer
identifies these from `CODEBASE_MAP.md` if not given).
**Output:** `<run_dir>/reviews/feature-NN-conformance.md` — list of findings
with severity (`blocker`, `major`, `minor`) and pointers to file:line.

#### Code Quality Reviewer
**Purpose:** Review code for structural issues: duplication, poor
encapsulation, missing/low-quality tests, API design, error handling at
boundaries. Do NOT flag stylistic issues a linter would catch.
**Input:** Same as conformance reviewer.
**Output:** `<run_dir>/reviews/feature-NN-quality.md`.

#### QA Runner
**Purpose:** Execute QA scenarios against the running system.
**Input:** Specific scenarios from `<run_dir>/qa/scenarios.md` for the
current batch + how-to-run instructions from `AUDIT_PLAN.md`.
**Output:** `<run_dir>/qa/results/NN-results.md`. Files bug reports to
`<run_dir>/bugs/bug-NNN.md` for issues found.
**Critical:** Use ONLY the tools and commands specified in the scenarios.
Before running test steps, verify the target verification step matches
expectations. If the first interaction doesn't match, STOP and report a setup
failure.

#### Fixer
**Purpose:** Fix one bug using reproduce-first methodology.
**Input:** One `<run_dir>/bugs/bug-NNN.md` + relevant source paths.
**Output:** Fix applied directly to the working tree of the target project.
Writes `<run_dir>/fixes/fix-NNN.md` summarizing root cause, change, and how
it was verified locally.
**Critical:** Reproduce the bug first (capture in an automated test if
feasible), fix root cause, verify manually and via tests, consider systemic
prevention (sibling bugs in the same area). One Fixer at a time — fixes
apply serially to the same working tree to avoid conflicts.

#### Verifier
**Purpose:** Confirm a fix actually resolved its bug by re-running ONLY the
specific reviews/scenarios that originally failed for that bug.
**Input:** `<run_dir>/bugs/bug-NNN.md` + `<run_dir>/fixes/fix-NNN.md` +
the original failing scenario or review pointer.
**Output:** Updates `<run_dir>/bugs/bug-NNN.md` with `status: resolved` or
`status: still-failing` plus evidence. If still failing after 3 fix-verify
rounds, sets `status: escalated` and the orchestrator surfaces it to the
user.

### Post-Verification Subagents

#### Cross-Feature Reviewer
**Purpose:** End-to-end audit looking across features for inconsistencies
the per-feature reviews wouldn't catch: contradictory behaviors between
features, gaps at integration seams, regressions introduced by fixes.
**Input:** `<run_dir>/SPEC.md` + summary of all reviews and QA results +
the index of `<run_dir>/fixes/*.md` describing what changed during this run.
**Output:** `<run_dir>/reviews/cross-feature.md`. Files any new bugs.

#### Report Author
**Purpose:** Produce the final user-facing audit report.
**Input:** `<run_dir>/STATE.md` + `<run_dir>/SPEC.md` + all reviews + all
QA results + all fixes + all ambiguities.
**Output:** `<run_dir>/REPORT.md` — sections:
1. **Summary** — pass/fail per feature
2. **Issues found and fixed** — bugs, fixes, verification evidence
3. **Issues escalated** — bugs that hit the 3-round cap
4. **Genuine spec ambiguities** — items from `<run_dir>/ambiguities/` with
   recommended interpretations
5. **Verification commands run** — final build/test/lint output summary
6. **Recommendations** — non-blocking suggestions for future work

#### Retrospective
**Purpose:** Capture what the run taught Enact-Audit.
**Author:** The orchestrator writes `<run_dir>/META.md` after the report.
**Contents:** What worked, what failed, review or QA friction, prompt or
process changes worth considering. Keep it short and evidence-based.

## Bug Report Template

Each `<run_dir>/bugs/bug-NNN.md`:

```markdown
# Bug NNN: [Title]

## Source
[Which review file or QA scenario surfaced this]

## Severity
[blocker | major | minor]

## Spec Reference
[Feature id from SPEC.md, e.g., F-12, plus quoted spec passage]

## Observed
[What the implementation does]

## Expected
[What the spec requires]

## Reproduction
[Exact steps or test invocation]

## Status
[open | in-progress | resolved | still-failing | escalated]
```

## Ambiguity Report Template

Each `<run_dir>/ambiguities/NNN-ambiguity.md`:

```markdown
# Ambiguity NNN: [Title]

## Spec Passage
> [verbatim quote from the design doc]

## Possible Interpretations
1. [interpretation A]
2. [interpretation B]

## Implementation Choice
[What the code actually does, with file:line pointers]

## Recommended Default
[Which interpretation Enact-Audit used for verification, and why]

## Question for User
[Single concrete question to resolve the ambiguity]
```

## Orchestrator Execution Protocol

When you (the orchestrator) execute the audit, run end-to-end in this
session — there is no clear-context handoff. All edits (fixtures, fixes)
apply serially to the target project's working tree as-is. Do not create
branches, check git state, stash changes, or commit anything; the user
manages version control. Subagents that do file edits hand back when
finished and the next subagent reads the updated tree.

1. Create `<run_dir>` and initialize `STATE.md`
2. Write `AUDIT_PLAN.md` (this happens at the start of the same session)
3. **Discovery phase:** Codebase Surveyor -> Spec Decomposer ->
   Verification Planner -> QA Strategist -> Fixture Builder (whenever
   surfaces are missing). Update `STATE.md` after each step. Do not exit
   Discovery until QA is actually executable.
4. **Verification & Remediation phase:** For each feature batch serially:
   - Spawn parallel reviewers (conformance, quality, security if relevant)
   - Spawn QA Runner for the batch's scenarios
   - Triage findings: file bugs in `<run_dir>/bugs/`, ambiguities in
     `<run_dir>/ambiguities/`
   - For each bug: Fixer -> Verifier (cap 3 rounds, then escalate). Fixers
     run one at a time so edits to the working tree don't conflict.
   - Update `STATE.md` after every subagent run
5. **Post-verification phase:** Cross-Feature Reviewer -> Report Author ->
   write `META.md`
6. Run final verification commands from `AUDIT_PLAN.md` (build/test/lint).
   If they fail, file bugs and re-enter remediation for those items.
7. If interrupted or uncertain, re-read `STATE.md`, inspect the relevant
   files under `<run_dir>`, and resume from the recorded next step.
8. Present `<run_dir>/REPORT.md` to the user as the final summary.

### Handling Failures

- **Subagent produces poor output:** Re-run with a more specific prompt. Do
  not attempt to fix it yourself.
- **Reviewer or QA Runner accidentally read forbidden inputs:** Discard the
  output, re-run with the prohibition restated more forcefully and explicit
  paths to avoid.
- **QA Runner reports a setup failure (target verification step doesn't
  match):** Treat as a fixture/strategy bug, not a product bug. Re-run the
  Strategist or Fixture Builder to repair the harness, then re-run QA.
  Never quietly downgrade to code-review-only.
- **No QA path exists at the end of Discovery:** Re-spawn the Strategist
  with explicit direction to design new debug surfaces, then run the
  Fixture Builder. This is part of the audit's job and needs no user
  approval.
- **Fixer can't resolve a bug after 3 rounds:** Mark `status: escalated` in
  the bug file, surface in the final report, do not block the rest of the
  audit.
- **Cross-Feature Reviewer flags new bugs:** File them, run another
  remediation cycle for those items only.
- **Context getting large:** The orchestrator's own context should stay
  small because it reads summaries, not source. If approaching limits,
  summarize `<run_dir>` state into `STATE.md` and continue.

## Scaling Guidelines

Scale audit effort to **codebase complexity** and **spec size**, not just
file count.

| Complexity | Surveyors | Spec Decomposers | Reviewer Sets per Batch | Feature Batches |
|------------|-----------|------------------|-------------------------|-----------------|
| Low (single component, simple spec) | 0 | 1 | 1 (conformance + quality) | 1-2 |
| Medium (cross-cutting, moderate spec) | 1 | 1 | 1-2 | 3-6 |
| High (deep interdependencies, large spec, security-sensitive) | 1 | 1-2 | 2-3 (add security) | 6-15 |

Low-complexity audits may collapse Discovery into a single combined run
(one subagent produces `SPEC.md` + `VERIFICATION.md` + `qa/scenarios.md`
together). The orchestration planner decides the right level of ceremony.

QA enabling work scales independently. Even a low-complexity audit may need
substantial Fixture Builder work if the system has no test surface. Budget
for it explicitly rather than skipping QA.
