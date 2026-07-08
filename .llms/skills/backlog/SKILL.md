---
name: backlog
description: Convert a loose bullet-point list of 10-30 bugs/issues into high-quality, standalone task files written to /tmp/backlog/. The skill's primary value is interviewing the user to deeply understand each problem; it does not pre-write reproduction steps or code pointers (the implementer agent handles those). Use when the user supplies a backlog dump, asks to "groom the backlog", or asks to turn rough notes into actionable tasks. Triggers on backlog, bug list, issue list, groom, file tasks, write tasks, ticketize.
---

# Backlog Grooming

Turn a rough list of bugs/issues into a set of high-quality, standalone task
files that deeply describe each problem so a future agent can pick it up
without any additional context from the original conversation.

The input is a bullet-point list of 10-30 short descriptions. The output is one
markdown file per task in `/tmp/backlog/`.

**The core value of this skill is the interview.** A one-line bug from the
user is almost never a task — it is a hypothesis about what they want fixed.
Interviewing them turns "the CSV export is broken" into "exports of empty
date ranges include a header row when the user expects an empty file."
The implementer can find the code and reproduce the bug; only the user can
tell you what "broken" actually means to them and why it matters.

Do **not** spend effort pre-writing detailed reproduction steps, screenshots,
or code pointers. The implementer is a capable agent — it reproduces and
locates the code itself. Your job is to capture the *problem*, not the
execution plan.

## Required reading

Before doing anything else, load:

- `~/.llms/skills/grill-me/SKILL.md` — the interview pattern you will use to
  clarify each ambiguous description. This is the most important phase of
  the skill; read it carefully.
- Lightly skim `./CLAUDE.md`, `./AGENTS.md`, and any `./.llms/skills/` or
  `./.claude/skills/` entries so you know what subsystems exist in this
  project. You need this to cluster related tasks (see "Cluster by
  subsystem" below), not to write code pointers.

If the project has no documentation, glance at the top-level directory
structure so you can recognize when two tasks touch the same area.

## Pipeline

Run these phases in order. Do not skip ahead.

### 1. Parse the input list

Read the user's bullet list and produce a numbered working list. Preserve the
user's original wording verbatim alongside your parsed interpretation so
nothing gets quietly rewritten.

If the list is shorter than 10 items or longer than 30, confirm with the user
before proceeding — the skill is sized for that range.

### 2. Interview the user (this is the main phase)

For each item, invoke the `grill-me` pattern until you understand the problem
well enough to write it down in detail. Ask one focused follow-up at a time,
with your recommended answer, and stop when further questions would have
diminishing returns.

The goal is not to extract reproduction steps or test cases — it is to
understand:

- **What is wrong, in the user's framing.** Not "the CSV export is broken"
  but "the CSV export produces a header-only file when the date range is
  empty, and the user wants zero rows in that case."
- **Why it matters.** What does the user actually want to do that this
  breaks? A cosmetic glitch and a data-loss bug are very different
  priorities even if the symptom looks similar.
- **What "fixed" looks like.** Sometimes the user's bullet is a wish for a
  redesign, not a bug fix. Tease that apart.
- **What's been ruled out.** Anything they already know does not work, so
  the implementer does not waste a round on it.

Batch related questions across items only when it would clearly save the
user time; otherwise stay one-at-a-time per `grill-me`'s rules. If a
question can be answered by reading the codebase, read the codebase
instead of asking — but lean toward asking the user about *intent* and
reading the code only for *facts*.

Common things to clarify:

- which surface/screen/command the user means when the wording is generic
- expected vs. actual behavior when only one is stated
- scope: cosmetic, functional, blocking
- whether a "fix" is a UX/API redesign or a literal bug fix
- any prior attempts the user already ruled out

Do **not** ask the user how to reproduce. The implementer will figure that
out. Ask *what* is broken, not *how* to trigger it.

### 3. Cluster by subsystem

Group tasks by the subsystem they touch. You usually have enough signal
from the user's wording and your light familiarity with the project to do
this — e.g. "all the CSV export tasks", "all the auth-flow tasks", "all the
UI styling nits." When a task is genuinely cross-cutting or you cannot
tell, put it in its own singleton cluster.

This clustering feeds the next phase. It does not need to be perfect — it
is a heuristic for spreading related tasks across the numbering sequence
so they are unlikely to be in flight (and therefore unlikely to merge-
conflict) at the same time.

### 4. Assign task numbers (interleave clusters)

Task numbers determine the order `backlog-execute` launches tasks, and the
in-flight window equals `PARALLELISM` (default 3). Tasks with adjacent
numbers are likely to be in flight simultaneously, so adjacent numbers
should touch *different* subsystems — that minimizes merge conflicts when
the integration lane cherry-picks approved branches together.

Algorithm: sort clusters by size descending, then round-robin assign
numbers across clusters. Within a cluster, preserve original input order.

Worked example with three clusters (A=5 tasks, B=3 tasks, C=2 tasks):

```
001 → A1   002 → B1   003 → C1
004 → A2   005 → B2   006 → C2
007 → A3   008 → B3
009 → A4
010 → A5
```

Any window of 3 consecutive numbers now touches at most 3 different
subsystems instead of 3 tasks in the same one. When one cluster dominates
(e.g. 20 of 30 tasks in the same subsystem), perfect interleaving is
impossible — do your best; the integration lane handles residual conflicts.

This breaks the "task number == bullet number" contract from earlier
versions of the skill. That is intentional. When you report back to the
user, include a mapping from original bullet number to assigned task
number so they can find their items.

### 5. Write task files

Create `/tmp/backlog/` if it does not exist. Write one file per task as
`/tmp/backlog/NNN-<kebab-slug>.md` where `NNN` is the number assigned in
phase 4.

Use the **task template** below. Every task must be standalone — assume the
implementer has not seen the input list, the user, or this conversation.

The template intentionally omits universal boilerplate (standard acceptance
criteria, the generic going-deeper checklist, QA blocker policy, UX
expectations). That content is appended in phase 6 by a script. The
sections in the template are reserved for **task-specific** extensions of
those universal items — write only what is unique to this bug.

### 6. Append general implementation instructions

After all task files are written, run:

```sh
bash ~/.llms/skills/backlog/append-general-instructions.sh /tmp/backlog/
```

This appends a `# How to Implement This Task` section to each task file
containing the universal acceptance criteria, going-deeper checklist, QA
blocker policy, and UX expectations. The script is idempotent — files that
already contain the section are skipped, so it is safe to re-run.

Do **not** stop without running this script. Implementers rely on the
universal section being present.

After the script completes, print a summary to the user listing each
created file with its title and the original bullet number it came from
(see "Output" below).

## Task template

Each `/tmp/backlog/NNN-<slug>.md` file must follow this structure:

```markdown
# <Clear, specific title summarizing the problem or desired change>

## Problem

<The most important section. Several paragraphs (or a tight bulleted list)
describing the problem in detail, grounded in what came out of the
interview:

- What is wrong, specifically (not "the export is broken" but "exports of
  empty date ranges include a header row when the user expects an empty
  file").
- When it happens — which surface, which inputs, which state. Brief — no
  step-by-step click path. Enough that the implementer can identify the
  surface and reproduce it themselves.
- What the user expects to happen instead.
- Why it matters to the user — what task of theirs is blocked or degraded.
- Any prior attempts the user has already ruled out.

Stand-alone — no references to "the list" or "the conversation". The
implementer should finish this section knowing *what they are being asked
to fix* without needing to ask follow-up questions.>

## Acceptance criteria

<Only **task-specific** criteria. Do NOT include the standard items
(post-fix evidence, typecheck/lint/test, regression test) — those are
appended automatically. Examples of what belongs here:

- [ ] Count updates correctly across reload.
- [ ] Tooltip stays attached to its anchor at all widths.

If the task has no extra criteria beyond the standard set, omit this
section.>

## Implementation notes

<Anything the implementer needs that is not derivable from the code or the
problem description: constraints, related decisions, prior attempts the
user mentioned, design intent that came out in the interview. Keep short.
Omit the section if there is nothing to add.

Do NOT pre-load code pointers here — the implementer can locate code
itself. Use this for things only the user could tell you. A task-specific
reproduction hint like "only repros with two clients in the same room" or
"requires `RUST_LOG=debug` to surface the panic" belongs here when it is
non-obvious.>

## Going one level deeper

<Task-specific deeper considerations the implementer should keep in mind
while fixing this bug. Do NOT re-state the universal checklist (adjacent
surfaces, architectural root causes, debug surfaces, follow-up tasks) —
that is appended automatically. Use this section for issue-specific
extensions, e.g. "Check sibling commands for the same arg-parsing bug."
Omit if there is nothing task-specific to add.>

## UX expectations

<Task-specific UX considerations, e.g. "Sanity-check that other heavily
wrapped phrases still wrap naturally" or "the orphaned arrow makes the UI
look broken — prioritize visual continuity." For non-UI projects, this
section is usually about API ergonomics, error message clarity, or CLI
output formatting. Do NOT re-state the generic UX guidance — it is
appended automatically. Omit if nothing task-specific applies.>
```

Sections other than `Problem` may be omitted entirely when they have no
task-specific content. The appended `# How to Implement This Task` section
will still cover the universal expectations.

## Conventions

- Filenames: `NNN-<kebab-slug>.md`, zero-padded to 3 digits, slug derived
  from the title (lowercase, hyphen-separated, trim stop-words). Numbering
  is assigned in phase 4 via interleaved clustering, **not** by input
  order — surface the bullet-number → task-number mapping in your final
  summary so the user can still find their items.
- Do not include a global index file unless the user asks for one; the
  filenames are the index.
- Do not delete or overwrite existing files in `/tmp/backlog/` without
  confirming with the user — they may be in flight.
- Keep each task self-contained. Do not write "see task 004 for context" —
  duplicate the context instead.
- Honor any documentation-style rules in the project's `AGENTS.md` /
  `CLAUDE.md` (e.g. "no `no longer` phrasing"). Read those files before
  writing tasks.

## Anti-patterns

- Writing tasks straight from the bullet list without interviewing.
  Ambiguous tasks produce wasted implementation work, and the interview
  is the point of this skill.
- Investigating root causes during grooming. That is the implementer's
  job. Stay focused on *what is broken*, not *what is causing it*.
- Writing tasks that assume the implementer remembers the user's intent.
  Every file must read cleanly to a stranger.
- Pre-writing detailed reproduction steps, environment scaffolding, or
  click paths. The implementer reproduces — the task only needs to
  identify the surface and the bad behavior. (A *task-specific* hint like
  "only repros with two clients" is fine in Implementation notes; it is
  the boilerplate Environment/Steps/Expected/Actual scaffolding that you
  skip.)
- Capturing screenshots or transcripts during grooming. The implementer
  captures pre-fix and post-fix evidence per the universal QA section;
  pre-grooming evidence rots and rarely matches what the implementer
  needs.
- Pre-writing code pointers or a "Suspected area" section. The implementer
  locates code itself; pre-writing biases the fix toward the wrong file
  and rots quickly.
- Asking the user *how* to reproduce a bug. Ask *what* is broken; the
  implementer figures out *how*.
- Numbering tasks by input order instead of by interleaved cluster. The
  point of round-robin numbering is to minimize merge conflicts during
  parallel execution; sequential numbering defeats it.
- Skipping the cluster phase because "the bullets are already grouped." The
  user's grouping is by topic of complaint; cluster by *subsystem touched*,
  which may differ.
- Creating one giant aggregate task for related items. Prefer one file per
  discrete issue; the implementer can batch if they choose.
- Re-stating the universal acceptance-criteria / going-deeper /
  UX-expectations boilerplate inside a task. The script appends it; you
  only write task-specific extensions.
- Forgetting to run `append-general-instructions.sh` before stopping.
  Implementers expect the universal section to be present.

## Output

When done, print:

- the count of tasks created
- a one-line table of `NNN — title` for each file
- a **bullet-number → task-number** mapping (`bullet 1 → 001`, `bullet 2 →
  004`, ...) so the user can find the task corresponding to each item in
  their original list
- the cluster grouping you used for numbering, one line per cluster:
  `Cluster <name>: 001, 004, 007, ...`
- confirmation that `append-general-instructions.sh` ran successfully
- the path to `/tmp/backlog/` so the user can browse

Then stop. Do not start implementing tasks unless the user asks.
