---
name: batch-edit
description: Use when applying the same transformation to many entries in large files using parallel subagents. Triggers on batch edit, bulk update, mass modify, parallel file edits, batch transform, updating all entries.
disable-model-invocation: true
---

# Parallel Batch Edit

Apply a uniform transformation across many entries in large
files by splitting work into chunks, processing chunks in
parallel via subagents, and collecting results.

## The Golden Rule

**Never let two subagents edit the same file.** Each subagent
gets its own chunk file to read or write.

## What You Need From the User

Only one thing is strictly required: **what transformation to
apply**. Use AskUserQuestion only if the transformation itself
is ambiguous. Infer everything else autonomously.

## What You Infer Yourself

Inspect the source file(s) and determine all of these without
asking:

- **Source file(s)**: Identify from context or grep the repo
- **Entry delimiter**: Read the first ~100 lines. Look for
  repeating structural patterns (TOML `[[array]]` headers,
  markdown `##` sections, blank-line-separated blocks, XML
  tags, JSON array elements, etc.)
- **I/O mode**: Whether subagents edit chunks in place
  (transform mode) or read input chunks and write to separate
  output files (generate mode). See below.
- **Batch size**: Mechanical edits: 10-20 entries per chunk.
  Complex/creative changes: 5-10 per chunk.
- **Max parallel agents**: 5 simultaneous subagents per round
  unless the user specifies otherwise
- **Model**: Sonnet for mechanical/formulaic edits. Opus only
  when the transformation requires complex judgment or
  creativity.

Log your inferred parameters before splitting so the user can
course-correct if needed.

## I/O Modes

Determine which mode fits the task:

**Transform mode** — subagents edit chunk files in place.
Use when the output replaces the input (e.g., rewriting fields
in every TOML entry). The reassembled chunks replace the
original file.

**Generate mode** — subagents read input chunks and write
results to separate output files. Use when input and output
are different (e.g., reading source entries and appending
generated content to another file, or producing a new file
from the source data). Each subagent writes to its own output
file; reassembly concatenates the outputs and appends/writes
to the destination.

Both modes split input the same way. The difference is what
the subagent is told to produce and where results go.

## Execution Steps

### 1. Split the Input

Read the source file and split it into chunk files at entry
boundaries. Write chunks to a temporary directory:

```
.batch-edit-chunks/
  input/
    header.txt        # File preamble before first entry
    chunk-001.txt     # Entries 1-N
    chunk-002.txt     # Entries N+1-2N
    ...
  output/             # Generate mode only
```

Use Bash with a Python one-liner or small inline script to
split. Preserve the exact content — no reformatting. Track
the total entry count and number of chunks.

**Splitting rules:**
- Each chunk must be a valid standalone fragment (complete
  entries only, never split mid-entry)
- Preserve leading/trailing whitespace and newlines exactly
- Save any file header/preamble (content before the first
  entry) separately as `header.txt`
- Name chunks with zero-padded numbers for correct ordering

### 2. Process Chunks in Rounds

For each round, spawn up to max-parallel-agents subagents
simultaneously. Each subagent receives:

- The chunk file path to read (and edit, in transform mode)
- The transformation/generation instructions (from user)
- In generate mode: an output file path to write results to
- Any reference material needed

**Subagent prompt template (transform mode):**

```
Apply the following transformation to every entry in the
chunk file at [chunk_path]:

[transformation_instructions]

Rules:
- Edit the file in place using the Edit tool
- Process every entry in the file — do not skip any
- Preserve all content not related to the transformation
- Do not reorder, add, or remove entries
- When done, report how many entries you modified
```

**Subagent prompt template (generate mode):**

```
Read the input entries from [input_chunk_path].
For each entry, produce [description of output].
Write all output to [output_chunk_path] using the Write tool.

[generation_instructions]

Rules:
- Process every entry in the input — do not skip any
- Write output in the same order as the input entries
- When done, report how many entries you processed
```

Launch subagents with:
- `subagent_type: "general-purpose"`
- `run_in_background: true`

After launching a round, **stop and wait for
`<task-notification>` messages**. Do NOT poll, sleep, or
check output files.

### 3. Between Rounds

After each round completes:
- Log progress: "Round N complete: chunks X-Y processed
  (Z/total entries done)"
- Spot-check one chunk from the completed round by reading
  a few entries to verify correctness
- If a subagent failed or skipped entries, re-run that chunk
- Launch the next round

### 4. Reassemble

**Transform mode:** Concatenate edited chunks back into the
original file:

```bash
cat .batch-edit-chunks/input/header.txt \
    .batch-edit-chunks/input/chunk-*.txt \
    > [original_file]
```

**Generate mode:** Concatenate output chunks to the
destination:

```bash
cat .batch-edit-chunks/output/chunk-*.txt \
    >> [destination_file]
```

(Use `>` or `>>` as appropriate for the task.)

Verify the result:
- Line/entry count is reasonable
- Spot-check entries from different positions
- Run any relevant build/validation commands

### 5. Clean Up

```bash
rm -rf .batch-edit-chunks/
```

## Quick Reference

| Rule | Detail |
|------|--------|
| No shared files | One chunk file per subagent |
| Infer don't ask | Determine delimiter, batch size, I/O mode, model autonomously |
| Two I/O modes | Transform (edit in place) vs. generate (read input, write output) |
| Split first | Never skip the split step |
| Complete entries | Never split mid-entry |
| Preserve exactly | No reformatting during split |
| Wait properly | Stop after launching; wait for notifications |
| Spot-check | Verify one chunk per round |
| Re-run failures | Retry failed chunks, don't skip them |
| Log parameters | Show inferred settings before splitting |

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Assuming output == input file | Determine I/O mode; use generate mode when output differs |
| Asking user for delimiter/batch size | Inspect the file and infer it |
| Multiple agents editing one file | Split into chunk files first |
| Splitting mid-entry | Split only on entry boundaries |
| Polling for agent completion | Stop and wait for task-notification |
| Skipping failed chunks | Re-run them in a later round |
| Using opus for simple transforms | Sonnet default; opus for complex judgment |
| Forgetting the header | Save preamble separately, prepend on reassemble |
| Not cleaning up chunks dir | Delete .batch-edit-chunks/ when done |
