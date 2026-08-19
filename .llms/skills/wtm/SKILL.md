---
name: wtm
description: Execute a task on master
---

Please implement a `wt` task on the master or main branch, without creating a worktree or new branch.

## Inherit the worktree task contract

Before acting, read `~/.llms/skills/wt/SKILL.md` completely. Apply all of its repository
isolation, verification, candidate, promotion-mandate, CI-repair, remote-push, demo, runtime,
and safety rules except where this skill explicitly replaces them.

This skill replaces these `wt` assumptions:

- The work will take place on the main or master branch, without creating a worktree.
