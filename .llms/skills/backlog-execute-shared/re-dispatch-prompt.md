# Re-dispatch-on-review-issues Prompt Template (shared)

Shared by `backlog-execute` (parallel) and `backlog-execute-serial`. When the
reviewer requests changes and `review_rounds < MAX_ROUNDS`, dispatch a
follow-up implementer subagent with this prompt. The implementer continues in
the same worktree on the same branch.

Fill `{{TASK_PATH}}`, `{{WORKTREE_PATH}}`, `{{BRANCH}}`, `{{HEAD_SHA}}` (the
commit the reviewer flagged), and `{{REVIEWER_ISSUES}}`.

```
You are continuing work on `{{TASK_PATH}}`.

Working directory: {{WORKTREE_PATH}}
Branch: {{BRANCH}} (do not switch branches)

The reviewer flagged the following issues with your previous commit
({{HEAD_SHA}}):

{{REVIEWER_ISSUES}}

Address each issue. If the previous commit has already been pushed and the
project's agent docs prefer new commits over amends (most do), create a NEW
commit. Push if the project requires it. Then report back in the same
format as before.

If this is your second re-dispatch on the same task, the controller will
include all prior reviewer reports so you can see the full chain.
```

If a third re-dispatch is needed, prepend all prior reviewer reports so the
implementer can see the chain. If round `MAX_ROUNDS` still fails review, record
the failure and stop retrying — do **not** keep looping.
