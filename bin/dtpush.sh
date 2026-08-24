#!/bin/sh

just fmt && git add -A && git commit -a --amend -C HEAD && just code-review-worktree && git push
