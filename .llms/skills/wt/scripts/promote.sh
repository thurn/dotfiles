#!/bin/sh

set -eu

usage() {
  echo "usage: $0 WORKTREE REPO_ROOT BRANCH" >&2
  exit 64
}

promote_locked() {
  locked_worktree=$1
  locked_repo_root=$2
  locked_branch=$3

  git -C "$locked_worktree" rebase master
  git -C "$locked_repo_root" checkout master
  git -C "$locked_repo_root" merge --ff-only "$locked_branch"
  git -C "$locked_repo_root" push origin master
}

if [ "${1:-}" = "--locked" ]; then
  [ "$#" -eq 4 ] || usage
  promote_locked "$2" "$3" "$4"
  exit
fi

[ "$#" -eq 3 ] || usage

worktree=$1
repo_root=$2
branch=$3

common_git_dir=$(git -C "$repo_root" rev-parse --path-format=absolute --git-common-dir)
promotion_lock="$common_git_dir/wt-promotion.lock"
script_dir=$(CDPATH= cd -P -- "$(dirname -- "$0")" && pwd)
script_path="$script_dir/$(basename -- "$0")"

echo "Waiting for repository-wide worktree promotion lock: $promotion_lock"

if command -v lockf >/dev/null 2>&1; then
  exec lockf -k "$promotion_lock" \
    "$script_path" --locked "$worktree" "$repo_root" "$branch"
fi

if command -v flock >/dev/null 2>&1; then
  exec flock "$promotion_lock" \
    "$script_path" --locked "$worktree" "$repo_root" "$branch"
fi

echo "Cannot serialize promotion: neither lockf nor flock is available." >&2
exit 69
