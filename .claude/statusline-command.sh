#!/bin/bash

input=$(cat)

# Extract basic info
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name')

# Calculate context window usage
usage=$(echo "$input" | jq '.context_window.current_usage')
if [ "$usage" != "null" ]; then
  current=$(echo "$usage" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
  size=$(echo "$input" | jq '.context_window.context_window_size')
  pct=$((current * 100 / size))
  context_info=$(printf ' \033[1;33m[%d%%]\033[0m' "$pct")
else
  context_info=""
fi

# Calculate git branch + dirty/clean indicator
branch=$(git -C "$cwd" branch --show-current 2>/dev/null)
if [ -n "$branch" ]; then
  if [ -n "$(git -C "$cwd" status --porcelain 2>/dev/null)" ]; then
    git_info=$(printf ' \033[1;35m[%s*]\033[0m' "$branch")
  else
    git_info=$(printf ' \033[1;35m[%s]\033[0m' "$branch")
  fi
else
  git_info=""
fi

# Print status line
printf '\033[1;31m%s@%s\033[0m \033[1;32m%s\033[0m \033[1;34m[%s]\033[0m%s%s' "$(whoami)" "$(hostname -s)" "$cwd" "$model" "$context_info" "$git_info"
