#!/bin/zsh

set -euo pipefail

readonly script_dir="${0:A:h}"
readonly repo_root="$(git -C "${script_dir}" rev-parse --show-toplevel)"
readonly sync_script="${script_dir}/sync.sh"
readonly existing_hooks_path="$(git -C "${repo_root}" config --local --get core.hooksPath || true)"

if [[ "${1:-}" != "" && "${1:-}" != "--yes" ]]; then
  print -u2 "Usage: ${0:t} [--yes]"
  exit 2
fi

if [[ -n "${existing_hooks_path}" && "${existing_hooks_path}" != ".githooks" ]]; then
  print -u2 "This repository already uses a different Git hooks path: ${existing_hooks_path}"
  print -u2 "Merge those hooks into .githooks before running setup."
  exit 1
fi

if [[ "$(git -C "${repo_root}" config --local --get btt.syncEnabled || true)" == "true" ]]; then
  "${sync_script}" import
  git -C "${repo_root}" config --local core.hooksPath .githooks
  print "BetterTouchTool Git synchronization is already configured."
  exit 0
fi

if [[ "${1:-}" != "--yes" ]]; then
  print "This will replace BetterTouchTool's Default preset with:"
  print "  ${script_dir}/Default.bttpreset"
  print -n "Continue on this new, empty BetterTouchTool install? [y/N] "
  read -r reply
  [[ "${reply:l}" == "y" || "${reply:l}" == "yes" ]] || exit 1
fi

"${sync_script}" import --force

git -C "${repo_root}" config --local core.hooksPath .githooks
git -C "${repo_root}" config --local btt.syncEnabled true

print "Configured Git hooks for ${repo_root}"
print "BetterTouchTool configuration will now synchronize during Git operations."
