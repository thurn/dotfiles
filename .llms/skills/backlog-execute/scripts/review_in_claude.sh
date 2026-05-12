#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  review_in_claude.sh [options] [review prompt...]

Options:
  --project-root PATH    Run Claude from this project root. Defaults to $PWD.
  --prompt-file PATH     Read the review prompt from PATH. Use - for stdin.
  --prompt TEXT          Use TEXT as the review prompt.
  --model MODEL          Pass MODEL to claude --model.
  --effort LEVEL         Pass LEVEL to claude --effort. Defaults to high.
  --max-budget-usd USD   Pass a spend cap to claude.
  -h, --help             Show this help.

If no prompt is passed, stdin is used when data is piped in.
USAGE
}

project_root=$PWD
prompt_file=
prompt_text=
model=${CLAUDE_REVIEW_MODEL:-}
effort=${CLAUDE_REVIEW_EFFORT:-high}
max_budget=${CLAUDE_REVIEW_MAX_BUDGET_USD:-}
declare -a prompt_args=()

while (($#)); do
  case "$1" in
    --project-root)
      [[ $# -ge 2 ]] || { echo "missing value for --project-root" >&2; exit 2; }
      project_root=$2
      shift 2
      ;;
    --prompt-file)
      [[ $# -ge 2 ]] || { echo "missing value for --prompt-file" >&2; exit 2; }
      prompt_file=$2
      shift 2
      ;;
    --prompt)
      [[ $# -ge 2 ]] || { echo "missing value for --prompt" >&2; exit 2; }
      prompt_text=$2
      shift 2
      ;;
    --model)
      [[ $# -ge 2 ]] || { echo "missing value for --model" >&2; exit 2; }
      model=$2
      shift 2
      ;;
    --effort)
      [[ $# -ge 2 ]] || { echo "missing value for --effort" >&2; exit 2; }
      effort=$2
      shift 2
      ;;
    --max-budget-usd)
      [[ $# -ge 2 ]] || { echo "missing value for --max-budget-usd" >&2; exit 2; }
      max_budget=$2
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      prompt_args+=("$@")
      break
      ;;
    -*)
      echo "unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      prompt_args+=("$1")
      shift
      ;;
  esac
done

if ! command -v claude >/dev/null 2>&1; then
  echo "claude command not found on PATH" >&2
  exit 127
fi

if ! project_root=$(cd "$project_root" && pwd -P); then
  echo "project root does not exist: $project_root" >&2
  exit 2
fi

read_prompt() {
  if [[ -n $prompt_file ]]; then
    if [[ $prompt_file == "-" ]]; then
      cat
    else
      cat "$prompt_file"
    fi
  elif [[ -n $prompt_text ]]; then
    printf '%s\n' "$prompt_text"
  elif ((${#prompt_args[@]})); then
    printf '%s\n' "${prompt_args[*]}"
  elif [[ ! -t 0 ]]; then
    cat
  else
    usage >&2
    exit 2
  fi
}

prompt=$(read_prompt)
if [[ -z ${prompt//[[:space:]]/} ]]; then
  echo "review prompt is empty" >&2
  exit 2
fi

prompt_path=$(mktemp "${TMPDIR:-/tmp}/claude-review.XXXXXX")
trap 'rm -f "$prompt_path"' EXIT

cat >"$prompt_path" <<'PROMPT'
You are performing a code review for one backlog task.

Review-only rules:
- Do not edit files or make commits.
- Inspect the actual diff, artifacts, and project instructions.
- Prioritize bugs, regressions, missing acceptance criteria, missing tests,
  evidence gaps, and commit hygiene issues.
- Report findings first, ordered by severity.
- If changes are required, each finding must be actionable and include
  file:line references when possible.
- Reply with exactly one of these statuses:
  - APPROVED
  - CHANGES REQUESTED

PROMPT

{
  printf 'Working directory: %s\n\n' "$project_root"
  printf '## Review request\n\n'
  printf '%s\n' "$prompt"
} >>"$prompt_path"

claude_args=(
  --print
  --output-format text
  --no-session-persistence
  --effort "$effort"
  --allowedTools
  Read
  Grep
  Glob
  LS
  "Bash(git *)"
  "Bash(ls *)"
  "Bash(find *)"
  "Bash(sed *)"
  "Bash(cat *)"
  "Bash(wc *)"
  "Bash(test *)"
  "Bash(make *)"
  "Bash(npm *)"
  "Bash(pnpm *)"
  "Bash(yarn *)"
  "Bash(cargo *)"
  "Bash(pytest *)"
  --disallowedTools
  Edit
  MultiEdit
  Write
  NotebookEdit
)

if [[ -n $model ]]; then
  claude_args+=(--model "$model")
fi

if [[ -n $max_budget ]]; then
  claude_args+=(--max-budget-usd "$max_budget")
fi

cd "$project_root"
claude "${claude_args[@]}" <"$prompt_path"
