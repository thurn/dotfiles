#!/bin/sh
#
# Run Claude Code non-interactively as an independent reviewer of a code
# change. Writes the review as plain text under /tmp/reviews/ and prints the
# resulting filepath on stdout. Never asks Claude to edit or fix anything.
#
# Progress and errors go to stderr; stdout is exactly one line: the filepath.

set -eu

REVIEWS_DIR=${REVIEW_OUTPUT_DIR:-/tmp/reviews}
MODEL=${REVIEW_MODEL:-claude-sonnet-5}
EFFORT=${REVIEW_EFFORT:-high}
MAX_DIFF_BYTES=${REVIEW_MAX_DIFF_BYTES:-400000}
TIMEOUT_SECONDS=${REVIEW_TIMEOUT_SECONDS:-1800}

EX_USAGE=64
EX_DATAERR=65
EX_UNAVAILABLE=69
EX_SOFTWARE=70
EX_TEMPFAIL=75

usage() {
  cat >&2 <<'EOF'
usage: independent-review.sh (--prompt TEXT | --prompt-file FILE) [options]

Runs Claude Code (`claude -p`) as a single-pass independent reviewer and
writes the review to a new file under /tmp/reviews/. Prints that filepath.

Required (exactly one):
  --prompt TEXT        The original prompt/task that produced this change.
  --prompt-file FILE   Read that original prompt from FILE ("-" for stdin).

Options:
  --target TARGET      What to review. One of:
                         (omitted)    committed + uncommitted changes on the
                                      current branch vs. its base branch
                         path/to/file a file or directory
                         some-branch  a branch or any git ref
                         a..b / a...b a git ref range
                         123 / #123   a GitHub pull request number
  --since REF          Review committed, staged, and unstaged changes since
                       REF, plus all current untracked files in full. Cannot
                       be combined with --target.
  --base BRANCH        Base branch for the default/branch comparison.
                       Default: origin/HEAD, else main, else master.
  --repo DIR           Repository to review. Default: current directory.
  --model MODEL        Review model. Default: claude-sonnet-5
                       (env REVIEW_MODEL).
  --effort LEVEL       low|medium|high|xhigh|max. Default: high.
  --timeout SECONDS    Stop Claude after this many seconds. Default: 1800
                       (env REVIEW_TIMEOUT_SECONDS).
  --out FILE           Write the review here instead of a generated path.
  --print-prompt       Print the assembled review prompt and exit without
                       calling Claude. For checking what would be reviewed.
  -h, --help           Show this help.

Exit codes: 0 ok, 64 usage, 65 nothing to review, 69 claude unavailable or
unauthenticated, 70 the review run failed, 75 Claude temporarily unavailable
(usage-limited or timed out), 130 interrupted, 143 terminated.
EOF
  exit $EX_USAGE
}

die() {
  code=$1
  shift
  printf '%s\n' "independent-review: $*" >&2
  exit "$code"
}

note() {
  printf '%s\n' "independent-review: $*" >&2
}

prompt_text=
prompt_file=
target=
since=
base=
repo=.
out=
print_prompt=no

while [ $# -gt 0 ]; do
  case $1 in
    --prompt)
      [ $# -ge 2 ] || usage
      prompt_text=$2
      shift 2
      ;;
    --prompt-file)
      [ $# -ge 2 ] || usage
      prompt_file=$2
      shift 2
      ;;
    --target)
      [ $# -ge 2 ] || usage
      target=$2
      shift 2
      ;;
    --since)
      [ $# -ge 2 ] || usage
      since=$2
      shift 2
      ;;
    --base)
      [ $# -ge 2 ] || usage
      base=$2
      shift 2
      ;;
    --repo)
      [ $# -ge 2 ] || usage
      repo=$2
      shift 2
      ;;
    --model)
      [ $# -ge 2 ] || usage
      MODEL=$2
      shift 2
      ;;
    --effort)
      [ $# -ge 2 ] || usage
      EFFORT=$2
      shift 2
      ;;
    --timeout)
      [ $# -ge 2 ] || usage
      TIMEOUT_SECONDS=$2
      shift 2
      ;;
    --out)
      [ $# -ge 2 ] || usage
      out=$2
      shift 2
      ;;
    --print-prompt)
      print_prompt=yes
      shift
      ;;
    -h | --help)
      usage
      ;;
    *)
      printf '%s\n' "independent-review: unknown argument: $1" >&2
      usage
      ;;
  esac
done

if [ -n "$prompt_file" ]; then
  [ -z "$prompt_text" ] || die $EX_USAGE "pass only one of --prompt / --prompt-file"
  if [ "$prompt_file" = "-" ]; then
    prompt_text=$(cat)
  else
    [ -r "$prompt_file" ] || die $EX_USAGE "cannot read prompt file: $prompt_file"
    prompt_text=$(cat "$prompt_file")
  fi
fi

[ -n "$prompt_text" ] || usage
[ -z "$target" ] || [ -z "$since" ] ||
  die $EX_USAGE "pass only one of --target / --since"
case $TIMEOUT_SECONDS in
  "" | *[!0-9]* | 0)
    die $EX_USAGE "timeout must be a positive integer number of seconds"
    ;;
esac

# --- environment checks ------------------------------------------------------

command -v claude >/dev/null 2>&1 ||
  die $EX_UNAVAILABLE "the \`claude\` CLI is not on PATH. Install Claude Code (https://claude.com/claude-code) and make sure \`claude\` is executable, then retry."

command -v git >/dev/null 2>&1 || die $EX_UNAVAILABLE "\`git\` is not on PATH."

[ -d "$repo" ] || die $EX_USAGE "not a directory: $repo"
repo_root=$(git -C "$repo" rev-parse --show-toplevel 2>/dev/null) ||
  die $EX_USAGE "not inside a git repository: $repo"

# --- work area ---------------------------------------------------------------

work=$(mktemp -d "${TMPDIR:-/tmp}/independent-review.XXXXXX")
review_pid=
cleanup() {
  rm -rf "$work"
}
cancel() {
  signal=$1
  status=$2
  trap - INT TERM
  if [ -n "$review_pid" ] && kill -0 "$review_pid" 2>/dev/null; then
    kill -"$signal" "$review_pid" 2>/dev/null || true
    if [ "$signal" = INT ]; then
      kill -TERM "$review_pid" 2>/dev/null || true
    fi
    wait "$review_pid" 2>/dev/null || true
  fi
  note "review cancelled"
  exit "$status"
}
trap cleanup EXIT
trap 'cancel INT 130' INT
trap 'cancel TERM 143' TERM

context="$work/context.txt"
: >"$context"

# --- resolve the base branch -------------------------------------------------

resolve_base() {
  if [ -n "$base" ]; then
    git -C "$repo_root" rev-parse --verify --quiet "$base" >/dev/null ||
      die $EX_USAGE "base ref not found: $base"
    printf '%s\n' "$base"
    return
  fi
  for candidate in origin/HEAD origin/main origin/master main master; do
    if git -C "$repo_root" rev-parse --verify --quiet "$candidate" >/dev/null; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  echo ""
}

base_ref=$(resolve_base)
branch=$(git -C "$repo_root" rev-parse --abbrev-ref HEAD 2>/dev/null || echo HEAD)

# --- collect the change ------------------------------------------------------

scope_label=
diff_file="$work/diff.patch"
: >"$diff_file"

append_untracked() {
  # Untracked files never appear in `git diff`, so include their contents.
  untracked_list="$work/untracked.paths"
  git -C "$repo_root" ls-files -z --others --exclude-standard >"$untracked_list" ||
    return 0
  [ -s "$untracked_list" ] || return 0
  {
    echo
    echo "===== UNTRACKED FILES (not in the diff above) ====="
  } >>"$diff_file"
  xargs -0 sh -c '
    repo_root=$1
    diff_file=$2
    shift 2
    for path do
      full="$repo_root/$path"
      if [ ! -f "$full" ]; then
        printf "%s\n" "--- $path (not a regular file, contents omitted)" >>"$diff_file"
        continue
      fi
      if ! grep -Iq . "$full" 2>/dev/null; then
        printf "%s\n" "--- $path (binary, contents omitted)" >>"$diff_file"
        continue
      fi
      size=$(wc -c <"$full" | tr -d " ")
      if [ "$size" -gt 200000 ]; then
        printf "%s\n" "--- $path (${size} bytes, too large to inline; read it directly)" >>"$diff_file"
        continue
      fi
      {
        printf "%s\n" "--- $path (new file, full contents)"
        cat "$full"
        echo
      } >>"$diff_file"
    done
  ' sh "$repo_root" "$diff_file" <"$untracked_list"
}

if [ -n "$since" ]; then
  git -C "$repo_root" rev-parse --verify --quiet "$since" >/dev/null ||
    die $EX_USAGE "since ref not found: $since"
  scope_label="committed and uncommitted changes since '$since', plus all current untracked files in full"
  git -C "$repo_root" diff "$since" >"$diff_file" ||
    die $EX_USAGE "could not diff since ref: $since"
  append_untracked
else
case $target in
  "")
    if [ -n "$base_ref" ]; then
      merge_base=$(git -C "$repo_root" merge-base "$base_ref" HEAD 2>/dev/null || echo "")
    else
      merge_base=""
    fi
    if [ -n "$merge_base" ]; then
      scope_label="all committed and uncommitted changes on branch '$branch' relative to $base_ref (merge-base $merge_base)"
      git -C "$repo_root" diff "$merge_base" >"$diff_file" || true
    else
      scope_label="uncommitted changes plus the most recent commit on '$branch' (no base branch could be resolved)"
      git -C "$repo_root" diff HEAD >"$diff_file" || true
      git -C "$repo_root" show HEAD >>"$diff_file" || true
    fi
    append_untracked
    ;;
  *..*)
    git -C "$repo_root" rev-parse --verify --quiet "${target%%..*}" >/dev/null ||
      die $EX_USAGE "left side of range not found: $target"
    scope_label="git ref range $target"
    git -C "$repo_root" diff "$target" >"$diff_file" ||
      die $EX_USAGE "could not diff range: $target"
    ;;
  *)
    if [ -e "$repo_root/$target" ] || [ -e "$target" ]; then
      scope_label="path '$target'"
      if [ -n "$base_ref" ]; then
        merge_base=$(git -C "$repo_root" merge-base "$base_ref" HEAD 2>/dev/null || echo HEAD)
      else
        merge_base=HEAD
      fi
      git -C "$repo_root" diff "$merge_base" -- "$target" >"$diff_file" || true
      if [ ! -s "$diff_file" ] && [ -f "$repo_root/$target" ]; then
        scope_label="file '$target' (unchanged relative to $merge_base; reviewing it in full)"
        {
          printf '%s\n' "--- $target (full contents)"
          cat "$repo_root/$target"
        } >"$diff_file"
      elif [ ! -s "$diff_file" ] && [ -f "$target" ]; then
        scope_label="file '$target' (unchanged relative to $merge_base; reviewing it in full)"
        {
          printf '%s\n' "--- $target (full contents)"
          cat "$target"
        } >"$diff_file"
      fi
    elif printf '%s\n' "$target" | grep -Eq '^#?[0-9]+$'; then
      pr=${target#\#}
      command -v gh >/dev/null 2>&1 ||
        die $EX_UNAVAILABLE "target '$target' looks like a pull request number but the \`gh\` CLI is not on PATH."
      scope_label="GitHub pull request #$pr"
      gh pr diff "$pr" --patch --repo "$(git -C "$repo_root" remote get-url origin 2>/dev/null || echo '')" >"$diff_file" 2>"$work/gh.err" ||
        gh pr diff "$pr" --patch >"$diff_file" 2>"$work/gh.err" ||
        die $EX_UNAVAILABLE "could not fetch PR #$pr: $(cat "$work/gh.err")"
      {
        echo
        echo "===== PULL REQUEST METADATA ====="
        gh pr view "$pr" --json title,body,baseRefName,headRefName 2>/dev/null || true
      } >>"$diff_file"
    elif git -C "$repo_root" rev-parse --verify --quiet "$target" >/dev/null; then
      scope_label="git ref '$target'"
      if [ -n "$base_ref" ]; then
        git -C "$repo_root" diff "$base_ref...$target" >"$diff_file" || true
      else
        git -C "$repo_root" show "$target" >"$diff_file" || true
      fi
    else
      die $EX_USAGE "target is not a path, a git ref, a ref range, or a PR number: $target"
    fi
    ;;
esac
fi

[ -s "$diff_file" ] ||
  die $EX_DATAERR "nothing to review: no changes found for ${scope_label}."

diff_bytes=$(wc -c <"$diff_file" | tr -d ' ')
truncated=no
if [ "$diff_bytes" -gt "$MAX_DIFF_BYTES" ]; then
  head -c "$MAX_DIFF_BYTES" "$diff_file" >"$work/diff.trunc"
  mv "$work/diff.trunc" "$diff_file"
  truncated=yes
fi

# --- build the review prompt -------------------------------------------------

{
  echo "You are an independent reviewer. Another engineer just completed a code"
  echo "change and you are reviewing it cold, in a single pass. Your job is to find"
  echo "real defects, not to be agreeable and not to restate what the change does."
  echo
  printf '%s\n' "Repository: $repo_root"
  printf '%s\n' "Branch: $branch"
  printf '%s\n' "Base: ${base_ref:-<none resolved>}"
  printf '%s\n' "Review scope: $scope_label"
  if [ "$truncated" = yes ]; then
    printf '%s\n' "NOTE: the diff below was truncated at ${MAX_DIFF_BYTES} bytes of ${diff_bytes}."
    echo "Use git and file reads to inspect anything that was cut off."
  fi
  echo
  echo "===== ORIGINAL TASK PROMPT (what the author was asked to do) ====="
  printf '%s\n' "$prompt_text"
  echo "===== END ORIGINAL TASK PROMPT ====="
  echo
  echo "===== WHAT TO REPORT ====="
  cat <<'EOF'
Report only findings in these two categories.

1. Bugs — the code does something other than what it should.
   Logic errors, wrong or inverted conditions, off-by-one, unhandled edge
   cases (empty, null/undefined, zero, single element, duplicate, missing
   key), incorrect error handling, resource or state leaks, ordering and
   concurrency/race problems, stale or unsynchronized derived state,
   misuse of an API or violation of a contract that surrounding code
   depends on, and behavior that contradicts the original task prompt above
   (including parts of the task silently left unimplemented).

2. Architectural problems — the code will break or mislead later.
   Brittle production code that depends on incidental details rather than
   stated contracts; brittle tests that assert on implementation internals,
   mutable/production data, timing, iteration order, or incidental strings
   instead of stable observable behavior; missing test coverage for a
   behavior this change actually introduced or altered; code duplication
   that creates a real divergence hazard (two copies of the same rule that
   must be edited together); leaky or misplaced abstractions and layering
   violations that hide coupling; interfaces that make incorrect usage easy.

===== WHAT NOT TO REPORT =====
Do NOT report any of the following. They are out of scope and reporting them
makes this review less useful:
  - Style, formatting, naming, comment wording, import order, file layout.
  - Aesthetic or subjective preferences, "consider renaming", "might be
    cleaner if".
  - Security findings of any kind: vulnerabilities, injection, authn/authz,
    secrets handling, input sanitization, dependency CVEs, threat modeling.
  - Performance micro-optimizations with no measured impact.
  - Pre-existing issues untouched by this change, unless the change makes
    them actively worse.
  - Speculative concerns you cannot tie to specific lines of real code.
Silence on those topics is correct behavior, not an oversight.

===== HOW TO WORK =====
  - Read the actual code around every candidate finding before reporting it.
    The diff alone is not enough context to judge correctness.
  - For claims about missing or changed behavior, verify the complete
    observable contract rather than one changed component in isolation. Trace
    all cooperating paths and mechanisms, compare the before/after result where
    useful (including timing or ordering when relevant), and check whether the
    requirement is satisfied elsewhere. Do not infer a defect merely because
    one proposed implementation is absent.
  - You are READ-ONLY. Do NOT edit, create, delete, format, or fix any file,
    and do not run any command that mutates the repository or the working
    tree. Describe problems; the caller applies fixes.
  - Do not delegate to subagents. This is a single-pass review.
  - Prefer precision over volume. A short list of real defects is far more
    valuable than a long list of maybes. If you are not reasonably confident
    something is wrong, leave it out or mark it clearly as low confidence.
  - Report at most 10 findings, most severe first.
  - It is a perfectly good outcome to find nothing. If the change is sound,
    say so plainly.

===== OUTPUT FORMAT =====
Plain text, no markdown headers, no preamble, no closing pleasantries.
Start with one line: "VERDICT: <clean | minor issues | material issues>".
Then, for each finding, exactly this block:

FINDING <n>: <one-line title>
  SEVERITY: <critical|high|medium>
  CATEGORY: <bug|brittle-code|brittle-test|duplication|coverage|architecture>
  CONFIDENCE: <high|medium|low>
  LOCATION: <path>:<line> (repeat this line for other affected locations)
  PROBLEM: <what is wrong, in terms of the code as written>
  FAILURE: <a concrete scenario — specific inputs or state — that produces
    the wrong result, or the concrete future edit that this breaks>
  SUGGESTION: <the direction of a fix, one or two sentences, no patch>

If there are no findings, output "VERDICT: clean" and one short paragraph
saying what you checked and why you believe the change is sound.
EOF
  echo
  echo "===== DIFF UNDER REVIEW ====="
  cat "$diff_file"
  echo "===== END DIFF ====="
} >"$work/review-prompt.txt"

# --- run the review ----------------------------------------------------------

if [ "$print_prompt" = yes ]; then
  note "not calling claude (--print-prompt); scope: $scope_label"
  cat "$work/review-prompt.txt"
  exit 0
fi

mkdir -p "$REVIEWS_DIR" || die $EX_SOFTWARE "cannot create output directory: $REVIEWS_DIR"

if [ -z "$out" ]; then
  slug=$(printf '%s' "$(basename "$repo_root")" | tr -c 'A-Za-z0-9._-' '-')
  bslug=$(printf '%s' "$branch" | tr -c 'A-Za-z0-9._-' '-')
  stamp=$(date +%Y%m%d-%H%M%S)
  out="$REVIEWS_DIR/${slug}-${bslug}-${stamp}-$$.txt"
fi

note "reviewing $scope_label"
note "model=$MODEL effort=$EFFORT diff_bytes=$diff_bytes"

# Run from the repository root so the reviewer's cwd, project settings, and
# relative paths in the diff all line up, even when --repo pointed elsewhere.
(
  cd "$repo_root" || exit $EX_SOFTWARE
  trap - INT TERM
  exec claude -p \
    --model "$MODEL" \
    --effort "$EFFORT" \
    --no-session-persistence \
    --output-format text \
    --append-system-prompt "You are acting as a read-only independent code reviewer. Never edit, create, or delete files, and never run commands that change the repository or working tree. Report findings only." \
    --allowedTools Read Grep Glob \
    "Bash(git diff:*)" "Bash(git log:*)" "Bash(git show:*)" \
    "Bash(git status:*)" "Bash(git ls-files:*)" "Bash(git blame:*)" \
    "Bash(rg:*)" \
    --disallowedTools Edit Write NotebookEdit \
    <"$work/review-prompt.txt" >"$work/review.txt" 2>"$work/claude.err"
)&
review_pid=$!

# A blocking `wait` can defer signal traps in some non-interactive shells.
# Check process liveness in the shell so cancellation is handled promptly
# without returning control to the calling agent. Enforce a deadline because
# Claude Code has sometimes stayed alive instead of returning a usage-limit
# error to non-interactive callers.
review_deadline=$(($(date +%s) + TIMEOUT_SECONDS))
timed_out=no
while kill -0 "$review_pid" 2>/dev/null; do
  if [ "$(date +%s)" -ge "$review_deadline" ]; then
    timed_out=yes
    note "review exceeded ${TIMEOUT_SECONDS}s; terminating Claude"
    kill -TERM "$review_pid" 2>/dev/null || true

    kill_deadline=$(($(date +%s) + 5))
    while kill -0 "$review_pid" 2>/dev/null &&
      [ "$(date +%s)" -lt "$kill_deadline" ]; do
      sleep 1
    done
    if kill -0 "$review_pid" 2>/dev/null; then
      note "Claude did not stop after 5s; killing it"
      kill -KILL "$review_pid" 2>/dev/null || true
    fi
    break
  fi
  sleep 1
done

set +e
wait "$review_pid"
status=$?
set -e
review_pid=

if [ "$timed_out" = yes ]; then
  die $EX_TEMPFAIL "Claude review timed out after ${TIMEOUT_SECONDS}s; retry when scripted Claude usage is available or raise --timeout if the service is merely slow."
fi

case $status in
  130 | 143)
    note "review cancelled"
    exit "$status"
    ;;
esac

first_line=$(head -n 1 "$work/review.txt" 2>/dev/null || true)
claude_diagnostic=$(cat "$work/claude.err" "$work/review.txt" 2>/dev/null | head -c 4000 || true)
if { [ "$status" -ne 0 ] || ! printf '%s\n' "$first_line" | grep -Eq '^VERDICT: '; } &&
  printf '%s\n' "$claude_diagnostic" |
    grep -Eqi "you.ve hit your .*(session|weekly|monthly|usage|opus).*limit|usage limit (reached|exceeded)|rate.?limit (reached|exceeded)|monthly Agent SDK credit|Agent SDK credit.*(exhausted|limit)|credit balance is too low"; then
  if [ -n "$claude_diagnostic" ]; then
    note "claude output was:"
    printf '%s\n' "$claude_diagnostic" >&2
  fi
  die $EX_TEMPFAIL "Claude scripted usage is currently limited; retry after the reset shown by Claude or use API billing."
fi

if [ "$status" -ne 0 ] || [ ! -s "$work/review.txt" ]; then
  # Claude Code reports some failures (notably "Not logged in") on stdout, so
  # diagnose against both streams.
  if [ -n "$claude_diagnostic" ]; then
    note "claude output was:"
    printf '%s\n' "$claude_diagnostic" >&2
  fi
  auth_diagnostic=$(cat "$work/claude.err" 2>/dev/null || true)
  review_error_bytes=$(wc -c <"$work/review.txt" | tr -d ' ')
  if [ "$review_error_bytes" -le 2000 ]; then
    auth_diagnostic="${auth_diagnostic}
$(cat "$work/review.txt" 2>/dev/null || true)"
  fi
  if printf '%s\n' "$auth_diagnostic" | grep -Eqi 'not logged in|unauthoriz(ed|ation)?|unauthenticat(ed|ion)|invalid[ _]api[ _]key|authentication[ _]error|api key.*invalid|please run .*(/login|login|setup-token)'; then
    die $EX_UNAVAILABLE "Claude Code is installed but not authenticated. Run \`claude\` interactively and sign in with /login (or \`claude setup-token\`, or set ANTHROPIC_API_KEY), then retry."
  fi
  die $EX_SOFTWARE "the review run failed (claude exited with status $status); no review file was written."
fi

{
  echo "Independent review"
  printf '%s\n' "Generated: $(date '+%Y-%m-%d %H:%M:%S %z')"
  printf '%s\n' "Repository: $repo_root"
  printf '%s\n' "Branch: $branch"
  printf '%s\n' "Scope: $scope_label"
  printf '%s\n' "Model: $MODEL (effort $EFFORT)"
  if [ "$truncated" = yes ]; then
    printf '%s\n' "Diff bytes reviewed: $diff_bytes (truncated to $MAX_DIFF_BYTES)"
  else
    printf '%s\n' "Diff bytes reviewed: $diff_bytes"
  fi
  echo "----------------------------------------------------------------------"
  echo
  cat "$work/review.txt"
} >"$out"

note "review written"
printf '%s\n' "$out"
