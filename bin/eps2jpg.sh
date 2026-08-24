#!/usr/bin/env zsh
set -euo pipefail

# eps2jpg.zsh
#
# Convert one or more EPS files to JPG using ImageMagick.
# Uses the user's preferred conversion pipeline:
#   magick -density 300 input.eps \
#     -background white -alpha remove -alpha off \
#     -resize x3181 \
#     -quality 92 \
#     output.jpg
#
# Defaults:
#   height  = 3181 px
#   density = 300
#   quality = 92
#
# Usage:
#   ./eps2jpg.zsh file1.eps file2.eps
#   ./eps2jpg.zsh --height 3181 --quality 92 *.eps
#   ./eps2jpg.zsh --overwrite *.eps

HEIGHT=3181
DENSITY=300
QUALITY=92
OVERWRITE=0

usage() {
  cat <<'EOF'
Usage:
  eps2jpg.zsh [options] file1.eps [file2.eps ...]

Options:
  --height N       Output height in pixels (default: 3181)
  --density N      EPS rasterization density (default: 300)
  --quality N      JPEG quality 1-100 (default: 92)
  --overwrite      Overwrite existing JPG outputs
  -h, --help       Show this help

Examples:
  eps2jpg.zsh artwork.eps
  eps2jpg.zsh --height 3181 --quality 92 *.eps
  eps2jpg.zsh --overwrite *.eps
EOF
}

die() {
  echo "Error: $*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

is_integer() {
  [[ "$1" == <-> ]]
}

ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --height)
      shift
      [[ $# -gt 0 ]] || die "--height requires a value"
      is_integer "$1" || die "--height must be an integer"
      HEIGHT="$1"
      ;;
    --density)
      shift
      [[ $# -gt 0 ]] || die "--density requires a value"
      is_integer "$1" || die "--density must be an integer"
      DENSITY="$1"
      ;;
    --quality)
      shift
      [[ $# -gt 0 ]] || die "--quality requires a value"
      is_integer "$1" || die "--quality must be an integer"
      QUALITY="$1"
      (( QUALITY >= 1 && QUALITY <= 100 )) || die "--quality must be between 1 and 100"
      ;;
    --overwrite)
      OVERWRITE=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      while [[ $# -gt 0 ]]; do
        ARGS+=("$1")
        shift
      done
      break
      ;;
    -*)
      die "Unknown option: $1"
      ;;
    *)
      ARGS+=("$1")
      ;;
  esac
  shift
done

[[ ${#ARGS[@]} -gt 0 ]] || {
  usage
  exit 1
}

require_cmd magick

for input in "${ARGS[@]}"; do
  [[ -f "$input" ]] || {
    echo "Skipping missing file: $input" >&2
    continue
  }

  case "$input:e:l" in
    eps|epsf|ps)
      ;;
    *)
      echo "Skipping non-EPS/PS file: $input" >&2
      continue
      ;;
  esac

  output="${input:r}.jpg"

  if [[ -e "$output" && "$OVERWRITE" -ne 1 ]]; then
    echo "Skipping existing output (use --overwrite): $output" >&2
    continue
  fi

  echo "Converting: $input -> $output"

  magick -density "$DENSITY" "$input" \
    -background white -alpha remove -alpha off \
    -resize "x${HEIGHT}" \
    -quality "$QUALITY" \
    "$output"
done
