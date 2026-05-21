#!/usr/bin/env bash
set -euo pipefail

input="$(cat)"

message="Codex finished a turn."
if command -v /usr/bin/python3 >/dev/null 2>&1; then
  parsed_message="$(
    /usr/bin/python3 -c '
import json
import sys

try:
    payload = json.load(sys.stdin)
except Exception:
    payload = {}

cwd = payload.get("cwd") or ""
last = (payload.get("last_assistant_message") or "").strip()

if last:
    first_line = next((line.strip() for line in last.splitlines() if line.strip()), "")
    if len(first_line) > 96:
        first_line = first_line[:93] + "..."
    print(f"Codex finished: {first_line}")
elif cwd:
    print(f"Codex finished work in {cwd}")
else:
    print("Codex finished a turn.")
' <<<"$input"
  )"

  if [[ -n "$parsed_message" ]]; then
    message="$parsed_message"
  fi
fi

notify_iterm() {
  local text="$1"
  local target_tty=""

  if [[ "${TERM_PROGRAM:-}" == "iTerm.app" || "${TERM_PROGRAM:-}" == "iTerm2" || "${__CFBundleIdentifier:-}" == "com.googlecode.iterm2" ]]; then
    printf '\033]9;%s\007' "$text" 2>/dev/null > /dev/tty && return 0

    if [[ -n "${TMUX:-}" ]] && command -v tmux >/dev/null 2>&1; then
      target_tty="$(tmux display-message -p '#{client_tty}' 2>/dev/null || true)"
      if [[ -n "$target_tty" && -w "$target_tty" ]] 2>/dev/null; then
        printf '\033]9;%s\007' "$text" 2>/dev/null > "$target_tty" && return 0
      fi
    fi
  fi

  return 1
}

notify_macos_fallback() {
  local text="$1"

  /usr/bin/osascript \
    -e 'on run argv' \
    -e 'display notification (item 2 of argv) with title (item 1 of argv)' \
    -e 'end run' \
    "Codex" "$text" >/dev/null 2>&1 </dev/null &
}

play_sound() {
  local sound="/System/Library/Sounds/Glass.aiff"

  if [[ -r "$sound" ]]; then
    /usr/bin/afplay "$sound" >/dev/null 2>&1 </dev/null &
  fi
}

notify_iterm "$message" || notify_macos_fallback "$message"
play_sound

printf '{}\n'
