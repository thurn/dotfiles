#!/bin/zsh

set -euo pipefail

readonly script_dir="${0:A:h}"
readonly repo_root="$(git -C "${script_dir}" rev-parse --show-toplevel)"
readonly git_dir="$(git -C "${repo_root}" rev-parse --absolute-git-dir)"
readonly preset_name="Dotfiles"
readonly preset_uuid="66291D50-BB12-48AB-9881-13CA48803E43"
readonly preset_path="${BTT_PRESET_PATH:-${script_dir}/Default.bttpreset}"
readonly state_path="${BTT_STATE_PATH:-${git_dir}/bettertouchtool-last-synced}"
readonly btt_app_path="${BTT_APP_PATH:-/Applications/BetterTouchTool.app}"
readonly osascript_path="${BTT_OSASCRIPT:-/usr/bin/osascript}"

usage() {
  print -u2 "Usage: ${0:t} export | import [--force]"
  exit 2
}

fail() {
  print -u2 "BetterTouchTool sync: $1"
  exit 1
}

canonicalize_preset() {
  /usr/bin/jq -S '
    walk(
      if type == "object" then
        del(.BTTPresetUUID, .BTTLastUpdatedAt, .BTTOrder)
        | if has("BTTLayoutIndependentActionChar")
            and ((.BTTLayoutIndependentActionChar | length) > 1)
          then
            del(.BTTLayoutIndependentActionChar)
          else
            .
          end
      else
        .
      end
    )
    | .BTTPresetContent |= (
        map(
          select(
            (((.BTTTriggers // []) | length) > 0)
            or has("BTTAppSpecificSettings")
          )
        )
        | sort_by(.BTTAppBundleIdentifier)
      )
    | walk(
        if type == "object" and ((.BTTTriggers? | type) == "array") then
          .BTTTriggers |= sort_by(.BTTUUID // "")
        else
          .
        end
      )
  ' "$1"
}

hash_preset() {
  canonicalize_preset "$1" | /usr/bin/shasum -a 256 | /usr/bin/awk '{print $1}'
}

read_state() {
  if [[ -f "${state_path}" ]]; then
    < "${state_path}"
  fi
}

write_state() {
  local checksum="$1"
  local temporary_state="${state_path}.tmp.$$"

  umask 077
  print -r -- "${checksum}" > "${temporary_state}"
  /bin/mv "${temporary_state}" "${state_path}"
}

[[ -f "${preset_path}" ]] || fail "tracked preset is missing: ${preset_path}"
[[ -d "${btt_app_path}" ]] || fail "BetterTouchTool is not installed in /Applications."
[[ -x "${osascript_path}" ]] || fail "osascript is unavailable: ${osascript_path}"
[[ -x /usr/bin/jq ]] || fail "jq is unavailable: /usr/bin/jq"

temporary_dir="$(mktemp -d "${TMPDIR:-/tmp}/btt-sync.XXXXXX")"
live_preset="${temporary_dir}/live.bttpreset"
raw_preset="${temporary_dir}/raw.bttpreset"

cleanup() {
  /bin/rm -f "${live_preset}" "${raw_preset}"
  /bin/rmdir "${temporary_dir}" 2>/dev/null || true
}
trap cleanup EXIT

export_live_preset() {
  /bin/rm -f "${live_preset}" "${raw_preset}"
  export BTT_EXPORT_PATH="${raw_preset}"
  export BTT_PRESET_NAME="${preset_name}"

  "${osascript_path}" >/dev/null <<'APPLESCRIPT'
tell application "BetterTouchTool"
  export_preset (system attribute "BTT_PRESET_NAME") outputPath (system attribute "BTT_EXPORT_PATH") compress false includeSettings false comment "Portable keybindings managed in thurn/dotfiles"
end tell
APPLESCRIPT

  for attempt in {1..50}; do
    [[ -s "${raw_preset}" ]] && break
    /bin/sleep 0.1
  done

  [[ -s "${raw_preset}" ]] || fail "BetterTouchTool did not produce a preset export."

  /usr/bin/sed -E \
    "s/(\"BTTPresetUUID\" : \")[^\"]+/\\1${preset_uuid}/" \
    "${raw_preset}" > "${live_preset}"

  /usr/bin/grep -q \
    "\"BTTPresetUUID\" : \"${preset_uuid}\"" \
    "${live_preset}" || fail "could not normalize the exported preset identifier."
}

import_tracked_preset() {
  export BTT_IMPORT_PATH="${preset_path}"

  "${osascript_path}" >/dev/null <<'APPLESCRIPT'
tell application "BetterTouchTool"
  import_preset (system attribute "BTT_IMPORT_PATH")
end tell
APPLESCRIPT

  for attempt in {1..50}; do
    export_live_preset
    [[ "$(hash_preset "${live_preset}")" == "$(hash_preset "${preset_path}")" ]] && return
    /bin/sleep 0.1
  done

  fail "the imported preset did not match the tracked preset. Both versions were preserved."
}

action="${1:-}"
force="${2:-}"
[[ $# -le 2 ]] || usage
[[ -z "${force}" || "${force}" == "--force" ]] || usage

repo_hash="$(hash_preset "${preset_path}")"

if [[ "${action}" == "import" && "${force}" == "--force" ]]; then
  import_tracked_preset
  write_state "${repo_hash}"
  print "Imported ${preset_path} into BetterTouchTool."
  exit 0
fi

export_live_preset

live_hash="$(hash_preset "${live_preset}")"
state_hash="$(read_state)"

case "${action}" in
  export)
    [[ -z "${force}" ]] || usage

    if [[ "${repo_hash}" == "${live_hash}" ]]; then
      write_state "${repo_hash}"
      print "BetterTouchTool preset is already synchronized."
    elif [[ -z "${state_hash}" ]]; then
      fail "this clone is not initialized; run ${script_dir}/setup.sh first."
    elif [[ "${repo_hash}" == "${state_hash}" ]]; then
      /bin/mv "${live_preset}" "${preset_path}"
      write_state "${live_hash}"
      print "Exported live BetterTouchTool changes to ${preset_path}"
    elif [[ "${live_hash}" == "${state_hash}" ]]; then
      fail "the tracked preset has unapplied changes; import it before committing."
    else
      fail "conflict: the tracked and live presets both changed. Neither was overwritten."
    fi
    ;;

  import)
    if [[ "${repo_hash}" == "${live_hash}" ]]; then
      write_state "${repo_hash}"
      print "BetterTouchTool preset is already synchronized."
    elif [[ -z "${state_hash}" ]]; then
      fail "this clone is not initialized; run ${script_dir}/setup.sh first."
    elif [[ "${live_hash}" == "${state_hash}" ]]; then
      import_tracked_preset
      write_state "${repo_hash}"
      print "Imported tracked BetterTouchTool changes."
    elif [[ "${repo_hash}" == "${state_hash}" ]]; then
      print "No tracked BetterTouchTool changes to import; live changes were preserved."
    else
      fail "conflict: the tracked and live presets both changed. Neither was overwritten."
    fi
    ;;

  *)
    usage
    ;;
esac
