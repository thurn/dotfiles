-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.font = wezterm.font 'Iosevka Term'
config.font_size = 13
config.hide_tab_bar_if_only_one_tab = true


config.color_scheme = 'Catppuccin Latte'

-- Don't prompt for confirmation when closing a window (migrated from iTerm2).
config.window_close_confirmation = 'NeverPrompt'

-- Large scrollback buffer (iTerm2 ran with unlimited scrollback).
config.scrollback_lines = 1000000

-- ---------------------------------------------------------------------------
-- CMD+click file paths -> open in VS Code (works under tmux)
-- ---------------------------------------------------------------------------

-- 1. Linkify paths (with a slash) + optional :line:col, route to our scheme.
config.hyperlink_rules = wezterm.default_hyperlink_rules()
table.insert(config.hyperlink_rules, {
  -- anchored paths (/, ~/, ./, ../) match every segment incl. extensionless
  -- ones (e.g. .../MacOS/wezterm); unanchored relatives still need a final
  -- .ext so prose like "and/or" is not linkified. Optional trailing :line:col.
  -- matches  /Applications/WezTerm.app/Contents/MacOS/wezterm ,
  --          docs/opponents/sigdecks.md , /Users/dthurn/x.ts:42:7 , ~/Desktop/a.png
  -- anchored segments end in a non-dot char so a trailing sentence period
  -- (".../pool.md.") is not swallowed; internal dots and dotfiles still match.
  regex = [[(?:(?:~|\.{1,2})?/[\w.\-]*[\w-](?:/[\w.\-]*[\w-])*|[\w.\-]+/(?:[\w.\-]+/)*[\w.\-]+\.\w+)(?::\d+){0,2}]],
  format = 'vsfile://$0',
})

-- 2. Resolve the path (relatives against the active tmux pane's cwd) and open
--    it in VS Code via the vscode:// URL scheme.
wezterm.on('open-uri', function(window, pane, uri)
  local prefix = 'vsfile://'
  if uri:sub(1, #prefix) ~= prefix then
    return true -- not ours: let WezTerm open browser links normally
  end
  local target = uri:sub(#prefix + 1)

  -- peel off :line:col if present
  local path, line, col = target:match '^(.-):(%d+):(%d+)$'
  if not path then path, line = target:match '^(.-):(%d+)$' end
  if not path then path = target end

  -- expand a leading ~ to the home directory
  if path:sub(1, 1) == '~' then path = wezterm.home_dir .. path:sub(2) end

  -- resolve relative paths against the ACTIVE TMUX PANE's cwd
  if path:sub(1, 1) ~= '/' then
    local ok, stdout = wezterm.run_child_process {
      '/opt/homebrew/bin/tmux', 'display-message', '-p', '#{pane_current_path}',
    }
    local cwd
    if ok then cwd = stdout:gsub('%s+$', '') end
    if not cwd or cwd == '' then
      -- fallback when not inside tmux: use WezTerm's own cwd
      local u = pane:get_current_working_dir()
      if u then cwd = (type(u) == 'userdata') and u.file_path or u:gsub('^file://[^/]*', '') end
    end
    if cwd and cwd ~= '' then path = cwd .. '/' .. path end
  end

  local url = 'vscode://file' .. path
  if line then url = url .. ':' .. line end
  if col then url = url .. ':' .. col end
  wezterm.open_with(url) -- macOS hands vscode:// to VS Code
  return false -- we handled it; suppress default
end)

-- 3. CMD+click opens links, even under tmux (mouse mode on).
-- tmux forwards clicks to itself unless a "bypass" modifier is held; we make
-- CMD that bypass. IMPORTANT: the bypass modifier is STRIPPED before matching
-- mouse_bindings, so a CMD+click arrives looking like a plain (NONE) click.
-- Under tmux that is the ONLY way a click reaches these bindings (plain clicks
-- go to tmux), so binding NONE -> open-link yields "CMD+click opens links".
config.bypass_mouse_reporting_modifiers = 'SUPER'
local act = wezterm.action
config.mouse_bindings = {
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'NONE',
    action = act.CompleteSelectionOrOpenLinkAtMouseCursor 'ClipboardAndPrimarySelection',
  },
}

-- ---------------------------------------------------------------------------
-- Custom key bindings migrated from iTerm2.
-- These send raw byte sequences to the shell (readline/emacs-style editing),
-- mirroring the old iTerm2 "Send Hex Code" / "Send Escape Sequence" mappings.
-- ---------------------------------------------------------------------------
config.keys = {
  { key = 'Enter', mods = 'SHIFT', action = act.SendString '\n' }, -- Shift+Enter sends a newline
  { key = ';', mods = 'CTRL', action = act.SendString '\x1b[D' }, -- left arrow
  { key = 'd', mods = 'CTRL', action = act.SendString '\x1b[C' }, -- right arrow
  { key = 'f', mods = 'CTRL', action = act.SendString '\x1bf' },  -- forward one word (Alt-F)
  { key = 'j', mods = 'CTRL', action = act.SendString '\x1b[B' }, -- down arrow
  { key = 'k', mods = 'CTRL', action = act.SendString '\x1b[A' }, -- up arrow
  { key = 'l', mods = 'CTRL', action = act.SendString '\x1bb' },  -- backward one word (Alt-B)
  { key = 'n', mods = 'CTRL', action = act.SendString '\x0b' },   -- kill to end of line (^K)
  { key = 'r', mods = 'CTRL', action = act.SendString '\x17' },   -- delete word backward (^W)
  { key = 'u', mods = 'CTRL', action = act.SendString '\x19' },   -- yank (^Y)
}

-- and finally, return the configuration to wezterm
return config
