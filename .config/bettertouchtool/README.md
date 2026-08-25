# BetterTouchTool configuration

`Default.bttpreset` is the Git-tracked source for the BetterTouchTool `Default`
preset. It contains triggers only; licenses, general preferences, usage data,
and BetterTouchTool's live databases are deliberately excluded.

## Set up a new Mac

1. Clone this dotfiles repository into the home directory.
2. Install BetterTouchTool in `/Applications`.
3. Start with an empty BetterTouchTool configuration.
4. Run:

   ```sh
   ~/.config/bettertouchtool/setup.sh
   ```

BetterTouchTool may display a security confirmation for the initial import.
Confirm it and select the option to allow future imports if offered.

The setup script imports the tracked preset, verifies the result, configures
this repository to use `.githooks`, and enables BetterTouchTool synchronization
for this clone. Use `--yes` to skip the setup script's own confirmation prompt.

## Normal workflow

- `git commit` exports the live preset and stages it before creating the commit.
- `git pull` imports a changed preset after a successful merge.
- Branch checkout and rebase hooks import changed presets as well.
- Hooks do nothing when the live preset and tracked file are already identical.

The synchronization stores its last successful checksum in the local Git
directory. If both Git and BetterTouchTool changed since that checksum, the
hook reports a conflict and leaves both versions untouched.

To disable synchronization for this clone:

```sh
git config --local --unset btt.syncEnabled
git config --local --unset core.hooksPath
```
