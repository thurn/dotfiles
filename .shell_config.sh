export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

alias cdd="cd ~/dreamtides"
alias cdq="cd ~/quest_prototype"
alias cdj="cd ~/journeys"
alias st="git status"
alias am="git commit -a --amend -C HEAD"
alias j="just"
alias claude="claude --dangerously-skip-permissions --remote-control"
alias abu="just abu"
alias codex="codex --dangerously-bypass-approvals-and-sandbox"
alias fd="fd -I"

alias ne="npm run editor2"
alias nr="npm run qai"

com() {
  local message

  ~/quest_prototype/scripts/regenerate-assets.sh --fast && git add -A || return

  if [ "$#" -eq 0 ]; then
    message=$(git-commit-message) || return
  else
    message="$*"
  fi

  git commit -a -m "$message" && git push
}

# iTerm2 shell integration — enables cmd+click on file paths, cwd tracking, etc.
# Wraps OSC escapes in tmux passthrough format when $TMUX is set.
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Unity CLI
. "/Users/dthurn/.unity/env"
