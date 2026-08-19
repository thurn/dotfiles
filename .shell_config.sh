export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

alias cdd="cd ~/dreamtides"
alias cdq="cd ~/quest_prototype"
alias cdm="cd ~/masonry"
alias cdj="cd ~/journeys"
alias st="git status"
alias am="git commit -a --amend -C HEAD"
alias j="just"
alias claude="claude --dangerously-skip-permissions --remote-control"
alias abu="just abu"
alias codex="codex --dangerously-bypass-approvals-and-sandbox"
alias fd="fd -I"
alias sg='git -C "$HOME" --git-dir="$HOME/shadowverse" --work-tree="$HOME"'

alias ne="npm run editor2"
alias nr="npm run qai"

com() {
  local message

  git add -A

  if [ "$#" -eq 0 ]; then
    message=$(git-commit-message) || return
  else
    message="$*"
  fi

  git commit -a -m "$message"
  tg push-master
}

# iTerm2 shell integration — enables cmd+click on file paths, cwd tracking, etc.
# Wraps OSC escapes in tmux passthrough format when $TMUX is set.
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
