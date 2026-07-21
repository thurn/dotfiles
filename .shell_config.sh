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
  ~/quest_prototype/scripts/regenerate-assets.sh && git add -A && git commit -a -m "$*"
}
