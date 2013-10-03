# Disable C-q and C-s for suspend/resume
stty -ixon

if $emacs_shell ;
then
else
  bindkey '^k' history-search-backward
  bindkey '^j' history-search-forward
  bindkey '^f' forward-word
  bindkey '^d' forward-char
  bindkey '^l' backward-word
  bindkey '^;' backward-char
  bindkey '^r' backward-kill-word
  bindkey '^u' yank
  bindkey '^n' kill-line
  bindkey '\ex' kill-char
  bindkey '^e' end-of-line
  bindkey '^a' beginning-of-line
  bindkey '\ek' set-mark-command
  bindkey '\ed' kill-word
fi
