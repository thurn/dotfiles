# Disable C-q and C-s for suspend/resume
stty -ixon

if $emacs_shell ;
then
else
  bindkey '^j' history-beginning-search-forward
  bindkey '<up>' history-beginning-search-forward
  bindkey '\ec' history-beginning-search-backward
  bindkey '<down>' history-beginning-search-backward
  bindkey '^f' forward-word
  bindkey '^d' forward-char
  bindkey '^l' backward-word
  bindkey '\ez' backward-char
  bindkey '^r' backward-kill-word
  bindkey '^u' yank
  bindkey '^n' kill-line
  bindkey '\ex' kill-char
  bindkey '^e' end-of-line
  bindkey '^a' beginning-of-line
  bindkey '\ek' set-mark-command
  bindkey '\ed' kill-word
fi
