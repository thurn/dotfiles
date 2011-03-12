#!/usr/bin/env zsh
# -*- mode: zsh; -*-

emacs_shell=false
emacs_term=false
if (set -u; : $EMACS) 2> /dev/null
then
  if [[ $EMACS == "t" ]]
  then
    emacs_shell=true
  else
    emacs_term=true
  fi
fi

# Disable C-q and C-s for suspend/resume
stty -ixon

if ! $emacs_shell ; then
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
  bindkey '^e' kill-char
  bindkey '\ex' end-of-line
  bindkey '^a' beginning-of-line
  bindkey '\ek' set-mark-command
  bindkey '\ed' kill-word
  bindkey '^p' clear-screen
  bindkey '^e' clear-screen
  bindkey '^b' clear-screen
  bindkey '^a' beginning-of-line
fi

#History File Size
HISTSIZE=10000
SAVEHIST=10000
#History File Location
HISTFILE=~/.zsh_history
# Append to history intead of overwriting
setopt append_history
# Append to history after commands, instead of at shell exit
setopt inc_append_history
# Save timestamps for commands
setopt extended_history
# Don't display duplicates while searching
setopt hist_find_no_dups
# Remove older commands if they duplicate newer comamnds
setopt hist_ignore_all_dups
# Remove blanks from commands as they are added
setopt hist_reduce_blanks
# Remove commands with leading whitespace
setopt hist_ignore_space
# Remove the history command from the history list
setopt hist_no_store
# Remove function definitions from history
setopt hist_no_functions
# Omit older duplicates when writing history
setopt hist_save_no_dups
#Removed duplicate items from history first
setopt hist_expire_dups_first

# Use ... for ../.. etc
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/..
  else
        LBUFFER+=.
          fi
}

zle -N rationalise-dot
bindkey . rationalise-dot

# Make cd push the old directory onto the directory stack.
set -o AUTO_PUSHD

# don't push duplicate directories
set -o PUSHD_IGNORE_DUPS

# Tell emacs what directory I am in
if (set -u; : $EMACS) 2> /dev/null
then
  if [[ $EMACS == "t" ]]
  then
    # *shell*, do nothing.
    function precmd {}
  else
    # Emacs *term*
    function precmd {
      echo -e "\033AnSiTu" "$LOGNAME"
      echo -e "\033AnSiTc" "$(pwd)"
      echo -e "\033AnSiTh" "$(hostname)" 
    }
  fi
else
  # No emacs
  function precmd {
  }
fi

# A huge hack which sets $TERM to fix some programs 
if $emacs_shell ;
then
  export TERM=dumb
else
  export TERM=xterm-256color
fi

# Set default file creation to rw-rw-r--
umask 002

# prevent overwriting files with cat:
set -o noclobber
# stops ctrl+d from logging me out:
set -o ignoreeof           
# Emacs keybindings
set -o emacs

