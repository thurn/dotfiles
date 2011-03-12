#!/usr/bin/env zsh
# -*- mode: zsh; -*-

# Summaries only for du
alias du='du -s -h'

# Human readable free space
alias df='df -h'

# Colorize Grep
alias grep='grep --color=auto'

# Prompt before cp overwrite
alias cp='cp -i'

# Prompt before mv overwrite
alias mv='mv -i'

# Ping should eventually time out
alias ping='ping -c 5'

# Show process names in pgrep
alias pgrep="pgrep -l"

# Script which uses vimpager or vim -M intelligently
alias view='vim_wrapper.rb'

# Use the $PAGER variable instead of less
alias less=$PAGER

# Show history in less
alias history="history 0 | view"

# Set up "pie" as sed-like perl
alias pie='perl -p -i -e'

# Use trsh.pl instead of rm
alias rm='~/bin/trsh.pl'

# Github's cert is wrong, and I've never cared.
alias wget='wget --no-check-certificate'

# Use emacs in daemon mode by default
alias emacs_server='/usr/bin/env emacs --daemon'
alias emacs='emacs_wrapper.rb'
alias kill_emacs='emacsclient -e "(kill-emacs)"'

alias lg='ls | grep -i'
alias pd='popd'
alias mkdir='mkdir -p'
alias d='dirs -v'

# mkdir then cd to it
function mcd() { 
  mkdir -p "$1" && cd "$1"; 
}

# cd and then ls
function sd() {
  cd "$1" && ls;
}

if [[ $OSTYPE == darwin* ]]; then
  alias la='ls -A -G -F'
  alias ll='ls -A -l -G -F -h'
  alias ls='ls -G -F'
  alias ldd='otool -L'
  alias mkdir='mkdir -p -v'
elif [[ $OSTYPE == "linux-gnu" ]]; then
  alias la='ls -A -F --color=always'
  alias ll='ls -A -l -F -h --color=always'
  alias ls='ls -F --color=always'
  alias mkdir='mkdir -p -v'
elif [[ $OSTYPE == "cygwin" ]]; then
  alias la='ls -A -F --color=always'
  alias ll='ls -A -l -F -h --color=always'
  alias ls='ls -F --color=always'
  alias mkdir='mkdir -p -v'
elif [[ $OSTYPE == solaris* ]]; then
  alias la='ls -A -F'
  alias ll='ls -A -l -F'
  alias ls='ls -F'
fi
