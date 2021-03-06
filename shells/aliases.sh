#!/usr/bin/env sh
# -*- mode: sh; -*-

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

# Set up "pie" as sed-like perl
alias pie='perl -p -i -e'

# Use trsh.pl instead of rm
alias rm='~/bin/trsh.pl'

# Github's cert is wrong, and I've never cared.
alias wget='wget --no-check-certificate'

# Use emacs in daemon mode by default
# alias emacs='emacs_wrapper.rb'
alias kill-emacs='emacsclient -e "(kill-emacs)"'

alias gl="git log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"

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
  alias la='ls -w 76 -A -F --color=always'
  alias ll='ls -w 76 -A -l -F -h --color=always'
  alias ls='ls -w 76 -F --color=always'
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

alias mt="make test"

alias cd1="cd .."
alias cd2="cd ../.."
alias cd3="cd ../../.."
alias cd4="cd ../../../.."
alias cd5="cd ../../../../.."
alias cd6="cd ../../../../../.."
alias cd7="cd ../../../../../../.."
alias cd8="cd ../../../../../../../.."
alias cd9="cd ../../../../../../../../.."

alias serve="python -m SimpleHTTPServer"
