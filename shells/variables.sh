#!/usr/bin/env zsh
# -*- mode: sh; -*-

# set history size
export HISTSIZE=50000
export HISTFILESIZE=50000
export HISTCONTROL=ignoredups

# Set up less to read other file formats
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Address to push to for dotfiles updates
export dotfiles="git@github.com:thurn/dotfiles.git"

export SVN_EDITOR=emacsclient
export EDITOR=vim
export PAGER=less

# Helps emacsclient work properly
export DISPLAY=''

# Tells clojure where to find clojure.jar
export CLOJURE_EXT=~/.clojure

#export LS_COLORS="di=36"
#export LSCOLORS='Bxgxfxfxcxdxdxhbadbxbx'

# Remove duplicate path entries
export PATH=`echo $PATH | perl -aF: -ple'$_=join":",grep{!$o{$_}++}@F'`
export PYTHONPATH=`echo $PYTHONPATH | perl -aF: -ple'$_=join":",grep{!$o{$_}++}@F'`

export LSCOLORS='Fxcxcxdxbxegedabagacad'
export LS_COLORS='di=1;;40:ln=32;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'

export COLUMNS=80
