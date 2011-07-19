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
export EDITOR=emacsclient
export PAGER=emacspager.pl

# Helps emacsclient work properly
export DISPLAY=''

# Tells clojure where to find clojure.jar
export CLOJURE_EXT=~/.clojure

export LS_COLORS="di=36"
export LSCOLORS='Bxgxfxfxcxdxdxhbadbxbx'

# Remove duplicate path entries
export PATH=`echo $PATH | perl -aF: -ple'$_=join":",grep{!$o{$_}++}@F'`
export PYTHONPATH=`echo $PYTHONPATH | perl -aF: -ple'$_=join":",grep{!$o{$_}++}@F'`
