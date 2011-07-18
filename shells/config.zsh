#!/usr/bin/env zsh
# -*- mode: zsh; -*-

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

# Tell emacs *term* where I am
if $emacs_term ;
then
  function precmd {
    echo -e "\033AnSiTu" "$LOGNAME"
    echo -e "\033AnSiTc" "$(pwd)"
    echo -e "\033AnSiTh" "$(hostname)" 
  }
else
  function precmd { }
fi

# Override $TERM aspirationally
export TERM=xterm-256color

# Initialize colors.
autoload -U colors
colors

# Allow for functions in the prompt.
setopt PROMPT_SUBST

# Set the prompt.

# %U starts underline, %u stops it
# %{$fg[color]%} starts using a color
# %~ is pwd
# %f stops using a color
# %# is % for non-root shells and # for root shells
PROMPT='%U$fg[green]%~%u $fg[red]%n@%M
$fg[cyan]%# $reset_color'
