#
# Thor's zshrc
#

# Load local shell config, if any
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

source ~/.global/dotfile_import
source ~/.global/zshrc
source ~/.global/zsh_aliases

alias vim="echo 'no vim allowed'"

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# Interactive-Only Logic
if [[ $EMACS == "t" ]]; then
  echo "Emacs *shell*"!
else
  export TERM="xterm-color"
fi
