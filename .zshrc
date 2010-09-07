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

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# A huge hack which fixes some programs (e.g. emacs)
export TERM=xterm-color
