#
# Starter zshrc for a new system
#

source ~/.global/dotfile_import
source ~/.global/zshrc
source ~/.global/zsh_aliases

# Load local aliases, if any exist
if [ -f ~/.zsh_aliases ]; then
   source ~/.zsh_aliases
fi

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# Interactive-Only Logic
