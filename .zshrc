#
# dthurn's zshrc
#

# Load local shell config, if any
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

source ~/.global/dotfile_import
source ~/.global/zshrc
source ~/.global/shell_aliases

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return
