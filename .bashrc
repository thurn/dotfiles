#
# dthurn's bashrc
#

# Load local shell config, if any
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

source ~/.global/dotfile_import
source ~/.global/bashrc
source ~/.global/shell_aliases

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# Interactive-Only Logic

# Crazy hack!
export TERM="xterm-color"
