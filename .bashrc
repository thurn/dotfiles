#
# Starter bashrc for a new system
#

source ~/.global/bashrc
source ~/.global/bash_aliases

# Load local aliases, if any exist
if [ -f ~/.bash_aliases ]; then
   source ~/.bash_aliases
fi

export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# Interactive-Only Logic

export TERM="xterm-color"
