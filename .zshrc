#
# Thor's zshrc
#

git pull -q origin master > /dev/null

source ~/.global/zshrc
source ~/.global/zsh_aliases

# Load local shell config, if any
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

# Interactive-Only Logic
export TERM="xterm-color"
