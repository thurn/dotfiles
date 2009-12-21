#
# Starter zshrc for a new system
#

git pull origin master -q &> /dev/null

source ~/.global/zshrc
source ~/.global/zsh_aliases

# Load local aliases, if any exist
if [ -f ~/.zsh_aliases ]; then
   source ~/.zsh_aliases
fi

# Stop exectuting here if the shell is non-interactive
[ -z "$PS1" ] && return

export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# Interactive-Only Logic
