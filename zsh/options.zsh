#History File Size
export HISTSIZE=10000
export SAVEHIST=10000
#History File Location
export HISTFILE=~/.zsh_history
# Append to history intead of overwriting
setopt append_history
# Append to history after commands, instead of at shell exit
setopt inc_append_history
# Save timestamps for commands
setopt extended_history
# Don't display duplicates while searching
setopt hist_find_no_dups
# Remove older commands if they duplicate newer comamnds
setopt hist_ignore_all_dups
# Remove blanks from commands as they are added
setopt hist_reduce_blanks
# Remove commands with leading whitespace
setopt hist_ignore_space
# Remove the history command from the history list
setopt hist_no_store
# Remove function definitions from history
setopt hist_no_functions
# Omit older duplicates when writing history
setopt hist_save_no_dups
#Removed duplicate items from history first
setopt hist_expire_dups_first

# prevent overwriting files with cat:
set -o noclobber
# stops ctrl+d from logging me out:
set -o ignoreeof           
# Emacs keybindings
set -o emacs

