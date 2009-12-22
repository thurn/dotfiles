#
# Thor's zshrc
#

if which git > /dev/null;
  git pull origin master -q &> /dev/null
else
  ssh thurn.ca "cd /home/dthurn/public_html/thurn.ca/dotfiles && git pull" 
  if which dig > /dev/null;
  then
    if dig +time=1 +tries=1 +norecurse www.thurn.ca > /dev/null;
    then
      rsync -rtz thurn.ca::dthurndotfiles ~
    else
      date >> .no_internet
    fi  
  else
    rsync -rtz thurn.ca::dthurndotfiles ~
  fi
fi

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
