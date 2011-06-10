#!/bin/zsh
# dthurn's zshrc

# Local config
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

# Are we running in emacs?
emacs_shell=false
emacs_term=false
if (set -u; : $EMACS) 2> /dev/null
then
  if [[ $EMACS == "t" ]]
  then
    emacs_shell=true
  else
    emacs_term=true
  fi
fi

wd=$(pwd)
home=~

WHO=`who am i | sed -e 's/ .*//'`
ID_WHO=`id -u $WHO`
ID=`id -u`

# Only run from home directory, if not su-ed, if git is available
if [[ $wd == $home ]] && [[ $ID = $ID_WHO ]] && type git &> /dev/null && ! $emacs_shell && ! $emacs_term;
then
  nohup git pull origin master > /dev/null &
else
  echo "No git!"
fi

ZSH=$HOME/zsh
for config_file ($ZSH/*.zsh) source $config_file
