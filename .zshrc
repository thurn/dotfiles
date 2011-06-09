#!/bin/zsh
# dthurn's zshrc

# Local config
if [ -f ~/.shell_config ]; then
   source ~/.shell_config
fi

wd=$(pwd)
home=~

WHO=`who am i | sed -e 's/ .*//'`
ID_WHO=`id -u $WHO`
ID=`id -u`

if [[ $wd == $home ]] # Only run from home directory
then
  if [[ "$ID" = "$ID_WHO" ]] # Do not run if SUed
  then
    if type git &> /dev/null;
    then
      nohup git pull origin master > /dev/null 2> /dev/null &
    else # Without git, ssh into thurn.ca, run a git pull, then rsync
      echo "No git!"
    fi
  fi
fi

ZSH=$HOME/zsh
for config_file ($ZSH/*.zsh) source $config_file
