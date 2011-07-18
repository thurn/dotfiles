#!/bin/bash
# dthurn's bashrc

# Local config
if [ -f ~/.shell_config.sh ]; then
   source ~/.shell_config.sh
fi

wd=$(pwd)
home=~

WHO=`who am i | sed -e 's/ .*//'`
ID_WHO=`id -u $WHO`
ID=`id -u`

# Only run from home directory, if not su-ed, if git is available
if [[ $wd == $home ]] && [[ $ID = $ID_WHO ]] && type git &> /dev/null;
then
  nohup git pull origin master > /dev/null &
else
  echo "No git!"
fi

for config_file in ~/shells/*.bash
do
  source $config_file
done

for config_file in ~/shells/*.sh
do
  source $config_file
done
