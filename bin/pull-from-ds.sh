#!/bin/sh
echo "Pulling from $ds"
rsync -e ssh -a -z -L --delete-after --exclude '.*' --exclude 'html/intern/wiki/' $ds:/data/users/dthurn/www-git/ /Volumes/www/www
