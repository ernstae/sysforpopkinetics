#!/bin/sh

# get-agent-data
#   
# Designed to be run by the desktop's startup script, after ssh-agent
# and ssh-add have been run, it copies several environment variables
# originally set up ssh-agent into the file ~/$DIR/data for subsequent
# use by the cronssh and the cronscp scripts.

DIR=agent-data
FILE=$DIR/data
cd $HOME

if ! test -d $DIR; then
    mkdir $DIR
fi

echo SSH_AGENT_PID=$SSH_AGENT_PID > $FILE
echo SSH_AUTH_SOCK=$SSH_AUTH_SOCK >> $FILE

exit 0

