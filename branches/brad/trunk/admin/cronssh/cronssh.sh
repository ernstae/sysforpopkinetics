#!/bin/sh
#
# cronssh host command
#
# This script can execute a command on a remote host without need
# for login if the following asumptions are met:
# 1. Ordinary user
# 2. Desktop running currently running for that user
# 3. Desktop ran ssh-agent at startup
# 4. Desktop ran ssh-add shortly thereafter
# 5. Desktop ran our script, get-agent-data, shortly after that
#
# Because no password is needed, this script is convenient for use
# in scripts run from cron.

USER=$(whoami)y
DIR=agent-data
PWDIR=$(pwd)

cd ~/$DIR

eval `cat data`

export SSH_AGENT_PID
export SSH_AUTH_SOCK

cd $PWDIR

ssh $1 $2 

exit $?



