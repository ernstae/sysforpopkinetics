#!/bin/sh

# backup-home-full
#
# This script will backup up the home directory of a user on a 
# workstation to the directory $BACKDIR in that user's home
# directory on $RMTHOST. 
#
# The following assumptions are made:
# 1. The script is run by an ordinary user.  If it is scheduled by
#    cron, it must not be run by root.  In other words, use the
#    crontab -e  command to schedule this script.
# 2. The cronssh and cronscp scripts must be operational with respect
#    to the given $RMTHOST.

BACKDIR=backup
BIN=$HOME/bin/shell
MSGFILE=$OUTNAME.txt
OUTNAME=$(date +%Y-%m-%d-%H%M).home.full
OUTFILE=$OUTNAME.tgz
RMTHOST=whitechuck
TMP=/tmp
USER=$(whoami)



ERROR_TAR=1
ERROR_SCP=2

cd $TMP
rm *.home.full.*

if ! tar czf $OUTFILE $HOME > $MSGFILE 2>&1; then
    echo "$0 error: tar failed" | mail -s "Backup Failed" $USER
    exit $ERROR_TAR
fi
if ! $BIN/cronssh $RMTHOST "test -d $BACKDIR" > $MSGFILE 2>&1; then
    $BIN/cronssh $RMTHOST "mkdir $BACKDIR"
fi
if ! $BIN/cronscp $OUTFILE $RMTHOST:$BACKDIR/$OUTFILE > $MSGFILE 2>&1; then
    echo "$0 error: scp failed" | mail -s "Backup Failed" $USER
    exit $ERROR_SCP
fi

exit 0
