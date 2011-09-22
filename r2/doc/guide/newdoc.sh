#!/bin/sh

# To set up a directory for the development of a new HOWTO document,
# execute this command from the r2/doc/howto directory in your 
# CVS sandbox:
#
#     sh newdoc.sh SUBJECT
#
# The following components are created, where SUBJECT is the parameter
# to this script.
#
# A directory named 
#    SUBJECT
# The following files within SUBJECT
#        Makefile
#        SUBJECT.xml
#

if [ $# -eq 0 ] || [ $1 == -h ] || [ $1 == --help ]; then
    echo "usage: $0 SUBJECT"
    echo -e "\tSUBJECT is the subject of your howto"
    echo -e "\texample: CLOCK-SYNC"
    exit 1
fi

SUBJECT=$1

source ../webdir.sh

mkdir $SUBJECT
cp template/Makefile $SUBJECT/makefile
cp template/SUBJECT.xml $SUBJECT/x.xml
cd $SUBJECT
sed  "s/SUBJECT/$SUBJECT/" < makefile > Makefile
sed  "s/Subject/$SUBJECT/" < x.xml > $SUBJECT.xml
rm makefile x.xml

ssh $WEBHOST "mkdir $WEBDIR/guide/$SUBJECT > /dev/null 2>&1"

exit 0
