#!/bin/sh

# To set up a directory for the development of a new HOWTO document,
# execute this command from the r2/doc/howto directory in you 
# CVS sandbox:
#
#     ./new-howto.sh SUBJECT
#
# The following components are created, where SUBJECT is the parameter
# to this script.
#
# A directory named 
#    SUBJECT-rfpk-HOWTO
# The following files within SUBJECT-rfpk.HOWTO
#        Makefile
#        SUBJECT-rfpk-HOWTO.xml
#

if [ $# -eq 0 ] || [ $1 == -h ] || [ $1 == --help ]; then
    echo "usage: $0 SUBJECT"
    echo -e "\tSUBJECT is the subject of your howto"
    echo -e "\texample: CLOCK-SYNC"
    exit 1
fi

SUBJECT=$1
NAME=$SUBJECT-rfpk-HOWTO

mkdir $NAME
cp template/Makefile $NAME/makefile
cp template/SUBJECT-rfpk-HOWTO.xml $NAME/$NAME.xml
cd $NAME
sed  "s/SUBJECT/$SUBJECT/" < makefile > Makefile
rm makefile

exit 0
