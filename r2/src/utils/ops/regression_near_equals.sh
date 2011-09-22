#!/bin/bash

# take the good test and write to a temp file
# take the new test results and write to another temp file

GOODFILE=/tmp/regression_test_good_$$.txt
NEWFILE=/tmp/regression_test_new_$$.txt

sed -e 's|,| |g' -e 's|<[^>]*>\([ 0-9.e+-]*\)</[^>]*>|\n\1\n|g' -e 's|<[^>]*>.*</[^>]*>||g' $1  | sed -n -e 's|^ *$||' -e '/^[ 0-9.e+-][ 0-9.e+-]*$/p' -e 's|^-* ||g'  -e 's| -*$||g' -e 's| -* ||g' > $GOODFILE;

sed -e 's|,| |g' -e 's|<[^>]*>\([ 0-9.e+-]*\)</[^>]*>|\n\1\n|g' -e 's|<[^>]*>.*</[^>]*>||g' $2  | sed -n -e 's|^ *$||' -e '/^[ 0-9.e+-][ 0-9.e+-]*$/p' -e 's|^-* ||g'  -e 's| -*$||g' -e 's| -* ||g' > $NEWFILE;

/usr/local/bin/NearEqual $GOODFILE $NEWFILE $3 $4 $5
exit $?



