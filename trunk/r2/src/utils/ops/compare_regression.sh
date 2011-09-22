#!/bin/bash

# takes job_id as $1
# takes skip as $2

NEWFILE=/tmp/spkruntest-job-${1}/result.xml
VALIDFILE=/usr/local/spk/ops/regression_test/srun/spkruntest-job-${1}/result.xml

for file in $NEWFILE $VALIDFILE; do
    if [ ! -f "$file" ] 
	then
	echo "Could not find $file: $!";
	exit 1;
    fi
done

regression_near_equals.sh $VALIDFILE $NEWFILE 1e-3 1e-4 $2

