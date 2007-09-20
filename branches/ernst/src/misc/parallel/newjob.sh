#!/bin/bash

TMP=/tmp
NEXT_JOB=$TMP/demo_next_job
let n=1

if [ -z $SPK_SHARE ];
then
    SPK_SHARE=/usr/local/spk/share
fi
ARCH=LINUXX86_64   # the architecture of the head node
MODE=test

if [ -z "$1" ];
then
    echo "usage: newjob.sh (-r | individual_count)"
    exit 1
fi

if [ "$1" = "-r" ];  # reset
then
    rm -rf $SPK_SHARE/working/spktest/*
    rm -f $SPK_SHARE/log/spktest/*
    rm -f $NEXT_JOB
    exit 0
fi

if [ -f $NEXT_JOB ];
then
    n=`cat $NEXT_JOB`
    let n=$n+1
fi
printf "%d" $n > $NEXT_JOB

mkdir -m 2775 $SPK_SHARE/working/spk$MODE/spkjob-$n

$SPK_SHARE/arch/bin/$ARCH/spk$MODE/spkjob $n $1 $MODE

exit 0

