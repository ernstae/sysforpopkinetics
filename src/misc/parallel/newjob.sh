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

if [ -n "$1" ] && [ "$1" = "-r" ];  # reset
then
    rm -rf $SPK_SHARE/working/spktest/*
    rm -f $SPK_SHARE/log/spktest/*
    rm -f $NEXT_JOB
fi

if [ -f $NEXT_JOB ];
then
    n=`cat $NEXT_JOB`
    let n=$n+1
fi
printf "%06d" $N > $NEXT_JOB
printf "%06d" $N
printf "\n"

#mkdir $SPK_SHARE/working/spk$MODE/spkjob-$1
#$SPK_SHARE/arch/bin/$ARCH/spk$MODE/spkjob $1 $2 $MODE


