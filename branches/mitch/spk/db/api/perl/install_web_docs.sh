#!/bin/sh

source ../../../../../../doc/webdir.sh

pod2html Spkdb.pm > Spkdb.html
man -t blib/man3/* > Spkdb.ps
ps2pdf Spkdb.ps


scp Spkdb.html $WEBHOST:$WEBDIR/v0.1/specs/dbAPIperl/Spkdb.html
scp Spkdb.pdf  $WEBHOST:$WEBDIR/v0.1/specs/dbAPIperl/Spkdb.pdf

