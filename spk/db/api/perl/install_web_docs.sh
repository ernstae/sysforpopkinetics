#!/bin/sh

pod2html Spkdb.pm > Spkdb.html
man -t blib/man3/* > Spkdb.ps
ps2pdf Spkdb.ps

scp Spkdb.html whitechuck:/var/www/html/soft/v0.1/specs/dbAPIperl/Spkdb.html
scp Spkdb.pdf  whitechuck:/var/www/html/soft/v0.1/specs/dbAPIperl/Spkdb.pdf

