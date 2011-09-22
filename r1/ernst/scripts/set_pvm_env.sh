#!/bin/bash

############################################################
# by Andrew Ernst (ernst@u.washington.edu)
#
# Run this as follows:
#    ./set_pvm_env.sh >>~/.bashrc
#
############################################################

echo "setenv
halt
" | pvm | grep -i "PVM_" | sed -e 's/rsh/ssh/g'
echo "PVM_TMP=/tmp"
echo "export PVM_TMP PVM_ROOT PVM_ARCH PVM_RSH"
