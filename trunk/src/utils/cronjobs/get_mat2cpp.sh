#!/bin/bash

SERVER=https://svn.apl.washington.edu/packages
FILENAME=mat2cpp-`/bin/date "+%y-%m-%d"`.tar.gz
DIRNAME=mat2cpp-`/bin/date "+%y-%m-%d"`
DROPZONE=/u01/www/toronto.rfpk.washington.edu/doc
#FILENAME=mat2cpp-06-05-15.tar.gz
#DIRNAME=mat2cpp-06-05-15

wget -O /tmp/$FILENAME $SERVER/$FILENAME;

if [ -e "/tmp/$FILENAME" ] && [ -s "/tmp/$FILENAME" ]; then
    cd $DROPZONE; 
    rm -rfv $DROPZONE/mat2cpp*;
    /bin/tar xvfz /tmp/$FILENAME;
    mv $DIRNAME/doc mat2cpp
    mv /tmp/$FILENAME $DROPZONE/mat2cpp
    ln -s $DROPZONE/mat2cpp/mat2cpp.htm $DROPZONE/mat2cpp/index.html
    rm -rfv $DIRNAME
else
    echo "File not available";
fi


