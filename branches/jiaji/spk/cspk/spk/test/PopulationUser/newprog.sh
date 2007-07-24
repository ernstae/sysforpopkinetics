#!/bin/sh

# To set up the directory an file structure for a new program
# use the following command
#
#    ./newprog name
#
# where name is the execution name of the program.

if [ $# -eq 0 ] || [ $1 == -h ] || [ $1 == --help ]; then
    echo "usage: $0 name"
    echo -e "\tname is the execution name of the program"
    echo -e "\texample: ./newprog deq1"
    exit 1
fi

name=$1

mkdir $1
cp template/AUTHORS $name
cp template/autogen.sh $name
cp template/ChangeLog $name
cp template/config.h.in $name
cp template/COPYING $name
cp template/Makefile.am $name
cp template/NEWS $name
cp template/README $name


sed  "s/theprogramname/$name/" < template/configure.ac > $name/configure.ac

mkdir $name/src
cd $name/src

cp ../../../../../../../../notices/notice.cpp $name.cpp
echo "int main() {}" >> $name.cpp

echo "bin_PROGRAMS = $name"      > Makefile.am
echo "${name}_SOURCES = \\"       >> Makefile.am
echo "    $name.cpp \\"         >> Makefile.am
echo "    ../config.h \\"       >> Makefile.am
echo "${name}_LDFLAGS = -lspk"    >> Makefile.am


exit 0 
