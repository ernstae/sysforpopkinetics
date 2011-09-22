#!/bin/bash
# --------------------------------------------------------------------------
# Developers can change the following directories

if [ "$1" == "spk" ]
then
    echo "Building SPK Specific Install"
    install_dir=/usr/local
    mat2cpp_prefix=/usr/local
    qn01box_prefix=/usr/local
    postfix_dir="spktest"
    compile_flags="-DNDEBUG -O2 -Wall"
else
    boost_dir=/usr/include/boost-1_33_1
    mat2cpp_prefix=/usr/local
    qn01box_prefix=/usr/local
    postfix_dir=spktest
fi
# --------------------------------------------------------------------------
if [ "$2" == "-g" ]
then
	echo "Building Debug Version:"
	compile_flags="-g -Wall"
else
	echo "Building Optimized Version:"
	compile_flags="-DNDEBUG -O2 -Wall"
fi
if [ -e "non_par-$Today" ]
then
	rm -r -f non_par-$Today
fi
if [ -e non_par-$Today.tar.gz ]
then
	rm -f non_par-$Today.tar.gz
fi
for file in aclocal.m4 configure non_par/config.h.in *.gz
do
	if [ -e $file ]
	then
		rm $file
	fi
done
# 
Today=`date +%g-%m-%d`
sed configure.ac > configure.tmp -e \
	"s|(non_par, [0-9][0-9]-[0-9][0-9]-[0-9][0-9],|(non_par, $Today,|"
diff configure.ac  configure.tmp
mv   -f configure.tmp configure.ac
#
#
echo "aclocal"
aclocal
#
echo "autoheader"
autoheader
#
echo "autoconf"
autoconf
#
echo "autoheader"
autoheader
#
echo "automake --add-missing"
automake --add-missing
#
echo "./configure \\"
echo "	--prefix=$install_dir \\"
echo "	BOOST_DIR=$boost_dir \\"
echo "	MAT2CPP_PREFIX=$mat2cpp_prefix \\"
echo "	QN01BOX_PREFIX=$qn01box_prefix \\"
echo "	POSTFIX_DIR=$postfix_dir \\"
echo "	COMPILE_FLAGS=$compile_flags "
./configure \
	--prefix=$install_dir \
	BOOST_DIR=$boost_dir \
	MAT2CPP_PREFIX=$mat2cpp_prefix \
	QN01BOX_PREFIX=$qn01box_prefix \
	POSTFIX_DIR=$postfix_dir \
	COMPILE_FLAGS="$compile_flags"
#
# must build cpp/all_ok before running run_omhelp.sh
make
#
echo "./run_omhelp.sh"
./run_omhelp.sh
#
echo "make dist"
make dist
#
