# ! /bin/bash
# From:   Resource Facility for Population Kinetics
#           Department of Bioengineering Box 352255
#           University of Washington, Seattle, WA 98195-2255
# 
# This file is part of the System for Population Kinetics (SPK), which
# was developed with support from NIH grants RR-12609 and P41-
# EB001975. Please cite these grants in any publication for which this
# software is used and send a notification to the address given above.
# 
# SPK is Copyright (C) 1998-2005, by the University of Washington,
# Resource Facility for Population Kinetics, and is made available as
# free open source software under the terms of the University of
# Washington Free-Fork License as a public service.  A copy of the
# License can be found in the COPYING file in the root directory of this
# distribution.
# -----------------------------------------------------------------------
# Software:   Brad Bell (brad@apl.washington.edu)
# Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)
#
#
if [ "$1" == "-g" ]
then
	echo "Building Debug Version:"
	compile_flags="-g -Wall"
else
	echo "Building Optimized Version:"
	compile_flags="-DNDEBUG -O2 -Wall"
fi
for file in aclocal.m4 configure QN01Box/config.h.in *.gz
do
	if [ -e $file ]
	then
		rm $file
	fi
done
#
Today=`date +%g-%m-%d`
sed configure.ac > configure.tmp -e \
	"s/(QN01Box, [0-9][0-9]-[0-9][0-9]-[0-9][0-9],/(QN01Box, $Today,/"
diff configure.ac  configure.tmp
mv   configure.tmp configure.ac
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
echo "	--prefix=/usr/local \\"
echo "	POSTFIX_DIR=spktest \\"
echo "	CPPAD_PREFIX_DIR=/usr/local \\"
echo "	CPPAD_POSTFIX_DIR=spktest \\"
echo "	COMPILE_FLAGS=\"$compile_flags\""
./configure \
	--prefix=/usr/local \
	POSTFIX_DIR=spktest \
	CPPAD_PREFIX_DIR=/usr/local \
	CPPAD_POSTFIX_DIR=spktest \
	COMPILE_FLAGS="$compile_flags"
#
echo "./RunOMhelp.sh"
./RunOMhelp.sh
#
echo "make"
make
#
echo "make dist"
make dist
