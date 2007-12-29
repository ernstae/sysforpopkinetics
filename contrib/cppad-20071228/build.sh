# ! /bin/bash
# -----------------------------------------------------------------------------
# CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-07 Bradley M. Bell
#
# CppAD is distributed under multiple licenses. This distribution is under
# the terms of the 
#                     Common Public License Version 1.0.
#
# A copy of this license is included in the COPYING file of this distribution.
# Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
# -----------------------------------------------------------------------------
#
# Bash script for building the CppAD distribution.
#
# Default values used for arguments to configure during this script.
# These defaults are development system dependent and can be changed.
BOOST_DIR=/usr/include/boost-1_33_1
ADOLC_DIR=$HOME/adolc_base
FADBAD_DIR=$HOME/include
SACADO_DIR=$HOME/sacado_base
# -----------------------------------------------------------------------------
#
# date currently in configure.ac
version=`grep "^ *AC_INIT(" configure.ac | \
	sed -e "s/.*, *\([0-9]\{8\}\) *,.*/\1/"`
#
if [ "$1" = "all" ] && [ "$2" != "" ] && [ "$2" != "test" ]
then
	echo "./build.sh $1 $2"
	echo "is not valid, build.sh with no arguments lists valid choices."
	exit 1
fi
#
# Check if we are running all the test cases. 
if [ "$1" = "test" ] || ( [ "$1" = "all" ] & [ "$2" = "test" ] )
then
	date > build_test.log
	if [ -e cppad-$version ]
	then
		rm -rf cppad-$version
	fi
fi
#
# version
#
if [ "$1" = "version" ] || [ "$1" = "all" ]
then
	echo "build.sh version"
	#
	# Today's date in yy-mm-dd decimal digit format where 
	# yy is year in century, mm is month in year, dd is day in month.
	yyyymmdd=`date +%G%m%d`
	yyyy_mm_dd=`date +%G-%m-%d`
	#
	# automatically change version for certain files
	sed < cppad.spec > cppad.spec.$$ \
        	-e "s/cppad-[0-9]\{8\}/cppad-$yyyymmdd/g" \
        	-e "s/cppad-devel-[0-9]\{8\}/cppad-devel-$yyyymmdd/g" \
        	-e "s/cppad-doc-[0-9]\{8\}/cppad-doc-$yyyymmdd/g" \
        	-e "s/^Version: *[0-9]\{8\}/Version: $yyyymmdd/"
	#
	sed < AUTHORS > AUTHORS.$$ \
		-e "s/, [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} *,/, $yyyy_mm_dd,/"
	sed < configure.ac > configure.ac.$$\
		-e "s/(CppAD, [0-9]\{8\} *,/(CppAD, $yyyymmdd,/" 
	sed < omh/install_unix.omh > omh/install_unix.omh.$$ \
		-e "s/cppad-[0-9]\{8\}/cppad-$yyyymmdd/g"
	sed < omh/install_windows.omh > omh/install_windows.omh.$$ \
		-e "s/cppad-[0-9]\{8\}/cppad-$yyyymmdd/g"
	sed < configure > configure.$$ \
		-e "s/CppAD [0-9]\{8\}/CppAD $yyyymmdd/g" \
		-e "s/VERSION='[0-9]\{8\}'/VERSION='$yyyymmdd'/g" \
		-e "s/configure [0-9]\{8\}/configure $yyyymmdd/g" \
		-e "s/config.status [0-9]\{8\}/config.status $yyyymmdd/g" \
		-e "s/\$as_me [0-9]\{8\}/\$as_me $yyyymmdd/g" 
	chmod +x configure.$$
	sed < cppad/config.h > cppad/config.h.$$ \
		-e "s/CppAD [0-9]\{8\}/CppAD $yyyymmdd/g" \
		-e "s/VERSION \"[0-9]\{8\}\"/VERSION \"$yyyymmdd\"/g"
	list="
		cppad.spec
		AUTHORS
		configure.ac
		omh/install_unix.omh
		omh/install_windows.omh
		configure
		cppad/config.h
	"
	for name in $list
	do
		echo "diff $name $name.$$"
		diff $name $name.$$
		echo "mv   $name.$$ $name"
		mv   $name.$$ $name
	done
	#
	# change Autoconf version to today
	version=$yyyymmdd
	#
	if [ "$1" = "version" ]
	then
		exit 0
	fi
fi
#
# omhelp
#
if [ "$1" = "omhelp" ] || [ "$1" = "all" ]
then
	echo "build.sh omhelp"
	#
	for user in doc dev
	do
		echo "run_omhelp.sh $user"
		if ! ./run_omhelp.sh $user
		then
			exit 1
		fi
		if [ ! -e omhelp_$user.log ]
		then
			echo "omhelp_$user.log file is missing"
			exit 1
		fi
		if grep 'OMhelp Warning:' omhelp_$user.log
		then
			echo "omhelp_$user.log has warnings in it"
			exit 1
		fi
	done
	#
	if [ "$1" = "omhelp" ]
	then
		exit 0
	fi
fi
#
# automake
#
if [ "$1" = "automake" ] || [ "$1" = "all" ]
then
	echo "build.sh automake"
	#
	# check that autoconf and automake output are in original version
	#
	makefile_in=`sed configure.ac \
        	-n \
        	-e '/END AC_CONFIG_FILES/,$d' \
        	-e '1,/AC_CONFIG_FILES/d' \
        	-e 's/makefile/&.in/' \
        	-e 's/^[ \t]*//' \
        	-e '/makefile/p'`
	auto_output="
		depcomp 
		install-sh 
		missing 
		configure 
		cppad/config.h 
		cppad/config.h.in 
		$makefile_in
	"
	for name in $auto_output
	do
		if [ ! -e $name ]
		then
			echo "$name is not in subversion repository"
			exit 1
		fi
	done
	#
	echo "---------------------------------------------------------"
	echo "If aclocal generates warning messages, run ./fix_aclocal.sh"
	echo "aclocal"
	if ! aclocal
	then
		exit 1
	fi
	echo "---------------------------------------------------------"
	#
	echo "autoheader"
	if ! autoheader
	then
		exit 1
	fi
	#
	echo "autoconf"
	if ! autoconf
	then
		exit 1
	fi
	#
	echo "automake --add-missing"
	if ! automake --add-missing
	then
		exit 1
	fi
	link_list="missing install-sh depcomp"
	for name in $link_list
	do
		if [ -h "$name" ]
		then
			echo "Converting $name from a link to a regular file"
			cp $name $name.$$
			if ! mv $name.$$ $name
			then
				echo "Cannot convert $name"
				exit 1
			fi
		fi
	done
	#
	if [ "$1" = "automake" ]
	then
		exit 0
	fi
fi
#
# configure
#
if [ "$1" = "configure" ] || [ "$1" = "all" ]
then
	if [ "$2" = "test" ]
	then
		echo "build.sh configure test"
	else
		echo "build.sh configure"
	fi
	#
	TEST=""
	if [ "$1" = "configure" ] && [ "$2" = "test" ]
	then
		TEST="
			--with-Introduction
			--with-Example
			--with-TestMore
			--with-Speed
			--with-PrintFor"
		if [ -e doc/index.htm ]
		then
			TEST="$TEST
				--with-Documentation"
		fi
		if [ -e $BOOST_DIR/boost ]
		then
			TEST="$TEST 
				BOOST_DIR=$BOOST_DIR"
		fi
		if [ -e $ADOLC_DIR/include/adolc ]
		then
			TEST="$TEST 
				ADOLC_DIR=$ADOLC_DIR"
		fi
		if [ -e $FADBAD_DIR/FADBAD++ ]
		then
			TEST="$TEST 
				FADBAD_DIR=$FADBAD_DIR"
		fi
		if [ -e $SACADO_DIR/include/Sacado.hpp ]
		then
			TEST="$TEST 
				SACADO_DIR=$SACADO_DIR"
		fi
	fi
	TEST=`echo $TEST | sed -e 's|\t\t*| |g'`
	#
	echo "configure \\"
	echo "$TEST" | sed -e 's| | \\\n\t|g' -e 's|$| \\|' -e 's|^|\t|'
	echo "	CXX_FLAGS=\"-Wall -ansi -pedantic-errors -std=c++98\""
	#
	if ! ./configure $TEST \
		CXX_FLAGS="-Wall -ansi -pedantic-errors -std=c++98"
	then
		exit 1
	fi
	#
	# Fix makefile for what appears to be a bug in gzip under cygwin
	echo "fix_makefile.sh"
	./fix_makefile.sh
	#
	if [ "$1" = "configure" ]
	then
		exit 0
	fi
fi
#
# make
#
if [ "$1" = "make" ] || [ "$1" = "all" ]
then
	echo "build.sh make"
	#
	echo "make"
	if ! make
	then
		exit 1
	fi
	#
	if [ "$1" = "make" ]
	then
		exit 0
	fi
fi
#
# dist
#
if [ "$1" = "dist" ] || [ "$1" = "all" ]
then
	echo "build.sh dist"
	#
	if [ -e cppad-$version ]
	then
		echo "rm -f -r cppad-$version"
		if ! rm -f -r cppad-$version
		then
			echo "Build: cannot remove old cppad-$version"
			exit 1
		fi
	fi
	for file in cppad-*.tgz cppad-*.zip
	do
		if [ -e $file ]
		then
			echo "rm $file"
			rm $file
		fi
	done
	#
	echo "make dist"
	if ! make dist
	then
		exit 1
	fi
	#
	if [ ! -e cppad-$version.tar.gz ]
	then
		echo "cppad-$version.tar.gz does not exist"
		echo "perhaps version is out of date"
		#
		exit 1
	fi
	# change *.tgz to *.cpl.tgz
	if ! mv cppad-$version.tar.gz cppad-$version.cpl.tgz
	then
		echo "cannot move cppad-$version.tar.gz to cppad-$version.tgz"
		exit 1
	fi
	#
	#
	if [ "$1" = "dist" ]
	then
		exit 0
	fi
fi
if [ "$1" = "test" ] || ( [ "$1" = "all" ] && [ "$2" = "test" ] )
then
	#
	if [ -e cppad-$version ]
	then
		echo "rm -f -r cppad-$version"
		if ! rm -f -r cppad-$version
		then
			echo "Build: cannot remove old cppad-$version"
			exit 1
		fi
	fi
	#
	if [ -e "cppad-$version.cpl.tgz" ]
	then
		dir="."
	else
		if [ -e "doc/cppad-$version.cpl.tgz" ]
		then
			dir="doc"
		else
			echo "cannot find cppad-$version.cpl.tgz"
			exit 1
		fi
	fi
	#
	# check include files
	if ! ./check_include_def.sh  >> build_test.log
	then
		echo "./check_include_def.sh failed"
		exit 1
	fi
	if ! ./check_include_file.sh >> build_test.log
	then
		echo "./check_include_file.sh failed"
		exit 1
	fi
	if ! ./check_include_omh.sh  >> build_test.log
	then
		echo "./check_include_omh.sh failed"
		exit 1
	fi
	#
	# add a new line after last include file check
	echo ""                 >> build_test.log
	#
	echo "tar -xzf $dir/cppad-$version.cpl.tgz"
	if ! tar -xzf $dir/cppad-$version.cpl.tgz
	then
		exit 1
	fi
	#
	cd cppad-$version
	if ! ./build.sh configure test
	then
		echo "Error: build.sh configure test"  >> ../build_test.log
		echo "Error: build.sh configure test" 
		exit 1
	fi
	# gcc 3.4.4 with optimization generates incorrect warning; see 
	# 	http://cygwin.com/ml/cygwin-apps/2005-06/msg00161.html
	# The sed commands below are intended to remove them.
	if ! make            2>  make_error.log
	then
		sed -e '/stl_uninitialized.h:/d' make_error.log
		exit 1
	fi
	sed -e '/stl_uninitialized.h:/d' make_error.log >> ../build_test.log
	#
	list="
		example/example
		introduction/exp_apx/exp_apx
		introduction/get_started/get_started
		test_more/test_more
	"
	for program in $list
	do
		echo "running $program"
		echo "$program"   >> ../build_test.log
		if ! ./$program   >> ../build_test.log
		then
			failed="$program"
			echo "Error: $failed failed."
			echo "Error: $failed failed." >> ../build_test.log
			exit 1
		fi
		# add a new line between program outputs
		echo ""  >> ../build_test.log
	done
	list="
		adolc
		cppad
		double
		example
		fadbad
		profile
		sacado
	"
	seed="123"
	for name in $list
	do
		# Note that example does not use command line arguments,
		# but it does not currently care about their presence.
		echo "running speed/$name/$name correct $seed"
		echo "./speed/$name/$name correct $seed" >> ../build_test.log
		if ! ./speed/$name/$name correct  $seed  >> ../build_test.log
		then
			failed="speed/$name/$name"
			echo "Error: $failed failed."
			echo "Error: $failed failed." >> ../build_test.log
			exit 1
		fi
		# add a new line between program outputs
		echo ""  >> ../build_test.log
	done
	echo "openmp/run.sh"
	echo "openmp/run.sh" >> ../build_test.log
	if !  openmp/run.sh >> ../build_test.log
	then
		failed="openmp/run.sh $program"
		echo "Error: $failed failed."
		echo "Error: $failed failed." >> ../build_test.log
		exit 1
	fi
	echo "" >> ../build_test.log
	#
	if ! ./run_omhelp.sh doc
	then
		failed="run_omhelp.sh"
		echo "Error: $failed failed."
		echo "Error: $failed failed." >> ../build_test.log
		exit 1
	fi
	cat omhelp_doc.log        >> ../build_test.log
	#
	cd ..
	if [ "$1" = "test" ]
	then
		# end the build_test.log file with the date and time
		date >> build_test.log
		#
		dir=`pwd`
		echo "Check $dir/build_test.log for errors and warnings."
		exit 0
	fi
fi
if [ "$1" = "gpl+dos" ] || [ "$1" = "all" ]
then
	# create GPL licensed version
	echo "gpl_license.sh"
	if ! ./gpl_license.sh
	then
		echo "Error: gpl_license.sh failed."
		if [ "$2" = "test" ]
		then
			echo "Error: gpl_license.sh failed." >> build_test.log
		fi
		exit 1
	else
		echo "Ok: gpl_license.sh."
		if [ "$2" = "test" ]
		then
			echo "Ok: gpl_license.sh." >> build_test.log
		fi
	fi
	echo "./dos_format.sh"
	if ! ./dos_format.sh
	then
		echo "Error: dos_format.sh failed."
		if [ "$2" = "test" ]
		then
			echo "Error: dos_format.sh failed." >> build_test.log
		fi
		exit 1
	else
		echo "Ok: dos_format.sh."
		if [ "$2" = "test" ]
		then
			echo "Ok: dos_format.sh." >> build_test.log
		fi
	fi
	#
	if [ "$1" = "gpl+dos" ]
	then
		exit 0
	fi
fi
if [ "$1" = "move" ] || [ "$1" = "all" ] 
then
	# copy tarballs into doc directory
	list="
		cppad-$version.cpl.tgz
		cppad-$version.gpl.tgz
		cppad-$version.cpl.zip
		cppad-$version.gpl.zip
	"
	for file in $list
	do
		echo "mv $file doc/$file"
		if ! mv $file doc/$file
		then
			echo "Error: mv $file doc."
			if [ "$2" = "test" ]
			then
				echo "Error: mv $file doc." >> build_test.log
			fi
			exit 1
		fi
	done
	if [ "$1" = "move" ]
	then
		exit 0
	fi
fi
if [ "$1" = "all" ]
then
	if [ "$2" = "test" ]
	then
		# end the build_test.log file with the date and time
		date >> build_test.log
	fi
	exit 0
fi
#
if [ "$1" = "" ]
then
	echo "usage: build.sh option (where valid options are listed below)" 
else
	echo "$1 is not a valid option (valid options are listed below)"
fi
echo "option"
echo "------"
echo "version        update configure.ac and doc.omh version number"
echo "omhelp         build all the documentation in doc & dev directories"
echo "automake       run aclocal,autoheader,autoconf,automake -> configure"
echo "configure      excludes --with-*"
echo "configure test includes all the possible options except PREFIX_DIR"
echo "make           use make to build all of the requested targets"
echo "dist           create the distribution file cppad-version.cpl.tgz"
echo "test           unpack *.cpl.tgz, compile, tests, result in build_test.log"
echo "gpl+dos        create ./*.gpl.tgz, ./*.gpl.zip, and ./*.cpl.zip"
echo "move           move ./*.tgz and ./*.zip to doc directory"
echo
echo "build.sh all"
echo "This command will execute all the options in the order above with the"
echo "exception that \"configue test\" and \"test\" will be excluded."
echo
echo "build.sh all test"
echo "This command will execute all the options above in the order"
echo "with the exception of \"configure test\"  and \"move\"."
#
exit 1
