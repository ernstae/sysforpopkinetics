#! /bin/bash
#
if [ "$1" != "from" ] \
&& [ "$1" != "to" ] \
&& [ "$1" != "diff" ] \
&& [ "$1" != "revert" ]
then
	echo "copy.sh (from|to|diff|revert)"
	exit
fi
comment="Clean up temporary directory."
extra=" 
	r2/trunk/spk/cspk/ml/Makefile
	r2/trunk/spk/cspk/ml/gsl-1.10-miser.c
"
list="
$extra
	r1/trunk/brad/non_par/non_par/opt_measure.hpp
	r1/trunk/brad/non_par/cpp/opt_measure_ok.cpp
	r1/trunk/brad/non_par/build.sh
	r1/trunk/brad/non_par/Makefile.am
	r1/trunk/Optimizer/QN01Box/Test/Makefile.am 
	r1/trunk/Optimizer/QN01Box/Test/RunTest.cpp
	r1/trunk/Optimizer/QN01Box/Test/DanWood.cpp
	r1/trunk/Optimizer/QN01Box/QN01Box/QuasiNewton01Box.h
	r1/trunk/Optimizer/QN01Box/lib/is_symmetric.cpp
	r2/trunk/spk/cspk/spk/test/UnitTests/src/quasiNewtonAnyBoxTest.cpp
	r2/trunk/spk/cspk/spk/spk/quasiNewtonAnyBox.cpp
	r2/trunk/spk/cspk/spk/test/UnitTests/src/firstOrderOptTest.cpp
	r2/trunk/spk/cspk/spk/test/UnitTests/src/spk_non_par_test.cpp
	r2/trunk/spk/cspk/spkpred/test/unit/src/IndPredModelBaseTest.cpp
	r2/trunk/spk/aspk/spkcompiler/tests/nonmem/pop_fixedParaTest.cpp
	r2/trunk/spk/aspk/spkcompiler/spkcompiler/nonmem/NM_generateMakefile.cpp
"
if [ "$1" = "from" ]
then
	for global_name in $list 
	do
		local_name=`echo $global_name | sed -e 's|/|%|g'`
		echo "cp $HOME/$global_name \\"
		echo "	$local_name"
		if ! cp $HOME/$global_name $local_name
		then
			echo "cannot complete copy"
			exit 1
		fi
	done
fi
if [ "$1" = "to" ]
then
	for global_name in $list 
	do
		local_name=`echo $global_name | sed -e 's|/|%|g'`
		echo "cp $local_name \\"
		echo "$HOME/$global_name"
		if ! cp $local_name $HOME/$global_name 
		then
			echo "cannot complete copy"
			exit 1
		fi
	done
fi
if [ "$1" = "diff" ]
then
	for global_name in $list 
	do
		local_name=`echo $global_name | sed -e 's|/|%|g'`
		echo "diff $local_name \\"
		echo "$HOME/$global_name"
		diff $local_name $HOME/$global_name  | dos2unix
	done
fi
if [ "$1" = "revert" ]
then
	for global_name in $list
	do
		svn revert $HOME/$global_name
	done
fi
