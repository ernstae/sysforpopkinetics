# !/bin/bash
if [ -e doc ] 
then
	rm -r doc
fi
if [ ! -e cpp/bender_ok.out ]
then
	cd cpp
	./all_ok
	cd ..
fi
mkdir doc
cd doc
omhelp ../non_par.omh -noframe -debug
omhelp ../non_par.omh -noframe -xml -debug
omhelp ../non_par.omh -noframe -xml -debug -printable
cd ..
#
cd matlab
if [ -e doc ] 
then
	rm -r doc
fi
mkdir doc
cd doc
omhelp ../nonpar.omh -noframe -debug
omhelp ../nonpar.omh -noframe -debug -xml
omhelp ../nonpar.omh -noframe -debug -xml -printable
cd ../..
