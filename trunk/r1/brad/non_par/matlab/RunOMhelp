# !/bin/bash
if [ -e doc ] 
then
	rm -r doc
fi
mkdir doc
cd doc
omhelp ../nonpar.omh -debug
omhelp ../nonpar.omh -debug -xml
omhelp ../nonpar.omh -debug -xml -printable
cd ..
