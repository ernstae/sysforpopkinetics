#!/bin/bash
#
if [ -e htm ]
then
	rm -r htm
fi
mkdir htm
cd htm
omhelp ../markov_chain.omh -noframe -debug -xml
