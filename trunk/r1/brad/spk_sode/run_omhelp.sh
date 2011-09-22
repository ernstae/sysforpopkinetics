#! /bin/bash
if [ -d htm ]
then
	rm -r htm
fi
mkdir htm
cd htm
omhelp ../spk_sode.omh -xml -noframe
