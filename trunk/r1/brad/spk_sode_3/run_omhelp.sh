#! /bin/bash
if [ -d htm ]
then
	rm -rf htm
fi
mkdir htm
cd htm
omhelp ../spk_sode_sim.cpp -xml -debug
