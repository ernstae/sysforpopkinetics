#! /bin/bash
#
include_dir=/usr/local/include/spktest
lib_dir=/usr/local/lib/spktest
atlas_dir=/usr/lib/atlas
#
echo "g++ -g \\"
echo "	spk_sode_sim.cpp    \\"
echo "	spk_sode_model.cpp  \\"
echo "	-I$include_dir      \\" 
echo "	-L$lib_dir          \\" 
echo "	-lspk               \\"
echo "	-lQN01Box           \\"
echo "	-lgsl               \\"
echo "	-llapack            \\"
echo "	-llapack_atlas      \\"
echo "	-lcblas             \\"
echo "	-latlas             \\"
echo "	-lxerces-c          \\"
echo "	-o spk_sode_sim"
#
g++ -g \
	spk_sode_sim.cpp    \
	spk_sode_model.cpp  \
	-I$include_dir      \
	-L$lib_dir          \
	-L$atlas_dir        \
	-lspk               \
	-lQN01Box           \
	-lgsl               \
	-llapack            \
	-llapack_atlas      \
	-lcblas             \
	-latlas             \
	-lxerces-c          \
	-o spk_sode_sim

