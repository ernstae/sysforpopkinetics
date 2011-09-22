#! /bin/bash
#
include_dir=/usr/local/include/spktest
lib_dir=/usr/local/lib/spktest
atlas_dir=/usr/lib/atlas
#
program="spk_sode_sim"
#
echo "g++ -g \\"
echo "	spk_sode_model.cpp  \\"
echo "	$program.cpp        \\"
echo "	-I$include_dir      \\" 
echo "	-L$lib_dir          \\" 
echo "	-lspk               \\"
echo "	-lginac             \\"
echo "	-lQN01Box           \\"
echo "	-lgsl               \\"
echo "	-llapack            \\"
echo "	-llapack_atlas      \\"
echo "	-lcblas             \\"
echo "	-latlas             \\"
echo "	-lxerces-c          \\"
echo "	-o $program"
#
g++ -g \
	$program.cpp        \
	-I$include_dir      \
	-L$lib_dir          \
	-L$atlas_dir        \
	-lspk               \
	-lginac             \
	-lQN01Box           \
	-lgsl               \
	-llapack            \
	-llapack_atlas      \
	-lcblas             \
	-latlas             \
	-lxerces-c          \
	-o $program
#
echo "./$program"
./$program
