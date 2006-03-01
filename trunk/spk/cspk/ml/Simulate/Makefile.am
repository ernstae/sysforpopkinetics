# automake input file
#
noinst_PROGRAMS = Slope Line
#
AM_CPPFLAGS = \
	-g \
	-c \
	-I/usr/local/include/spktest/ 
#
AM_LDFLAGS = -L/usr/local/lib/spktest 
#
LDADD = \
	-lQN01Box \
	-lCppAD \
	-lgsl \
	-llapack \
	-lcblas \
	-latlas \
	-lg2c 
#
Slope_SOURCES       = Slope.cpp
Line_SOURCES        = Line.cpp
