# automake input file
#
noinst_PROGRAMS = monteDriver 
#
AM_CPPFLAGS = \
	-g \
	-c \
	-I.. \
	-I. \
	-I/usr/local/include/spktest/ 
#
AM_LDFLAGS = -L/usr/local/lib/spktest 
#
LDADD = \
	-lspkpred \
	-lspk \
	-lQN01Box \
	-lgsl \
	-llapack_atlas \
	-lcblas \
	-latlas \
	-lpthread \
	-lm \
	-lxerces-c 
#
monteDriver_SOURCES  = \
	AdaptIntegral.cpp \
	GridIntegral.cpp \
	Gsl2SpkError.cpp \
	MapBay.cpp \
	MapMonte.cpp \
	monteDriver.cpp \
	MontePopObj.cpp  \
	adapt.c \
	pow_ii.c
