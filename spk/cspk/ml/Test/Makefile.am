# automake input file
#
noinst_PROGRAMS = monteDriver 
#
AM_CPPFLAGS = \
	-g \
	-c \
	-I.. \
	-I. \
	-I/usr/local/include/spktest/ \
	-I/usr/share/pvm3/include
#
AM_LDFLAGS = -L/usr/local/lib/spktest \
             -L/usr/share/pvm3/lib/LINUX \
	     -L/usr/lib/atlas
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
	-lxerces-c \
	-lpvm3 \
	-lginac
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
