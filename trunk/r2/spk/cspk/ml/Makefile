# This makefile is counting on the files 
#
CPPFLAGS = \
	-g \
	-c \
        -I/usr/local/include/spktest

INSTALL_DIR = \
	/usr/local/src/spktest/ml

install : \
	adapt.h \
	AdaptIntegral.h \
	f2c.h \
	GridIntegral.h \
        Gsl2SpkError.h \
	MapBay.h \
	MapMonte.h \
	MontePopObj.h \
	\
	gsl-1.10-miser.c \
	\
	AdaptIntegral.cpp \
	adapt.c \
	GridIntegral.cpp \
	Gsl2SpkError.cpp \
	MapBay.cpp \
	MapMonte.cpp \
	monteDriver.cpp \
        monteAlpDriver.cpp \
	MontePopObj.cpp \
	pow_ii.c

	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)
	cp $? $(INSTALL_DIR)
