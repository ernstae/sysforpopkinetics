# This makefile is counting on the files 
#
CPPFLAGS = \
	-g \
	-c \
        -I../spk/spk

INSTALL_DIR = \
	/usr/local/spk/src/ml

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
