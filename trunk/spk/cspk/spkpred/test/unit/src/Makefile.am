bin_PROGRAMS = testall

testall_SOURCES = \
	compareToKnown.cpp \
	DiagCovTest.cpp \
	testall.cpp \
	compareToKnown.h \
	DiagCovTest.h \
	../config.h

testall_CXXFLAGS = -I/usr/local/cppunit/include

testall_LDFLAGS = -lspkpred -lspk -lspkopt /usr/local/lib/libnagc.a -lpthread -lm -lcppunit

INCLUDES = -I@top_srcdir@/
pkginclude_HEADERS = \
	compareToKnown.h \
	DiagCovTest.h
