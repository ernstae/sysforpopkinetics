bin_PROGRAMS = testall

testall_SOURCES = \
	hTildeTest.cpp \
	hTildeTest.h \
	lambdaTest.cpp \
	lambdaTest.h \
	mapObjTest.cpp \
	mapObjTest.h \
	ppdOptTest.cpp \
	ppdOptTest.h \
	ppedOptTest.cpp \
	ppedOptTest.h \
	testall.cpp \
	../config.h

testall_CXXFLAGS = -I/usr/local/cppunit/include

testall_LDFLAGS = -lspd -lspkopt /usr/local/lib/libnagc.a -lpthread -lm -lcppunit

INCLUDES = -I@top_srcdir@/

pkginclude_HEADERS = \
	hTildeTest.h \
	lambdaTest.h \
	mapObjTest.h \
	ppdOptTest.h \
	ppedOptTest.h
