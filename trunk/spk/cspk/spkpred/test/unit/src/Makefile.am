bin_PROGRAMS = testall

testall_SOURCES = \
	compareToKnown.cpp \
	DiagCovTest.cpp \
	FullCovTest.cpp \
	IndPredModelTest.cpp \
	PopPredModelTest.cpp \
	testall.cpp \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	PopPredModelTest.h \
	../config.h

testall_CXXFLAGS = -I/usr/local/cppunit/include

testall_LDFLAGS = -lspkpred -lspk -lspkopt -llapack -latlas_lapack -lcblas -latlas -lpthread -lm -lcppunit

INCLUDES = -I@top_srcdir@/
pkginclude_HEADERS = \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	PopPredModelTest.h
