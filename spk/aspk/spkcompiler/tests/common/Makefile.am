bin_PROGRAMS = test_common

COMMON = common

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
	clientTest.cpp \
        clientTest.h \
	ClientTranslatorTest.cpp \
	ClientTranslatorTest.h \
	ExpTreeGeneratorTest.cpp \
	ExpTreeGeneratorTest.h \
	SpkCompilerUtilTest.cpp \
	SymbolTest.cpp \
	SymbolTest.h \
	SymbolTableTest.cpp \
	SymbolTableTest.h \
	read_contentTest.cpp \
	read_contentTest.h \
        emit_IndDataTest.cpp \
	emit_IndDataTest.h \
        SpkMLToCppTest.cpp \
        SpkMLToCppTest.h \
	../nonmem/read_nonmem_datafile_gamma_wt_time_y.cpp \
	../nonmem/read_nonmem_datafile_gamma_wt_time_y.h \
	FitPopulationTestModel.cpp \
	FitPopulationTestModel.h

test_common_INCLUDES = \
	clientTest.h \
	ClientTranslatorTest.h \
	ExpTreeGeneratorTest.h \
	ExpNodeCarrierTest.h \
	SpkCompilerUtilTest.h \
	SymbolTest.h \
	SymbolTableTest.h \
	read_contentTest.h \
        SpkMLToCppTest.h \
	../nonmem/read_nonmem_datafile_gamma_wt_time_y.h \
	FitPopulationTestModel.h

AM_LDFLAGS = -lspk /usr/local/lib/libnagc.a -lpthread -lm -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
