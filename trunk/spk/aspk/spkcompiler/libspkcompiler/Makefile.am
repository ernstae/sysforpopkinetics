SUBDIRS = nonmem
lib_LTLIBRARIES = libspkcompiler.la

pkgincludedir = @includedir@/spkcompiler
NONMEM      = nonmem
INCLUDES    = -I@top_srcdir@/ -I$(NONMEM)/

AM_LFLAGS   = -i
AM_YFLAGS   = -d --verbose --debug
CXXFLAGS    = -g
AM_LDFLAGS  = ./nonmem/libnonmem.a

libspkcompiler_la_SOURCES = \
        config.h \
	read_content.h \
	read_content.cpp \
        ClientTranslator.h \
	SpkParameters.h \
	Symbol.cpp \
	Symbol.h \
	SymbolTable.cpp \
	SymbolTable.h \
	ExpTreeGenerator.cpp \
	ExpTreeGenerator.h \
	SpkMLToCpp.cpp \
	SpkMLToCpp.h \
	SpkCompilerUtil.cpp \
	SpkCompilerUtil.h \
        emit_IndData.cpp \
        emit_IndData.h

nobase_pkginclude_HEADERS = \
	$(NONMEM)/nonmem.h \
        $(NONMEM)/NonmemTranslator.h \
	$(NONMEM)/read_nonmem_driver.h \
	$(NONMEM)/read_nonmem_data.h \
	$(NONMEM)/read_nonmem_model.h \
        $(NONMEM)/emit_nonmem_driver.h \
	$(NONMEM)/emit_nonmem_model.h \
	$(NONMEM)/explang.h

pkginclude_HEADERS = \
	read_content.h \
	ClientTranslator.h \
	SpkParameters.h \
	Symbol.h \
	SymbolTable.h \
	ExpTreeGenerator.h \
	ExpNodeCarrier.h \
	client.h \
	SpkMLToCpp.h \
	SpkCompilerUtil.h \
	emit_IndData.h 

