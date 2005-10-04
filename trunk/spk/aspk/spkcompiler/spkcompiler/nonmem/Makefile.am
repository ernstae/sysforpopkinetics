noinst_LIBRARIES = libnonmem.a
AM_LFLAGS        = -i -Pnm_ -olex.yy.c
AM_YFLAGS        = -d --verbose --debug -p nm_
AM_CPPFLAGS      = -g -I./ -I../

MOSTLYCLEANFILES = explang.output \
                   explang.cpp \
                   explang.h \
                   lex_explang.c \
	           libnonmem.a

dist_libnonmem_a_SOURCES = \
        explang.ypp \
        lex_explang.l \
	nonmem.cpp \
	nonmem.h \
        CompartmentInfo.cpp \
        CompartmentInfo.h \
        CompModelInfo.cpp \
        CompModelInfo.h \
        NonmemTranslator.cpp \
        NonmemTranslator.h \
	NM_detAnalysisType.cpp \
	NM_parseData.cpp \
	NM_parseSource.cpp \
        NM_parseMonte.cpp \
	NM_generateMakefile.cpp \
	NM_parsePopAnalysis.cpp \
	NM_parseIndAnalysis.cpp \
	NM_parsePred.cpp \
	NM_parseAdvan.cpp \
	NM_generateIndData.cpp \
	NM_generateDataSet.cpp \
	NM_generatePred.cpp \
	NM_generateMontePars.cpp \
	NM_generateNonmemPars.cpp \
	NM_generateOdePred.cpp \
	NM_generateIndDriver.cpp \
	NM_generatePopDriver.cpp \
        countStrInLhs.cpp \
        countStrInLhs.h 
