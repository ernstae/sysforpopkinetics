noinst_PROGRAMS      = testall

AM_LDFLAGS           = -lxerces-c \
                       -lcppunit \
		       ../../spkcompiler/libcommon.a \
		       ../../spkcompiler/nonmem/libnonmem.a

AM_CPPFLAGS          = -I@top_srcdir@ -g

testall_DEPENDENCIES = ../../spkcompiler/libcommon.a \
		       ../../spkcompiler/nonmem/libnonmem.a

MAINTEINERCLEANFILES = Makefile.in

MOSTLYCLEANFILES     = Pred.h \
                       predEqn.cpp \
                       predEqn.fortran \
                       IndData.h \
                       DataSet.h \
                       testall \
                       fitDriver.cpp \
                       fitDriver \
                       monteDriver.cpp \
                       monteDriver \
                       NonmemPars.h \
                       MontePars.h \
                       result.xml \
                       Makefile.SPK \
                       spk_error.tmp \
                       sotware_error \
		       checkpoint.xml

testall_SOURCES      = \
	               test_nonmem.cpp \
                       DOMPrint.h \
                       DOMPrint.cpp \
                       DOMPrintErrorHandler.cpp \
                       DOMPrintErrorHandler.hpp \
                       DOMPrintFilter.cpp \
		       DOMPrintFilter.hpp \
                       DOMTreeErrorReporter.cpp \
                       DOMTreeErrorReporter.hpp \
	               lex_explangTest.cpp \
	               lex_explangTest.h \
	               explangTest.cpp \
	               explangTest.h \
	               ind_fixedParaTest.cpp \
	               ind_fixedParaTest.h \
                       ind_withIDTest.cpp \
                       ind_withIDTest.h \
	               ind_withoutIDTest.cpp \
	               ind_withoutIDTest.h \
	               ind_subprobTest.cpp \
                       ind_subprobTest.h \
	               ind_simTest.cpp \
                       ind_simTest.h \
	               ind_simNoEstTest.cpp \
	               ind_simNoEstTest.h \
                       ind_mdvTest.cpp \
                       ind_mdvTest.h \
                       pop_mdvTest.cpp \
                       pop_mdvTest.h \
	               pop_basicTest.cpp \
	               pop_basicTest.h \
	               pop_monteTest.cpp \
	               pop_monteTest.h \
	               pop_fixedParaTest.cpp \
	               pop_fixedParaTest.h \
	               pop_subprobTest.cpp \
                       pop_subprobTest.h \
                       pop_advan6Test.cpp \
                       pop_advan6Test.h \
                       CompartmentInfoTest.cpp \
                       CompartmentInfoTest.h \
                       CompModelInfoTest.cpp \
                       CompModelInfoTest.h \
                       countStrInLhsTest.cpp \
                       countStrInLhsTest.h \
                       linInterpTest.cpp \
		       linInterpTest.h \
                       dataSetTest.cpp \
                       dataSetTest.h \
		       ../../config.h

