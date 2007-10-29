noinst_PROGRAMS      = testall

# Set Linker flags
AM_LDFLAGS           = -lxerces-c \
                       -lcppunit \
			-ldl \
		       $(top_builddir)/spkcompiler/libcommon.a \
		       $(top_builddir)/spkcompiler/nonmem/libnonmem.a

# Set C preprocessor flags
AM_CPPFLAGS          = -I. -I$(top_builddir) 

# Set C++ compiler flags
if SPK_RELEASE
AM_CXXFLAGS          = -O3 -Dspk_release -DNDEBUG
else
AM_CXXFLAGS          = -g  -Dspk_debug
endif

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
                       OdePred.h \
                       result.xml \
                       Makefile.SPK \
                       spk_error.tmp \
                       sotware_error \
		       xml1.xml \
                       xml2.xml \
                       xml3. xml \
		       checkpoint.xml

testall_SOURCES      = \
	               testall.cpp \
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
	               pop_blockDiagCovTest.cpp \
	               pop_blockDiagCovTest.h \
	               pop_monteTest.cpp \
	               pop_monteTest.h \
	               pop_fixedParaTest.cpp \
	               pop_fixedParaTest.h \
	               pop_subprobTest.cpp \
                       pop_subprobTest.h \
	               pop_nonparamMethodGridTest.cpp \
                       pop_nonparamMethodGridTest.h \
	               pop_nonparamMethodRandomUniformTest.cpp \
                       pop_nonparamMethodRandomUniformTest.h \
	               pop_twoStageMethodTest.cpp \
                       pop_twoStageMethodTest.h \
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
                       ind_dataSetTest.cpp \
                       ind_dataSetTest.h \
                       pop_dataSetTest.cpp \
                       pop_dataSetTest.h \
                       pop_modifyDataItemsTest.cpp \
                       ../../config.h
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]
#	               ind_identTest.cpp \
#	               ind_identTest.h \
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]

