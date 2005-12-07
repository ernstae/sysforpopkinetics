#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;
//=============================================================================
//
// Create a Makefile for either an SPK Optimization request or
// a Monte Carlo integration request.
//
// Pre-condtions   - myIsMonte is set to true if the post-integration is
//                   going to be performed.  False otherwise.
// 
//                 - The current working directory is writable.
//
// Post-conditions - A file, Makefile.SPK, is saved in the current 
//                   working directory.  The make file defines targets
//                   that are either to build an optimization/simulation driver
//                   or a post-integration driver.
//
//=============================================================================
void NonmemTranslator::generateMakefile() const
{
  ofstream oMakefile( fMakefile );
  if( !oMakefile.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fMakefile ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  oMakefile << "PROD_DIR  = spkprod" << endl;
  oMakefile << "TEST_DIR  = spktest" << endl;
  oMakefile << endl;                                   

  oMakefile << "# C++ compiler flags to build a release version." << endl;
  oMakefile << "#CXX_FLAGS = -O3 -Dspk_release -DNDEBUG";
  oMakefile << endl;

  oMakefile << "# C++ compiler flags to build a debug version." << endl;
  oMakefile << "CXX_FLAGS = -g";

  oMakefile << endl;
  oMakefile << "# C++ compiler flags to turn on profiling" << endl;
  oMakefile << "# CXX_FLAGS += -pg -Dspk_profiling ";

  oMakefile << "# Define a macro \"ODEPRED\" when the job uses PRED." << endl;
  if( myModelSpec == ADVAN6 )
    oMakefile << "CXX_FLAGS += -DODEPRED"; // Define "ODEPRED" macro
  oMakefile << endl;                                        

  oMakefile << "LIBS      = -lspkpred -lspk -lQN01Box -lCppAD";
  oMakefile << (myIsMonte? " -lgsl" : "" ) << " -llapack -lg2c -latlas_lapack -lcblas -latlas -lpthread -lm -lxerces-c" << endl;
  oMakefile << endl;

  oMakefile << "COMMON_INCLUDE = \\" << endl;
  if( myModelSpec == PRED )
     oMakefile << "\tPred.h \\" << endl;
  else if( myModelSpec == ADVAN6 )
     oMakefile << "\tOdePred.h \\" << endl;
  oMakefile << "\tDataSet.h \\" << endl;
  oMakefile << "\tIndData.h \\" << endl;
  oMakefile << "\tNonmemPars.h \\" << endl;
  oMakefile << endl;                                   

  if( !myIsMonte )
    {
      oMakefile << "prod : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\tspkDriver.cpp \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "*.o" << endl;    
    }
  else
    {
      oMakefile << "MONTE_SRC = \\" << endl;
      oMakefile << "\tmonteDriver.cpp \\" << endl; 
      oMakefile << "\tAnalyticIntegral.cpp \\" << endl;
      oMakefile << "\tGridIntegral.cpp \\" << endl;
      oMakefile << "\tMontePopObj.cpp \\" << endl;
      oMakefile << "\tMapBay.cpp \\" << endl;
      oMakefile << "\tMapMonte.cpp \\" << endl;
      oMakefile << "\tGsl2SpkError.cpp" << endl;
      oMakefile << endl;

      oMakefile << "MONTE_INCLUDE = \\" << endl;
      oMakefile << "\tAnalyticIntegral.h \\" << endl;
      oMakefile << "\tGridIntegral.h \\" << endl;
      oMakefile << "\tMontePopObj.h \\" << endl;
      oMakefile << "\tMapBay.h \\" << endl;
      oMakefile << "\tMapMonte.h \\" << endl;
      oMakefile << "\tGsl2SpkError.h" << endl;
      oMakefile << endl;

      oMakefile << "prod : " << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(PROD_DIR)/ml/* ." << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : " << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
  
      oMakefile << "monte_clean : " << endl;
      oMakefile << "\trm -f $(MONTE_SRC) $(MONTE_INCLUDE)" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\t$(MONTE_SRC) \\" << endl;
      oMakefile << "\t$(MONTE_INCLUDE) \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "\t*.o" << endl;
    }
  oMakefile.close();

  return;
}
