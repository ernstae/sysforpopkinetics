/**
 * @file NM_generateMakefile.cpp
 * Define NonmemTranslator::generateMakefile().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

// Use this to fix bug number 585
// (will no longer need this once bug is fixed in GSL)
# define SPK_REPLACE_MISER 1

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

  oMakefile << "# C++ compiler flages to build a 64-bit version." << endl;
  oMakefile << "CXX_FLAGS_64_BIT = " << endl;
  oMakefile << "if [ `uname -i` = \"x86_64\" ]; then  CXX_FLAGS_64_BIT = -DBA0_64BITS; fi" << endl;
  oMakefile << endl;                                   

  oMakefile << "# C++ compiler flags to build a production version." << endl;
  oMakefile << "CXX_FLAGS_PROD = -O3 -Dspk_release -DNDEBUG $(CXX_FLAGS_64_BIT) " << endl;
  oMakefile << endl;

  oMakefile << "# C++ compiler flags to build a test version." << endl;
  oMakefile << "# These are the same as the production flags because the" << endl;
  oMakefile << "# test system will be deployed into the production system." << endl;
  oMakefile << "CXX_FLAGS = " << endl;
  oMakefile << "CXX_FLAGS_TEST = $(CXX_FLAGS_PROD) " << endl;
  oMakefile << endl;

  oMakefile << "# C++ compiler flags to build a debug version." << endl;
  oMakefile << "CXX_FLAGS_DEBUG = -g $(CXX_FLAGS_64_BIT) " << endl;
  oMakefile << endl;

  oMakefile << "# C++ compiler flags to turn on profiling" << endl;
  oMakefile << "# CXX_FLAGS += -pg -Dspk_profiling $(CXX_FLAGS_64_BIT) " << endl;
  oMakefile << endl;

  if( myModelSpec == ADVAN6 )
  {
    oMakefile << "# Define a macro \"ODEPRED\" when the job uses PRED." << endl;
    oMakefile << "CXX_FLAGS += -DODEPRED"; // Define "ODEPRED" macro
  }
  oMakefile << endl;                                        

  oMakefile << "LIBS      = -lspkpred -lspk -lnon_par -lmat2cpp -lQN01Box";
  oMakefile << " -lgsl -llapack  -lcblas -latlas -lm -lxerces-c -lcln -lginac -lpvm3" << (myIsIdent? " -lbad -lbap -lbav -lba0 -lgslcblas" : "" ) << endl;
  oMakefile << endl;

  oMakefile << "COMMON_INCLUDE = \\" << endl;
  if( myIsIdent )
     oMakefile << "\tIdentPred.h \\" << endl;
  else if( myModelSpec == PRED )
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
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "prod_parallel : prod indDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) indDriver.cpp -o indDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "test : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "test_parallel : test indDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) indDriver.cpp -o indDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "debug : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "debug_parallel : debug indDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) indDriver.cpp -o indDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\tfitDriver.cpp \\" << endl;
      oMakefile << "\tindDriver.cpp \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\tindDriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "*.o" << endl;
    }
  else
    {
      oMakefile << "MONTE_CPP = \\" << endl;
      oMakefile << "\tAdaptIntegral.cpp \\" << endl;
      oMakefile << "\tGridIntegral.cpp \\" << endl;
      oMakefile << "\tMontePopObj.cpp \\" << endl;
      oMakefile << "\tMapBay.cpp \\" << endl;
      oMakefile << "\tMapMonte.cpp \\" << endl;
      oMakefile << "\tGsl2SpkError.cpp" << endl;
      oMakefile << endl;
      oMakefile << "MONTE_SRC = $(MONTE_CPP) " << endl;
    

      oMakefile << "MONTE_INCLUDE = \\" << endl;
      oMakefile << "\tAdaptIntegral.h \\" << endl;
      oMakefile << "\tGridIntegral.h \\" << endl;
      oMakefile << "\tMontePopObj.h \\" << endl;
      oMakefile << "\tMapBay.h \\" << endl;
      oMakefile << "\tMapMonte.h \\" << endl;
      oMakefile << "\tGsl2SpkError.h" << endl;
      oMakefile << endl;

# if SPK_REPLACE_MISER
      oMakefile << "prod : miser.o adapt.o pow_ii.o "           << endl;
# else
      oMakefile << "prod : adapt.o pow_ii.o "           << endl;
# endif
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(PROD_DIR)/ml/* ." << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o driver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o driver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "prod_parallel : prod "           << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(PROD_DIR)/ml/* ." << endl;
      oMakefile << "\tmake -f Makefile.SPK prod" << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o alpDriver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_PROD) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o alpDriver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

# if SPK_REPLACE_MISER
      oMakefile << "test : miser.o adapt.o pow_ii.o"           << endl;
# else
      oMakefile << "test : adapt.o pow_ii.o"           << endl;
# endif
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o driver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o driver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "test_parallel : test"           << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
      oMakefile << "\tmake -f Makefile.SPK test" << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o alpDriver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_TEST) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o alpDriver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

# if SPK_REPLACE_MISER
      oMakefile << "debug : miser.o adapt.o pow_ii.o "           << endl;
# else
      oMakefile << "debug : adapt.o pow_ii.o "           << endl;
# endif
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o driver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) monteDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o driver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "debug_parallel : debug"           << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
      oMakefile << "\tmake -f Makefile.SPK debug" << endl;
# if SPK_REPLACE_MISER
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) miser.o adapt.o pow_ii.o -o alpDriver ";
# else
      oMakefile << "\tg++ $(CXX_FLAGS_DEBUG) $(CXX_FLAGS) monteAlpDriver.cpp $(MONTE_CPP) adapt.o pow_ii.o -o alpDriver ";
# endif
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/lib/atlas ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR)/CppAD ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << " -I/usr/share/pvm3/include ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUX ";
      oMakefile << " -L/usr/share/pvm3/lib/LINUXX86_64 ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

# if SPK_REPLACE_MISER
      oMakefile << "miser.o : "                       << endl;
      oMakefile << "\tsed -e '184,184s/>=/>/g' -e'31,31d'  > miser.c ";
      oMakefile << "/usr/local/src/$(TEST_DIR)/ml/gsl-1.10-miser.c" << endl; 
      oMakefile << "\tgcc $(CXX_FLAGS) -c miser.c"   << endl;
      oMakefile << endl;
# endif

      oMakefile << "adapt.o : "                       << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/adapt.c ." << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/f2c.h   ." << endl;
      oMakefile << "\tgcc $(CXX_FLAGS) -c adapt.c"   << endl;
      oMakefile << endl;

      oMakefile << "pow_ii.o : "                       << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/pow_ii.c ." << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/f2c.h   ." << endl;
      oMakefile << "\tgcc $(CXX_FLAGS) -c pow_ii.c"   << endl;
      oMakefile << endl;
  
      oMakefile << "monte_clean : " << endl;
      oMakefile << "\trm -f $(MONTE_SRC) $(MONTE_INCLUDE) monteDriver.cpp monteAlpDriver.cpp" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\t$(MONTE_SRC) \\" << endl;
      oMakefile << "\tmonteDriver.cpp \\" << endl;
      oMakefile << "\tmonteAlpDriver.cpp \\" << endl;
      oMakefile << "\t$(MONTE_INCLUDE) \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\talpDriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "\t*.o" << endl;
    }
  oMakefile.close();

  return;
}
