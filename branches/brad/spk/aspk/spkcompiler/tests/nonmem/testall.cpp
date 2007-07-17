#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include <xercesc/util/PlatformUtils.hpp>

#include "lex_explangTest.h"
#include "explangTest.h"
#include "ind_withIDTest.h"
#include "ind_withoutIDTest.h"
#include "ind_simTest.h"
#include "ind_simNoEstTest.h"
#include "ind_fixedParaTest.h"
#include "ind_identTest.h"
#include "pop_basicTest.h"
#include "pop_fixedParaTest.h"
#include "pop_blockDiagCovTest.h"
#include "pop_monteTest.h"
#include "ind_mdvTest.h"
#include "pop_mdvTest.h"
#include "pop_mdvTest.h"
#include "ind_subprobTest.h"
#include "pop_subprobTest.h"
#include "pop_nonparamMethodGridTest.h"
#include "pop_nonparamMethodRandomUniformTest.h"
#include "pop_twoStageMethodTest.h"
#include "CompartmentInfoTest.h"
#include "CompModelInfoTest.h"
//#include "ind_advan6Test.h"
#include "pop_advan6Test.h"
#include "ind_dataSetTest.h"
#include "pop_dataSetTest.h"
#include "pop_modifyDataItemsTest.h"
//#include "ind_parseDataTest.h"
//#include "pop_parseDataTest.h"

#include "countStrInLhsTest.h"
#include "linInterpTest.h"
#include "linInterpTest.h"
using namespace std;
using namespace CppUnit;
using namespace xercesc;

// List all the available tests
void printall( const map<string, CppUnit::Test*>& list )
{
  printf( "Usage: testall [--list] [TESTS]\n" );
  printf( "\n" );
  printf( "--list - Displays the list of all available tests\n" ); 
  printf( "TESTS  - The space delimited list of unit test names\n" );
  printf( "\n" );
  map<string, CppUnit::Test*>::const_iterator p = list.begin();
  for( p; p != list.end(); p++ )
    printf( "\t%s\n", p->first.c_str() );
  return;
}

int main( int argc, const char * argv[] )
{
  map<string, CppUnit::Test*> master;
  vector<CppUnit::Test*> subset;

  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException & toCatch )
    {
      printf( "Error during Xerces-c Initialization.\n" );
      return -1;
    }

  // test for lexical analyzer
  master[ "lex_explangTest" ]   = lex_explangTest::suite();

  // test for syntax analyzer
  master[ "explangTest" ]       = explangTest::suite();

  // test for NonmemTranslator: ability to handle an Individual data set with ID
  master[ "ind_withIDTest" ]    = ind_withIDTest::suite();

  // test for NonmemTranslator: ability to handle an Individual data set missing ID
  master[ "ind_withoutIDTest" ] = ind_withoutIDTest::suite();

  // test for NonmemTranslator: ability to handle an indivisual data simulation followed by estimation
  master[ "ind_simTest" ] = ind_simTest::suite();

  // test for NonmemTranslator: ability to handle an individual data simulation only
  master[ "ind_simNoEstTest" ] = ind_simNoEstTest::suite();

  // test for NonmemTranslator: ability to handle a basic population level request
  master[ "pop_basicTest" ] = pop_basicTest::suite();

  // test for NonmemTranslator: ability to handle a population level Monte Carlo request
  master[ "pop_monteTest" ] = pop_monteTest::suite();

  // test for NonmemTranslator: ability to handle fixed parameters (not fixed effects)
  master[ "ind_fixedParaTest" ] = ind_fixedParaTest::suite();

  // test for NonmemTranslator: ability to handle individual identifiability
  master[ "ind_identTest" ] = ind_identTest::suite();

  // test for NonmemTranslator: ability to handle fixed parameters (not fixed effects)
  master[ "pop_fixedParaTest" ] = pop_fixedParaTest::suite();

  // test for NonmemTranslator: ability to handle block diagonal cov (omega)
  master[ "pop_blockDiagCovTest" ] = pop_blockDiagCovTest::suite();

  // test for NonmemTranslator: ability to handle a data set with rows without DVs.
  master[ "ind_mdvTest" ] = ind_mdvTest::suite();
  master[ "pop_mdvTest" ] = pop_mdvTest::suite();

  // test for NonmemTranslator: ability to handle Differential Equations
  //  master[ "ind_advan6Test" ] = ind_advan6Test::suite();
  master[ "pop_advan6Test" ] = pop_advan6Test::suite();

  // test for NonmemTranslator: ability to handle a request 
  // to repeat the whole process.
  master[ "ind_subprobTest" ] = ind_subprobTest::suite();
  master[ "pop_subprobTest" ] = pop_subprobTest::suite();

  // tests for NonmemTranslator: ability to handle a nonparametric method requests 
  master[ "pop_nonparamMethodGridTest" ]          = pop_nonparamMethodGridTest::suite();
  master[ "pop_nonparamMethodRandomUniformTest" ] = pop_nonparamMethodRandomUniformTest::suite();

  // test for NonmemTranslator: ability to handle a two-stage method request 
  master[ "pop_twoStageMethodTest" ] = pop_twoStageMethodTest::suite();

  // Test for CompartmentInfo
  master[ "CompartmentInfoTest" ] = CompartmentInfoTest::suite();

  // Test for CompModelInfo
  master[ "CompModelInfoTest" ] = CompModelInfoTest::suite();

  //master[ "NonmemSourceMLElementsTest" ] = NonmemSourceMLElementsTest::suite();
  //master[ "NonmemReportMLElementsTest" ] = NonmemSourceMLElementsTest::suite();
  master[ "countStrInLhsTest" ] = countStrInLhsTest::suite();

  // Test for Linear Interpolation
//  master[ "linInterpTest" ] = linInterpTest::suite();

  // Test for Linear Interpolation
  master[ "ind_dataSetTest" ] = ind_dataSetTest::suite();
  master[ "pop_dataSetTest" ] = pop_dataSetTest::suite();

  // Test for data.xml parsing feature
//  master[ "ind_parseDataTest" ] = ind_parseDataTest::suite();
//  master[ "pop_parseDataTest" ] = pop_parseDataTest::suite();

  // Inserting missing data items
  master[ "pop_modifyDataItemsTest" ] = pop_modifyDataItemsTest::suite();

  if( argc == 1 )
    {
      map<string, CppUnit::Test*>::const_iterator p = master.begin();
      for( p; p != master.end(); p++ )
	subset.push_back( p->second );
      printall( master );
    }
  else
    {
      if( strcmp( argv[1], "--list" ) == 0 )
	{
	  printall( master );
	}
      else
	{
	  for( int i=1; i<argc; i++ )
	    {  
	      map<string, CppUnit::Test*>::const_iterator p = master.find( argv[i] );
	      if( p != master.end() )
		{
		  subset.push_back( p->second );
		}
	      else
		{
		  fprintf( stderr, "!!! Can't find %s in the master list (type?) !!! \n",
			   argv[i] );
		}
	    }
	}
    }

  CppUnit::TextUi::TestRunner runner;

  int n = subset.size();
  for( int i=0; i<n; i++ )
    {
      runner.addTest( subset[i] );
    }

  runner.run();

  return 0;
}
