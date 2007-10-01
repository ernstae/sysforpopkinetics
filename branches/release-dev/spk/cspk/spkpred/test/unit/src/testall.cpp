#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "calcGroebnerBasisTest.h"
#include "DiagCovTest.h"
#include "FullCovTest.h"
#include "BlkDiagCovTest.h"
# if 0
// Mitch will revisit this (generates failure in test system)
#include "IdentPredBaseTest.h"
# endif
#include "IndPredModelTest.h"
#include "IndPredModelBaseTest.h"
#include "OdeBreakTest.h"
#include "OdePredBaseTest.h"
#include "PopPredModelTest.h"
#include "PopPredModelBaseTest.h"
#include "predNonparamMethodTest.h"
#include "predTwoStageMethodTest.h"

using namespace std;
using namespace CppUnit;

void usage()
{
  printf( "Usage: ./testall <LIST>\n" );
  printf( "\n" );
  printf( "<LIST> is optional.\n" );
  printf( "If it is omitted, all of the known test suites will be executed.\n" );
  printf( "Otherwise, it will be a space-separated list of CppUnit tests to be executed.\n" );
  return;
}

int main( int argc, const char * argv[] )
{
  //  Turning on the floating point error detection mechanism
  FpErrorChecker checkerON;

  map<string, CppUnit::Test*> master_list_of_tests;
  vector<CppUnit::Test*> tests_to_be_executed;

  master_list_of_tests[ "calcGroebnerBasisTest" ]     = calcGroebnerBasisTest   ::suite();
  master_list_of_tests[ "DiagCovTest" ]               = DiagCovTest             ::suite();
  master_list_of_tests[ "FullCovTest" ]               = FullCovTest             ::suite();
  master_list_of_tests[ "BlkDiagCovTest" ]            = BlkDiagCovTest          ::suite();
# if 0
// Mitch will revisit this (generates failure in test system)
  master_list_of_tests[ "IdentPredBaseTest" ]         = IdentPredBaseTest       ::suite();
# endif
  master_list_of_tests[ "IndPredModelTest" ]          = IndPredModelTest        ::suite();
  master_list_of_tests[ "IndPredModelBaseTest" ]      = IndPredModelBaseTest    ::suite();
  master_list_of_tests[ "OdeBreakTest" ]              = OdeBreakTest            ::suite();
  master_list_of_tests[ "OdePredBaseTest" ]           = OdePredBaseTest         ::suite();
  master_list_of_tests[ "PopPredModelTest" ]          = PopPredModelTest        ::suite();
  master_list_of_tests[ "PopPredModelBaseTest" ]      = PopPredModelBaseTest    ::suite();
  master_list_of_tests[ "predNonparamMethodTest" ]    = predNonparamMethodTest  ::suite();
  master_list_of_tests[ "predTwoStageMethodTest" ]    = predTwoStageMethodTest  ::suite();

  //
  // This is the case where user didn't select specific unit tests.
  // That means, run them all in the single process mode.
  //
  if( argc == 1 )
    {
      // push all existing unit test classes into the list
# if 0
      tests_to_be_executed.resize( master_list_of_tests.size() );
# endif
      map<string, CppUnit::Test*>::const_iterator p = master_list_of_tests.begin();
      for( p; p != master_list_of_tests.end(); p++ )
	tests_to_be_executed.push_back( p->second );
    }
  //
  // This is the case where user either specified a list of unit tests to run
  // or requested the parallel execution.
  //
  if( argc >= 2 )
    {
      if( strcmp( argv[1], "--help" ) == 0 || strcmp( argv[1], "?" ) == 0 )
	{
	  usage();
	  return 0;
	}
      for( int i=1; i<argc; i++ )
	{
	  map<string, CppUnit::Test*>::const_iterator p = master_list_of_tests.find( argv[i] );
	  if( p != master_list_of_tests.end() )
	    tests_to_be_executed.push_back( p->second );
	  else
	    fprintf( stderr, "!!! %s is not found in the master list (typo?) !!!\n", argv[i] );
	}
    }

  CppUnit::TextUi::TestRunner runner;

  int n = tests_to_be_executed.size();
  for( int i=0; i<n; i++ )
    {
      runner.addTest( tests_to_be_executed[i] );
    }

  runner.run();

  return 0;
}
