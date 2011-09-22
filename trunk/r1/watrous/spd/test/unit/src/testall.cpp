#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include <spd/FpErrorChecker.h>

#include "hTildeTest.h"
#include "lambdaTest.h"
#include "mapObjTest.h"
#include "ppdOptTest.h"
#include "ppedOptTest.h"

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

  master_list_of_tests[ "hTildeTest" ]               = hTildeTest::suite();	  
  master_list_of_tests[ "lambdaTest" ]               = lambdaTest::suite();	  
  master_list_of_tests[ "mapObjTest" ]               = mapObjTest::suite();	  
  master_list_of_tests[ "ppdOptTest" ]               = ppdOptTest::suite();	  
  master_list_of_tests[ "ppedOptTest" ]              = ppedOptTest::suite();	  
  
  //
  // This is the case where user didn't select specific unit tests.
  // That means, run them all in the single process mode.
  //
  if( argc == 1 )
    {
      // push all existing unit test classes into the list
      tests_to_be_executed.resize( master_list_of_tests.size() );
      map<string, CppUnit::Test*>::const_iterator p = master_list_of_tests.begin();
      for( p; p != master_list_of_tests.end(); p++ )
	tests_to_be_executed.push_back( p->second );
    }
  //
  // This is the case where user either specified a list of unit tests to run.
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
