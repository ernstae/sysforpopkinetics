#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "lex_explangTest.h"
#include "explangTest.h"
#include "ind_withIDTest.h"
#include "ind_withoutIDTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  map<string, CppUnit::Test*> master;
  vector<CppUnit::Test*> subset;

  // test for lexical analyzer
  master[ "lex_explangTest" ]   = lex_explangTest::suite();

  // test for syntax analyzer
  master[ "explangTest" ]       = explangTest::suite();

  // test for NonmemTranslator: ability to handle a data set with ID
  master[ "ind_withIDTest" ]    = ind_withIDTest::suite();

  // test for NonmemTranslator: ability to handle a data set with ID
  master[ "ind_withoutIDTest" ] = ind_withoutIDTest::suite();

  // test for NonmemTranslator: ability to handle a data set missing ID

  // test for NonmemTranslator: NonmemPars.h
  // test for NonmemTranslator: MontePars.h
  // test for NonmemTranslator: IndData.h
  // test for NonmemTranslator: DataSet.h
  // test for NonmemTranslator: Pred.h
  // test for NonmemTranslator: Makefile.SPK
  // test for NonmemTranslator: Makefile.MC (monte carlo)
  // test for NonmemTranslator: spkDriver.cpp
  // test for NonmemTranslator: monteDriver.cpp

  if( argc == 1 )
    {
      map<string, CppUnit::Test*>::const_iterator p = master.begin();
      for( p; p != master.end(); p++ )
	subset.push_back( p->second );
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


  CppUnit::TextUi::TestRunner runner;

  int n = subset.size();
  for( int i=0; i<n; i++ )
    {
      runner.addTest( subset[i] );
    }

  runner.run();

  return 0;
}
