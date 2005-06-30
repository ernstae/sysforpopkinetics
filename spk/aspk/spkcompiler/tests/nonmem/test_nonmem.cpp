#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "lex_explangTest.h"
#include "explangTest.h"
#include "ind_withIDTest.h"
#include "ind_withoutIDTest.h"
#include "ind_simTest.h"
#include "ind_simNoEstTest.h"
#include "ind_fixedParaTest.h"
#include "pop_basicTest.h"
#include "pop_fixedParaTest.h"
#include "pop_monteTest.h"
#include "ind_mdvTest.h"
#include "pop_mdvTest.h"
//#include "pop_diffeqnTest.h"
#include "ind_subprobTest.h"
#include "pop_subprobTest.h"

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

  // test for NonmemTranslator: ability to handle fixed parameters (not fixed effects)
  master[ "pop_fixedParaTest" ] = pop_fixedParaTest::suite();

  // test for NonmemTranslator: ability to handle a data set with rows without DVs.
  master[ "ind_mdvTest" ] = ind_mdvTest::suite();
  master[ "pop_mdvTest" ] = pop_mdvTest::suite();

  // test for NonmemTranslator: ability to handle Differential Equations
  //  master[ "pop_diffeqnTest" ] = pop_diffeqnTest::suite();

  // test for NonmemTranslator: ability to handle a request 
  // to repeat the whole process.
  master[ "ind_subprobTest" ] = ind_subprobTest::suite();
  master[ "pop_subprobTest" ] = pop_subprobTest::suite();

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
