#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "lex_explangTest.h"
#include "explangTest.h"
#include "ind_withID_NonmemTranslatorTest.h"
#include "ind_noID_NonmemTranslatorTest.h"
#include "ind_onlysim_NonmemTranslatorTest.h"
#include "NonmemTranslatorPopTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  map<string, CppUnit::Test*> master;
  vector<CppUnit::Test*> subset;

  master[ "lex_explangTest" ]         = lex_explangTest::suite();
  master[ "explangTest" ]             = explangTest::suite();
  master[ "ind_withID_NonmemTranslatorTest" ] = ind_withID_NonmemTranslatorTest::suite();
  master[ "ind_noID_NonmemTranslatorTest" ] = ind_noID_NonmemTranslatorTest::suite();
  master[ "ind_onlysim_NonmemTranslatorTest" ] = ind_onlysim_NonmemTranslatorTest::suite();
  master[ "NonmemTranslatorPopTest" ] = NonmemTranslatorPopTest::suite();

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
