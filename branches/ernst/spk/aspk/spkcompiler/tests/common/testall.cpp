#include <iostream>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "clientTest.h"
#include "seriesTest.h"
#include "SymbolTest.h"
#include "SymbolTableTest.h"
#include "SpkCompilerErrorTest.h"
#include "SpkCompilerExceptionTest.h"
//#include "ClientTranslatorTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  map<string, CppUnit::Test*> master;
  vector<CppUnit::Test*> subset;

  master[ "clientTest" ]               = clientTest::suite();
  master[ "seriesTest" ]               = seriesTest::suite();
  master[ "SymbolTest" ]               = SymbolTest::suite();
  master[ "SymbolTableTest" ]          = SymbolTableTest::suite();
  master[ "SpkCompilerErrorTest" ]     = SpkCompilerErrorTest::suite();
  master[ "SpkCompilerExceptionTest" ] = SpkCompilerExceptionTest::suite();
//  master[ "ClientTranslatorTest" ]     = ClientTranslatorTest::suite();

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
