#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "lex_explangTest.h"
#include "read_nonmem_driverTest.h"
#include "read_nonmem_dataTest.h"
#include "read_nonmem_modelTest.h"
#include "NonmemExpXlatorTest.h"
#include "emit_nonmem_driverTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  map<string, CppUnit::Test*> master;
  vector<CppUnit::Test*> subset;

  master[ "lex_explangTest" ]        = lex_explangTest::suite();
  master[ "NonmemExpXlatorTest" ]    =  NonmemExpXlatorTest::suite();
  master[ "read_nonmem_driverTest" ] = read_nonmem_driverTest::suite();
  master[ "read_nonmem_dataTest" ]   = read_nonmem_dataTest::suite();
  master[ "read_nonmem_modelTest" ]  = read_nonmem_modelTest::suite();
  master[ "emit_nonmem_driverTest" ] = emit_nonmem_driverTest::suite();

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
