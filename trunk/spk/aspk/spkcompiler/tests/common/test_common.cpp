#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "clientTest.h"
#include "SymbolTest.h"
#include "SymbolTableTest.h"
#include "SpkCompilerUtilTest.h"
#include "ExpTreeGeneratorTest.h"
#include "read_contentTest.h"
#include "emit_IndDataTest.h"
#include "emit_driverTest.h"
#include "SpkMLToCppTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;

  runner.addTest( clientTest::suite() );
  runner.addTest( SymbolTest::suite() );
  runner.addTest( SymbolTableTest::suite() );
  runner.addTest( SpkCompilerUtilTest::suite() );
  runner.addTest( ExpTreeGeneratorTest::suite() );
  runner.addTest( read_contentTest::suite() );  
  runner.addTest( emit_IndDataTest::suite() );
  runner.addTest( emit_driverTest::suite() );
  runner.addTest( SpkMLToCppTest::suite() );
  runner.run();

  return 0;
}
