#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "SymbolTest.h"
#include "SymbolTableTest.h"
#include "SpkCompilerUtilTest.h"
#include "ParseTreeTest.h"

using namespace std;
using namespace CppUnit;


int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;
  runner.addTest( SymbolTest::suite() );
  runner.addTest( SymbolTableTest::suite() );
  runner.addTest( SpkCompilerUtilTest::suite() );
  runner.addTest( ParseTreeTest::suite() );
  
  runner.run();

  return 0;
}
