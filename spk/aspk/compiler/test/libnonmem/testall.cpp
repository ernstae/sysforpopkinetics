#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "NonmemCompilerTest.h"
#include "NonmemExpXlatorTest.h"

using namespace std;
using namespace CppUnit;


int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;

  //  runner.addTest( NonmemExpXlatorTest::suite() );
  runner.addTest( NonmemCompilerTest::suite() );
  
  runner.run();

  return 0;
}
