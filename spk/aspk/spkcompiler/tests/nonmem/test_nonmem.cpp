#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "read_nonmem_driverTest.h"
#include "NonmemExpXlatorTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;

  runner.addTest( NonmemExpXlatorTest::suite() );
  runner.addTest( read_nonmem_driverTest::suite() );
  runner.run();

  return 0;
}
