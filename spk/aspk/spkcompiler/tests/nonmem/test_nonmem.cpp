#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "NonmemTranslatorTest.h"
#include "NonmemExpXlatorTest.h"
#include "NonmemDataReaderTest.h"

using namespace std;
using namespace CppUnit;


int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;

  runner.addTest( NonmemExpXlatorTest::suite() );
  runner.addTest( NonmemTranslatorTest::suite() );
  runner.addTest( NonmemDataReaderTest::suite() );  
  runner.run();

  return 0;
}
