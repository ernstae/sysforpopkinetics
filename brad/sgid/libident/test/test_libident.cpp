/*************************************************************************
 *
 * File: test_libident.cpp
 *
 *
 * Executes all of the unit tests for identifiability library.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// Identifiability unit tests header files.
#include "checkParamIdentTest.h"
#include "calcGroebnerBasisTest.h"

// CppUnit framework header files.
#include <cppunit/TextTestRunner.h>
#include <cppunit/ui/text/TestRunner.h>


/*************************************************************************
 *
 * Function: main
 *
 *//**
 * This main function executes all of the unit tests for
 * identifiability library.
 *
/*************************************************************************/

int main( int nArg, char* argCStr[] )
{
  //----------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------

  using namespace std;

  using namespace CppUnit;


  //----------------------------------------------------------
  // Execute all of the unit tests.
  //----------------------------------------------------------

  // Create a CppUnit test runner.
  CppUnit::TextUi::TestRunner unitTestRunner;

  // Register all of the unit tests.
  unitTestRunner.addTest( calcGroebnerBasisTest::suite() );
  unitTestRunner.addTest( checkParamIdentTest  ::suite() );

  // Run all of the unit tests.
  unitTestRunner.run();


  //----------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------

  return 0;
}
