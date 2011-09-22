/*************************************************************************
 *
 * File: printInMatrixTest.cpp
 *
 *
 * Test cases for printInMatrix()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/printInMatrix.h"
#include "printInMatrixTest.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

void printInMatrixTest::setUp()
{
    // initializations
}
void printInMatrixTest::tearDown()
{
    // clean up
}

Test* printInMatrixTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("printInMatrixTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<printInMatrixTest>(
                       "emptyCase", &printInMatrixTest::emptyCase));
  suiteOfTests->addTest(new TestCaller<printInMatrixTest>(
		       "legalNonEmptyCase", &printInMatrixTest::legalNonEmptyCase));

    return suiteOfTests;
}

void printInMatrixTest::emptyCase()
{
  using std::cout;
  using std::endl;
  cout << "This test displays outputs to standard output.  Bear with me." << endl;
  valarray<double> d(0);

  //
  // No matter what #cols says, as long as the array is empty, it should handle the case 
  // gracefully.
  //

  // 
  // [ Comment by Sachiko, 10/25/02 ]
  // This particular order of calls cause VC++ *internal* compiler error (C1001).
  //       printInMatrix( d, 0 );
  //       printInMatrix( d, 1 );
  //       printInMatrix( d, -1 );
  // or
  //       printInMatrix( d, 1 );
  //       printInMatrix( d, -1 );
  //       printInMatrix( d, 0 );
  //
  // [ excertion from VC++ help ]
  // "More rarely, such (i.e. optimization related) errors occur at very low optimization levels or 
  //  even when optimization is disabled. 
  //  In such cases, rewriting the line where the error is reported (or possibly several lines including 
  //  the one causing the error) may be a solution. If none of these options works, consult the technical 
  //  support help file or the technical support section in one of your manuals."
  //

  printInMatrix( d, 1 );
  printInMatrix( d, -1 );
  printInMatrix( d, 0 );
}
void printInMatrixTest::legalNonEmptyCase()
{
  using std::cout;
  using std::endl;

  cout << "This test displays outputs to standard output.  Bear with me." << endl;
  double data[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  valarray<double> d( data, 8 );
  printInMatrix( d, 1 );
  cout << endl;
  printInMatrix( d, 2 );
  cout << endl;

  bool bData[] = { true, false, false, true };
  valarray<bool> b( bData, 4 );
  printInMatrix( b, 2 );
  cout << endl;

/*
  // 
  // [ Comment by Sachiko, 10/25/2002 ]
  // As of the date this printInMatrix() function was submitted,
  // cout is not defined for valarrray objects.
  // So, the following block of code testing nested array
  // do not pass the test.
  // 
  valarray< valarray<double> > nested(3);
  for( int i=0, cols=1; i<3, cols<=8; i++, cols*=2 )
  {
    nested[i] = valarray<double>( data, cols );
  }
  printInMatrix( nested, 2 );
  printf("----------\n");
*/
}
