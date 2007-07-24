#include <iostream>
#include <string>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/cholesky.h"
#include "choleskyTest.h"

using namespace std;
using namespace CppUnit;

static void print( const valarray<double>& A, int n )
{
  int m = A.size() / n;
  assert( m * n == A.size() );

  cout << m << " by " << n << endl;
  for( int i=0; i<m; i++ )
    {
      cout << "[ ";
      for( int j=0; j<n; j++ )
	{
	  if( j > 0 )
	    cout << ", ";
	  cout << A[j*n+i];
	}
      cout << " ]" << endl;
    }
}
                                                                                
CppUnit::Test * choleskyTest::suite()
{
                                                                                  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "choleskyTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<choleskyTest>(
         "test3by3", 
	 &choleskyTest::test3by3 ) );

  return suiteOfTests;
}
void choleskyTest::setUp()
{
}

void choleskyTest::tearDown()
{
}

void choleskyTest::test3by3()
{
  const int n = 3;
  double a[] = { 2, 0, 1, 0, 3, 0, 1, 0, 1 };
  double e[] = { 1.414214, 0, 0.707107, 0, 1.73205, 0, 0, 0, 0.707107 };
  valarray<double> A( a, n*n );
  valarray<double> AChol = cholesky(A, n);
  valarray<double> expected( e, n*n );
  
  double tol = 0.01;
  
  for( int j=0; j<n; j++ )
    {
      for( int i=0; i<n; i++ )
	{
	  double diff = fabs( AChol[i+j*n] - expected[i+j*n] ) / expected[i+j*n];
	  if( diff > tol )
	    {
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( 
                   expected[i+j*n], 
		   AChol[i+j*n], 
		   diff );
	    }
	}
    }

  /*
    A:
    [ 2 0 1 ]
    [ 0 3 0 ]
    [ 1 0 1 ]

    AChol (lower triangle):
    [ 1.41421    0        0        ]
    [ 0          1.73205  0        ]
    [ 0.707107   0        0.707107 ]
  */
  return;
}
