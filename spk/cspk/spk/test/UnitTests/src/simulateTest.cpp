/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: simulateTest.cpp
 *
 *
 * Test cases for simulate()
 *
 * Author: Viet Nyuyen
 * Updated by: sachiko honda
 *
 *************************************************************************/
#include <iostream>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/simulate.h>
#include <spk/SpkValarray.h>
#include <spk/calcMean.h>
#include "simulateTest.h"
#include <spk/identity.h>
#include <nag.h>
#include <cmath>


using SPK_VA::valarray;
using namespace CppUnit;

static valarray<double> sampleCovariance(const valarray<double> &V, int nCols);

// A helper function that generates a random integer if one is needed.
static int myseed()
{
  time_t curTime = time(NULL);
  srand((unsigned)curTime);
  
  int val = rand();
  
  for (int i = 0; i < (unsigned)val; i++)
    {
      val = rand();
    }
  
  return (val - RAND_MAX/2);		// range is now: [-RAND_MAX/2, RAND_MAX/2]
}
//-----------------------------------------------------------------------------
// A simple model for testing purposes only
//-----------------------------------------------------------------------------
class simulateTestModel : public SpkModel
{
  valarray<double> _a, _b;
  valarray<int>   _N;
  int _i;
  
public:
  simulateTestModel(int nAlp, int nB, const valarray<int> & N )
    
  {
    _a.resize( nAlp );
    _b.resize( nB );
    _N.resize( N.size() );
    _N = N;
  }
  ~simulateTestModel(){}
protected:
  void doSelectIndividual(int i)
  {
    _i = i;
  }
  void doSetPopPar(const valarray<double>& alp)
  {
    _a = alp;
  }
  void doSetIndPar(const valarray<double>& b)
  {
    _b = b;
  }
  void doDataMean( valarray<double>& f ) const 
  {
    f = _a[ 0 ] + _b[ 0 ];
    
  }
  bool doDataMean_popPar( valarray<double>& ret ) const 
  {
    return false;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doDataVariance( valarray<double>& ret ) const
  {
    identity( _N[_i], ret );
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doIndParVariance( valarray<double>& D ) const
  {
    D = _a[ 1 ];
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
};

//-----------------------------------------------------------------------------
// A complex model for testing purposes only
//-----------------------------------------------------------------------------

class simulateTestModelComplex : public SpkModel
{
  valarray<double> _a, _b;
  valarray<int>    _N;
  int _i, r;
  
public:
  simulateTestModelComplex( int nAlp, int nB, valarray<int> & N )
  {
    _a.resize( nAlp );
    _b.resize( nB );
    _N.resize( N.size() );
    _N = N;
  }
  ~simulateTestModelComplex(){}
protected:
  void doSelectIndividual(int i)
  {
    _i = i;
  }
  void doSetPopPar(const valarray<double>& alp)
  {
    _a = alp;
  }
  void doSetIndPar(const valarray<double>& b)
  {
    _b = b;
  }
  void doDataMean( valarray<double>& f ) const 
  {    
    for (int i = 0; i <= 3; i++)
      {
	f[i] = _a[i] + _b[i];
      }
    
    for (int i = 4; i <= 7; i++)
      {
	f[i] = 5* (_a[i - 4] + _b[i - 4]);
      }
    
    for (int i = 8; i < _N[_i]; i++)
      {
	f[i] = 6* (_a[r] + _b[r]);
      }
    
  }
  bool doDataMean_popPar( valarray<double>& ret ) const 
  {
    return false;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doDataVariance( valarray<double>& Ri ) const
  {
    Ri = 0.0;
    for (int i = 0; i < _N[_i]; i++)
      {
	Ri[i*_N[_i] + i] = _a[4];
      }
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doIndParVariance( valarray<double>& D ) const
  { 
    D[0]  =  0.36;
    D[1]  = -0.12;
    D[2]  =  0.3;
    D[3]  =  0.0012;
    D[4]  = -0.12;
    D[5]  =  0.05;
    D[6]  = -0.099;
    D[7]  =  0.0036;
    D[8]  =  0.3;
    D[9]  = -0.099;
    D[10] =  0.4101;
    D[11] =  0.015;
    D[12] =  0.0012;
    D[13] =  0.0036;
    D[14] =  0.015;
    D[15] =  0.002776;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
};

void simulateTest::setUp()
{
}
void simulateTest::tearDown()
{
    // clean up
}

Test* simulateTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("simulateTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<simulateTest>("simulate",  &simulateTest::test_with_simplemodel));
  suiteOfTests->addTest(new TestCaller<simulateTest>("simulate",  &simulateTest::test_with_complexmodel));
  
  
  return suiteOfTests;
}
void simulateTest::test_with_simplemodel()
{
  using namespace std;
  int nAlp = 2;
  int nB   = 1;
  int nInd = 1000;
  int npts = 10;
  
  // Measurement values, y.
  valarray<double> yOut( nInd * npts );
  
  // Number of measurements for each individual. 
  valarray<int> N( npts, nInd );
  
  // Fixed effects - 2 of them
  valarray<double> alp( nAlp );
  
  alp[ 0 ] = 1.0;
  alp[ 1 ] = 5.0;
  
  // Bounds on random effects - 1 random effect
  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
    
  // Output matrix
  valarray<double> bAllOut( nB * nInd );
  
  // Seed value
  int seed = 1;
  
  //-------------------------------------------------------------------------
  simulateTestModel model(nAlp, nB, N);		// Simple Model -- 2 runs 
  //-------------------------------------------------------------------------
  
  //-------------------------------------------------------------------------
  //	First run:  With random seed set
  //
  simulate(model, alp, N, bLow, bUp, yOut, bAllOut, seed);
  //-------------------------------------------------------------------------
  
  valarray<double> mean(nB);
  mean = calcMean(bAllOut, nB);
  
  valarray<double> cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );      // should converge to zero
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, cov[0],  5.0*0.1 );  // should converge to alp[1] = 5.0
  
  //-------------------------------------------------------------------------
  //	Second run:  Without random seed set
  //
  simulate(model, alp, N, bLow, bUp, yOut, bAllOut, myseed() );
  //-------------------------------------------------------------------------
  
  mean = calcMean(bAllOut, nB);
  
  cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );      // should converge to zero
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, cov[0],  5.0*0.1 );  // should converge to alp[1] = 5.0
  
  return;
}
void simulateTest::test_with_complexmodel()
{
  using namespace std;
  
  // #of individuals
  const int nInd = 2000;

  //Fixed effects - 5 of them
  int nAlp = 5;

  // Bounds on random effects - 4 random effects
  int nB = 4;
  
  // Number of measurements for each individual. 
  valarray<int> N( nInd );
  
  for (int i = 0; i < nInd; i++)
    {
      N[i] = 8 + (i+1) % 3;
    }

  int y_length = N.sum();

  // Measurement values, y.
  valarray<double> yOut( y_length );
  
  valarray<double> alp( nAlp );
    
  alp[ 0 ] = 1;
  alp[ 1 ] = 0.9;
  alp[ 2 ] = 0.9;
  alp[ 3 ] = 0.8;
  alp[ 4 ] = 0.1;
  
  valarray<double> bLow ( -2.0, nB );
  valarray<double> bUp  ( +2.0, nB );
  
  // Output matrix
  valarray<double> bAllOut( nB * nInd );
  
  // Seed value
  int seed = 1;

  //-------------------------------------------------------------------------
  simulateTestModelComplex modelcomplex( nAlp, nB, N );// Complex Model -- 2 runs 
  //-------------------------------------------------------------------------
    
  //-------------------------------------------------------------------------
  //	First run:  With random seed set
  //
  simulate(modelcomplex, alp, N, bLow, bUp, yOut, bAllOut, seed);
  //-------------------------------------------------------------------------
  
  valarray<double> mean( nB );
  mean = calcMean(bAllOut, nB);
  valarray<double> Cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.36, Cov[0], 0.36 * 0.1 );
  
  //-------------------------------------------------------------------------
  //	Second run:  Without random seed set
  //
  simulate(modelcomplex, alp, N, bLow, bUp, yOut, bAllOut, myseed() );
  //-------------------------------------------------------------------------
  
  mean = calcMean(bAllOut, nB);
  
  Cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.36, Cov[0], 0.36 * 0.1 );
  
  return;
	
} // end test_simulatecomplexmodel()

/*
$begin sampleCovariance$$

$spell
covariance
const
kth
ath
bth
iostream
namespace
std
Cov
endl
cout
$$

$section Creates a sample covariance matrix$$

$index sample testing data model measurement covariance$$

$table
$bold Prototype:$$ $cend 
$syntax/DoubleMatrix sampleCovariance(const DoubleMatrix &/V/))/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Generates a sample covariance matrix from the rows of $italic V$$.

$pre

The formula for the entry of the kth row and the nth column in 
the covariance matrix is:

	        M 
	 1     ---
	---    \    (V[k,j] - u_k)*(V[n,j] - u_n)    						
	M-1    /__						
	       j=1

$$
$pre

$$
where M is the number of columns in V and V[a,b] is the element at the ath row and 
bth column of $italic V$$.  The symbols u_k and u_n refer to the mean value of 
the kth row and the mean value of the nth row, respectively.  
The "*" symbol implies scalar multiplication.

$pre

$$
Refer to the function $mref/calcMean/$$ for calculating the mean value 
of a row in a matrix. 



$head Return Value$$

Returns a $italic Q$$ by $italic Q$$ matrix, where $italic Q$$ is the number
of rows in V.


$head Arguments$$

$syntax/
/V/
/$$
The $code DoubleMatrix$$ $italic V$$ must have at least two columns and one row,
i.e., M > 1 and Q > 0.


$head Example$$

If you compile, link, and run the following program,
$codep
	
    #include <iostream>
    #include "sampleCovariance.h"

	void main()
	{
		using namespace std;

		DoubleMatrix V(3,4), SampleCov;
		double *pV = V.data();
		double *pSampleCov = SampleCov.data();

		// Setting V to a matrix:
		//    [ -1  3  2  2 ]
		//    [ 9  1  2  -2 ]
		//    [ 0 -4  -1  6 ]

		pV[0] = -1;
		pV[1] = 9;
		pV[2] = 0;
		pV[3] = 3;
		pV[4] = 1;
		pV[5] = 4;
		pV[6] = 2;
		pV[7] = 2;
		pV[8] = -1;
		pV[9] = 2;
		pV[10] = -2;
		pV[11] = 6;

		SampleCov = sampleCovariance(V);

		cout << "A sample covariance of V: " << endl;
		SampleCov.print();
    }

$$

the matrix 
$math%
    [ 3  -7  2.8333 ]
    [ -7  21.667  -10.8333 ]
    [ 2.8333  -10.8333  10.9167]
%$$
will be printed.

$end

*/
valarray<double> sampleCovariance(const valarray<double> &V, int nCols)
{
  assert (nCols > 1); // covariance requires nCols >= 2
  
  int i, j, k, n;     // iterators
  int nRows = V.size() / nCols; // number of random effects
  
  double sum = 0.0;
  
  assert (nRows > 0);
  
  valarray<double> sampleCov(nRows * nRows);
  
  valarray<double> mean = calcMean(V, nRows);            // needed for covariance
  for (i = 0, k = 0; i < nRows; i++)                     // <--|  i and j are chosen from the
    {							 //    |	
      for (j = 0; j < nRows; j++, k++)                   // <--|  nRows rows available
	{
	  for (n = 0; n < nCols; n++)                    // n indexes through all nCols subjects
	    {
	      sum += (V[n*nRows + i] - mean[i]) * (V[n*nRows + j] - mean[j]);
	    }
	  
	  sampleCov[k] = sum / (nCols-1);                // sampleCov will now be filled out in row-major form:
	  sum = 0.0;                                     // this is okay because sampleCov is symmetric
	}
    }	
	
  return sampleCov;
}

