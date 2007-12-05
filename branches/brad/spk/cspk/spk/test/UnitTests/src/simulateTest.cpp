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
#include "../../../spk/simulate.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/calcMean.h"
#include "simulateTest.h"
#include "../../../spk/identity.h"
#include <cmath>


using SPK_VA::valarray;
using SPK_VA::slice;
using namespace CppUnit;
using namespace std;

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
// A simple population model for testing purposes only
//-----------------------------------------------------------------------------
class simulateTestPopModel : public SpkModel<double>
{
  valarray<double> alp, b;
  valarray<int>   N;
  int who;
  
public:
  simulateTestPopModel(int nAlp, int nB, const valarray<int> & NIn )
    : alp( nAlp ), b( nB ), N( NIn )
    
  {
  }
  ~simulateTestPopModel(){}
protected:
  void doSelectIndividual(int i)
  {
    who = i;
  }
  void doSetPopPar(const valarray<double>& alpIn)
  {
    alp = alpIn;
  }
  void doSetIndPar(const valarray<double>& bIn)
  {
    b = bIn;
  }
  void doDataMean( valarray<double>& fiOut ) const 
  {
    fiOut.resize( N[who] );
    fiOut = alp[ 0 ] + b[ 0 ];
    
  }
  bool doDataMean_popPar( valarray<double>& fi_alpOut ) const 
  {
    fi_alpOut.resize( N[who] * alp.size() );
    return false;
  }
  bool doDataMean_indPar( valarray<double>& fi_bOut ) const
  {
    fi_bOut.resize( N[who] * b.size() );
    return false;
  }
  void doDataVariance( valarray<double>& RiOut ) const
  {
    RiOut.resize( N[who] * N[who] );
    identity( N[who], RiOut );
  }
  bool doDataVariance_popPar( valarray<double>& Ri_alpOut ) const
  {
    Ri_alpOut.resize( N[who] * N[who] * alp.size() );
    return false;
  }
  bool doDataVariance_indPar( valarray<double>& Ri_bOut ) const
  {
    Ri_bOut.resize( N[who] * N[who] * b.size() );
    return false;
  }
  void doIndParVariance( valarray<double>& DOut ) const
  {
    DOut.resize( b.size() * b.size() );
    DOut = alp[ 1 ];
  }
  bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
  {
    D_alpOut.resize( b.size() * b.size() * alp.size() );
    return false;
  }
};

//-----------------------------------------------------------------------------
// A complex population model for testing purposes only
//-----------------------------------------------------------------------------

class simulateTestPopModelComplex : public SpkModel<double>
{
  valarray<double> alp, b;
  valarray<int>    N;
  int who;
  
public:
  simulateTestPopModelComplex( int nAlp, int nB, valarray<int> & NIn )
  : alp( nAlp ), b( nB ), N( NIn )
  {
  }
  ~simulateTestPopModelComplex(){}
protected:
  void doSelectIndividual(int i)
  {
    who = i;
  }
  void doSetPopPar(const valarray<double>& alpIn)
  {
    alp = alpIn;
  }
  void doSetIndPar(const valarray<double>& bIn)
  {
    b = bIn;
  }
  void doDataMean( valarray<double>& fiOut ) const 
  {    
    fiOut.resize( N[who] );
    for (int i = 0; i <= 3; i++)
      {
	fiOut[i] = alp[i] + b[i];
      }
    
    for (int i = 4; i <= 7; i++)
      {
	fiOut[i] = 5* (alp[i - 4] + b[i - 4]);
      }
    
    for (int i = 8; i < N[who]; i++)
      {
	fiOut[i] = 6* (alp[ (i+1)%3 ] + b[(i+1)%3 ]);
      }
    
  }
  bool doDataMean_popPar( valarray<double>& fi_alpOut ) const 
  {
    fi_alpOut.resize( N[who] * alp.size() );
    return false;
  }
  bool doDataMean_indPar( valarray<double>& fi_bOut ) const
  {
    fi_bOut.resize( N[who] * b.size() );
    return false;
  }
  void doDataVariance( valarray<double>& RiOut ) const
  {
    RiOut.resize( N[who] * N[who] );
    RiOut = 0.0;
    for (int i = 0; i < N[who]; i++)
      {
	RiOut[i*N[who] + i] = alp[4];
      }
  }
  bool doDataVariance_popPar( valarray<double>& Ri_alpOut ) const
  {
    Ri_alpOut.resize( N[who] * N[who] * alp.size() );
    return false;
  }
  bool doDataVariance_indPar( valarray<double>& Ri_bOut ) const
  {
    Ri_bOut.resize( N[who] * N[who] * b.size() );
    return false;
  }
  void doIndParVariance( valarray<double>& DOut ) const
  { 
    DOut.resize( b.size() * b.size() );
    DOut[0]  =  0.36;
    DOut[1]  = -0.12;
    DOut[2]  =  0.3;
    DOut[3]  =  0.0012;
    DOut[4]  = -0.12;
    DOut[5]  =  0.05;
    DOut[6]  = -0.099;
    DOut[7]  =  0.0036;
    DOut[8]  =  0.3;
    DOut[9]  = -0.099;
    DOut[10] =  0.4101;
    DOut[11] =  0.015;
    DOut[12] =  0.0012;
    DOut[13] =  0.0036;
    DOut[14] =  0.015;
    DOut[15] =  0.002776;
  }
  bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
  {
    D_alpOut.resize( b.size() * b.size() * alp.size() );
    return false;
  }
};
//-----------------------------------------------------------------------------
// A simple individual model for testing purposes only
//-----------------------------------------------------------------------------
class simulateTestIndModel : public SpkModel<double>
{
  valarray<double>  b;
  int nY, nB;
  
public:
  simulateTestIndModel( int nBIn, int nYIn )
    : nB( nBIn ), b( nBIn ), nY( nYIn )
  {
  }
  ~simulateTestIndModel(){}
protected:
  void doSetIndPar( const valarray<double>& bIn )
  {
    b = bIn;
  }
  void doDataMean( valarray<double>& fOut ) const 
  {
    fOut.resize( nY );
    fOut = 0.0;
  }
  bool doDataMean_indPar( valarray<double>& f_bOut ) const
  {
    f_bOut.resize( nY * nB );
    f_bOut = 0.0;
  }
  void doDataVariance( valarray<double>& ROut ) const
  {
    ROut.resize( nY * nY );
    identity( nY, ROut );
  }
  bool doDataVariance_indPar( valarray<double>& R_bOut ) const
  {
    R_bOut.resize( nY * nY * nB );
    R_bOut = 0.0;
    return false;
  }
};

class SimulateExampleIndModel : public SpkModel<double>
{
  valarray<double>  _b;
  const int nB, nY;

public:
  SimulateExampleIndModel( int nBIn, int nYIn )
    : nB(nBIn), nY(nYIn), _b(nBIn)
  {}
  ~SimulateExampleIndModel(){}
protected:
  void doSetIndPar(const valarray<double>& b)
  {
    _b = b;
  }
  void doDataMean( valarray<double> & fOut ) const 
  {
    //--------------------------------------------------------------
    //
    // Calculates
    //
    //            /       \ 
    //     f(b) = |  b(1)  |  .
    //            \       / 
    //
    //--------------------------------------------------------------
    fOut = _b[0];
  }

  bool doDataMean_indPar( valarray<double> & f_bOut ) const
  {
    //--------------------------------------------------------------
    //
    // Calculates
    //
    //              /     \ 
    //     f_b(b) = |  1  |  .
    //              \     / 
    //
    //--------------------------------------------------------------
    f_bOut = 1.0;
    return true;
  }
  void doDataVariance( valarray<double> & ROut ) const
  {
    //--------------------------------------------------------------
    //
    // Calculates
    //
    //            /                                \ 
    //     R(b) = |  1  0  0  0  0  0  0  0  0  0  |
    //            |  0  1  0  0  0  0  0  0  0  0  |
    //            |  0  0  1  0  0  0  0  0  0  0  |
    //            |  0  0  0  1  0  0  0  0  0  0  |
    //            |  0  0  0  0  1  0  0  0  0  0  |
    //            |  0  0  0  0  0  1  0  0  0  0  |
    //            |  0  0  0  0  0  0  1  0  0  0  |
    //            |  0  0  0  0  0  0  0  1  0  0  |
    //            |  0  0  0  0  0  0  0  0  1  0  |
    //            |  0  0  0  0  0  0  0  0  0  1  |
    //            \                                / 
    //
    //--------------------------------------------------------------
    ROut = 0.0;
    ROut[ slice( 0, nY, nY+1 ) ] = 1.0;
  }
  bool doDataVariance_indPar( valarray<double> & R_bOut ) const
  {
    //--------------------------------------------------------------
    //
    // Calculates
    //
    //              /     \ 
    //     R_b(b) = |  0  |  .
    //              \     / 
    //
    //--------------------------------------------------------------
    R_bOut = 0.0;
    return false;
  }
};

//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

void simulateTest::ind_example()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------
  
  using namespace std;
  
  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------
  
  // Number of measurements.
  int nY = 10;
  
  // size of b vector
  int nB = 1;
  
  // Measurement values, y.
  valarray<double> y( nY );

  // Seed
  int seed = 3;
  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------
  
  valarray<double> b ( 0.0, nB );
    
  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------
  
  SimulateExampleIndModel model( nB, nY );
  
  //------------------------------------------------------------
  // Simulate measurements for each individual.
  //------------------------------------------------------------
  simulate(model, nY, b, y, 1);

  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------
  // {1.11227, 0.608056, -0.712082, -1.71895, -0.400054, -2.27172, 0.866331, -1.03258, -0.358203, -1.11381}
//  cout << "yOut:" << y << endl;
//  cout << endl;

  CPPUNIT_ASSERT_DOUBLES_EQUAL(  1.11227,  y[0], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(  0.608056, y[1], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -0.712082, y[2], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -1.71895,  y[3], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -0.400054, y[4], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -2.27172,  y[5], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(  0.86631,  y[6], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -1.03258,  y[7], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -0.358203, y[8], 0.001 ); 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -1.11381,  y[9], 0.001 ); 
}
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
  suiteOfTests->addTest(new TestCaller<simulateTest>(
	       "ind_example",  &simulateTest::ind_example));
  /*
  suiteOfTests->addTest(new TestCaller<simulateTest>(
	       "pop_simple_seed",  &simulateTest::pop_simple_seed));
  suiteOfTests->addTest(new TestCaller<simulateTest>(
               "pop_complex_seed", &simulateTest::pop_complex_seed));

  suiteOfTests->addTest(new TestCaller<simulateTest>(
               "ind_simple_seed",  &simulateTest::ind_simple_seed));
  
  suiteOfTests->addTest(new TestCaller<simulateTest>(
               "ind_simple_noseed",  &simulateTest::ind_simple_noseed));
  */  
  return suiteOfTests;
}
void simulateTest::ind_simple_seed()
{
  int nB = 1;
  int nY = 1000;

  simulateTestIndModel model( nB, nY );

  // y(i) = f(b) + e, where e~N(0, R(b))

  valarray<double> b( 1.0, nB );
  valarray<double> f( nY );
  valarray<double> R( nY * nY );
  valarray<double> yOut( nY );
  int seed = 1;

  model.setIndPar( b );
  model.dataMean( f );

  simulate( model, nY, b, yOut, seed );

  valarray<double> e = yOut - f;
  double std = sqrt( (e * e).sum()/nY );

  // The standard deviation should converge to 1.0 because the diagonal elements
  // of R are all one: sqrt( var ) = sqrt( 1.0 ) = 1.0 = std.
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, std, 0.05 );
  

  // With the same seed, simulation results should be always the same.
  valarray<double> yn( nY );
  int n = 5;

  for( int i=0; i<n; i++ )
    simulate( model, nY, b, yn, seed );

  for( int i=0; i<nY; i++ )
    CPPUNIT_ASSERT_EQUAL( yOut[i], yn[i] );
  
}
void simulateTest::ind_simple_noseed()
{
  int nB = 1;
  int nY = 10;

  simulateTestIndModel model( nB, nY );

  // y(i) = f(b) + e, where e~N(0, R(b))

  valarray<double> b( 1.0, nB );
  valarray<double> f( nY );
  valarray<double> R( nY * nY );
  valarray<double> y0( nY );
  valarray<double> yn( nY );
  int seed = 3;
  int n    = 5;

  model.setIndPar( b );
  model.dataMean( f );

  srand( seed );
  simulate( model, nY, b, y0 );

  for( int i=0; i<n; i++ )
    simulate( model, nY, b, yn );

  bool ok = false;
  for( int i=0; i<nY; i++ )
    ok |= y0[i] != yn[i];
  CPPUNIT_ASSERT_MESSAGE( "Two sets of simulated data did not differ", ok );
}

#include "../../../spk/printInMatrix.h"
void simulateTest::pop_simple_seed()
{
  using namespace std;
  int nAlp = 2;
  int nB   = 1;
  int nInd = 1000;
  int npts = 10;
  int nRepeats = 3;
  
  // Measurement values, y.
  valarray<double> yOut( nInd * npts );
  valarray<double> ynOut( nInd * npts );
  
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
  int seed = 3;
  
  //-------------------------------------------------------------------------
  simulateTestPopModel model(nAlp, nB, N);
  //-------------------------------------------------------------------------
  
  //-------------------------------------------------------------------------
  //	First run:  With a specific seed
  //
  simulate(model, alp, N, bLow, bUp, yOut, bAllOut, seed);
  for( int i=0; i<nRepeats; i++ )
    {
      simulate( model, alp, N, bLow, bUp, ynOut, bAllOut, seed );
    }
  for( int i=0; i<nInd*npts; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( yOut[i], ynOut[i] );
    }
  //-------------------------------------------------------------------------

  valarray<double> mean(nB);
  mean = calcMean(bAllOut, nB);
  
  valarray<double> cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );      // should converge to zero
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, cov[0],  5.0*0.1 );  // should converge to alp[1] = 5.0
  
  //-------------------------------------------------------------------------
  //	Second run:  With a random seed set
  //
  simulate(model, alp, N, bLow, bUp, yOut, bAllOut, myseed() );
  //-------------------------------------------------------------------------
  
  mean = calcMean(bAllOut, nB);
  
  cov = sampleCovariance(bAllOut, nInd);
  
  // Assert answers
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.11 );     // should converge to zero
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, cov[0],  5.0*0.1 );  // should converge to alp[1] = 5.0
  

  //-------------------------------------------------------------------------
  // Third run: A seed set only once
  //
  bool ok = false;
  srand( seed );
  simulate( model, alp, N, bLow, bUp, yOut, bAllOut );
  for( int i=0; i<nRepeats; i++ )
    {
      simulate( model, alp, N, bLow, bUp, ynOut, bAllOut );
    }
  for( int i=0; i<nInd*npts; i++ )
    {
      ok |= yOut[i] != ynOut[i];
    }
  CPPUNIT_ASSERT_MESSAGE( "Two sets of simulated data did not differ!", ok );
  //-------------------------------------------------------------------------

  return;
}
void simulateTest::pop_complex_seed()
{
  using namespace std;
  
  // #of individuals
  const int nInd = 1000;

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
  simulateTestPopModelComplex modelcomplex( nAlp, nB, N );// Complex Model -- 2 runs 
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
  /*
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, mean[0], 0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.36, Cov[0], 0.36 * 0.1 );
  */  
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

