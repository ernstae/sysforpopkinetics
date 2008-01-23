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
 * File: elsq_xTest.cpp
 *
 *
 * Test cases for elsq_x function
 *
 * Author:  Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <iomanip>
#include <cfloat>
#include <cmath>
#include <fstream>
#include <string>
#include <limits>
#include <cassert>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/elsq_x.h"
#include "../../../spk/centdiff.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/matmax.h"
#include "../../../spk/det.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/elsq.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/matabs.h"
#include "../../../spk/subtract.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"

#include "elsq_xTest.h"

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;

/*******************************************************************************
 *
 *        STATIC FUNCTION DECLARATIONS
 *
 *******************************************************************************/

static bool hasPosDet( const DoubleMatrix& dmatA );
static bool hasPosDet( const valarray<double>& A, int n );

/*******************************************************************************
 *
 *        User SpkModel class
 *
 *******************************************************************************/

class UserModelElsq_xTest : public SpkModel<double>
{
    valarray<double> x;
    const int _nB;
    const int _nY;

public:
    UserModelElsq_xTest()
      : _nB(3), _nY(2), x(3)
    {};
    ~UserModelElsq_xTest(){};

private:
  void doSetIndPar(const valarray<double> &bval)
    {
        x = bval;
	assert( x.size() == bval.size() );
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // h(x) = [ x(3) ]
      //        [ x(3) ]
      //
      ret.resize(_nY);
      ret[0] = 1.0 * x[2];
      ret[1] = 1.0 * x[2];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const 
    {
      //
      // h_x(x) = [ 0 , 0 , 1 ]
      //          [ 0 , 0 , 1 ]
      //
      ret.resize( _nY * _nB );
      for( int i=0; i<_nY*_nB; i++ )
        ret[i] = 0.0;
      ret[4] = 1.0;
      ret[5] = 1.0;

      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // Q(x) = [ 2 * x(1) ,   0   ]
      //        [     0    ,  x(2) ]
      //
      
      ret.resize(_nY * _nY);

      ret[0] = 2.0 * x[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = x[1];
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // Q(x)^-1 = [ 1.0 / (2.0*x(1)),   0.0        ]
      //           [ 0.0                 1.0 / x(2) ]
      //
      ret.resize(_nY * _nY);

      ret[0] = 1.0 / (2.0 * x[0]);
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / x[1];
    }

    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // Q_x(x) = [ 2 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 1 , 0 ]
      //
      ret.resize( _nY*_nY * _nB );
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = 2.0;
      ret[7] = 1.0;

      return true;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // Q^-1_x(x) = [ -1.0 / (2.0 * x(1))^2  0.0             0.0    ]
      //             [ 0.0                    0.0             0.0    ]
      //             [ 0.0                    0.0             0.0    ]
      //             [ 0.0                    -1.0 / x(2)^2   0.0    ]
      //
      ret.resize(_nY*_nY*_nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / ( (2.0 * x[0])*(2.0 * x[0]) );
      ret[7] = -1.0 / (x[1] * x[1]);
      return true;
    }
};


void elsq_xTest::setUp()
{
    // initializations
}
void elsq_xTest::tearDown()
{
    // clean up
}

Test* elsq_xTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("elsq_xTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<elsq_xTest>("testElsq_x", &elsq_xTest::testElsq_x));
    suiteOfTests->addTest(new TestCaller<elsq_xTest>("testElsq_xValarray", &elsq_xTest::testElsq_xValarray));

    return suiteOfTests;
}
/*
---------------------------------------------------------------------------
             Begin: Tests for DoubleMatrix version
---------------------------------------------------------------------------
*/

void elsq_xTest::testElsq_x()
{
    using namespace std;

    double (*pElsq)( const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix & );
    const DoubleMatrix (*pElsq_x)( 
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix & );
    pElsq   = &elsq;
    pElsq_x = &elsq_x;

    DoubleMatrix dvecX(3,1); dvecX.fill(1.0);
    DoubleMatrix dvecZ(2,1); dvecZ.fill(0.0);
    
    // Initialize vector stepSize which specifies the step size
    DoubleMatrix stepSize(dvecX.nr(), 1);
    double  temp,
           *pdX = dvecX.data(),
           *pdS = stepSize.data();
    for( int i=stepSize.nr()-1; i>=0; i-- ){
        
        temp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pdX[i];
        pdS[i] = 10.0 * ( temp - pdX[i] );
        
        /*pdS[i] = 0.5; //to get k = 2^2 * error*/
    }

    testder( pElsq, pElsq_x, dvecX, dvecZ, stepSize );
}

//
// testder() for the DoubleMatrix version of elsq_x function
//
// Compare results from the evaluation of a user-defined derivative to
// the approximation based on Jacobian algorithm.
// Returns true if the differences are within the epcilon quality and 
// false otherwise.
//
void elsq_xTest::testder( double (*pElsq)(const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix & ),
                     const DoubleMatrix (*pElsq_x)( 
                                     const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix &,
                                     const DoubleMatrix & ),
                     const DoubleMatrix &dvecX,
                     const DoubleMatrix &dvecZ, 
                     const DoubleMatrix &dvecStepSize ){

    using namespace std;

    const int nX = dvecX.nr();
    const int nZ = dvecZ.nr();
    assert( dvecStepSize.nr() == nX );
    
    valarray<double> x = dvecX.toValarray();

    UserModelElsq_xTest model;
    valarray<double> h, h_x, Q, Q_x, QInv;

    model.setIndPar( x );

    model.dataMean(h);
    model.dataMean_indPar(h_x);
    model.dataVariance(Q);
    model.dataVariance_indPar(Q_x);

    CPPUNIT_ASSERT_MESSAGE( "hasPosDet( dmatQ )", hasPosDet( DoubleMatrix( Q, nZ ) ) );
    model.dataVarianceInv(QInv);

    // Compute analytical solution for elsq_x
    DoubleMatrix dmatDerivExact = pElsq_x(dvecZ, DoubleMatrix( h, 1 ), DoubleMatrix( Q, nZ ), DoubleMatrix( QInv, nZ ), DoubleMatrix( h_x, nX ), DoubleMatrix( Q_x, nX ));

    // Compute numerical solution of the derivative of elsq(x)
    typedef Elsq< SpkModel<double> > ELSQ_PROTO;
    ELSQ_PROTO elsqOb(&model, dvecZ);
    DoubleMatrix dmatApx        = centdiff<ELSQ_PROTO>( elsqOb, dvecX, dvecStepSize );

    for( int i = 0; i<dmatDerivExact.nr()*dmatDerivExact.nc(); i++ )
    {
      double expected = dmatDerivExact.data()[i];
      double actual   = dmatApx.data()[i];
      CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected!=0.0? expected * 0.001 : 0.001 ) );
    }
}


//
// Function: hasPosDet
//
// Returns true if the determinant of the matrix A is positive.
//
static bool hasPosDet( const DoubleMatrix& dmatA )
{
  // Compute b and c such that det(A) = b * 2^c.
  double b;
  long c;
  det( dmatA , &b, &c );
  return ( b > 0.0 );
}
/*
---------------------------------------------------------------------------
             End: Tests for DoubleMatrix version
---------------------------------------------------------------------------
*/

/*
---------------------------------------------------------------------------
             Begin: Tests for valarray version
---------------------------------------------------------------------------
*/
void elsq_xTest::testElsq_xValarray()
{
    using namespace std;

    double (*pElsq)( const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> & );
    const valarray<double> (*pElsq_x)( 
                     const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> &,
                     const valarray<double> & );
    pElsq   = &elsq;
    pElsq_x = &elsq_x;

    const int nX = 3;
    const int n  = 2;

    valarray<double> x(1.0, nX);
    valarray<double> z(0.0, n);
    
    // Initialize vector stepSize which specifies the step size
    valarray<double> stepSize( x.size() );
    double  temp;

    for( int i=nX-1; i>=0; i-- ){
        
        temp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0 ) + x[i];
        stepSize[i] = 10.0 * ( temp - x[i] );
        
        /*pdS[i] = 0.5; //to get k = 2^2 * error*/
    }

    testder( pElsq, pElsq_x, x, z, stepSize );
}



//
// testder() for the valarray version of elsq_x function
//
// Compare results from the evaluation of a user-defined derivative to
// the approximation based on Jacobian algorithm.
// Returns true if the differences are within the epcilon quality and 
// false otherwise.
//
void elsq_xTest::testder( double (*pElsq)(const valarray<double> &,
					  const valarray<double> &,
					  const valarray<double> &,
					  const valarray<double> & ),
			  const valarray<double> (*pElsq_x)( 
							    const valarray<double> &,
							    const valarray<double> &,
							    const valarray<double> &,
							    const valarray<double> &,
							    const valarray<double> &,
							    const valarray<double> & ),
			  const valarray<double> &x,
			  const valarray<double> &z, 
			  const valarray<double> &stepSize )
{
  using namespace std;
  
  const int nX = x.size();
  const int nZ = z.size();
  assert( stepSize.size() == nX );
  
  UserModelElsq_xTest model;
  valarray<double> h, h_x, Q, Q_x, QInv;
  
  model.setIndPar( x );
  
  model.dataMean(h);
  model.dataMean_indPar(h_x);
  model.dataVariance(Q);
  model.dataVariance_indPar(Q_x);
  
  CPPUNIT_ASSERT_MESSAGE( "hasPosDet( Q )", hasPosDet( Q, nZ ) );
  model.dataVarianceInv(QInv);
  
  // Compute analytical solution for elsq_x
  valarray<double> derivExact = pElsq_x(z, h, Q, QInv, h_x, Q_x );
  
  // Compute numerical solution of the derivative of elsq(x)
  typedef Elsq< SpkModel<double> > ELSQ_PROTO;
  ELSQ_PROTO elsqOb(&model, DoubleMatrix(z, 1) );
  valarray<double> approx = centdiff<ELSQ_PROTO>( elsqOb, DoubleMatrix(x, 1), DoubleMatrix(stepSize, 1) ).toValarray();
  
  for( int i = 0; i<derivExact.size(); i++ )
    {
      double expected = derivExact[i];
      double actual   = approx[i];
      
      CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected!=0.0? expected * 0.001 : 0.001 ) );
    }
}


/*************************************************************************
 *
 * Function: hasPosDet
 *
 *
 * Returns true if the determinant of the matrix A is positive.
 *
 *************************************************************************/

static bool hasPosDet( const valarray<double>& A, int n )
{
  // Compute b and c such that det(A) = b * 2^c.
  double b;
  long c;
  det( A, n, &b, &c );
  return ( b > 0.0 );
}
/*
---------------------------------------------------------------------------
             End: Tests for valarray version
---------------------------------------------------------------------------
*/
