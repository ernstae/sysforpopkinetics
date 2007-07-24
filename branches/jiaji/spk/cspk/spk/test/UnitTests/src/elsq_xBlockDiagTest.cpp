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
 * File: elsq_xBlockDiagTest.cpp
 *
 *
 * Test cases for elsq_xBlockDiag function
 *
 * Author:  Jiaji Du
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

#include "../../../spk/centdiff.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/det.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/elsq.h"
#include "../../../spk/elsq_xBlockDiag.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/matabs.h"
#include "../../../spk/subtract.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"

#include "elsq_xBlockDiagTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

/*******************************************************************************
 *
 *        STATIC FUNCTION DECLARATIONS
 *
 *******************************************************************************/
static bool hasPosDet( const DoubleMatrix& dmatA );

/*******************************************************************************
 *
 *        User SpkModel class
 *
 *******************************************************************************/
class UserModelElsq_xBlockDiagTest : public SpkModel<double>
{
    DoubleMatrix _b;
    const int _nB;
    const int _nY;

public:
    UserModelElsq_xBlockDiagTest()
      : _nB(3), _nY(3), _b(3)
    {};
    ~UserModelElsq_xBlockDiagTest(){};

private:
  void doSetIndPar(const valarray<double> &bval)
    {
        _b.fromValarray(bval);
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // h(x) = [ x(3) ]
      //        [ x(3) ]
      //        [ x(3) ]
      //
      ret.resize(_nY);
      const double *pX = _b.data();

      ret[0] = pX[2];
      ret[1] = pX[2];
	  ret[2] = pX[2];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const 
    {
      //
      // h_x(x) = [ 0 , 0 , 1 ]
      //          [ 0 , 0 , 1 ]
      //          [ 0 , 0 , 1 ]
      //
      ret.resize( _nY * _nB, 0.0 );

      ret[6] = 1.0;
      ret[7] = 1.0;
      ret[8] = 1.0;

      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // Q(x) = [ 2 * x(1),   0  ,    0    ]
      //        [     0   ,  x(2),    0.5  ]
      //        [     0   ,  0.5 ,    x(2) ]
      //
      
      ret.resize( _nY * _nY, 0.0 );
      const double *x = _b.data();

      ret[0] = 2.0 * x[0];
      ret[5] = 0.5;
      ret[7] = 0.5;
	  ret[4] = x[1];
      ret[8] = x[1];
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // Q(x)^-1 = [ 1.0/2.0/x(1),     0.0    ,      0.0    ]
      //           [ 0.0         ,    x(2)/det,    -0.5/det ]
      //           [ 0.0         ,    -0.5/det,    x(2)/det ]
      //
	  // det = x(2) * x(2) - 0.25 
      //
      ret.resize( _nY * _nY, 0.0 );
      const double *x = _b.data();
      const double det = x[1] * x[1] - 0.25;

      ret[0] = 1.0/2.0/x[0];
	  ret[5] = -0.5/det;
      ret[7] = -0.5/det;
      ret[4] = x[1]/det;
	  ret[8] = x[1]/det;
    }

    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // Q_x(x) = [ 2 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 1 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 0 , 0 ]
      //          [ 0 , 1 , 0 ]
      //
      ret.resize( _nY*_nY * _nB, 0.0 );

      ret[0] = 2.0;
      ret[13] = 1.0;
      ret[17] = 1.0;

      return true;
    }
};

void elsq_xBlockDiagTest::setUp()
{
    // initializations
}
void elsq_xBlockDiagTest::tearDown()
{
    // clean up
}

Test* elsq_xBlockDiagTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("elsq_xBlockDiagTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<elsq_xBlockDiagTest>(
                         "testElsq_xBlockDiag",
                          &elsq_xBlockDiagTest::testElsq_xBlockDiag));

    return suiteOfTests;
}

void elsq_xBlockDiagTest::testElsq_xBlockDiag()
{
    using namespace std;

    double (*pElsq)( const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix & );
    DoubleMatrix (*pElsq_xBlockDiag)( 
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix &,
                     const DoubleMatrix & );
    pElsq   = &elsq;
    pElsq_xBlockDiag = &elsq_xBlockDiag;

    DoubleMatrix dvecX(3,1);  dvecX.fill(1.0);
    DoubleMatrix dvecZ(3,1);  dvecZ.fill(0.0);
    
    // Initialize vector stepSize which specifies the step size
    DoubleMatrix stepSize(dvecX.nr(), 1);
    double  temp,
           *pdX = dvecX.data(),
           *pdS = stepSize.data();
    for( int i=stepSize.nr()-1; i>=0; i-- )
	{
        temp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pdX[i];
        pdS[i] = 10.0 * ( temp - pdX[i] );
    }        

    testder( pElsq, pElsq_xBlockDiag, dvecX, dvecZ, stepSize );
}


/***************************************************************************************
 *
 * testder for ELSQ_XBLOCKDIAG function
 *
 * Compare results from the evaluation of a user-defined derivative to
 * the approximation based on Jacobian algorithm.
 * Returns true if the differences are within the epcilon quality and 
 * false otherwise.
 *
 ***************************************************************************************/
void elsq_xBlockDiagTest::testder( double (*pElsq)( const DoubleMatrix &,
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix & ),
                                  DoubleMatrix (*pElsq_xBlockDiag)( 
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix &,
                                                    const DoubleMatrix & ),
                                  const DoubleMatrix &dvecX,
                                  const DoubleMatrix &dvecZ,
                                  const DoubleMatrix &dvecStepSize )

{
    using namespace std;

    const int nX = dvecX.nr();
    const int nZ = dvecZ.nr();
    CPPUNIT_ASSERT( dvecStepSize.nr() == nX );

    valarray<double> h, h_x, Q, Q_x, QInv;

    UserModelElsq_xBlockDiagTest model;
    valarray<double> x = dvecX.toValarray();

    model.setIndPar( x );

    model.dataMean(h);
    CPPUNIT_ASSERT( h.size() == nZ );

    model.dataMean_indPar(h_x);
    CPPUNIT_ASSERT( h_x.size() == nZ * nX );

    model.dataVariance(Q);
    CPPUNIT_ASSERT( Q.size() == nZ * nZ );
    
    model.dataVariance_indPar(Q_x);
    CPPUNIT_ASSERT( Q_x.size() == nZ * nZ * nX );

    CPPUNIT_ASSERT_MESSAGE( "hasPosDet( dmatQ )", hasPosDet( DoubleMatrix( Q, nZ ) ) );
    model.dataVarianceInv(QInv);

    // Compute analytical solution for elsq_xBlockDiag
	DoubleMatrix dvecR;
    subtract( dvecZ, DoubleMatrix( h, 1 ), dvecR );
	DoubleMatrix dvecN( 2, 1 );
	dvecN.data()[ 0 ] = 1;
	dvecN.data()[ 1 ] = 2;
    DoubleMatrix dmatDerivExact = pElsq_xBlockDiag( dvecR, 		                                       
						    DoubleMatrix( QInv, nZ ), 
						    DoubleMatrix( h_x, nX ), 
						    DoubleMatrix( Q_x, nX ),
						    dvecN );

    // Compute numerical solution of the derivative of elsqDiag(x)
    typedef Elsq< SpkModel<double> > ELSQ_PROTO;
    ELSQ_PROTO elsqOb(&model, dvecZ);
    DoubleMatrix dmatApx        = centdiff<ELSQ_PROTO>( elsqOb, dvecX, dvecStepSize );

    for( int i = 0; i<dmatDerivExact.nr()*dmatDerivExact.nc(); i++ )
    {
      double expected = dmatDerivExact.data()[i];
      double actual   = dmatApx.data()[i];
      CPPUNIT_ASSERT_DOUBLES_EQUAL(expected, actual, (expected==0? 0.001 : fabs(expected)*0.001) );
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

static bool hasPosDet( const DoubleMatrix& dmatA )
{
  // Compute b and c such that det(A) = b * 2^c.
  double b;
  long c;
  det( dmatA , &b, &c );
  return ( b > 0.0 );
}
