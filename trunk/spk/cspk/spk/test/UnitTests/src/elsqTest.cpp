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
 * File: elsqTest.cpp
 *
 *
 * Test cases for elsq()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/elsq.h"
#include "elsqTest.h"

#include <cstdlib>
#include <ctime>
#include <cmath>
#include "../../../spk/inverse.h"
#include "../../../spk/det.h"
#include "../../../spk/subtract.h"
#include "../../../spk/transpose.h"
#include "../../../spk/pi.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/multiply.h"
#include "../../../spk/allZero.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

static double compareAgainst(
            const DoubleMatrix &dvecZ,     // column vector
            const DoubleMatrix &dvecH,     // column vector
            const DoubleMatrix &dmatQ,     // symmetric matrix Q(x)
            const DoubleMatrix &dmatInvQ   // inverse of Q
            );

static double compareAgainst(
            const valarray<double> &z,     // column vector
            const valarray<double> &h,     // column vector
            const valarray<double> &Q,     // symmetric matrix Q(x)
            const valarray<double> &invQ   // inverse of Q
            );
static double oneByOneToScalar( const DoubleMatrix &dmatA );
static bool hasPosDet( const DoubleMatrix& dmatA );
static bool hasPosDet( const valarray<double>& A, int n );

/*******************************************************************************
 *
 *        User SpkModel class
 *
 *******************************************************************************/
class UserModelElsqTest : public SpkModel<double>
{
    DoubleMatrix _b;
    const int _nB;
    const int _nY;

public:
    UserModelElsqTest()
      : _nB(3), _nY(2)
    {};
    ~UserModelElsqTest(){};

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
      //
      ret.resize(_nY);
      const double *pX = _b.data();

      ret[0] = 1.0 * pX[2];
      ret[1] = 1.0 * pX[2];
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
      const double *x = _b.data();

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
      const double *x = _b.data();

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
      const double * x = _b.data();
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / ( (2.0 * x[0])*(2.0 * x[0]) );
      ret[7] = -1.0 / (x[1] * x[1]);
      return true;
    }
};


void elsqTest::setUp()
{
    // initializations
}
void elsqTest::tearDown()
{
    // clean up
}

Test* elsqTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("elsqTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<elsqTest>("valarrayTest", &elsqTest::valarrayTest));
    suiteOfTests->addTest(new TestCaller<elsqTest>("fixedTest",    &elsqTest::fixedTest));
    suiteOfTests->addTest(new TestCaller<elsqTest>("genericTest",  &elsqTest::genericTest));

    return suiteOfTests;
}
void elsqTest::valarrayTest()
{
    using namespace std;
/*
    valarray<double> x(1.0, 3);
    valarray<double> z(0.0, 2);
    valarray<double> h(2);
    valarray<double> Q(2 * 2);
    valarray<double> invQ(2 * 2);

    UserModelElsqTest model;

    double dElsqAns, dTestElsqAns;    

    model.setIndPar( x );

    model.dataMean(h);
    model.dataVariance(Q);
    model.dataVarianceInv(invQ);

    dElsqAns     = elsq(z, h, Q, invQ );
    dTestElsqAns = compareAgainst(z, h, Q, invQ );

    // Compare the two results from evaluation of elsq
    CPPUNIT_ASSERT_DOUBLES_EQUAL(dElsqAns, dTestElsqAns, 
        fabs(dTestElsqAns-dElsqAns)/fabs(dTestElsqAns)*DBL_EPS_EQUAL_MULT );
*/
    /* Testing with generic data */
    vector<int> Ns;
    Ns.push_back(1);
    Ns.push_back(3);

    int n;
    for(int cnt=0; cnt<Ns.size(); cnt++)
    {
        n=Ns[cnt];

        valarray<double> x( n );
        valarray<double> z( n );
        valarray<double> h( n );
        valarray<double> Q( 0.0, n * n );
        valarray<double> invQ( n * n );

        double dElsqAns, dTestElsqAns;

        for( int i=0; i<n; i++)
        {
            x[i] = rand() / 10000.0;
            x[i] = rand() / 10000.0;
            x[i] = rand() / 10000.0;
        }

        //
        // Create a positive symmetric matrix
        //
        do{
            for( int j=0; j<n; j++ ){
                for( int k=0; k<n; k++ ){
                    if( k == j )
                        Q[k+j*n] = rand() / 10000.0;
                }
            }
        }while( !hasPosDet( Q, n ) );
  
        invQ = inverse(Q, n);

        dElsqAns     = elsq(z, h, Q, invQ);
        dTestElsqAns = compareAgainst(z, h, Q, invQ);
    
        // Compare the two results from evaluation of elsq
        CPPUNIT_ASSERT( fabs(dTestElsqAns-dElsqAns)/fabs(dTestElsqAns) <= 0.001 );

    }
}
void elsqTest::fixedTest()
{
    using namespace std;

    /* Testing with sample user models */
    valarray<double> x(1.0, 3);
    DoubleMatrix dvecZ(2,1); dvecZ.fill(0.);    /* Z : yi */
    valarray<double> h(2);       /* h : fi */
    valarray<double> Q(2 * 2);        /* R : Ri */
    valarray<double> QInv(2 * 2);
    DoubleMatrix dummy;

    UserModelElsqTest model;

    double dElsqAns, dTestElsqAns;    

    model.setIndPar( x );

    model.dataMean(h);
    model.dataVariance(Q);
    model.dataVarianceInv(QInv);

    dElsqAns     = elsq(dvecZ, DoubleMatrix( h, 1 ), DoubleMatrix( Q, 2 ), DoubleMatrix( Q, 2 ) );
    dTestElsqAns = compareAgainst(dvecZ, DoubleMatrix( h, 1 ), DoubleMatrix( Q, 2 ), DoubleMatrix( Q, 2 ) );

    // Compare the two results from evaluation of elsq
    CPPUNIT_ASSERT( fabs(dTestElsqAns-dElsqAns)/fabs(dTestElsqAns) <= 0.001 );
}
void elsqTest::genericTest()
{
    using namespace std;
    /* Testing with generic data */
    vector<int> Ns;
    Ns.push_back(1);
    Ns.push_back(3);

    int n;
    for(int cnt=0; cnt<Ns.size(); cnt++)
    {
        n=Ns[cnt];

        DoubleMatrix dvecX(n,1),
                     dvecZ(n,1),
                     dvecH(n,1),
                     dmatQ(n,n), // assume symmetric and invertible
                     dmatInvQ(n,n);

        double *pdVecX = dvecX.data(),
               *pdVecZ = dvecZ.data(),
               *pdVecH = dvecH.data(),
               *pdMatQ = dmatQ.data(),
               *pdInvQ = dmatInvQ.data(),
                dElsqAns, dTestElsqAns;

        for( int i=0; i<n; i++)
        {
            pdVecX[i] = rand() / 10000.0;
            pdVecZ[i] = rand() / 10000.0;
            pdVecH[i] = rand() / 10000.0;
        }

        //
        // Create a positive symmetric matrix
        //
        dmatQ.fill(0.0);
        do{
            for( int j=0; j<n; j++ ){
                for( int k=0; k<n; k++ ){
                    if( k == j )
                        pdMatQ[k+j*n] = rand() / 10000.0;
                }
            }
        }while( !hasPosDet( dmatQ ) );
  
        dmatInvQ = inverse(dmatQ);

        dElsqAns     = elsq(dvecZ, dvecH, dmatQ, dmatInvQ);
        dTestElsqAns = compareAgainst(dvecZ, dvecH, dmatQ, dmatInvQ);
    
        // Compare the two results from evaluation of elsq
        CPPUNIT_ASSERT( fabs(dTestElsqAns-dElsqAns)/fabs(dTestElsqAns) <= 0.001 );

    }
}

static double compareAgainst(
            const DoubleMatrix &dvecZ,     // column vector
            const DoubleMatrix &dvecH,     // column vector
            const DoubleMatrix &dmatQ,     // symmetric matrix Q(x)
            const DoubleMatrix &dmatInvQ   // inverse of Q
            )
{

    assert( dvecZ.nc() == 1 );
    assert( dvecH.nc() == 1 );
    assert( dmatQ.nr() == dmatQ.nc() );
    assert( hasPosDet( dmatQ ) );

    DoubleMatrix dmatQ2PI( dmatQ.nr(), dmatQ.nc() );
    
	const double *pQ    = dmatQ.data();
    double *pQ2PI = dmatQ2PI.data();

    for( int i=0; i<dmatQ.nr()*dmatQ.nc(); i++ )
        pQ2PI[i] = pQ[i] * 2.0 * PI;

    // Compute det(Q2PI) = b * 2^c .
    double b;
    long c;
    det( dmatQ2PI , &b, &c );

    // Compute log(det(Q2PI)).
    double dTerm1 = log( b ) + c * log( 2.0 );

    DoubleMatrix  dmatR = subtract( dvecZ, dvecH );

    DoubleMatrix  term2 = multiply(multiply(transpose(dmatR),dmatInvQ), dmatR);
    double        dTerm2 = oneByOneToScalar(term2);

    return (dTerm1 + dTerm2) / 2.0;
}
static double compareAgainst(
            const valarray<double> &z,     // column vector
            const valarray<double> &h,     // column vector
            const valarray<double> &Q,     // symmetric matrix Q(x)
            const valarray<double> &invQ   // inverse of Q
            )
{
    const int n = z.size();

    assert( Q.size() == n * n );
    assert( h.size() == n );

    //assert( hasPosDet( dmatQ ) );

    valarray<double> Q2PI( n * n );
    
    Q2PI = Q * 2.0 * PI;

    // Compute det(Q2PI) = b * 2^c .
    double b;
    long c;
    det( Q2PI, n, &b, &c );

    // Compute log(det(Q2PI)).
    double dTerm1 = log( b ) + c * log( 2.0 );

    valarray<double>  residual = z - h;

    valarray<double>  term2 = multiply( multiply( transpose( residual, n ), n, invQ, n ), n, residual, 1 );
    double        dTerm2 = term2[0];

    return (dTerm1 + dTerm2) / 2.0;
}

/******************************************************************************************
 *
 *
 *               (Static) functions
 *
 *
 ******************************************************************************************/

static double oneByOneToScalar( const DoubleMatrix &dmatA ){
    assert( dmatA.nr() == 1 );
    assert( dmatA.nc() == 1 );

    return (dmatA.data())[0];
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

static bool hasPosDet( const valarray<double>& A, int n )
{
  // Compute b and c such that det(A) = b * 2^c.
  double b;
  long c;
  det( A , n, &b, &c );
  return ( b > 0.0 );
}
