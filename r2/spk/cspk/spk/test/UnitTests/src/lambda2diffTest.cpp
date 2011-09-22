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
 * File: lamda2diffTest.cpp
 *
 *
 * Unit test for lamda2diffTest.
 *
 * Author: Sachiko Honda + Mitch Watrous
 *
 *************************************************************************/
#include <cmath>
#include <cfloat>
#include <iomanip>
#include <cassert>
#include <string>
#include <iostream>
#include <fstream>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/lambda2diff.h"
#include "../../../spk/subtract.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/inverse.h"
#include "../../../spk/matabs.h"
#include "../../../spk/matmax.h"
#include "../../../spk/identity.h"
#include "../../../spk/isGreaterThanOrEqualTo.h"
#include "../../../spk/isLessThanOrEqualTo.h"
#include "../../../spk/allTrue.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/allZero.h"
#include "../../../spk/SpkValarray.h"

#include "lambda2diffTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void lambda2diffTest::setUp()
{
    // initializations
}
void lambda2diffTest::tearDown()
{
    // clean up
}

Test* lambda2diffTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite("lambda2diffTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<lambda2diffTest>(
                         "twoByTwoTest", &lambda2diffTest::twoByTwoTest));
    suiteOfTests->addTest(new TestCaller<lambda2diffTest>(
                         "noTruncationErrorsTest", &lambda2diffTest::noTruncationErrorsTest));

    return suiteOfTests;
}

/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

static bool epsEqualTest( const DoubleMatrix &dmatX, 
                          const DoubleMatrix &dmatY, 
                          const DoubleMatrix &dmatScale, 
                          std::string xName,
                          std::string yName );
/*************************************************************************
 *
 * twoByTwoTest
 *
 *
 * This test is very similar to the test in the specification example.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Model for twoByTwoTest
 *------------------------------------------------------------------------*/

class UserModelLambda2diffTwoByTwoTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int          _i;
    const int _nA;
    const int _nB;
    const int _nY;


public:
    UserModelLambda2diffTwoByTwoTest(int nA, int nB, int nY) 
      : _nA(nA), _nB(nB), _nY(nY), _a(nA), _b(nB) {};
    ~UserModelLambda2diffTwoByTwoTest(){};

private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
      //
      // D(alp) = [ alp(1)  0.0    ]
      //          [ 0.0     alp(1) ]
      //
      ret.resize(_nB * _nB);

      ret[0] = _a[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = _a[0];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
      //
      // D_alp(alp) = [ 1.0  0.0 ]
      //              [ 0.0  0.0 ]
      //              [ 0.0  0.0 ]
      //              [ 1.0  0.0 ]
      //
      ret.resize(_nB * _nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;

      ret[0] = 1;
      ret[3] = 1;

      return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      //
      // D(alp)^(-1) = [ 1.0 / alp(1)       0.0    ]
      //               [ 0.0          1.0 / alp(1) ]
      //
      ret.resize(_nB * _nB);

      ret[0] = 1.0 / _a[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / _a[0];
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
      //
      // D^(-1)_alp(alp) = [ -1.0 / alp(1)^2  0.0 ]
      //                   [  0.0             0.0 ]
      //                   [  0.0             0.0 ]
      //                   [ -1.0 / alp(1)^2  0.0 ]
      //
      ret.resize(_nB * _nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / ( _a[0] * _a[0] );
      ret[3] = -1.0 / ( _a[0] * _a[0] );

      return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      // 
      // f(alp,b) = [ alp(2) + b(2) ]
      //            [ alp(2) + b(2) ]
      //
      ret.resize(_nY);
      ret[0] = _a[1]+ _b[1];
      ret[1] = _a[1]+ _b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
      //
      // f_alp(alp,b) = [ 0.0   1.0 ]
      //                [ 0.0   1.0 ]
      //
      ret.resize(_nY * _nA);
      ret[0] = 0;
      ret[1] = 0;
      ret[2] = 1;
      ret[3] = 1;
      return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // f_b(alp,b)   = [ 0.0   1.0 ]
      //                [ 0.0   1.0 ]
      //
      ret.resize(_nY * _nB);
      ret[0] = 0;
      ret[1] = 0;
      ret[2] = 1;
      ret[3] = 1;
      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // R(alp, b)  = [  b(1)  0.0  ]
      //              [  0.0   b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = _b[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = _b[0];
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
      //
      // R_alp(alp,b) = [ 0.0  0.0 ]
      //                [ 0.0  0.0 ]
      //                [ 0.0  0.0 ]
      //                [ 0.0  0.0 ]
      //
      ret.resize(_nY * _nY * _nA);
      for( int i=0; i<_nY*_nY*_nA; i++ )
        ret[i] = 0.0;
      return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // R_b(alp,b)   = [ 1.0  0.0 ]
      //                [ 0.0  0.0 ]
      //                [ 0.0  0.0 ]
      //                [ 1.0  0.0 ]
      //
      ret.resize(_nY * _nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;
      ret[0] = 1;
      ret[3] = 1;
      return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // R(alp, b)^(-1)  = [  1.0  / b(1)        0.0  ]
      //                   [  0.0          1.0 / b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = 1.0 / _b[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / _b[0];
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // R^(-1)_b(alp,b)   = [ -1.0  / b(1)^2  0.0 ]
      //                     [ 0.0             0.0 ]
      //                     [ 0.0             0.0 ]
      //                     [ -1.0  / b(1)^2  0.0 ]
      //
      ret.resize(_nY  *_nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;
      ret[0] = -1.0 / ( _b[0] * _b[0] );
      ret[3] = -1.0 / ( _b[0] * _b[0] );
      return true;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
      //
      // R^(-1)_alp(alp,b) = [ 0.0  0.0 ]
      //                     [ 0.0  0.0 ]
      //                     [ 0.0  0.0 ]
      //                     [ 0.0  0.0 ]
      //
      ret.resize(_nY * _nY * _nA);
      for( int i=0; i<_nY*_nY*_nA; i++ )
        ret[i] = 0.0;
      return false;
    }
};

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void lambda2diffTest::twoByTwoTest()
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;
    cout << setiosflags(ios::scientific) << setprecision(15);


    //------------------------------------------------------------
    // Define the problem.
    //------------------------------------------------------------

    const int n = 2;
    const int nB   = n;
    const int nAlp = n;
    const int nY   = n;
    UserModelLambda2diffTwoByTwoTest model(nAlp, nB, nY);
    const double step = 0.0001;
    int i;
    double absTol = 50.0 * step * step;
    double tol;

    DoubleMatrix dvecY    (n,1);
    DoubleMatrix dvecAlp  (n,1);
    DoubleMatrix dvecB    (n,1);
    DoubleMatrix dvecBStep(n,1);

    dvecAlp  .fill(1.0);
    dvecY    .fill(1.0);
    dvecB    .fill(1.0);
    dvecBStep.fill(step);

    DoubleMatrix dmatLambda_b_bOut(nB, nB);
    DoubleMatrix dmatLambda_b_b_alpOut(nB*nB, nAlp);
    DoubleMatrix dmatLambda_b_b_bOut(nB*nB, nB);


    //------------------------------------------------------------
    // Test lambda_b_b.
    //------------------------------------------------------------

    DoubleMatrix dmatLambda_b_bCorrect(n,n);
    dmatLambda_b_bCorrect.data()[0] =  2.0;
    dmatLambda_b_bCorrect.data()[1] = -2.0;
    dmatLambda_b_bCorrect.data()[2] = -2.0;
    dmatLambda_b_bCorrect.data()[3] =  3.0;

    try{
        lambda2diff(model, dvecY, dvecAlp, dvecB, dvecBStep, 
            &dmatLambda_b_bOut, 0, 0, true );
    }
    catch(...)
    {
      CPPUNIT_ASSERT(false);
    }

	tol = 0;
    for(i=0; i<n*n; i++)
    {
		tol += fabs(dmatLambda_b_bCorrect.data()[i]);
	}
    tol *= absTol/n/n;

    for(i=0; i<n*n; i++)
    {
		if( dmatLambda_b_bCorrect.data()[i] || dmatLambda_b_bOut.data()[i] )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_bCorrect.data()[i], dmatLambda_b_bOut.data()[i], tol);
    }

    //------------------------------------------------------------
    // Test lambda_b_b_alp.
    //------------------------------------------------------------

    DoubleMatrix dmatLambda_b_b_alpCorrect(n*n,n);
    double* pdLambda_b_b_alpCorrect=dmatLambda_b_b_alpCorrect.data();
    pdLambda_b_b_alpCorrect[0] = -1.0;
    pdLambda_b_b_alpCorrect[1] =  0.0;
    pdLambda_b_b_alpCorrect[2] =  0.0;
    pdLambda_b_b_alpCorrect[3] = -1.0;
    pdLambda_b_b_alpCorrect[4] =  4.0;
    pdLambda_b_b_alpCorrect[5] = -2.0;
    pdLambda_b_b_alpCorrect[6] = -2.0;
    pdLambda_b_b_alpCorrect[7] =  0.0;

    try{
        lambda2diff(model, dvecY, dvecAlp, dvecB, dvecBStep, 
            0, &dmatLambda_b_b_alpOut, 0, true );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    tol = 0;
    for(i=0; i<n*n*n; i++)
    {
		tol += fabs(dmatLambda_b_b_alpCorrect.data()[i]);
	}
    tol *= absTol/n/n/n;

    for(i=0; i<n*n*n; i++)
    {
	    if( dmatLambda_b_b_alpCorrect.data()[i] || dmatLambda_b_b_alpOut.data()[i] )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_b_alpCorrect.data()[i], dmatLambda_b_b_alpOut.data()[i], tol );
    }

    //------------------------------------------------------------
    // Test lambda_b_b_b.
    //------------------------------------------------------------

    DoubleMatrix dmatLambda_b_b_bCorrect(n*n,n);
    double* pdLambda_b_b_bCorrect=dmatLambda_b_b_bCorrect.data();
    pdLambda_b_b_bCorrect[0] = -4.0;
    pdLambda_b_b_bCorrect[1] =  4.0;
    pdLambda_b_b_bCorrect[2] =  4.0;
    pdLambda_b_b_bCorrect[3] = -2.0;
    pdLambda_b_b_bCorrect[4] =  4.0;
    pdLambda_b_b_bCorrect[5] = -2.0;
    pdLambda_b_b_bCorrect[6] = -2.0;
    pdLambda_b_b_bCorrect[7] =  0.0;

    try{
        lambda2diff(model, dvecY, dvecAlp, dvecB, dvecBStep, 
            0, 0, &dmatLambda_b_b_bOut, true );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

	tol = 0;
    for(i=0; i<n*n*n; i++)
    {
		tol += fabs(dmatLambda_b_b_bCorrect.data()[i]);
	}
    tol *= absTol/n/n/n;

    for(i=0; i<n*n*n; i++)
    {
		if( dmatLambda_b_b_bCorrect.data()[i] || dmatLambda_b_b_bOut.data()[i] )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_b_bCorrect.data()[i], dmatLambda_b_b_bOut.data()[i], tol );
    }
}



/*************************************************************************
 *
 * Function: noTruncationErrorsTest
 *
 *
 * This test defines the model evaluation functions f, R, and D such
 * that no truncation problems when computing the finite difference
 * approximations for the derivatives, i.e., so that there are no 
 * higher order terms in the Taylor expansion of lambda(alp, b) that
 * are neglected by the derivative approximations.
 *
 * Specifically, this test uses the following form for lambda(alp,b):
 *
 *                       3                3
 *     lambda(alp, b) =  -  log(2 PI)  -  -  log[ alp    ] 
 *                       2                2          (1)
 *
 *
 *                                  -                            -  2
 *                       3         |                              |
 *                    +  -  alp    |  b    +  b    +  b     -  1  |    .
 *                       2     (1) |   (1)     (2)     (3)        |
 *                                  -                            -
 *
 * It accomplishes this by setting 
 *
 *                         T
 *     y     =  ( 1, 1, 1 )   ,
 *
 *
 *     f   (b)  =  b    +  b    +  b     , for j = 1, 2, 3.
 *      (j)         (1)     (2)     (3)
 *
 *                     /
 *                    |     1 / alp    , for j = k,
 *                    |            (1)
 *     R(alp, b)  =  <
 *                    |     0 ,          for j != k,
 *                    |
 *                     \
 *
 * and by dropping the D terms from the expression for lambda(alp, b).
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Local Class Declarations
 *------------------------------------------------------------------------*/

class UserModelLambda2diffNoTruncationErrorsTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int          _i;

    //---------------------------------------------------
    // Set
    //                          T
    //      y     =  ( 1, 1, 1 )   ,
    // 
    //---------------------------------------------------

    const int _nY;
    const int _nA;
    const int _nB;

public:
  UserModelLambda2diffNoTruncationErrorsTest( int nA, int nB, int nY )
    : _nA(nA), _nB(nB), _nY(nY) {};   
    ~UserModelLambda2diffNoTruncationErrorsTest(){};

    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a.resize( aval.size() );
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
      _b.resize( bval.size() );
      _b = bval;
    }

    //---------------------------------------------------
    // Set 
    //
    //     D  =  I    .
    //            nB
    //
    //---------------------------------------------------

    void doIndParVariance( valarray<double>& ret ) const
    {
      ret.resize(_nB * _nB);
      for( int j=0; j<_nB; j++ )
      {
        for( int i=0; i<_nB; i++ )
        {
          ret[i+j*_nB] = (i==j? 1.0 : 0.0);
        }
      }
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        ret.resize( _nB * _nB * _nA );
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    void doIndParVarianceInv(valarray<double>& ret ) const
    {
      ret.resize(_nB * _nB);
      for( int j=0; j<_nB; j++ )
      {
        for( int i=0; i<_nB; i++ )
        {
          ret[i+j*_nB] = (i==j? 1.0 : 0.0);
        }
      }
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        ret.resize( _nB * _nB * _nA );
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }

    //---------------------------------------------------
    // Set
    // 
    //     f   (b)  =  b    +  b    +  b     , for j = 1, 2, 3.
    //      (j)         (1)     (2)     (3)
    //
    //---------------------------------------------------

    void doDataMean( valarray<double>& ret ) const
    {
        ret.resize( _nY );
        int j;
        int k;
        for ( j = 0; j < _nY; j++ )
        {
          ret[j] = 0.0;
          for ( k = 0; k < _nB; k++ )
          {
            ret[j] += _b[k];
          }
        }
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        ret.resize( _nY * _nA );
        for( int i=0; i<_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        ret.resize(_nY * _nB);
        for( int j=0; j<_nB; j++ )
        {
          for( int i=0; i<_nY; i++ )
            ret[i+j*_nY] = (i==j? 1.0 : 0.0);
        }
        return true; 
    }

    //---------------------------------------------------
    // Set
    //                     /
    //                    |     1 / alp    , for j = k,
    //                    |            (1)
    //     R(alp, b)  =  <
    //                    |     0 ,          for j != k,
    //                    |
    //                     \
    // 
    //---------------------------------------------------

    void doDataVariance( valarray<double>& ret ) const
    {
      ret.resize(_nY * _nY);
      for( int j=0; j<_nY; j++ )
      {
        for( int i=0; i<_nY; i++ )
        {
          ret[i+j*_nY] = (i==j? 1.0 / _a[0] : 0.0);
        }
      }
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        ret.resize( _nY * _nY * _nA );
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;

        ret[( 0 * _nY + 0 ) + 0 * ( _nY * _nY )] = -1.0 / (  _a[0] *  _a[0] ); 
        ret[( 1 * _nY + 1 ) + 0 * ( _nY * _nY )] = -1.0 / (  _a[0] *  _a[0] ); 
        ret[( 2 * _nY + 2 ) + 0 * ( _nY * _nY )] = -1.0 / (  _a[0] *  _a[0] ); 
        return true;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        ret.resize( _nY * _nY * _nB );
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      ret.resize(_nY * _nY);
      for( int j=0; j<_nY; j++ )
      {
        for( int i=0; i<_nY; i++ )
        {
          ret[i+j*_nY] = (i==j?  _a[0] : 0.0);
        }
      }
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        ret.resize(_nY * _nY * _nB);
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;

        ret[0] = 1;
        ret[3] = 1;
        return true;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        ret.resize(_nY * _nY * _nA);
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
};
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

 void lambda2diffTest::noTruncationErrorsTest()
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;
    cout << setiosflags(ios::scientific) << setprecision(15);


    //------------------------------------------------------------
    // Define the problem.
    //------------------------------------------------------------

    const bool withD = false;
    const int  nY    = 3;
    const int nB   = nY;
    const int nAlp = 1;
    
    UserModelLambda2diffNoTruncationErrorsTest model( nAlp, nB, nY );

    int i;

    DoubleMatrix dvecY    ( nY,   1 );
    DoubleMatrix dvecAlp  ( nAlp, 1 );
    DoubleMatrix dvecB    ( nB,   1 );
    DoubleMatrix dvecBStep( nB,   1 );

    dvecY.fill  ( 1.0 );
    dvecAlp.fill( 2.0 );
    dvecB.fill  ( 3.0 );

    //------------------------------------------------------------
    // Compute the derivatives of Lambda.
    //------------------------------------------------------------

    DoubleMatrix dmatLambda_b_bOut    ( nB,    nB );
    DoubleMatrix dmatLambda_b_b_alpOut( nB*nB, nAlp );
    DoubleMatrix dmatLambda_b_b_bOut  ( nB*nB, nB );

    double step = 0.0001;
    dvecBStep.fill( step );

    try{  
        lambda2diff(model, 
          dvecY, 
          dvecAlp, 
          dvecB, 
          dvecBStep, 
          &dmatLambda_b_bOut, 
          &dmatLambda_b_b_alpOut, 
          &dmatLambda_b_b_bOut, 
          withD );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    // The errors in the second derivative approximations should 
    // be of the same magnitude as the step size squared. 
    double absTol = 50.0 * step * step;
    double tol;

    //------------------------------------------------------------
    // Test lambda_b_b.
    //------------------------------------------------------------

    // Compute the analytic value,
    //
    //                                -         -
    //      ~  ~                     |  A  A  A  |
    //      d  d  lambda(alp, b)  =  |  A  A  A  |  ,
    //       b  b                    |  A  A  A  |
    //                                -         -
    // where 
    //
    //      A  =  3 alp   .
    //                 1
    //
    DoubleMatrix dmatLambda_b_bCorrect( nB, nB );
    double* alp = dvecAlp.data();
    dmatLambda_b_bCorrect.fill( 3.0 * alp[0] );

    // Compute the scale so that the test in isDmatEpsEqual will
    // correspond to an absolute tolerance test.
    DoubleMatrix dmatScale1( nB, nB );
    dmatScale1.fill( absTol / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

    for( i=0; i<nB*nB; i++ )
    {
        tol = dmatScale1.data()[i];
        CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_bCorrect.data()[i], dmatLambda_b_bOut.data()[i], tol);
    }

    //------------------------------------------------------------
    // Test lambda_b_b_alp.
    //------------------------------------------------------------

    // Compute the analytic value,
    //
    //      ~  ~                                           T
    //      d  d  d    lambda(alp, b)  =  ( 3, 3, ... , 3 )   .
    //       b  b  alp
    //
    // where there are nB * nB elements in this vector.
    DoubleMatrix dmatLambda_b_b_alpCorrect( nB * nB, nAlp );
    dmatLambda_b_b_alpCorrect.fill( 3.0 );

    // Compute the scale so that the test in isDmatEpsEqual will
    // correspond to an absolute tolerance test.
    //
    // Sachiko
    // Since this is 3rd order approximation, it should tolerate more than the previous 2nd order.
    // Took out DBL_EPS_EQUAL_MULT from denominator.
    DoubleMatrix dmatScale2( nB * nB, nAlp );
    dmatScale2.fill( absTol / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON )  );

    for( i=0; i<nB*nB*nAlp; i++ )
    {
        tol = dmatScale2.data()[i];
        CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_b_alpCorrect.data()[i], dmatLambda_b_b_alpOut.data()[i], tol);
    }

    //------------------------------------------------------------
    // Test lambda_b_b_b.
    //------------------------------------------------------------

    // Compute the analytic value,
    //
    //                                   -         -
    //      ~  ~                        |  0  0  0  |
    //      d  d  d  lambda(alp, b)  =  |  0  0  0  |  ,
    //       b  b  b                    |  0  0  0  |
    //                                  |  .  .  .  |
    //                                  |  .  .  .  |
    //                                  |  .  .  .  |
    //                                  |  0  0  0  |
    //                                  |  0  0  0  |
    //                                  |  0  0  0  |
    //                                   -         -
    //
    // where there are nB * nB rows in this matrix.
    DoubleMatrix dmatLambda_b_b_bCorrect( nB * nB, nB );
    dmatLambda_b_b_bCorrect.fill( 0.0 );

    // Compute the scale so that the test in isDmatEpsEqual will
    // correspond to an absolute tolerance test.
    //
    // Sachiko
    // Since this is 3rd order approximation, it should tolerate more than the previous 2nd order.
    // Took out DBL_EPS_EQUAL_MULT from denominator.
    DoubleMatrix dmatScale3( nB * nB, nB );
    dmatScale3.fill( absTol / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

    for( i=0; i<nB*nB*nB; i++ )
    {
        tol = fabs(dmatLambda_b_b_bCorrect.data()[i]-dmatLambda_b_b_bOut.data()[i])
                /fabs(dmatLambda_b_b_bCorrect.data()[i])*DBL_EPS_EQUAL_MULT;
        CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatLambda_b_b_bCorrect.data()[i], dmatLambda_b_b_bOut.data()[i], tol);
    }

    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    return;
}


