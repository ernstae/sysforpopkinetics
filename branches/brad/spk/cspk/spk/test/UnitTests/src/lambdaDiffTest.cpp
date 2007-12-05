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
 * File: lambdaDiffTest.cpp
 *
 *
 * Unit test for lambdaDiff.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include "Test.h"
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "DoubleMatrix.h"
#include "lambdaDiff.h"
#include "lambdaDiffTest.h"

#include <iostream>
#include <string>
#include <cmath>
#include <cfloat>

#include "SpkValarray.h"
#include "lambda.h"
#include "lambdaDiff.h"
#include "inverse.h"
#include "subtract.h"
#include "namespace_population_analysis.h"
#include "SpkModel.h"
#include "multiply.h"
#include "DBL_EPS_EQUAL_MULT.h"
#include "DoubleMatrix.h"
#include "isDblEpsEqual.h"
#include "identity.h"
#include "allZero.h"

using SPK_VA::valarray;

static const int _nB   = 2;
static const int _nAlp = 2;
static const int _nY   = 2;
static const int _n    = 2;

static const DoubleMatrix elsq_x_x( 
    const DoubleMatrix &dvecX,          // parameter
    const DoubleMatrix &dvecZ,          // data
    const DoubleMatrix &dvecH,          // h(x)
    const DoubleMatrix &dmatH_x,    
    const DoubleMatrix &dmatH_x_x,
    const DoubleMatrix &dmatQ,          // Q(x)
    const DoubleMatrix &dmatQ_x,
    const DoubleMatrix &dmatQinv,       // Q(x)inv
    const DoubleMatrix &dmatQinv_x,
    const DoubleMatrix &dmatQinv_x_x
    );

static const DoubleMatrix getZeroMatrix( const int m, const int n )
{
    DoubleMatrix zero(m, n);
    zero.fill(0.0);
    return zero;
}
static const DoubleMatrix funAlp_alp(    const DoubleMatrix &dvecAlp );
static const DoubleMatrix funAlp_alp_alp(const DoubleMatrix &dvecAlp );
static const DoubleMatrix funAlp_alp_b(  const DoubleMatrix &dvecAlp );
static const DoubleMatrix funAlp_b(      const DoubleMatrix &dvecAlp );
static const DoubleMatrix funAlp_b_alp(  const DoubleMatrix &dvecAlp );
static const DoubleMatrix funAlp_b_b(    const DoubleMatrix &dvecAlp );

static const DoubleMatrix funB_alp(     const DoubleMatrix &dvecB );
static const DoubleMatrix funB_alp_alp( const DoubleMatrix &dvecB );
static const DoubleMatrix funB_alp_b(   const DoubleMatrix &dvecB );
static const DoubleMatrix funB_b(       const DoubleMatrix &dvecB );
static const DoubleMatrix funB_b_alp(   const DoubleMatrix &dvecB );
static const DoubleMatrix funB_b_b(     const DoubleMatrix &dvecB );

static const DoubleMatrix funD(         const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_alp(     const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_alp_alp( const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_alp_b(   const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_b(       const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_b_alp(   const DoubleMatrix &dvecAlp );
static const DoubleMatrix funD_b_b(     const DoubleMatrix &dvecAlp );

static const DoubleMatrix funDinv(         const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_alp(     const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_alp_alp( const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_alp_b(   const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_b(       const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_b_alp(   const DoubleMatrix &dvecAlp );
static const DoubleMatrix funDinv_b_b(     const DoubleMatrix &dvecAlp );

static const DoubleMatrix funF(        const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_alp(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_alp_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_alp_b(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_b(      const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_b_alp(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funF_b_b(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );

static const DoubleMatrix funR(        const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_alp(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_alp_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_alp_b(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_b(      const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_b_alp(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funR_b_b(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );

static const DoubleMatrix funRinv(        const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_alp(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_alp_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_alp_b(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_b(      const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_b_alp(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );
static const DoubleMatrix funRinv_b_b(    const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB );

/**************************************************************
 *   class UserModel declaration
 **************************************************************/
class lambdaDiffTest::UserModelLambdaDiffTest : public SpkModel<double>
{
    DoubleMatrix _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nY;
public:
    UserModelLambdaDiffTest(int nA, int nB, int nY)
      : _nA(nA), _nB(nB), _nY(nY)
    {};    
    ~UserModelLambdaDiffTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a.fromValarray(aval);
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b.fromValarray(bval);
    }

    void doIndParVariance( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funD(_a);
        retDM.toValarray(ret);
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funD_alp( _a);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funDinv(_a);
        retDM.toValarray(ret);
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funDinv_alp(_a);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funF(_a, _b);
        retDM.toValarray(ret);
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funF_alp(_a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funF_b(_a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funR(_a, _b);
        retDM.toValarray(ret);
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funR_alp(_a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funR_b(_a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    } 
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funRinv(_a, _b);
        retDM.toValarray(ret);
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funRinv_b( _a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        DoubleMatrix retDM = funRinv_alp(_a, _b);
        bool isAllZero = allZero(retDM);
        retDM.toValarray(ret);
        return !isAllZero;
    }

};

void lambdaDiffTest::setUp()
{
    // Use _n=2 for all alp, b and y, though they could have all different sizes
    _model = new UserModelLambdaDiffTest(_nAlp, _nB, _nY);

    //
    // alpha = [1]
    //         [1]
    ///
    _alp = new DoubleMatrix(_nAlp,1);
    _alp->fill(1.0);

    //
    // b = [1]
    //     [1]
    //
    _b = new DoubleMatrix(_nB,1);
    _b->fill(1.0);

    //
    // _y = [1]
    //      [1]
    //
    _y = new DoubleMatrix(_nY,1);
    _y->fill(1.0);

    double dtemp;
    int i;
    
    _alpStep = new DoubleMatrix(_nAlp,1);
    for( i=0; i<_nAlp; i++ ){
        dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + _alp->data()[i];
        _alpStep->data()[i] = 10.0 * ( dtemp - _alp->data()[i] );
    }

    _bStep = new DoubleMatrix(_nB,1);
    for( i=0; i<_nB; i++ ){
        dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + _b->data()[i];
        _bStep->data()[i] = 10.0 * ( dtemp -_b->data()[i] );
    }

}
void lambdaDiffTest::tearDown()
{
    delete _model;
    delete _alp;
    delete _b;
    delete _alpStep;
    delete _bStep;
    delete _y;
}

Test* lambdaDiffTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite;

    suiteOfTests->addTest(new TestCaller<lambdaDiffTest>("testLocalfunctions", testLocalfunctions));
    suiteOfTests->addTest(new TestCaller<lambdaDiffTest>("testLambda_alp_xWithD", testLambda_alp_xWithD));
    suiteOfTests->addTest(new TestCaller<lambdaDiffTest>("testLambda_alp_xWithoutD", testLambda_alp_xWithoutD));
    suiteOfTests->addTest(new TestCaller<lambdaDiffTest>("testLambda_b_xWithD", testLambda_b_xWithD));
    suiteOfTests->addTest(new TestCaller<lambdaDiffTest>("testLambda_b_xWithoutD", testLambda_b_xWithoutD));
    return suiteOfTests;
}
/*********************************************
 * test local functions
 *********************************************/
void lambdaDiffTest::testLocalfunctions()
{
    int i;

    DoubleMatrix dvecB        = *_b;
        assertDoublesEqual(_nB, dvecB.nr(), 0);
        assertDoublesEqual(1,   dvecB.nc(), 0);
        const double* pB      = dvecB.data();
        for( i=0; i<_nB; i++ )
            assertDoublesEqual(1.0, pB[i], 0.0);

    DoubleMatrix dvecAlp        = *_alp;
        assertDoublesEqual(_nAlp, dvecAlp.nr(), 0);
        assertDoublesEqual(1,     dvecAlp.nc(), 0);
        const double* pAlp      = dvecAlp.data();
        for( i=0; i<_nAlp; i++ )
            assertDoublesEqual(1.0, pAlp[i], 0);

    DoubleMatrix dvecY        = *_y;
        assertDoublesEqual(_nY, dvecY.nr(), 0);
        assertDoublesEqual(1,   dvecY.nc(), 0);
        const double* pY      = dvecY.data();
        for( i=0; i<_nY; i++ )
            assertDoublesEqual(1.0, pY[i], 0.0);

    DoubleMatrix dmatB_alp      = funB_alp(dvecB); // 2 by 2
        assertDoublesEqual(_nB,   dmatB_alp.nr(), 0);
        assertDoublesEqual(_nAlp, dmatB_alp.nc(), 0);
        const double *pB_alp  =   dmatB_alp.data();
        assertDoublesEqual( 0.0, pB_alp[0], 0.0 );
        assertDoublesEqual( 0.0, pB_alp[1], 0.0 );
        assertDoublesEqual( 0.0, pB_alp[2], 0.0 );
        assertDoublesEqual( 0.0, pB_alp[3], 0.0 );

    DoubleMatrix dmatB_alp_alp       = funB_alp_alp(dvecB); // 4 by 2
        assertDoublesEqual(_nB*_nAlp, dmatB_alp_alp.nr(), 0);
        assertDoublesEqual(_nAlp,     dmatB_alp_alp.nc(), 0);
        const double *pB_alp_alp    = dmatB_alp_alp.data();
        assertDoublesEqual( 0.0, pB_alp_alp[0], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[1], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[2], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[3], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[4], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[5], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[6], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_alp[7], 0.0 );

    DoubleMatrix dmatB_alp_b        = funB_alp_b( dvecB ); // 4 by 2
        assertDoublesEqual(_nB*_nAlp, dmatB_alp_b.nr(), 0);
        assertDoublesEqual(_nB,       dmatB_alp_b.nc(), 0);
        const double *pB_alp_b      = dmatB_alp_alp.data();
        assertDoublesEqual( 0.0, pB_alp_b[0], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[1], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[2], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[3], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[4], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[5], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[6], 0.0 );
        assertDoublesEqual( 0.0, pB_alp_b[7], 0.0 );

    DoubleMatrix dvecF       = funF(dvecAlp, dvecB);
        assertDoublesEqual(_nY,dvecF.nr(), 0);
        assertDoublesEqual(1,  dvecF.nc(), 0);
        const double * pF    = dvecF.data();
        assertDoublesEqual( pAlp[1] + pB[1], pF[0], 0 );
        assertDoublesEqual( pAlp[1] + pB[1], pF[1], 0 );

    DoubleMatrix dmatF_alp      = funF_alp(dvecAlp, dvecB); // 2 by 2
        assertDoublesEqual(_nY,   dmatF_alp.nr(), 0);
        assertDoublesEqual(_nAlp, dmatF_alp.nc(), 0);
        const double * pF_alp   = dmatF_alp.data();
        assertDoublesEqual( 0.0, pF_alp[0], 0 );
        assertDoublesEqual( 0.0, pF_alp[1], 0 );
        assertDoublesEqual( 1.0, pF_alp[2], 0 );
        assertDoublesEqual( 1.0, pF_alp[3], 0 );

    DoubleMatrix dmatF_alp_alp      = funF_alp_alp(dvecAlp, dvecB ); // 4 by 2
        assertDoublesEqual(_nY*_nAlp, dmatF_alp_alp.nr(), 0);
        assertDoublesEqual(_nAlp,     dmatF_alp_alp.nc(), 0);
        const double * pF_alp_alp   = dmatF_alp_alp.data();
        assertDoublesEqual( 0.0, pF_alp_alp[0], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[1], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[2], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[3], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[4], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[5], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[6], 0 );
        assertDoublesEqual( 0.0, pF_alp_alp[7], 0 );

    DoubleMatrix dmatF_alp_b        = funF_alp_b(dvecAlp, dvecB ); // 4 by 2
        assertDoublesEqual(_nY*_nAlp, dmatF_alp_b.nr(), 0);
        assertDoublesEqual(_nB,       dmatF_alp_b.nc(), 0);
        const double * pF_alp_b     = dmatF_alp_b.data();
        assertDoublesEqual( 0.0, pF_alp_b[0], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[1], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[2], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[3], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[4], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[5], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[6], 0 );
        assertDoublesEqual( 0.0, pF_alp_b[7], 0 );

    DoubleMatrix dmatR        = funR(dvecAlp, dvecB);
        assertDoublesEqual(_nY, dmatR.nr(), 0);
        assertDoublesEqual(_nY, dmatR.nc(), 0);
        const double *pR      = dmatR.data();
        assertDoublesEqual( pB[0], pR[0], 0 );
        assertDoublesEqual( 0.0,   pR[1], 0 );
        assertDoublesEqual( 0.0,   pR[2], 0 );
        assertDoublesEqual( pB[0], pR[3], 0 );

    DoubleMatrix dmatR_b          = funR_b( dvecAlp, dvecB );
        assertDoublesEqual(_nY*_nY, dmatR_b.nr(), 0);
        assertDoublesEqual(_nB,     dmatR_b.nc(), 0);
        const double* pR_b        = dmatR_b.data();
        assertDoublesEqual( 1.0, pR_b[0], 0 );
        assertDoublesEqual( 0.0, pR_b[1], 0 );
        assertDoublesEqual( 0.0, pR_b[2], 0 );
        assertDoublesEqual( 1.0, pR_b[3], 0 );
        assertDoublesEqual( 0.0, pR_b[4], 0 );
        assertDoublesEqual( 0.0, pR_b[5], 0 );
        assertDoublesEqual( 0.0, pR_b[6], 0 );
        assertDoublesEqual( 0.0, pR_b[7], 0 );

    DoubleMatrix dmatR_alp        = funR_alp(dvecAlp, dvecB ); // 4 by 2
        assertDoublesEqual(_nY*_nY, dmatR_alp.nr(), 0);
        assertDoublesEqual(_nAlp,   dmatR_alp.nc(), 0);
        const double *pR_alp      = dmatR_alp.data();
        assertDoublesEqual( 0.0, pR_alp[0], 0 );
        assertDoublesEqual( 0.0, pR_alp[1], 0 );
        assertDoublesEqual( 0.0, pR_alp[2], 0 );
        assertDoublesEqual( 0.0, pR_alp[3], 0 );
        assertDoublesEqual( 0.0, pR_alp[4], 0 );
        assertDoublesEqual( 0.0, pR_alp[5], 0 );
        assertDoublesEqual( 0.0, pR_alp[6], 0 );
        assertDoublesEqual( 0.0, pR_alp[7], 0 );

    DoubleMatrix dmatRinv     = funRinv(dvecAlp, dvecB);
        assertDoublesEqual(_nY, dmatRinv.nr(), 0);
        assertDoublesEqual(_nY, dmatRinv.nc(), 0);
        const double *pRInv   = dmatRinv.data();
        double div            = pR[0]*pR[3] - pR[1]*pR[2];
        assertImplementation(div != 0, "ad-bc != 0", __LINE__, __FILE__);
        assertDoublesEqual(  pR[3] / div, pRInv[0], 0 );   // b[0]^(-1)
        assertDoublesEqual( -pR[2],       pRInv[1], 0 );
        assertDoublesEqual( -pR[1],       pRInv[2], 0 );
        assertDoublesEqual(  pR[0] / div, pRInv[3], 0 );   // b[0]^(-1)

    DoubleMatrix dmatRinv_b       = funRinv_b(dvecAlp, dvecB);
        assertDoublesEqual(_nY*_nY, dmatRinv_b.nr(), 0); 
        assertDoublesEqual(_nB,     dmatRinv_b.nc(), 0);   
        const double* pRinv_b     = dmatRinv_b.data();
        assertDoublesEqual( -1.0 / (pB[0]*pB[0]), pRinv_b[0], 0 );  // -b[0]^(-2)
        assertDoublesEqual( 0.0,                  pRinv_b[1], 0 );
        assertDoublesEqual( 0.0,                  pRinv_b[2], 0 );
        assertDoublesEqual( -1.0 / (pB[0]*pB[0]), pRinv_b[3], 0 );  // -b[0]^(-2)
        assertDoublesEqual( 0.0,                  pRinv_b[4], 0 );
        assertDoublesEqual( 0.0,                  pRinv_b[5], 0 );
        assertDoublesEqual( 0.0,                  pRinv_b[6], 0 );
        assertDoublesEqual( 0.0,                  pRinv_b[7], 0 );

    DoubleMatrix dmatRinv_b_alp       = funRinv_b_alp( dvecAlp, dvecB );
        assertDoublesEqual(_nY*_nY*_nB, dmatRinv_b_alp.nr(), 0); 
        assertDoublesEqual(_nAlp,       dmatRinv_b_alp.nc(), 0);   
        const double* pRinv_b_alp     = dmatRinv_b_alp.data();
        assertDoublesEqual( 0.0,                       pRinv_b_alp[4],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[1],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[2],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[4],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[4],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[5],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[6],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[7],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[8],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[9],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[10], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[11], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[12], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[13], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[14], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_alp[15], 0 );

    DoubleMatrix dmatRinv_b_b         = funRinv_b_b( dvecAlp, dvecB );
        assertDoublesEqual(_nY*_nY*_nB, dmatRinv_b_b.nr(), 0); 
        assertDoublesEqual(_nB,         dmatRinv_b_b.nc(), 0);   
        const double* pRinv_b_b       = dmatRinv_b_b.data();
        assertDoublesEqual( 2.0 / (pB[0]*pB[0]*pB[0]), pRinv_b_b[0],  0 );  // 2*b[0]^(-3)
        assertDoublesEqual( 0.0,                       pRinv_b_b[1],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[2],  0 );
        assertDoublesEqual( 2.0 / (pB[0]*pB[0]*pB[0]), pRinv_b_b[3],  0 );  // 2*b[0]^(-2)
        assertDoublesEqual( 0.0,                       pRinv_b_b[4],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[5],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[6],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[7],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[8],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[9],  0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[10], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[11], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[12], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[13], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[14], 0 );
        assertDoublesEqual( 0.0,                       pRinv_b_b[15], 0 );

    DoubleMatrix dmatRinv_alp     = funRinv_alp(dvecAlp, dvecB); // 4 by 2
        assertDoublesEqual(_nY*_nY, dmatRinv_alp.nr(), 0);
        assertDoublesEqual(_nAlp,   dmatRinv_alp.nc(), 0);
        const double *pRinv_alp   = dmatRinv_alp.data();
        assertDoublesEqual( 0.0, pRinv_alp[0], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[1], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[2], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[3], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[4], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[5], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[6], 0 );
        assertDoublesEqual( 0.0, pRinv_alp[7], 0 );

    DoubleMatrix dmatRinv_alp_alp       = funRinv_alp_alp( dvecAlp, dvecB ); // 8 by 2
        assertDoublesEqual(_nY*_nY*_nAlp, dmatRinv_alp_alp.nr(), 0);
        assertDoublesEqual(_nAlp,         dmatRinv_alp_alp.nc(), 0);
        const double *pRinv_alp_alp     = dmatRinv_alp_alp.data();
        assertDoublesEqual( 0.0, pRinv_alp_alp[0], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[1], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[2], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[3], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[4], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[5], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[6], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_alp[7], 0 );

    DoubleMatrix dmatRinv_alp_b       = funR_alp_b( dvecAlp, dvecB ); // 8 by 2
        assertDoublesEqual(_nY*_nY*_nB, dmatRinv_alp_b.nr(), 0);
        assertDoublesEqual(_nAlp,       dmatRinv_alp_b.nc(), 0);
        const double *pRinv_alp_b     = dmatRinv_alp_b.data();
        assertDoublesEqual( 0.0, pRinv_alp_b[0], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[1], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[2], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[3], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[4], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[5], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[6], 0 );
        assertDoublesEqual( 0.0, pRinv_alp_b[7], 0 );


    DoubleMatrix dmatD        = funD(dvecAlp);
        assertDoublesEqual(_nB, dmatD.nr(), 0);
        assertDoublesEqual(_nB, dmatD.nc(), 0);
        const double *pD      = dmatD.data();
        assertDoublesEqual(pAlp[0], pD[0], 0);
        assertDoublesEqual(0,       pD[1], 0);
        assertDoublesEqual(0,       pD[2], 0);
        assertDoublesEqual(pAlp[0], pD[3], 0);

    DoubleMatrix dmatDinv     = funDinv(dmatD);
        assertDoublesEqual(_nB, dmatDinv.nr(), 0);
        assertDoublesEqual(_nB, dmatDinv.nc(), 0);
        const double *pDInv    = dmatDinv.data();
        div = pD[0]*pD[3] - pD[1]*pD[2];
        assertImplementation(div != 0, "ad-bc != 0", __LINE__, __FILE__);
        assertDoublesEqual( pD[3] / div, pDInv[0], 0 );
        assertDoublesEqual( -pD[2],      pDInv[1], 0 );
        assertDoublesEqual( -pD[1],      pDInv[2], 0 );
        assertDoublesEqual( pD[0] / div, pDInv[3], 0 );

    DoubleMatrix dmatD_alp        = funD_alp( dvecAlp ); // 4 by 2
        assertDoublesEqual(_nB*_nB, dmatD_alp.nr(), 0);
        assertDoublesEqual(_nAlp,   dmatD_alp.nc(), 0);
        const double *pD_alp      = dmatD_alp.data();
        assertDoublesEqual(1.0, pD_alp[0], 0.0);
        assertDoublesEqual(0.0, pD_alp[1], 0.0);
        assertDoublesEqual(0.0, pD_alp[2], 0.0);
        assertDoublesEqual(1.0, pD_alp[3], 0.0);
    
    DoubleMatrix dmatDinv_alp     = funDinv_alp( dvecAlp ); // 4 by 2
        assertDoublesEqual(_nB*_nB, dmatDinv_alp.nr(), 0);
        assertDoublesEqual(_nAlp,   dmatDinv_alp.nc(), 0);
        const double* pDinv_alp   = dmatDinv_alp.data();
        assertDoublesEqual( -1.0 / (pAlp[0]*pAlp[0]), pDinv_alp[0], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[1], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[2], 0.0);
        assertDoublesEqual( -1.0 / (pAlp[0]*pAlp[0]), pDinv_alp[3], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[4], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[5], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[6], 0.0);
        assertDoublesEqual( 0.0,                      pDinv_alp[7], 0.0);

    DoubleMatrix dmatDinv_alp_alp       = funDinv_alp_alp( dvecAlp );
        assertDoublesEqual(_nB*_nB*_nAlp, dmatDinv_alp_alp.nr(), 0);
        assertDoublesEqual(_nAlp,         dmatDinv_alp_alp.nc(), 0);
        const double* pDinv_alp_alp     = dmatDinv_alp_alp.data();
        assertDoublesEqual( 2.0 / (pAlp[0]*pAlp[0]*pAlp[0]), pDinv_alp_alp[0],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[1],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[2],  0.0 );
        assertDoublesEqual( 2.0 / (pAlp[0]*pAlp[0]*pAlp[0]), pDinv_alp_alp[3],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[4],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[5],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[6],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[7],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[8],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[9],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[10], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[11], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[12], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[13], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[14], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_alp[15], 0.0 );


    DoubleMatrix dmatDinv_alp_b         = funDinv_alp_b( dvecAlp  );
        assertDoublesEqual(_nB*_nB*_nAlp, dmatDinv_alp_b.nr(), 0);
        assertDoublesEqual(_nB  ,         dmatDinv_alp_b.nc(), 0);
        const double* pDinv_alp_b       = dmatDinv_alp_b.data();
        assertDoublesEqual( 0.0,                             pDinv_alp_b[0],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[1],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[2],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[3],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[4],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[5],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[6],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[7],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[8],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[9],  0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[10], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[11], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[12], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[13], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[14], 0.0 );
        assertDoublesEqual( 0.0,                             pDinv_alp_b[15], 0.0 );

    DoubleMatrix dmatB_b      = funB_b( dvecB );
        assertDoublesEqual(_nB, dmatB_b.nr(), 0 );
        assertDoublesEqual(_nB, dmatB_b.nc(), 0 );
        const double* pB_b    = dmatB_b.data();
        assertDoublesEqual(0.0, pB_b[0], 0.0);
        assertDoublesEqual(0.0, pB_b[1], 0.0);
        assertDoublesEqual(0.0, pB_b[2], 0.0);
        assertDoublesEqual(0.0, pB_b[3], 0.0);

    DoubleMatrix dmatB_b_alp      = funB_b_alp( dvecB );
        assertDoublesEqual(_nB*_nB, dmatB_b_alp.nr(), 0 );
        assertDoublesEqual(_nAlp,   dmatB_b_alp.nc(), 0 );
        const double* pB_b_alp    = dmatB_b_alp.data();
        assertDoublesEqual(0.0, pB_b_alp[0], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[1], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[2], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[3], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[4], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[5], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[6], 0.0);
        assertDoublesEqual(0.0, pB_b_alp[7], 0.0);

    DoubleMatrix dmatB_b_b        = funB_b_b( dvecB );
        assertDoublesEqual(_nB*_nB, dmatB_b_b.nr(), 0 );
        assertDoublesEqual(_nB,     dmatB_b_b.nc(), 0 );
        const double* pB_b_b      = dmatB_b_b.data();
        assertDoublesEqual(0.0, pB_b_b[0], 0.0);
        assertDoublesEqual(0.0, pB_b_b[1], 0.0);
        assertDoublesEqual(0.0, pB_b_b[2], 0.0);
        assertDoublesEqual(0.0, pB_b_b[3], 0.0);
        assertDoublesEqual(0.0, pB_b_b[4], 0.0);
        assertDoublesEqual(0.0, pB_b_b[5], 0.0);
        assertDoublesEqual(0.0, pB_b_b[6], 0.0);
        assertDoublesEqual(0.0, pB_b_b[7], 0.0);

    DoubleMatrix dmatF_b      = funF_b(dvecAlp, dvecB); // 2 by 2
        assertDoublesEqual(_nY,  dmatF_b.nr(), 0);
        assertDoublesEqual(_nB,  dmatF_b.nc(), 0);
        const double * pF_b   = dmatF_b.data();
        assertDoublesEqual( 0.0, pF_b[0], 0 );
        assertDoublesEqual( 0.0, pF_b[1], 0 );
        assertDoublesEqual( 1.0, pF_b[2], 0 );
        assertDoublesEqual( 1.0, pF_b[3], 0 );

    DoubleMatrix dmatF_b_alp      = funF_b_alp( dvecAlp, dvecB );
        assertDoublesEqual(_nY*_nB, dmatF_b_alp.nr(), 0);
        assertDoublesEqual(_nAlp,   dmatF_b_alp.nc(), 0);
        const double* pF_b_alp    = dmatF_b_alp.data();
        assertDoublesEqual( 0.0, pF_b_alp[0], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[1], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[2], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[3], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[4], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[5], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[6], 0 );
        assertDoublesEqual( 0.0, pF_b_alp[7], 0 );

    DoubleMatrix dmatF_b_b        = funF_b_b( dvecAlp, dvecB );
        assertDoublesEqual(_nY*_nB, dmatF_b_b.nr(), 0);
        assertDoublesEqual(_nB,     dmatF_b_b.nc(), 0);
        const double* pF_b_b      = dmatF_b_b.data();
        assertDoublesEqual( 0.0, pF_b_b[0], 0 );
        assertDoublesEqual( 0.0, pF_b_b[1], 0 );
        assertDoublesEqual( 0.0, pF_b_b[2], 0 );
        assertDoublesEqual( 0.0, pF_b_b[3], 0 );
        assertDoublesEqual( 0.0, pF_b_b[4], 0 );
        assertDoublesEqual( 0.0, pF_b_b[5], 0 );
        assertDoublesEqual( 0.0, pF_b_b[6], 0 );
        assertDoublesEqual( 0.0, pF_b_b[7], 0 );

    DoubleMatrix dmatD_b           = funD_b( dvecAlp );
        assertDoublesEqual(_nB*_nB,  dmatD_b.nr(), 0);
        assertDoublesEqual(_nB,      dmatD_b.nc(), 0);
        const double* pD_b         = dmatD_b.data();
        assertDoublesEqual( 0.0,     pD_b[0], 0 );
        assertDoublesEqual( 0.0,     pD_b[1], 0 );
        assertDoublesEqual( 0.0,     pD_b[2], 0 );
        assertDoublesEqual( 0.0,     pD_b[3], 0 );
        assertDoublesEqual( 0.0,     pD_b[4], 0 );
        assertDoublesEqual( 0.0,     pD_b[5], 0 );
        assertDoublesEqual( 0.0,     pD_b[6], 0 );
        assertDoublesEqual( 0.0,     pD_b[7], 0 );

    DoubleMatrix dmatDinv_b        = funDinv_b( dvecAlp );
        assertDoublesEqual(_nB*_nB,  dmatDinv_b.nr(), 0);
        assertDoublesEqual(_nB,      dmatDinv_b.nc(), 0);
        const double* pDinv_b      = dmatDinv_b.data();
        assertDoublesEqual( 0.0,     pDinv_b[0], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[1], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[2], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[3], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[4], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[5], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[6], 0 );
        assertDoublesEqual( 0.0,     pDinv_b[7], 0 );

    DoubleMatrix dmatDinv_b_alp       = funDinv_b_alp( dvecAlp );
        assertDoublesEqual(_nB*_nB*_nB, dmatDinv_b_alp.nr(), 0);
        assertDoublesEqual(_nAlp,       dmatDinv_b_alp.nc(), 0);
        const double* pDinv_b_alp     = dmatDinv_b_alp.data();
        assertDoublesEqual( 0.0,     pDinv_b_alp[0],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[1],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[2],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[3],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[4],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[5],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[6],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[7],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[8],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[9],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[10], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[11], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[12], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[13], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[14], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_alp[15], 0 );

    DoubleMatrix dmatDinv_b_b         = funDinv_b_b( dvecAlp );
        assertDoublesEqual(_nB*_nB*_nB, dmatDinv_b_b.nr(), 0);
        assertDoublesEqual(_nB,         dmatDinv_b_b.nc(), 0);
        const double* pDinv_b_b       = dmatDinv_b_b.data();
        assertDoublesEqual( 0.0,     pDinv_b_b[0],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[1],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[2],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[3],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[4],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[5],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[6],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[7],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[8],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[9],  0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[10], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[11], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[12], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[13], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[14], 0 );
        assertDoublesEqual( 0.0,     pDinv_b_b[15], 0 );

}
/*********************************************
 * approx for and derivatives of lambda_alp
 *********************************************/
void lambdaDiffTest::testLambda_alp_xWithD()
{
    using namespace population_analysis;

    DoubleMatrix actualLambda_alpOut(1, _nAlp);
    DoubleMatrix actualLambda_alp_alpOut(_nAlp, _nAlp);
    DoubleMatrix actualLambda_alp_bOut(_nAlp, _nB);
    DoubleMatrix exactLambda_alpOut(1, _nAlp);
    DoubleMatrix exactLambda_alp_alpOut(_nAlp, _nAlp);
    DoubleMatrix exactLambda_alp_bOut(_nAlp, _nB);

    int i;
    double tol = 0.0001;

    /*
    for( i=0; i<_nAlp; i++ )
    {
        x = _alp->data()[i];
        h = _alpStep->data()[i];

        //tol.data()[i] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2.0*h*h*h)) * (DBL_EPSILON * DBL_EPS_EQUAL_MULT * DBL_EPS_EQUAL_MULT);
    }
    */

    //-------------------------- 1st derivative with respect to alpha -----------------------//
    try{
        lambdaDiff(*_model, *_y, *_alp, *_alpStep, *_b, *_bStep, WITH_RESPECT_TO_ALP, 
            &actualLambda_alpOut, &actualLambda_alp_alpOut, &actualLambda_alp_bOut, true );
    }
    catch(...)
    {
        assertImplementation(false, "lambdaDiff failed", __LINE__, __FILE__);
    }

    assertImplementation( exactDeriv_x(*_model, *_y,*_alp,*_b, &exactLambda_alpOut,0,true),
        "exactDeriv_x(...) == true", __LINE__, __FILE__);
/*
cout << "exactLambda_alpOut (w/D) = " << exactLambda_alpOut << endl;
cout << "actualLambda_alpOut (w/D)   = " << actualLambda_alpOut   << endl;
*/
    for( i=0; i<_nAlp; i++ )
    {
        if( exactLambda_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alpOut.data()[i] - actualLambda_alpOut.data()[i])/fabs(exactLambda_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_alpOut.data()[i], actualLambda_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to alp
    //
    //exactLambda_alp_alpOut = lambda_alp_alpDiff(*_model, *_y, *_alp, *_b, *_alpStep, true);

    
    assertImplementation( exactDeriv_x_alp(*_y,*_alp,*_b, &exactLambda_alp_alpOut,WITH_RESPECT_TO_ALP,true),
        "exactDeriv_x_alp(...) == true", __LINE__, __FILE__);
    

/*
cout << "exactLambda_alp_alpOut (w/D) = " << exactLambda_alp_alpOut << endl;
cout << "actualLambda_alp_alpOut (w/D)   = " << actualLambda_alp_alpOut   << endl;
*/
    for( i=0; i<_nAlp*_nAlp; i++ )
    {
        if( exactLambda_alp_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alp_alpOut.data()[i] - actualLambda_alp_alpOut.data()[i])/fabs(exactLambda_alp_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_alp_alpOut.data()[i], actualLambda_alp_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to b
    //
    //exactLambda_alp_bOut = lambda_alp_bDiff(*_model, *_y, *_alp, *_b, *_bStep, true);
    
    assertImplementation( exactDeriv_x_b(*_y,*_alp,*_b, &exactLambda_alp_bOut,WITH_RESPECT_TO_ALP,true),
        "exactDeriv_x_b(...)==true", __LINE__, __FILE__);
    
/*
cout << "exactLambda_alp_bOut (w/D) = " << exactLambda_alp_bOut << endl;
cout << "actualLambda_alp_bOut (w/D)   = " << actualLambda_alp_bOut   << endl;
*/  
    for(i=0; i<_nAlp*_nB; i++)
    {
        if( exactLambda_alp_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alp_bOut.data()[i] - actualLambda_alp_bOut.data()[i])/fabs(exactLambda_alp_bOut.data()[i])*DBL_EPS_EQUAL_MULT;
        assertDoublesEqual(exactLambda_alp_bOut.data()[i], actualLambda_alp_bOut.data()[i], tol);
    }
}

void lambdaDiffTest::testLambda_alp_xWithoutD()
{
    using namespace population_analysis;

    DoubleMatrix actualLambda_alpOut(1, _nAlp);
    DoubleMatrix actualLambda_alp_alpOut(_nAlp, _nAlp);
    DoubleMatrix actualLambda_alp_bOut(_nAlp, _nB);
    DoubleMatrix exactLambda_alpOut(1, _nAlp);
    DoubleMatrix exactLambda_alp_alpOut(_nAlp, _nAlp);
    DoubleMatrix exactLambda_alp_bOut(_nAlp, _nB);

    double tol = 0.0001;

    try{
        lambdaDiff(*_model, *_y, *_alp, *_alpStep, *_b, *_bStep, WITH_RESPECT_TO_ALP, 
        &actualLambda_alpOut, &actualLambda_alp_alpOut, &actualLambda_alp_bOut, false );
    }
    catch(...)
    {
        assertImplementation(false, "lambdaDiff failed", __LINE__, __FILE__);
    }

    assertImplementation(exactDeriv_x(*_model, *_y,*_alp,*_b, &exactLambda_alpOut,0,false),
        "exactDeriv_x == true", __LINE__, __FILE__);

    int i;

    //
    // checking central difference approximation of lambda (w/alp)
    //
    for( i=0; i<_nAlp; i++ )
    {   
        if( exactLambda_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alpOut.data()[i] - actualLambda_alpOut.data()[i])/fabs(exactLambda_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_alpOut.data()[i], actualLambda_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to alp
    //
    //exactLambda_alp_alpOut = lambda_alp_alpDiff(*_model, *_y, *_alp, *_b, *_alpStep, false);

    assertImplementation(exactDeriv_x_alp(*_y,*_alp,*_b, &exactLambda_alp_alpOut,WITH_RESPECT_TO_ALP,false),
        "exactDeriv_x_alp == true", __LINE__, __FILE__);
/*
cout << "exactLambda_alp_alpOut (no D) = " << exactLambda_alp_alpOut << endl;
cout << "actualLambda_alp_alpOut (no D) = " << actualLambda_alp_alpOut << endl;
*/  
    for( i=0; i<_nAlp*_nAlp; i++ )
    {   
        if( exactLambda_alp_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alp_alpOut.data()[i] - actualLambda_alp_alpOut.data()[i])/fabs(exactLambda_alp_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_alp_alpOut.data()[i], actualLambda_alp_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to b
    //
    //exactLambda_alp_bOut = lambda_alp_bDiff(*_model, *_y, *_alp, *_b, *_bStep, false);
    
    assertImplementation( exactDeriv_x_b(*_y,*_alp,*_b, &exactLambda_alp_bOut,WITH_RESPECT_TO_ALP,false),
        "exactDeriv_x_b", __LINE__, __FILE__);
    
/*
cout << "exactLambda_alp_bOut (no D) = " << exactLambda_alp_bOut << endl;
cout << "actualLambda_alp_bOut (no D) = " << actualLambda_alp_bOut << endl;
*/
    for( i=0; i<_nAlp*_nB; i++ )
    {   
        if( exactLambda_alp_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_alp_bOut.data()[i] - actualLambda_alp_bOut.data()[i])/fabs(exactLambda_alp_bOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_alp_bOut.data()[i], actualLambda_alp_bOut.data()[i], tol);
    }
}

/*********************************************
 * approx for and derivatives of lambda_b
 *********************************************/
void lambdaDiffTest::testLambda_b_xWithD()
{
    using namespace population_analysis;
    DoubleMatrix actualLambda_bOut(1, _nB);
    DoubleMatrix actualLambda_b_alpOut(_nB, _nAlp);
    DoubleMatrix actualLambda_b_bOut(_nB, _nB);
    DoubleMatrix exactLambda_bOut(1, _nB);
    DoubleMatrix exactLambda_b_alpOut(_nB, _nAlp);
    DoubleMatrix exactLambda_b_bOut(_nB, _nB);
    double tol = 0.0001;

    try{
        lambdaDiff(*_model, *_y, *_alp, *_alpStep, *_b, *_bStep, WITH_RESPECT_TO_B, 
            &actualLambda_bOut, &actualLambda_b_alpOut, &actualLambda_b_bOut, true );
    }
    catch(...)
    {
        assertImplementation(false, "lambdaDiff failed", __LINE__, __FILE__);
    }

    assertImplementation( exactDeriv_x(*_model, *_y,*_alp,*_b,0,&exactLambda_bOut,true),
        "exactDeriv_x(...)==true", __LINE__, __FILE__);

    int i;
    // checking central difference approximation of lambda (w/b)
    for(i=0; i<_nB; i++)
    {
        if( exactLambda_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_bOut.data()[i] - actualLambda_bOut.data()[i])/fabs(exactLambda_bOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_bOut.data()[i], actualLambda_bOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/b) of lambda, with respect to alp
    //
    //exactLambda_b_alpOut = lambda_b_alpDiff(*_model, *_y, *_alp, *_b, *_alpStep, true);

    
    assertImplementation( exactDeriv_x_alp(*_y,*_alp,*_b,&exactLambda_b_alpOut,WITH_RESPECT_TO_B,true),
        "exactDeriv_x_alp(...)==true", __LINE__, __FILE__);
    
/*
cout << "exactLambda_b_alpOut (w/D) = " << exactLambda_b_alpOut << endl;
cout << "actualLambda_b_alpOut (w/D) = " << actualLambda_b_alpOut << endl;
*/
    for(i=0; i<_nB*_nAlp; i++)
    {
        if( exactLambda_b_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_b_alpOut.data()[i] - actualLambda_b_alpOut.data()[i])/fabs(exactLambda_b_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_b_alpOut.data()[i], actualLambda_b_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to b
    //
    //exactLambda_b_bOut = lambda_b_bDiff(*_model, *_y, *_alp, *_b, *_bStep, true);

    
    assertImplementation( exactDeriv_x_b(*_y,*_alp,*_b,&exactLambda_b_bOut,WITH_RESPECT_TO_B,true),
        "exactDeriv_x_b(...)==true", __LINE__, __FILE__);
    
/*
cout << "exactLambda_b_bOut (w/D) = " << exactLambda_b_bOut << endl;
cout << "actualLambda_b_bOut (w/D) = " << actualLambda_b_bOut << endl;
*/
    for(i=0; i<_nB*_nB; i++)
    {
        if( exactLambda_b_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_b_bOut.data()[i]-actualLambda_b_bOut.data()[i])/fabs(exactLambda_b_bOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_b_bOut.data()[i], actualLambda_b_bOut.data()[i], tol);
    }
}
void lambdaDiffTest::testLambda_b_xWithoutD()
{
    using namespace population_analysis;
    DoubleMatrix actualLambda_bOut(1, _nB);
    DoubleMatrix actualLambda_b_alpOut(_nB, _nAlp);
    DoubleMatrix actualLambda_b_bOut(_nB, _nB);
    DoubleMatrix exactLambda_bOut(1, _nB);
    DoubleMatrix exactLambda_b_alpOut(_nB, _nAlp);
    DoubleMatrix exactLambda_b_bOut(_nB, _nB);
    double tol;

    try{
        lambdaDiff(*_model, *_y, *_alp, *_alpStep, *_b, *_bStep, WITH_RESPECT_TO_B, 
        &actualLambda_bOut, &actualLambda_b_alpOut, &actualLambda_b_bOut, false );
    }
    catch(...)
    {
        assertImplementation(false, "lambdaDiff failed", __LINE__, __FILE__);
    }

    assertImplementation( exactDeriv_x(*_model, *_y,*_alp,*_b,0,&exactLambda_bOut,false),
        "exactDeriv_x(...)==true", __LINE__, __FILE__);

    int i;
    // checking central difference approximation of lambda (w/alp)
    for(i=0; i<_nAlp; i++)
    {
        if( exactLambda_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_bOut.data()[i]-actualLambda_bOut.data()[i])/fabs(exactLambda_bOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_bOut.data()[i], actualLambda_bOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to alp
    //
    //exactLambda_b_alpOut = lambda_b_alpDiff(*_model, *_y, *_alp, *_b, *_alpStep, false);
    
    assertImplementation( exactDeriv_x_alp(*_y,*_alp,*_b,&exactLambda_b_alpOut,WITH_RESPECT_TO_B,false),
        "exactDeriv_x_alp(...)==true", __LINE__, __FILE__);
    
/*
cout << "exactLambda_b_alpOut (no D) = " << exactLambda_b_alpOut << endl;
cout << "actualLambda_b_alpOut (no D) = " << actualLambda_b_alpOut << endl;
*/
    for(i=0; i<_nAlp*_nAlp; i++)
    {
        if( exactLambda_b_alpOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_b_alpOut.data()[i]-actualLambda_b_alpOut.data()[i])/fabs(exactLambda_b_alpOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_b_alpOut.data()[i],actualLambda_b_alpOut.data()[i], tol);
    }

    //
    // checking derivative of central difference aproximation (w/alp) of lambda, with respect to b
    //
    //exactLambda_b_bOut = lambda_b_bDiff(*_model, *_y, *_alp, *_b, *_bStep, false);
    
    assertImplementation( exactDeriv_x_b(*_y,*_alp,*_b,&exactLambda_b_bOut,WITH_RESPECT_TO_B,false),
        "exactDeriv_x_b(...)==true", __LINE__, __FILE__);
    
/*
cout << "exactLambda_b_bOut (no D) = " << exactLambda_b_bOut << endl;
cout << "actualLambda_b_bOut (no D) = " << actualLambda_b_bOut << endl;
*/

    for(i=0; i<_nAlp*_nB; i++)
    {
        if( exactLambda_b_bOut.data()[i] == 0 )
            tol = 0.0001;
        else
            tol = fabs(exactLambda_b_bOut.data()[i]-actualLambda_b_bOut.data()[i])/fabs(exactLambda_b_bOut.data()[i])*DBL_EPS_EQUAL_MULT;

        assertDoublesEqual(exactLambda_b_bOut.data()[i], actualLambda_b_bOut.data()[i], tol);
    }
}


/*****************************************************************************
 *
 *  Local functions
 * 
 * Assumption:
 * 
 * alp = [ 1 ]
 *       [ 1 ]
 *
 * b   = [ 1 ]
 *       [ 1 ]
 *
 * y   = [ 1 ]
 *       [ 1 ]
 *
 ****************************************************************************/
/*---------------------------------------------------------------------------
 * alp related (for debug)
 *---------------------------------------------------------------------------*/
//
// alp_alp = [ 0  0 ]
//           [ 0  0 ]
//
static const DoubleMatrix funAlp_alp(const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nAlp, _nAlp);

}

//
// alp_b     = [ 0  0 ]
//           [ 0  0 ]
//
static const DoubleMatrix funAlp_b( const DoubleMatrix &dvecAlp  )
{
    /*DoubleMatrix B_b(_nB, _nB);
    B_b = identity(2);
    return B_b;
    */
    return getZeroMatrix(_nAlp, _nB);
}

//
// alp_alp_alp=[ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funAlp_alp_alp(const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nAlp*_nAlp, _nAlp);
}

//
// alp_alp_b = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funAlp_alp_b( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nAlp*_nAlp, _nB);
}

//
// alp_b_al  = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funAlp_b_alp( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nAlp*_nB, _nAlp);
}

//
// alp_b_b =   [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funAlp_b_b( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nAlp*_nB, _nB);
}

/*---------------------------------------------------------------------------
 * b related (for debug)
 *---------------------------------------------------------------------------*/
//
// b_alp   = [ 0  0 ]
//           [ 0  0 ]
//
static const DoubleMatrix funB_alp(const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nB, _nAlp);

}

//
// b_b     = [ 0  0 ]
//           [ 0  0 ]
//
static const DoubleMatrix funB_b( const DoubleMatrix &dvecB  )
{
    /*DoubleMatrix B_b(_nB, _nB);
    B_b = identity(2);
    return B_b;
    */
    return getZeroMatrix(_nB, _nB);
}

//
// b_alp_alp = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funB_alp_alp(const DoubleMatrix &dvecB ){
    return getZeroMatrix(_nB*_nAlp, _nAlp);
}

//
// b_alp_b   = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funB_alp_b( const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nB*_nAlp, _nB);
}

//
// b_b_alp   = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funB_b_alp( const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nB*_nB, _nAlp);
}

//
// b_b_b =     [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funB_b_b( const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nB*_nB, _nB);
}

/*---------------------------------------------------------------------------
 * D related
 *---------------------------------------------------------------------------*/
//
// D = [alp[0]   0   ]
//     [   0   alp[0]]
//
static const DoubleMatrix funD( const DoubleMatrix &dvecAlp )
{
    DoubleMatrix dmatD(_nB, _nB);
    double *pdD = dmatD.data();
    const double *pdAlpha = dvecAlp.data();

    pdD[0] = pdAlpha[0];
    pdD[1] = 0.0;
    pdD[2] = 0.0;
    pdD[3] = pdAlpha[0];
    return dmatD;
}

//
// D_alp = [ 1  0 ]
//         [ 0  0 ]
//         [ 0  0 ]
//         [ 1  0 ]
//
static const DoubleMatrix funD_alp( const DoubleMatrix &dvecAlp )
{
    DoubleMatrix dmatD_alp( _nB*_nB, _nAlp );
    double *pdD_alp = dmatD_alp.data();
    dmatD_alp.fill(0.0);

    pdD_alp[0] = 1.0;
    pdD_alp[3] = 1.0;
    return dmatD_alp;
}
//
// D_b = [ 0   0 ]
//       [ 0   0 ]
//       [ 0   0 ]
//       [ 0   0 ]
//
static const DoubleMatrix funD_b( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nB*_nB, _nB);
}
//
// D_alp_alp = 8 by 2 filled with 0
//
static const DoubleMatrix funD_alp_alp( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nAlp, _nAlp);
}
//
// D_alp_b   = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funD_alp_b( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nAlp, _nB);
}
//
// D_b_alp   = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funD_b_alp(  const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nB, _nAlp);
}
//
// D_b_b     = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funD_b_b( const DoubleMatrix &dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nB, _nB);
}
/*---------------------------------------------------------------------------
 * Dinv related
 *---------------------------------------------------------------------------*/
//
// Dinv = [ 1/alp[0]     0     ]
//        [    0      1/alp[0] ]   
// 
static const DoubleMatrix funDinv( const DoubleMatrix& dvecAlp )
{
    DoubleMatrix dmatDinv(_nB, _nB);
    double* pDinv = dmatDinv.data();
    const double* pAlp = dvecAlp.data();

    pDinv[0] = 1/pAlp[0];
    pDinv[1] = 0.0;
    pDinv[2] = 0.0;
    pDinv[3] = 1/pAlp[0];
    return dmatDinv;
}

//
// Dinv_alp = [  -1/alp[0]^2   0  ]
//            [     0          0  ]
//            [     0          0  ]
//            [  -1/alp[0]^2   0  ]
//
static const DoubleMatrix funDinv_alp(const DoubleMatrix & dvecAlp )
{
    DoubleMatrix dmatDinv_alp(_nB*_nB, _nAlp);
    double* pDinv_alp = dmatDinv_alp.data();
    const double* pAlp = dvecAlp.data();
    dmatDinv_alp.fill(0.);

    pDinv_alp[0] = -1/(pAlp[0]*pAlp[0]);
    pDinv_alp[3] = -1/(pAlp[0]*pAlp[0]);
    return dmatDinv_alp;
}

//
// Dinv_alp_alp = [  2/alp[0]^3    0  ]
//                [     0          0  ]
//                [     0          0  ]
//                [  2/alp[0]^3    0  ]
//                [     0          0  ]
//                [     0          0  ]
//                [     0          0  ]
//                [     0          0  ]
//
//
static const DoubleMatrix funDinv_alp_alp( const DoubleMatrix & dvecAlp )
{
    DoubleMatrix Dinv_alp_alp(_nB*_nB*_nAlp, _nAlp);
    double * pDinv_alp_alp = Dinv_alp_alp.data();
    const double * pAlp = dvecAlp.data();
    Dinv_alp_alp.fill(0.0);
    pDinv_alp_alp[0] = 2.0 / (pAlp[0]*pAlp[0]*pAlp[0]);
    pDinv_alp_alp[3] = 2.0 / (pAlp[0]*pAlp[0]*pAlp[0]);
    return Dinv_alp_alp;
}
//
// Dinv_alp_b = [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//
static const DoubleMatrix funDinv_alp_b( const DoubleMatrix & dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nAlp, _nB);
}

//
// Dinv_b = [ 0  0 ]
//          [ 0  0 ]
//          [ 0  0 ]
//          [ 0  0 ]
//
static const DoubleMatrix funDinv_b( const DoubleMatrix& dvecAlp )
{
    return getZeroMatrix(_nB*_nB, _nB);
}
//
// Dinv_b_alp = [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//
static const DoubleMatrix funDinv_b_alp( const DoubleMatrix& dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nB, _nAlp);
}
//
// Dinv_b_b = [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//
static const DoubleMatrix funDinv_b_b( const DoubleMatrix& dvecAlp )
{
    return getZeroMatrix(_nB*_nB*_nB, _nB);
}

/*---------------------------------------------------------------------------
 * f related
 *---------------------------------------------------------------------------*/
//
// f = [ alp[1] + b[1] ]
//     [ alp[1] + b[1] ]
//
static const DoubleMatrix funF( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatF(_nY, 1);
    const double *pdAlpha = dvecAlp.data();
	const double *pdB     = dvecB.data();

    dmatF.fill( pdAlpha[1] + pdB[1] );
    return dmatF;
}

//
// f_alp = [ 0  1 ]
//         [ 0  1 ]
//
static const DoubleMatrix funF_alp( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatF_alp(_nY, _nAlp);
    double *pdF_alp = dmatF_alp.data();

    pdF_alp[0] = 0.0;
    pdF_alp[1] = 0.0;
    pdF_alp[2] = 1.0;
    pdF_alp[3] = 1.0;
    return dmatF_alp;
}

//
// f_b = [ 0  1 ]
//       [ 0  1 ]
//
static const DoubleMatrix funF_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatF_b(_nY, _nB);
    double *pdF_b = dmatF_b.data();

    pdF_b[0] = 0.0;
    pdF_b[1] = 0.0;
    pdF_b[2] = 1.0;
    pdF_b[3] = 1.0;
    return dmatF_b;
}

// f_alp_alp = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funF_alp_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nAlp, _nAlp);
}

//
// f_alp_b =   [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funF_alp_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nAlp, _nB);
}

//
// f_b_alp =   [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funF_b_alp(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nB, _nAlp);
}

//
// f_b_b =     [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funF_b_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nB, _nB);
}

/*---------------------------------------------------------------------------
 * R related
 *---------------------------------------------------------------------------*/

//
// R(alpha, b) = [ b[0]  0   ]
//               [  0   b[0] ]
//
static const DoubleMatrix funR(  const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatR(_nY, _nY);
    double *pdR = dmatR.data();
	const double *pdB = dvecB.data();

    pdR[0] = pdB[0];
    pdR[1] = 0.0;
    pdR[2] = 0.0;
    pdR[3] = pdB[0];
    return dmatR;
}

//
// R_alp = [ 0  0 ]
//         [ 0  0 ]
//         [ 0  0 ]
//         [ 0  0 ]
//
static const DoubleMatrix funR_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY, _nAlp);
}

//
// R_b   = [ 1  0 ]
//         [ 0  0 ]
//         [ 0  0 ]
//         [ 1  0 ]
//
static const DoubleMatrix funR_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatR_b(_nY*_nY, _nB);
    double *pdR_b = dmatR_b.data();
    dmatR_b.fill(0.0);
    pdR_b[0] = 1.0;
    pdR_b[3] = 1.0;
    return dmatR_b;
}
//
// R_alp_alp = [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funR_alp_alp( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY*_nAlp, _nAlp);
}

//
// R_alp_b =   [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funR_alp_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY*_nAlp, _nB);
}

//
// R_b_alp =   [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funR_b_alp( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY*_nB, _nAlp);
}

//
// R_b_b =     [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//             [ 0  0 ]
//
static const DoubleMatrix funR_b_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY*_nB, _nB);
}

/*---------------------------------------------------------------------------
 * Rinv related
 *---------------------------------------------------------------------------*/
//
// Rinv     = [ b[0]^(-1)      0     ]
//            [  0         b[0]^(-1) ]
//
static const DoubleMatrix funRinv( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix dmatRinv(_nY, _nY);
    double* pRinv = dmatRinv.data();
    const double* pB = dvecB.data();
    pRinv[0] = 1.0/pB[0];
    pRinv[1] = 0.0;
    pRinv[2] = 0.0;
    pRinv[3] = 1.0/pB[0];
    return dmatRinv;
}

//
// Rinv_alp = [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//            [ 0  0 ]
//
static const DoubleMatrix funRinv_alp( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    return getZeroMatrix(_nY*_nY, _nAlp);
}
//
// Rinv_b = [ -b[0]^(-2)  0 ]
//          [   0         0 ]
//          [   0         0 ]
//          [ -b[0]^(-2)  0 ]
//
static const DoubleMatrix funRinv_b( const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB )
{
    DoubleMatrix Rinv_b(_nY*_nY, _nB);
    double * pRinv_b = Rinv_b.data();
    const double * pAlp = dvecAlp.data();
    const double * pB   = dvecB.data();
    Rinv_b.fill(0.0);

    pRinv_b[0] = -1.0/(pB[0]*pB[0]);
    pRinv_b[3] = -1.0/(pB[0]*pB[0]);

    return Rinv_b;
}

//
// Rinv_b_alp = [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//
static const DoubleMatrix funRinv_b_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB)
{
    return getZeroMatrix(_nY*_nY*_nB, _nAlp);
}
//
// Rinv_b_b = [ 2b[0]^(-3)  0 ]
//            [   0         0 ]
//            [   0         0 ]
//            [ 2b[0]^(-3)  0 ]
//            [   0         0 ]
//            [   0         0 ]
//            [   0         0 ]
//            [   0         0 ]
//
static const DoubleMatrix funRinv_b_b(const DoubleMatrix& dvecAlp, const DoubleMatrix& dvecB)
{
    DoubleMatrix Rinv_b_b(_nY*_nY*_nB, _nB);
    double * pRinv_b_b = Rinv_b_b.data();
    const double * pB = dvecB.data();
    Rinv_b_b.fill(0);

    pRinv_b_b[0] = 2.0 / (pB[0]*pB[0]*pB[0]);
    pRinv_b_b[3] = 2.0 / (pB[0]*pB[0]*pB[0]);
    return Rinv_b_b;
}
//
// Rinv_alp_alp = [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//                [ 0  0 ]
//
static const DoubleMatrix funRinv_alp_alp(const DoubleMatrix &dvecAlp, const DoubleMatrix &dvecB)
{
    return getZeroMatrix(_nY*_nY*_nAlp, _nAlp);
}
//
// Rinv_alp_b = [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//              [ 0  0 ]
//
static const DoubleMatrix funRinv_alp_b(const DoubleMatrix& dvecAlp, const DoubleMatrix& dvecB)
{
    return getZeroMatrix(_nY*_nY*_nAlp, _nB);
}

/*
#include "rvec.h"
#include "replaceJth.h"
#include "divByScalar.h"

static const DoubleMatrix lambda_alp_alpDiff(SpkModel<double>& model, const DoubleMatrix& y, const DoubleMatrix& alp, const DoubleMatrix& b, 
                                             const DoubleMatrix& h, bool withD)
{
    int nAlp = alp.nr();
    int nB = b.nr();
    assert(h.nr() == nAlp);

    DoubleMatrix lAlp, lLambda_alp;
    DoubleMatrix uAlp, uLambda_alp;

    const double *pAlp = alp.data();
    const double *pH   = h.data();
    double *pLAlp, *pUAlp;
    DoubleMatrix ret(nAlp, nAlp);
    DoubleMatrix jth(nAlp,1);

    for(int j=0; j<nAlp; j++)
    {
        lAlp = alp;
        uAlp = alp;

        pLAlp = lAlp.data();
        pUAlp = uAlp.data();

        pLAlp[j] = pLAlp[j] - pH[j];
        pUAlp[j] = pUAlp[j] + pH[j];

        lLambda_alp = lambda_alp(model, y, lAlp, b, withD);
        uLambda_alp = lambda_alp(model, y, uAlp, b, withD);

        jth = rvec( divByScalar(subtract(uLambda_alp, lLambda_alp), 2.0*pH[j]) );
        replaceJth(ret, j, jth);
    }
    return ret;

}
static const DoubleMatrix lambda_alp_bDiff(SpkModel<double>& model, const DoubleMatrix& y, const DoubleMatrix& alp, const DoubleMatrix& b, 
                                             const DoubleMatrix& h, bool withD)
{
    int nAlp = alp.nr();
    int nB = b.nr();
    assert(h.nr() == nB);

    DoubleMatrix lB, lLambda_alp;
    DoubleMatrix uB, uLambda_alp;

    const double *pB = b.data();
    const double *pH = h.data();
    double *pLB, *pUB;
    DoubleMatrix ret(nAlp, nB);
    DoubleMatrix jth(nAlp,1);

    for(int j=0; j<nB; j++)
    {
        lB = b;
        uB = b;

        pLB = lB.data();
        pUB = uB.data();

        pLB[j] = pLB[j] - pH[j];
        pUB[j] = pUB[j] + pH[j];

        lLambda_alp = lambda_alp(model, y, alp, lB, withD);
        uLambda_alp = lambda_alp(model, y, alp, uB, withD);

        jth = rvec( divByScalar(subtract(uLambda_alp, lLambda_alp), 2.0*pH[j]) );
        replaceJth(ret, j, jth);
    }
    return ret;

}
static const DoubleMatrix lambda_b_alpDiff(SpkModel<double>& model, const DoubleMatrix& y, const DoubleMatrix& alp, const DoubleMatrix& b, 
                             const DoubleMatrix& h, bool withD)
{
    int nAlp = alp.nr();
    int nB = b.nr();
    assert(h.nr() == nAlp);

    DoubleMatrix lAlp, lLambda_b;
    DoubleMatrix uAlp, uLambda_b;

    const double *pAlp = alp.data();
    const double *pH   = h.data();
    double *pLAlp, *pUAlp;
    DoubleMatrix ret(nB, nAlp);
    DoubleMatrix jth(nB,1);

    for(int j=0; j<nAlp; j++)
    {
        lAlp = alp;
        uAlp = alp;

        pLAlp = lAlp.data();
        pUAlp = uAlp.data();

        pLAlp[j] = pLAlp[j] - pH[j];
        pUAlp[j] = pUAlp[j] + pH[j];

        lLambda_b = lambda_b(model, y, lAlp, b, withD);
        uLambda_b = lambda_b(model, y, uAlp, b, withD);

        jth = rvec( divByScalar(subtract(uLambda_b, lLambda_b), 2.0*pH[j]) );
        replaceJth(ret, j, jth);
    }
    return ret;
}
static const DoubleMatrix lambda_b_bDiff(SpkModel<double>& model, const DoubleMatrix& y, const DoubleMatrix& alp, const DoubleMatrix& b, 
                                             const DoubleMatrix& h, bool withD)
{
    int nAlp = alp.nr();
    int nB = b.nr();
    assert(h.nr() == nB);

    DoubleMatrix lB, lLambda_b;
    DoubleMatrix uB, uLambda_b;

    const double *pB = b.data();
    const double *pH = h.data();
    double *pLB, *pUB;
    DoubleMatrix ret(nB, nB);
    DoubleMatrix jth(nB,1);

    for(int j=0; j<nB; j++)
    {
        lB = b;
        uB = b;

        pLB = lB.data();
        pUB = uB.data();

        pLB[j] = pLB[j] - pH[j];
        pUB[j] = pUB[j] + pH[j];

        lLambda_b = lambda_b(model, y, alp, lB, withD);
        uLambda_b = lambda_b(model, y, alp, uB, withD);

        jth = rvec( divByScalar(subtract(uLambda_b, lLambda_b), 2.0*pH[j]) );
        replaceJth(ret, j, jth);
    }
    return ret;

}
*/
/*---------------------------------------------------------------------------
 * Exact derivatives
 *---------------------------------------------------------------------------*/
bool lambdaDiffTest::exactDeriv_x(
    SpkModel<double> &model,
    const DoubleMatrix &dvecY,
    const DoubleMatrix &dvecAlp,
    const DoubleMatrix &dvecB,
    DoubleMatrix *exactLambda_alpOut,
    DoubleMatrix *exactLambda_bOut,
    const bool withD
    )
{
    if(exactLambda_alpOut)
        *exactLambda_alpOut = lambda_alp(model, dvecY, dvecAlp, dvecB);
    if(exactLambda_bOut)
        *exactLambda_bOut   = lambda_b(model, dvecY, dvecAlp, dvecB);

    return true;
}

#include "namespace_population_analysis.h"
#include "inverse.h"
#include "add.h"

bool lambdaDiffTest::exactDeriv_x_alp(
    const DoubleMatrix &dvecY,
    const DoubleMatrix &dvecAlp,
    const DoubleMatrix &dvecB,
    DoubleMatrix *exactLambda_x_alpOut,
    int   var,
    const bool withD)
{
    using namespace std;
    using namespace population_analysis;

    int n = dvecY.nr();
    DoubleMatrix    term1,term2,zero(n,1);
    DoubleMatrix    dvecX, dmatX_alp, dmatX_x_alp;
    DoubleMatrix    dvecF, dmatF_alp, dmatF_x_alp;
    DoubleMatrix    dmatR, dmatR_alp, dmatR_x_alp;
    DoubleMatrix    dmatRinv, dmatRinv_alp, dmatRinv_x_alp;
    DoubleMatrix    dmatD, dmatD_alp, dmatD_x_alp;
    DoubleMatrix    dmatDinv, dmatDinv_alp, dmatDinv_x_alp;
    zero.fill(0.0);

    dvecF    = funF(dvecAlp, dvecB);
    dmatR    = funR(dvecAlp, dvecB);
    dmatRinv = funRinv(dvecAlp, dvecB);
    dmatD    = funD(dvecAlp);
    dmatDinv = funDinv(dvecAlp);

        if( var == WITH_RESPECT_TO_ALP ){
            dvecX          = dvecAlp;
            dmatX_alp      = funAlp_alp(     dvecAlp);
            dmatX_x_alp    = funAlp_alp_alp( dvecAlp); // 4 by 2
            dmatF_alp        = funF_alp(       dvecAlp, dvecB ); // 2 by 2
            dmatF_x_alp    = funF_alp_alp(   dvecAlp, dvecB ); // 4 by 2
            dmatR_alp        = funR_alp(       dvecAlp, dvecB ); // 4 by 2
            dmatRinv_alp     = funRinv_alp(    dvecAlp, dvecB ); // 4 by 2
            dmatRinv_x_alp = funRinv_alp_alp(dvecAlp, dvecB ); // 8 by 2
            dmatD_alp        = funD_alp(       dvecAlp); // 4 by 2
            dmatDinv_alp     = funDinv_alp(    dvecAlp); // 4 by 2
            dmatDinv_x_alp = funDinv_alp_alp(dvecAlp); // 4 by 2
        }
        else if( var == WITH_RESPECT_TO_B ){
            dvecX          = dvecB;
            dmatX_alp      = funB_alp(       dvecB );
            dmatX_x_alp    = funB_b_alp(     dvecB );
            dmatF_alp        = funF_b(         dvecAlp, dvecB );
            dmatF_x_alp    = funF_b_alp(     dvecAlp, dvecB );
            dmatR_alp        = funR_b(         dvecAlp, dvecB );
            dmatRinv_alp     = funRinv_b(      dvecAlp, dvecB );
            dmatRinv_x_alp = funRinv_b_alp(  dvecAlp, dvecB );
            dmatD_alp        = funD_b(         dvecAlp );
            dmatDinv_alp     = funDinv_b(      dvecAlp );
            dmatDinv_x_alp = funDinv_b_alp(  dvecAlp );
        }
        else{
            cerr << "Fail: in exactDeriv_x_alp: 6th parameter is invalid." << endl;
            return false;
        }

        term1 = elsq_x_x(dvecAlp,dvecY,dvecF,dmatF_alp,dmatF_x_alp,dmatR,dmatR_alp,dmatRinv,dmatRinv_alp,dmatRinv_x_alp);
        
        if(withD){
            term2 = elsq_x_x(dvecAlp,zero, dvecX, dmatX_alp,dmatX_x_alp,dmatD,dmatD_alp,dmatDinv,dmatDinv_alp,dmatDinv_x_alp);
            *exactLambda_x_alpOut = add(term1, term2);
        }
        else{
            *exactLambda_x_alpOut = term1;
        }
   
    return true;
}
bool lambdaDiffTest::exactDeriv_x_b(
    const DoubleMatrix &dvecY,
    const DoubleMatrix &dvecAlp,
    const DoubleMatrix &dvecB,
    DoubleMatrix *exactLambda_x_bOut,
    int   var,
    const bool withD)
{
    using namespace std;
    using namespace population_analysis;
    int n = dvecY.nr();
    DoubleMatrix    term1,term2,zero(n,1);
    DoubleMatrix    dvecX, dmatX_b, dmatX_x_b;
    DoubleMatrix    dvecF, dmatF_b, dmatF_x_b;
    DoubleMatrix    dmatR, dmatR_b, dmatR_x_b;
    DoubleMatrix    dmatRinv, dmatRinv_b, dmatRinv_x_b;
    DoubleMatrix    dmatD, dmatD_b, dmatD_x_b;
    DoubleMatrix    dmatDinv, dmatDinv_b, dmatDinv_x_b;
    zero.fill(0.0);

    dvecF    = funF(dvecAlp, dvecB);
    dmatR    = funR(dvecAlp, dvecB);
    dmatRinv = funRinv(dvecAlp, dvecB);
    dmatD    = funD(dvecAlp);
    dmatDinv = funDinv(dvecAlp);

        if( var == WITH_RESPECT_TO_ALP ){
            dvecX          = dvecAlp;
            dmatX_b        = funAlp_b(       dvecAlp );
            dmatX_x_b      = funAlp_alp_b(   dvecAlp  ); // 4 by 2
            dmatF_b        = funF_b(       dvecAlp, dvecB ); // 2 by 2
            dmatF_x_b      = funF_alp_b(     dvecAlp, dvecB ); // 4 by 2
            dmatR_b        = funR_b(       dvecAlp, dvecB ); // 4 by 2
            dmatRinv_b     = funRinv_b(       dvecAlp, dvecB ); // 4 by 2
            dmatRinv_x_b   = funRinv_alp_b(     dvecAlp, dvecB ); // 8 by 2
            dmatD_b        = funD_b(       dvecAlp); // 4 by 2
            dmatDinv_b     = funDinv_b(    dvecAlp); // 4 by 2
            dmatDinv_x_b   = funDinv_alp_b(  dvecAlp ); // 4 by 2
        }
        else if( var == WITH_RESPECT_TO_B ){
            dvecX          = dvecB;
            dmatX_b        = funB_b(         dvecB );
            dmatX_x_b      = funB_b_b(       dvecB );
            dmatF_b        = funF_b(         dvecAlp, dvecB );
            dmatF_x_b      = funF_b_b(       dvecAlp, dvecB );
            dmatR_b        = funR_b(         dvecAlp, dvecB );
            dmatRinv_b     = funRinv_b(      dvecAlp, dvecB );
            dmatRinv_x_b   = funRinv_b_b(    dvecAlp, dvecB );
            dmatD_b        = funD_b(         dvecAlp  );
            dmatDinv_b     = funDinv_b(      dvecAlp );
            dmatDinv_x_b   = funDinv_b_b(    dvecAlp );
        }
        else{
            cerr << "Fail: in exactDeriv_x_b: 6th parameter is invalid." << endl;
            return false;
        }

        term1 = elsq_x_x(dvecB,dvecY,dvecF,dmatF_b,dmatF_x_b,dmatR,dmatR_b,dmatRinv,dmatRinv_b,dmatRinv_x_b);
        
        if(withD){
            term2 = elsq_x_x(dvecB,zero, dvecX,dmatX_b,dmatX_x_b,dmatD,dmatD_b,dmatDinv,dmatDinv_b,dmatDinv_x_b);
            *exactLambda_x_bOut = add(term1, term2);
        }
        else{
            *exactLambda_x_bOut = term1;
        }
   

    return true;
}

/**************************************
 *  eslq_x_x 
 **************************************/
#include "subtract.h"
#include "transpose.h"
#include "AkronBtimesC.h"
#include "rvec.h"
#include "divByScalar.h"
#include "mulByScalar.h"
#include "add.h"

static const DoubleMatrix elsq_x_x( 
    const DoubleMatrix &dvecX,          // parameter
    const DoubleMatrix &dvecZ,          // data
    const DoubleMatrix &dvecH,          // h(x)
    const DoubleMatrix &dmatH_x,    
    const DoubleMatrix &dmatH_x_x,
    const DoubleMatrix &dmatQ,          // Q(x)
    const DoubleMatrix &dmatQ_x,
    const DoubleMatrix &dmatQinv,        // Q(x)inv
    const DoubleMatrix &dmatQinv_x,
    const DoubleMatrix &dmatQinv_x_x
    )
{
    using namespace std;

    DoubleMatrix    term1, term2, term1sub2div2, term3, term4, term5, term4add5, term6, term6div2, 
                    term7, term8, term9, term7add8div2, term7add8div2Trans;
    DoubleMatrix ZsubH, tranZsubH, identX;
    DoubleMatrix dmatElsq_x_x;

    ZsubH     = subtract(dvecZ, dvecH);
    tranZsubH = transpose(ZsubH);
    identX    = identity(dvecX.nr());

    term1 = AkronBtimesC(transpose(rvec(dmatQ)), identX, dmatQinv_x_x);
    term2 = multiply(transpose(dmatQinv_x), dmatQ_x);
    term1sub2div2 = divByScalar( add(term1, term2), -2.0 );

    term3 = mulByScalar( AkronBtimesC(multiply(tranZsubH,dmatQinv), identX, dmatH_x_x), -1.0);

    term4 = AkronBtimesC(tranZsubH, transpose(dmatH_x), dmatQinv_x);
    term5 = multiply(multiply(transpose(dmatH_x), dmatQinv),dmatH_x);
    term4add5 = subtract( term5, term4 );

    term6 = AkronBtimesC( transpose(rvec(multiply(ZsubH, tranZsubH))), identX, dmatQinv_x_x );
    term6div2 = divByScalar(term6, 2.0);


    term7 = AkronBtimesC( transpose(dmatH_x), transpose(ZsubH), dmatQinv_x );
    term8 = AkronBtimesC( transpose(ZsubH), transpose(dmatH_x), dmatQinv_x );
    term7add8div2 = divByScalar( add(term7, term8), -2.0 );
    term7add8div2Trans = transpose(term7add8div2);
    
    dmatElsq_x_x = add( term1sub2div2, add( term3, add(term4add5, add(term6div2, term7add8div2Trans))));
    
    return dmatElsq_x_x;
}
