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
 * File: lambdaTest.cpp
 *
 *
 * Unit test for lambda().
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Function: lambdaTest
 *
 *
 * Performs the unit test for lambda().
 *
 *************************************************************************/
/**************************************************************
 *   Include files
 **************************************************************/
#include <cmath>
#include <iomanip>
#include <ctime>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/lambda.h"
#include "../../../spk/matabs.h"
#include "../../../spk/matmax.h"
#include "../../../spk/pi.h"
#include "../../../spk/subtract.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/multiply.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/lambda.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/AkronBtimesC.h"
#include "../../../spk/subtract.h"
#include "../../../spk/transpose.h"
#include "../../../spk/identity.h"
#include "../../../spk/rvec.h"
#include "../../../spk/add.h"
#include "../../../spk/det.h"
#include "../../../spk/inverse.h"

#include "lambdaTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

/**************************************************************
 *   (Static) elsq_x_x function declarations
 **************************************************************/
static DoubleMatrix elsq_x_x( 
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

/**************************************************************
 *   (Static) exact Lambda and its derivatives declarations
 **************************************************************/

static double funExactLambda(          
                              const DoubleMatrix &dvecAlp, 
                              const DoubleMatrix &dvecB,
                              const DoubleMatrix &dvecY,
                              const bool withD = true);
static DoubleMatrix funExactLambda_alp(
                              const DoubleMatrix &dvecAlp, 
                              const DoubleMatrix &dvecB,
                              const DoubleMatrix &dvecY,
                              const bool withD = true);
static DoubleMatrix funExactLambda_b(  
                              const DoubleMatrix &dvecAlp, 
                              const DoubleMatrix &dvecB,
                              const DoubleMatrix &dvecY,
                              const bool withD = true);
/**************************************************************
 *   class UserModel declaration
 **************************************************************/

class lambdaTest::UserModelLambdaTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int          _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelLambdaTest(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~UserModelLambdaTest(){};
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
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(alpha, b) = [ alp(2) + b(2) ]
      //               [ alp(2) + b(2) ]
      //
      ret.resize(_nYi);
      ret[0] = _a[1] + _b[1];
      ret[1] = _a[1] + _b[1];

    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
      //
      // f_alp = [ 0  1 ]
      //         [ 0  1 ]
      //
      ret.resize(_nYi * _nA);
      ret[0] = 0.0;
      ret[1] = 0.0;
      ret[2] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // f_b   = [ 0  1 ]
      //         [ 0  1 ]
      //
      ret.resize(_nYi * _nB);
      ret[0] = 0.0;
      ret[1] = 0.0;
      ret[2] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // R(alp, b) = [ b(1)  0   ]
      //             [  0   b(1) ]
      //
      ret.resize(_nYi * _nYi);
      ret[0] = _b[0];
      ret[1] =  0.0;
      ret[2] =  0.0;
      ret[3] = _b[0];
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
      //
      // R_alp = [ 0  0 ]
      //         [ 0  0 ]
      //         [ 0  0 ]
      //         [ 0  0 ]
      //
      ret.resize(_nYi *_nYi * _nA);
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;
      return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // R_b   = [ 1  0 ]
      //         [ 0  0 ]
      //         [ 0  0 ]
      //         [ 1  0 ]
      //
      ret.resize(_nYi *_nYi * _nB);
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;

      ret[0] = 1.0;
      ret[3] = 1.0;
      return true;
    }  
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // R(alp, b)^-1 = [ 1 / b(1)      0   ]
      //                [  0       1 / b(1) ]
      //
      ret.resize(_nYi * _nYi);
      ret[0] = 1.0 / _b[0];
      ret[1] =  0.0;
      ret[2] =  0.0;
      ret[3] = 1.0 / _b[0];
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // R^(-1)_b(alp,b) = [ -1.0 / b(1)^2   0 ]
      //                   [ 0               0 ]
      //                   [ 0               0 ]
      //                   [ -1.0 / b(1)^2   0 ]
      //
      ret.resize(_nYi * _nYi * _nB);
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / ( _b[0] * _b[0] );
      ret[3] = -1.0 / ( _b[0] * _b[0] );
      return true;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
      //
      // R^(-1)_alp = [ 0  0 ]
      //              [ 0  0 ]
      //              [ 0  0 ]
      //              [ 0  0 ]
      //
      ret.resize(_nYi * _nYi * _nA);
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;
      return false;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
      //
      // D = [alp(1)   0   ]
      //     [   0   alp(1)]
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
      // D_alp = [ 1  0 ]
      //         [ 0  0 ]
      //         [ 0  0 ]
      //         [ 1  0 ]
      //
      ret.resize(_nB * _nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;
      ret[0] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      //
      // Dinv = [ alp(1)^-1      0     ]
      //        [     0      alp(1)^-1 ]
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
      // Dinv_alp = [ -1.0 / alp(1)^2  0 ]
      //            [ 0                0 ]
      //            [ 0                0 ]
      //            [ -1.0 / alp(1)^2  0 ]
      //
      ret.resize(_nB * _nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;
      ret[0] = -1.0 / (_a[0] * _a[0]);
      ret[3] = -1.0 / (_a[0] * _a[0]);
      return true;
    }
};


void lambdaTest::setUp()
{
    const int nA = 2;
    const int nB = 2;
    const int nY = 2;
    _model = new UserModelLambdaTest(nA, nB, nY);
    _y     = new DoubleMatrix();
    _alp   = new DoubleMatrix();
    _b     = new DoubleMatrix();

    lambdaTest::setY(*_y);
    lambdaTest::setAlp(*_alp);
    lambdaTest::setB(*_b);
}
void lambdaTest::tearDown()
{
    delete _model;
    delete _y;
    delete _alp;
    delete _b;
}

Test* lambdaTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("lambdaTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<lambdaTest>("testWithD",    &lambdaTest::testWithD));
    suiteOfTests->addTest(new TestCaller<lambdaTest>("testWithoutD", &lambdaTest::testWithoutD));

    return suiteOfTests;
}

void lambdaTest::testWithD()
{
    srand(time(0));
    double       lambdaOut = 0.0;
    DoubleMatrix lambda_alpOut,
                 lambda_bOut;
   
    test(*_model, *_y, *_alp, *_b, lambdaOut, lambda_alpOut, lambda_bOut, true);
}
void lambdaTest::testWithoutD()
{
    double       lambdaOut = 0.0;
    DoubleMatrix lambda_alpOut,
                 lambda_bOut;
   
    test(*_model, *_y, *_alp, *_b, lambdaOut, lambda_alpOut, lambda_bOut, false);
}
/**************************************************************
 *   Private functions
 **************************************************************/
/*
 * y = [1]
 *     [1]
 */
void lambdaTest::setY(DoubleMatrix &y)
{
    y.resize(2,1);
    y.fill(rand()/(pow(2.0,32.0)) * 100.0 );
}

/*
 * alpha = [1]
 *         [1]
 */
void lambdaTest::setAlp(DoubleMatrix &alp)
{
    alp.resize(2,1);
    alp.fill(rand()/(pow(2.0,32.0)) * 100.0);
}

/*
 * b = [1]
 *     [1]
 */
void lambdaTest::setB(DoubleMatrix &b)
{
    b.resize(2,1);
    b.fill(rand()/(pow(2.0,32.0)) * 100.0);
}


void lambdaTest::test(
    SpkModel<double> &model, 
    const DoubleMatrix &dvecY, 
    const DoubleMatrix &dvecAlp, 
    const DoubleMatrix &dvecB, 
    double lambdaOut, 
    DoubleMatrix &lambda_alpOut, 
    DoubleMatrix &lambda_bOut,
    const bool withD)
{
    DoubleMatrix exactTemp;

    double       dblexactTemp;
    int i;

    lambdaOut     = lambda(model, dvecY, dvecAlp, dvecB, withD);
    lambda_alpOut = lambda_alp(model, dvecY, dvecAlp, dvecB, withD);
    lambda_bOut   = lambda_b(model, dvecY, dvecAlp, dvecB, withD);

    dblexactTemp = funExactLambda(dvecAlp, dvecB, dvecY, withD);
    CPPUNIT_ASSERT_DOUBLES_EQUAL( dblexactTemp, lambdaOut, 0.0001);

    exactTemp = funExactLambda_alp(dvecAlp, dvecB, dvecY, withD);
    for( i=0; i<exactTemp.nr()*exactTemp.nc(); i++ )
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL(
            exactTemp.data()[i], 
            lambda_alpOut.data()[i], 0.0001
            );
    }
    
    exactTemp = funExactLambda_b(dvecAlp, dvecB, dvecY, withD);
    for( i=0; i<exactTemp.nr()*exactTemp.nc(); i++ )
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL( 
            exactTemp.data()[i],
            lambda_bOut.data()[i], 0.0001
            );
    }
}

/**************************************************************
 *   (Static) elsq_x_x function definition
 **************************************************************/


static DoubleMatrix elsq_x_x( 
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
    term5 = multiply(multiply(transpose(dmatH_x),dmatQinv),dmatH_x);
    term4add5 = subtract( term5, term4 );

    term6 = AkronBtimesC( transpose(rvec(multiply(ZsubH,tranZsubH))), identX, dmatQinv_x_x );
    term6div2 = divByScalar(term6, 2.0);


    term7 = AkronBtimesC( transpose(dmatH_x), transpose(ZsubH), dmatQinv_x );
    term8 = AkronBtimesC( transpose(ZsubH), transpose(dmatH_x), dmatQinv_x );
    term7add8div2 = divByScalar( add(term7, term8), -2.0 );
    term7add8div2Trans = transpose(term7add8div2);
    
    dmatElsq_x_x = add( term1sub2div2, add( term3, add(term4add5, add(term6div2, term7add8div2Trans))));
    
    return dmatElsq_x_x;
}
/**************************************************************
 *   (Static) Exact Lambda, Labmda_alp, and Lambda_b 
 **************************************************************/
static double funExactLambda(const DoubleMatrix &dvecAlp, 
                             const DoubleMatrix &dvecB, 
                             const DoubleMatrix &dvecY,
                             const bool withD){
	// Updated 1-8-01 Alyssa
	// fixed for const correctness
    const double *pdAlp = dvecAlp.data();
    const double *pdB   = dvecB.data();
    const double *pdY   = dvecY.data();
    double term1, term2;

    term1 = 0.5 * log( pow( (2.0*PI*pdB[0]), 2.0 ) )
          + 0.5 * (pow(pdY[0]-pdAlp[1]-pdB[1], 2.0) + pow(pdY[1]-pdAlp[1]-pdB[1], 2.0))/pdB[0];

    if( withD ){
        term2 = 0.5 * log( pow(2.0*PI*pdAlp[0],2.0) )
              + 0.5 * (pow(pdB[0], 2.0) + pow(pdB[1], 2.0))/pdAlp[0];
        return term1 + term2;
    }
    else{
        return term1;
    }
}

static DoubleMatrix funExactLambda_alp(const DoubleMatrix &dvecAlp, 
                                       const DoubleMatrix &dvecB,
                                       const DoubleMatrix &dmatY,
                                       const bool withD){
	// Updated 1-8-01 Alyssa
	// fixed for const correctness
    const double *pdAlp = dvecAlp.data();
    const double *pdB   = dvecB.data();
    DoubleMatrix term1(1, dvecAlp.nr()), term2(1, dvecAlp.nr());
    const double *pdY   = dmatY.data();

    term1.fill(0.0);
    term2.fill(0.0);

    double *pdTerm1 = term1.data();
    double *pdTerm2 = term2.data();
    
    pdTerm1[0] = 0.0;
    pdTerm1[1] = ( 2.0*(pdAlp[1]+pdB[1]) - (pdY[0]+pdY[1]) )/pdB[0];

    if( withD ){
        //pdAns[0] = 1.0/pdAlp[0] - 0.5*(pdB[0]*pdB[0] + pdB[1]*pdB[1]) / pdAlp[0]*pdAlp[0];
        //pdAns[1] = -2.0 * (1.0 - pdAlp[1] - pdB[1]) / pdB[0];
        pdTerm2[0] = 1.0/pdAlp[0]
                   + 0.5 * (-pow(pdAlp[0],-2.0)*pow(pdB[0],2.0)-pow(pdAlp[0],-2.0)*pow(pdB[1],2.0));
        pdTerm2[1] = 0.0;
        return add( term1, term2 );
        
    }
    else{
        return term1;
    }
}
static DoubleMatrix funExactLambda_b(const DoubleMatrix &dvecAlp, 
                                     const DoubleMatrix &dvecB, 
                                     const DoubleMatrix &dvecY,
                                     const bool withD){
	
	// Updated 1-8-01 Alyssa
	// fixed for const correctness
    DoubleMatrix term1(1, dvecB.nr()), term2(1, dvecB.nr());
    const double *pdAlp   = dvecAlp.data();
    const double *pdB     = dvecB.data();
    const double *pdY     = dvecY.data();
    double *pdTerm1 = term1.data();
    double *pdTerm2 = term2.data();

    term1.fill(0.0);
    term2.fill(0.0);

    pdTerm1[0] = 1.0/pdB[0]  
                -0.5 * (pow(pdB[0],-2.0)*pow((pdY[0]-pdAlp[1]-pdB[1]),2.0)
                        +pow(pdB[0],-2.0)*pow((pdY[1]-pdAlp[1]-pdB[1]),2.0));
    pdTerm1[1] = -(pdY[0]+pdY[1]-2.0*pdAlp[1]-2.0*pdB[1])/pdB[0] ;

    if( withD ){
        //pdAns[0] = 1.0/pdB[0] - pow((1.0 - pdAlp[1] - pdB[1]*pdB[1]), 2.0) + pdB[0]/pdAlp[0];
        //pdAns[1] = -2.0 * (1.0 - pdAlp[1] - pdB[1]) / pdB[0] + pdB[1]/pdAlp[0];
        pdTerm2[0] = pdB[0]/pdAlp[0];
        pdTerm2[1] = pdB[1]/pdAlp[0];
        return add( term1, term2 );
    }
    else{
        return term1;
    }
}
