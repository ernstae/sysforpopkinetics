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
 * File: estimateBTest.cpp
 *
 *
 * Unit test for estimateB.
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
#include "../../../spk/estimateB.h"
#include "estimateBTest.h"

#include <cstdlib>
#include "../../../spk/SpkModel.h"
#include "../../../spk/transpose.h"
#include "../../../spk/subtract.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/NaiveFoModel.h"

using SPK_VA::valarray;
using namespace CppUnit;

 /**************************************************************
 *   class UserModel declaration
 **************************************************************/
class estimateBTest::UserModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA, _nB, _nYi;
public:
    UserModel(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~UserModel(){};
private:
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
    void doIndParVariance( valarray<double>& ret ) const
    {
      //
      // D(alp) = [ a(2)   0.0  ]
      //          [ 0.0    a(2) ]
      //
      ret.resize(_nB * _nB);
      for( int i=0; i<_nB*_nB; i++ )
        ret[i] = 0.0;
      ret[0] = _a[1];
      ret[3] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
      //
      // D_alp(alp) = [ 0.0  1.0 ]
      //              [ 0.0  0.0 ]
      //              [ 0.0  0.0 ]
      //              [ 0.0  1.0 ]
      //
      ret.resize(_nB*_nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;
      ret[4] = 1;
      ret[7] = 1;
      return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      //
      // D(alp)^(-1) = [ 1.0 / a(2)    0.0       ]
      //               [ 0.0           1.0 / a() ]
      //
      ret.resize(_nB * _nB);
      ret[0] = 1.0 / _a[1];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / _a[1];
    }
    bool doIndParVarianceInv_a( valarray<double>& ret ) const
    {
      //
      // D(alp)^(-1)_alp = [ 0.0   -1.0 / a(2)^2 ]
      //                   [ 0.0   0.0           ]
      //                   [ 0.0   0.0           ]
      //                   [ 0.0   -1.0 / a(2)^2 ]
      //               
      ret.resize(_nB*_nB * _nA);
      for( int i=0; i<_nB*_nB*_nA; i++ )
        ret[i] = 0.0;

      ret[4] = -1.0 / (_a[1] * _a[1]);
      ret[7] = -1.0 / (_a[1] * _a[1]);
      return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(alp,b) = [ b(1) ]
      //            [ b(2) ]
      //
      ret.resize( _nYi );
      ret[0] = _b[0];
      ret[1] = _b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
      //
      // f_alp(alp,b) = [ 0.0  0.0 ]
      //                [ 0.0  0.0 ]
      //
      ret.resize( _nYi * _nA );
      ret[0] = 0.0;
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 0.0;
      return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // f_b(alp,b) = [ 1.0  0.0 ]
      //              [ 0.0  1.0 ]
      //
      ret.resize( _nYi * _nB );
      ret[0] = 1.0;
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0;
      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // R(alp,b) = [ alp(1)  0.0    ]
      //            [ 0.0     alp(1) ]
      //
      ret.resize(_nYi * _nYi);
      ret[0] = _a[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = _a[0];
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
      //
      // R_alp(alp,b) = [ 1.0   0.0 ]
      //                [ 0.0   0.0 ]
      //                [ 0.0   0.0 ]
      //                [ 1.0   0.0 ]
      //
      ret.resize( _nYi*_nYi * _nA );
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;
      ret[0] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // R_b(alp,b)   = [ 0.0   0.0 ]
      //                [ 0.0   0.0 ]
      //                [ 0.0   0.0 ]
      //                [ 0.0   0.0 ]
      //
      ret.resize( _nYi*_nYi * _nB );
      for( int i=0; i<_nYi*_nYi*_nB; i++ )
        ret[i] = 0.0;
      return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // R(alp,b)^(-1) = [ 1.0 / alp(1)  0.0          ]
      //                 [ 0.0           1.0 / alp(1) ]
      //
      ret.resize(_nYi * _nYi);
      ret[0] = 1.0 / _a[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / _a[0];
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
      //
      // R(alp,b)^(-1)_alp = [ -1.0 / alp(1)^2    0.0 ]
      //                     [ 0.0                0.0 ]
      //                     [ 0.0                0.0 ]
      //                     [  -1.0 / alp(1)^2   0.0 ]
      //
      ret.resize( _nYi*_nYi, _nA );
      for( int i=0; i<_nYi*_nYi*_nA; i++ )
        ret[i] = 0.0;
      ret[0] = -1.0 / (_a[0] * _a[0]);
      ret[3] = -1.0 / (_a[0] * _a[0]);
      return true;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // R(alp,b)^(-1)_b   = [ 0.0   0.0 ]
      //                     [ 0.0   0.0 ]
      //                     [ 0.0   0.0 ]
      //                     [ 0.0   0.0 ]
      //
      ret.resize( _nYi*_nYi, _nB );
      for( int i=0; i<_nYi*_nYi*_nB; i++ )
        ret[i] = 0.0;
      return false;
    }   
};

void estimateBTest::setUp()
{
    _nA = 2;
    _nB = 2;
    _nY = 2;
    
    _y = new DoubleMatrix(_nY,1);
    double *dY = _y->data();
    dY[0] = 1;
    dY[1] = 2;

    _eps = 1.0e-3;
    _mitr = 40;

    //
    // alp = [ 1 ]
    //       [ 1 ]
    //
    _alp = new DoubleMatrix(_nA,1);
    _alp->fill(1);

    //
    // b   = [ 3/4 ]
    //       [ 3/4 ]
    //
    _bin = new DoubleMatrix(_nB,1);
    _bin->fill(.75);

    //
    // bUp = [ 10 ]
    //       [ 10 ]
    //
    _bup = new DoubleMatrix(_nB,1);
    _bup->fill(10);
    
    //
    // bLow= [ 0 ]
    //       [ 0 ]
    //
    _blow = new DoubleMatrix(_nB,1);
    _blow->fill(0);
    
    //
    // bStep=[ 0.1 ]
    //       [ 0.1 ]
    //
    _bstep = new DoubleMatrix(_nB,1);
    _bstep->fill(0.1);
}
void estimateBTest::tearDown()
{
    delete _y;
    delete _alp;
    delete _bin;
    delete _bup;
    delete _blow;
    delete _bstep;
}

Test* estimateBTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "estimateBTest" );

    // duplicate the following example for each test case, replacing the case name.
    //suiteOfTests->addTest(new TestCaller<estimateBTest>("testExample", testExample));
    suiteOfTests->addTest(new TestCaller<estimateBTest>("testEstimateB", &estimateBTest::testEstimateB));


    return suiteOfTests;
}

void estimateBTest::testEstimateB()
{
    using namespace std;

    DoubleMatrix dvecBhatOut(_nB,1);
    DoubleMatrix dvecBtildeOut(_nB,1);
    DoubleMatrix dmatBtilde_alpOut(_nB,_nA);

    UserModel model(_nA, _nB, _y->nr());
    model.selectIndividual(0);
    Optimizer optimizer( _eps, _mitr, 0 );

    try{
        estimateB(model, _blsq, optimizer, *_y, 
            *_alp, *_bin, *_blow, *_bup, *_bstep, &dvecBhatOut, &dvecBtildeOut, &dmatBtilde_alpOut);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    
    checkAns( transpose(dvecBhatOut), transpose(dvecBtildeOut), dmatBtilde_alpOut );

    //
    // Test with the naive FO model
    //
    NaiveFoModel naiveFOmodel ( & model, _bstep->toValarray() );
    try{
        estimateB(naiveFOmodel, true, optimizer, *_y, 
            *_alp, *_bin, *_blow, *_bup, *_bstep, &dvecBhatOut, &dvecBtildeOut, &dmatBtilde_alpOut);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    
    checkAns( transpose(dvecBhatOut), transpose(dvecBtildeOut), dmatBtilde_alpOut );

}
void estimateBTest::testExample()
{
    using namespace std;
    int    Ny=2;
    int    Nb=2;
    int    Na=2;
    bool   blsq      = false;
    double epsilon   = 1.0e-3; 
    int    mitr      = 40; 
    DoubleMatrix dataForAll(Ny,1); 
    DoubleMatrix alp(Na,1); 
    DoubleMatrix bIn(Nb,1); 
    DoubleMatrix bUp(Nb,1); 
    DoubleMatrix bLow(Nb,1);
    DoubleMatrix bStep(Nb,1);
    DoubleMatrix bHatOut(Nb,1);
    DoubleMatrix bTildeOut(Nb,1);
    DoubleMatrix bTilde_alpOut(Nb,Na);

    double *dY = dataForAll.data();
    dY[0] = 1;
    dY[1] = 2;

    alp.fill(1);
    bIn.fill(.75);
    bUp.fill(10);
    bLow.fill(0);
    bStep.fill(0.1);

	// Test Modefied Laplace and Expected Hessian objectives
    UserModel model( Na, Nb, Ny );
	Optimizer optimizer( epsilon, mitr, 0 );

    try{
        estimateB(model, false, optimizer, dataForAll, 
        alp, bIn, bLow, bUp, bStep, 
        &bHatOut, &bTildeOut, &bTilde_alpOut);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    cout << "b^' = " << endl;
    (transpose(bHatOut)).print();
    cout << endl;

    cout << "b~' = " << endl;
    (transpose(bTildeOut)).print();
    cout << endl;

    cout << "b~_alp = " << endl;
    bTilde_alpOut.print();
    
}
/*-----------------------------------------------------------------------
 *  Utility functions
 *-----------------------------------------------------------------------*/

#include "../../../spk/subtract.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/allTrue.h"
#include "../../../spk/matmax.h"
#include "../../../spk/matabs.h"
#include <cmath>
#include <cfloat>
void estimateBTest::checkAns(
                    const DoubleMatrix& actualBhatOutT, 
                    const DoubleMatrix& actualBtildeOutT, 
                    const DoubleMatrix& actualBtilde_alpOut)
{
    using namespace std;

    int nB = actualBhatOutT.nc();
    int nA = actualBtilde_alpOut.nc();
    const double *pActualBhatT         = actualBhatOutT.data();
    const double *pActualBtildeT       = actualBtildeOutT.data();
    const double *pActualBtilde_alpOut = actualBtilde_alpOut.data();

    DoubleMatrix correctBhatT(1,nB);
    DoubleMatrix correctBtildeT(1,nB);
    DoubleMatrix correctBtilde_alp(nB,nA);
    double *pCorrectBhatT      = correctBhatT.data();
    double *pCorrectBtildeT    = correctBtildeT.data();
    double *pCorrectBtilde_alp = correctBtilde_alp.data();

    pCorrectBhatT[0] = .5;
    pCorrectBhatT[1] = 1.;

    pCorrectBtildeT[0] = .5;
    pCorrectBtildeT[1] = 1.;

    pCorrectBtilde_alp[0] = -0.25;
    pCorrectBtilde_alp[1] = -0.5;
    pCorrectBtilde_alp[2] = 0.25;
    pCorrectBtilde_alp[3] = 0.5;

    DoubleMatrix dmatTiny(nB,nA);
    dmatTiny.fill(1e-10);
    const double* pTiny = dmatTiny.data();

    int i;

    // Test bHat
    for(i=0; i<nB; i++)
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL(
            pCorrectBhatT[i], 
            pActualBhatT[i], 
            fabs(pActualBhatT[i]-pCorrectBhatT[i])
            /fabs(pCorrectBhatT[i]+DBL_EPSILON*2)*DBL_EPS_EQUAL_MULT );
    }

    // Test bTilde
    for(i=0; i<nB; i++)
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL(
            pCorrectBtildeT[i], 
            pActualBtildeT[i], 
            fabs(pActualBtildeT[i]-pCorrectBtildeT[i])
            /fabs(pCorrectBtildeT[i]+DBL_EPSILON*2)*DBL_EPS_EQUAL_MULT );
    }

    // Test bTilde_alp
    for(i=0; i<nB*nA; i++)
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL(
            pCorrectBtilde_alp[i], 
            pActualBtilde_alpOut[i],
            fabs(pActualBtilde_alpOut[i]-pCorrectBtilde_alp[i])
            /fabs(pCorrectBtilde_alp[i]+DBL_EPSILON*2)*DBL_EPS_EQUAL_MULT );
    }

}

