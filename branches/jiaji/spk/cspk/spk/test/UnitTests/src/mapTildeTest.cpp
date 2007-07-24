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
 * File: mapTildeTest.cpp
 *
 *
 * Unit test for mapTilde.
 *
 * Author: Sachiko Honda (mapTildeExampleTest),
 *         Mitch Watrous (mapTildeParabolicTest and mapTildeQuadraticTest)
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include <iomanip>
#include <cfloat>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/transpose.h"
#include "../../../spk/matmax.h"
#include "../../../spk/matabs.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/subtract.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/pi.h"
#include "../../../spk/mapObj.h"
#include "../../../spk/isLessThanOrEqualTo.h"
#include "../../../spk/allTrue.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/identity.h"

#include "../../../spk/mapTilde.h"
#include "../../../spk/SpkValarray.h"

#include "mapTildeTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void mapTildeTest::setUp()
{
    // initializations
}
void mapTildeTest::tearDown()
{
    // clean up
}

Test* mapTildeTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("mapTildeTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<mapTildeTest>(
                         "mapTildeExampleTest", 
                         &mapTildeTest::mapTildeExampleTest));
    
    suiteOfTests->addTest(new TestCaller<mapTildeTest>(
                         "mapTildeParabolicTest", 
                         &mapTildeTest::mapTildeParabolicTest));
    suiteOfTests->addTest(new TestCaller<mapTildeTest>(
                         "mapTildeQuadraticTest", 
                         &mapTildeTest::mapTildeQuadraticTest));
    suiteOfTests->addTest(new TestCaller<mapTildeTest>(
                         "mapTildeZeroIterationTest", 
                         &mapTildeTest::mapTildeZeroIterationTest));
    
    return suiteOfTests;
}


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

static void setExampleInSpec(
    DoubleMatrix &dmatD,            // D
    DoubleMatrix &dvecY,            // y
    double       &dEps,             // eps
    int          &iMaxitr,          // mitr
    int          &level,            // level
    DoubleMatrix &dvecBinitial,     // bIn
    DoubleMatrix &dvecBupper,       // bUp
    DoubleMatrix &dvecBlower,       // bLow
    DoubleMatrix &dvecBstep,        // bStep
    DoubleMatrix &dvecBout,         // bOut
    DoubleMatrix &dvecNormOut,      // normOut
    DoubleMatrix &drowMapObj_bOut,      // mapObj_bOut
    DoubleMatrix &dmatMapObj_b_bOut     // mapObj_b_bOut
    );
static void getCorrectAnswer(
    DoubleMatrix &dvecCorrectBout, 
    DoubleMatrix &dvecCorrectNormOut, 
    DoubleMatrix &drowCorrectMapObj_bOut, 
    DoubleMatrix &dmatCorrectMapObj_b_bOut);

/*************************************************************************
 *
 * Function: mapTildeExampleTest
 *
 *
 * This test implements the example problem from the mapTilde specification. 
 *
 *************************************************************************/
class UserModelMapTildeExampleTest : public SpkModel<double>
{
    valarray<double> _b;
    int _i;
    const int _nB;
    const int _nY;

public:
    UserModelMapTildeExampleTest(int nB, int nY)
      :
      _nB(nB), _nY(nY), _b(nB)
    {};  
    ~UserModelMapTildeExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        assert(bval.size() == _nB);
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(b).
      // Returns a 2 dimensional column vector:
      //   [ b(2) ]
      //   [ b(2) ]
      //
      ret.resize(_nB);
      ret[0] = _b[1];
      ret[1] = _b[1];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // The derivative of f(b) with respect to b.
      // Returns a 2 by 2 matrix:
      //   [ 0  1 ]
      //   [ 0  1 ]
      //
      ret.resize(_nY * _nB);
      ret[0] = 0.0;
      ret[1] = 0.0;
      ret[2] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      assert(_b[0] != 0 );
      //
      // R(b).
      // Returns a 2 by 2 matrix:
      //   [ exp^b(1)    0     ]
      //   [    0     exp^b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = exp( _b[0] );
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = exp( _b[0] );

    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // The derivative of R(b) with respect to b.
      // Returns a 4 by 2 matrix:
      //   [ exp^b(1)    0    ]
      //   [    0        0    ]
      //   [    0        0    ]
      //   [ exp^b(1)    0    ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i< _nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = exp(_b[0]);
      ret[3] = exp(_b[0]);
      return true;
    }

    void doDataVarianceInv( valarray<double>& ret ) const
    {
      assert(_b[0] != 0 );
      //
      // R(b)^-1
      // Returns a 2 by 2 matrix:
      //   [ 1.0 / exp^b(1)     0          ]
      //   [     0          1.0 / exp^b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = 1.0 / exp( _b[0] );
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / exp( _b[0] );
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // The derivative of R(b) with respect to b.
      // Returns a 4 by 2 matrix:
      //   [ -1.0 / (exp^b(1))^2    0    ]
      //   [               0        0    ]
      //   [               0        0    ]
      //   [ -1. 0/ (exp^b(1))^2    0    ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i< _nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / ( exp(_b[0]) * exp(_b[0]) );
      ret[3] = -1.0 / ( exp(_b[0]) * exp(_b[0]) );
      return true;
    }   
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        //  D     = [ 1.0  0.0 ]
        //          [ 0.0  0.5 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 0.5;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        //  D^-1  = [ 1.0  0.0 ]
        //          [ 0.0  0.5 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 2.0;
    }
};

void mapTildeTest::mapTildeExampleTest()
{
    using namespace std;

    // Fixed case test: the case found in the O-matrix prototype counterpart
    const int   n = 2;
    const int   m = 2;
    DoubleMatrix dmatD(n,n);
    DoubleMatrix dvecY(m,1);
    double       dEps    = -1;
    int          iMaxitr = -1;
    int          level   = -1;
    DoubleMatrix dvecBinitial(n,1);
    DoubleMatrix dvecBupper(n,1);
    DoubleMatrix dvecBlower(n,1);
    DoubleMatrix dvecBstep(n,1);
    DoubleMatrix dvecBout(n,1);
    DoubleMatrix dvecNormOut(n,1);         // normOut
    DoubleMatrix drowMapObj_bOut(1,n);     // mapObj_bOut
    DoubleMatrix dmatMapObj_b_bOut(n,n);   // mapObj_b_bOut
    
    DoubleMatrix dvecCorrectBout(n,1);
    DoubleMatrix dvecCorrectNormOut(n*n,1);
    DoubleMatrix drowCorrectMapObj_bOut(1,n);
    DoubleMatrix dmatCorrectMapObj_b_bOut(n,n);
    DoubleMatrix scale(n,1);

    UserModelMapTildeExampleTest model(n,m);
    
    cout << setiosflags(ios::scientific) << setprecision(20);
    
    setExampleInSpec(
        dmatD, dvecY, dEps, iMaxitr, level, dvecBinitial, dvecBupper, dvecBlower, dvecBstep,
        dvecBout, dvecNormOut, drowMapObj_bOut, dmatMapObj_b_bOut
        );
    getCorrectAnswer(dvecCorrectBout, dvecCorrectNormOut, drowCorrectMapObj_bOut, dmatCorrectMapObj_b_bOut);
    
    Optimizer optimizer( dEps, iMaxitr, level );
    try{
        mapTilde( model,
            dvecY, optimizer, dvecBinitial, dvecBlower, dvecBupper, 
            dvecBout, dvecBstep, dvecNormOut, &drowMapObj_bOut, &dmatMapObj_b_bOut, true );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    // This is the relative error this test will allow between the values 
    // output by mapTilde and the "correct" values from O-Matrix.
    double relErrorAllowed=1.0;

    // Set the amount of relative error allowed by isDblEpsEqual(). 
    double relErrorIsDblEpsEqual = DBL_EPS_EQUAL_MULT * DBL_EPSILON;

    // Set scale so that isDmatEpsEqual() will return true if the following 
    // relationship is satisfied for each element of bOut and bCorrect:
    // 
    //     | bOut(i) - bCorrect(i) |
    //     --------------------------  <=  relErrorAllowed .
    //           | bOut(i) | 
    //
    scale = matabs( subtract(dvecCorrectBout, dvecBout)  );
    scale = mulByScalar( scale, relErrorAllowed/relErrorIsDblEpsEqual );

    CPPUNIT_ASSERT( isDmatEpsEqual( dvecBout, dvecCorrectBout, scale ) );

    // Set scale so that isDmatEpsEqual() will return true if the following 
    // relationship is satisfied for each element of normOut and normCorrect:
    // 
    //     | normOut(i) - normCorrect(i) |
    //     --------------------------------  <=  relErrorAllowed .
    //              | normOut(i) |
    //
    scale = matabs( dvecNormOut );
    scale = mulByScalar( scale, relErrorAllowed / relErrorIsDblEpsEqual );
    
    CPPUNIT_ASSERT( isDmatEpsEqual( dvecNormOut, dvecCorrectNormOut, scale ) );


    // Set scale so that isDmatEpsEqual() will return true if the following 
    // relationship is satisfied for each element of MapObj_bOut and MapObj_bCorrect:
    // 
    //     | MapObj_bOut(i) - MapObj_bCorrect(i) |
    //     ----------------------------------------  <=  relErrorAllowed .
    //              | MapObj_bOut(i) |
    //
    scale = matabs( drowMapObj_bOut );
    scale = mulByScalar( scale, relErrorAllowed / relErrorIsDblEpsEqual );

    CPPUNIT_ASSERT( isDmatEpsEqual( drowMapObj_bOut, drowCorrectMapObj_bOut, scale ) );

    // Set scale so that isDmatEpsEqual() will return true if the following 
    // relationship is satisfied for each element of MapObj_b_bOut and MapObj_b_bCorrect:
    // 
    //     | MapObj_b_bOut(i, j) - MapObj_b_bCorrect(i, j) |
    //     --------------------------------------------------  <=  relErrorAllowed .
    //                | MapObj_b_bOut(i, j) |
    //
    scale = matabs( dmatMapObj_b_bOut );
    scale = mulByScalar( scale, relErrorAllowed / relErrorIsDblEpsEqual );

    CPPUNIT_ASSERT( isDmatEpsEqual( dmatMapObj_b_bOut, dmatCorrectMapObj_b_bOut, scale ) );
}
//****************************************************************
//
//  Testing for zero iteration
//
//  When the max number of iterations allowed is
//  set to zero, the output value, BOut, should be
//  exactly same as BIn.
//
//****************************************************************

void mapTildeTest::mapTildeZeroIterationTest()
{
    using namespace std;

    // Fixed case test: the case found in the O-matrix prototype counterpart
    const int   nB = 2;
    const int   nY = 2;
    DoubleMatrix dmatD(nB,nB);
    DoubleMatrix dvecY(nY,1);
    double       dEps    = -1;
    int          iMaxitr = -1;
    int          level   = -1;
    DoubleMatrix dvecBinitial(nB,1);
    DoubleMatrix dvecBupper(nB,1);
    DoubleMatrix dvecBlower(nB,1);
    DoubleMatrix dvecBstep(nB,1);
    DoubleMatrix dvecBout(nB,1);
    DoubleMatrix dvecNormOut(nB,1);         // normOut
    DoubleMatrix drowMapObj_bOut(1,nB);     // mapObj_bOut
    DoubleMatrix dmatMapObj_b_bOut(nB,nB);   // mapObj_b_bOut
    
    DoubleMatrix dvecCorrectBout(nB,1);
    DoubleMatrix dvecCorrectNormOut(nB*nB,1);
    DoubleMatrix drowCorrectMapObj_bOut(1,nB);
    DoubleMatrix dmatCorrectMapObj_b_bOut(nB,nB);
    DoubleMatrix scale(nB,1);

    UserModelMapTildeExampleTest model(nB,nY);
    
    cout << setiosflags(ios::scientific) << setprecision(20);
    
    setExampleInSpec(
        dmatD, dvecY, dEps, iMaxitr, level, dvecBinitial, dvecBupper, dvecBlower, dvecBstep,
        dvecBout, dvecNormOut, drowMapObj_bOut, dmatMapObj_b_bOut
        );
    
    //
    // Fix the number of maximum iterations to zero.
    //

    getCorrectAnswer(dvecCorrectBout, dvecCorrectNormOut, drowCorrectMapObj_bOut, dmatCorrectMapObj_b_bOut);
    Optimizer optimizer( dEps, 0, level );
    try{
        mapTilde( model,
            dvecY, optimizer, dvecBinitial, dvecBlower, dvecBupper, 
            dvecBout, dvecBstep, dvecNormOut, &drowMapObj_bOut, &dmatMapObj_b_bOut, true );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    for( int i=0; i<nB; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( dvecBinitial.data()[i], dvecBout.data()[i] );
    }
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: setExampleInSpec
 *
 * 
 * Description
 * -----------
 *
 * Set parameters to values described in the O-matrix prototype's mapTilde.mat.
 *
 *  D     = [ 1.0  0.0 ]
 *          [ 0.0  0.5 ]
 *
 *  y     = [ 2.0 ]
 *          [ 2.0 ]
 *
 *  eps   = 1.0e-3
 *
 *  mitr  = 40
 *
 *  bIn   = [ 1.2 ]
 *          [ 0.8 ]
 *
 *  bUp   = [ 4.0 ]
 *          [ 4.0 ]
 *
 *  bLow  = [ -4.0 ]
 *          [ -4.0 ]
 *
 *  bStep = [ 0.05 ]
 *          [ 0.05 ]
 *
 *  initialize bOut
 *  initialize normOut
 *  initialize mapObj_bOut
 *  initialize mapObj_b_bOut
 *
 *************************************************************************/

static void setExampleInSpec(
    DoubleMatrix &dmatD,            // D
    DoubleMatrix &dvecY,            // y
    double       &dEps,             // eps
    int          &iMaxitr,          // mitr
    int          &level,            // level
    DoubleMatrix &dvecBinitial,     // bIn
    DoubleMatrix &dvecBupper,       // bUp
    DoubleMatrix &dvecBlower,       // bLow
    DoubleMatrix &dvecBstep,        // bStep
    DoubleMatrix &dvecBout,         // bOut
    DoubleMatrix &dvecNormOut,      // normOut
    DoubleMatrix &drowMapObj_bOut,      // mapObj_bOut
    DoubleMatrix &dmatMapObj_b_bOut     // mapObj_b_bOut
    )
{
  

    //
    //  D     = [ 1.0  0.0 ]
    //          [ 0.0  0.5 ]
    //
    double *pdD = dmatD.data();
    dmatD.fill(0.0);
    pdD[0] = 1.0;
    pdD[3] = 0.5;

    //
    //  y     = [ 2.0 ]
    //          [ 2.0 ]
    //
    dvecY.fill(2.0);
    
    //
    //  eps   = 1.0e-3
    //
    dEps = 1.0e-3;

    //
    //  mitr  = 40
    //
    iMaxitr = 40;

    //
    // level = 0
    //
    level = 0;

    //
    //  bIn   = [ 1.2 ]
    //          [ 0.8 ]
    //
    double *pdBinitial = dvecBinitial.data();
    pdBinitial[0] = 1.2;
    pdBinitial[1] = 0.8;
    
    //
    //  bUp   = [ 4.0 ]
    //          [ 4.0 ]
    //
    dvecBupper.fill(4.0);
    
    //
    //  bLow  = [ -4.0 ]
    //          [ -4.0 ]
    //
    dvecBlower.fill(-4.0);

    //
    //  bStep = [ 0.05 ]
    //          [ 0.05 ]
    //
    dvecBstep.fill(0.05);
    
    //
    //  allocate bOut
    //
    dvecBout.fill(-1);

    //
    //  alloate  normOut
    //
    dvecNormOut.fill(-1);

    //
    // allocate mapObj_bOut
    //
    drowMapObj_bOut.fill(-1);

    //
    // allocate mapObj_b_bOut
    //
    dmatMapObj_b_bOut.fill(-1);
    
}
/*************************************************************************
 *
 * Function: getCorrectAnswer
 *
 * 
 * Description
 * -----------
 *
 * Returns the correct answer for the above fixed example case.
 *
 * Bout          = [4.167515526715557e-004]
 *                 [9.997916131744145e-001]
 *
 * normOut       = [1.971920801019458e000]
 *                 [1.001091981357366e000]
 *                 [4.406303182302429e-002]
 *                 [6.564655188399626e-004]
 *
 * mapObj_bOut   = [6.371510708813588e-004, 1.580679926505013e-004]
 *
 * mapObj_b_bOut = [2.00033741585875950000e+000, 1.99977943613014690000e+000]
 *                 [1.99894643766312720000e+000, 3.99805181561295740000e+000]
 *
 *************************************************************************/

static void getCorrectAnswer(
    DoubleMatrix &dvecCorrectBout, 
    DoubleMatrix &dvecCorrectNormOut, 
    DoubleMatrix &drowCorrectMapObj_bOut, 
    DoubleMatrix &dmatCorrectMapObj_b_bOut)
{
    double *dBout = dvecCorrectBout.data();
    double *dNormOut = dvecCorrectNormOut.data();
    double *dMapObj_bOut = drowCorrectMapObj_bOut.data();
    double *dMapObj_b_bOut = dmatCorrectMapObj_b_bOut.data();

    dBout[0]          = 4.167515526715557e-004;
    dBout[1]          = 9.997916131744145e-001;

    dNormOut[0]       = 1.971920801019458e000;
    dNormOut[1]       = 1.001091981357366e000;
    dNormOut[2]       = 4.406303182302429e-002;
    dNormOut[3]       = 6.564655188399626e-004;

    dMapObj_bOut[0]   = 6.371510708813588e-004;
    dMapObj_bOut[1]   = 1.580679926505013e-004;

    dMapObj_b_bOut[0] = 2.00033741585875950000e+000;
    dMapObj_b_bOut[1] = 1.99894643766312720000e+000;
    dMapObj_b_bOut[2] = 1.99977943613014690000e+000;
    dMapObj_b_bOut[3] = 3.99805181561295740000e+000;

    return;
}

/*************************************************************************
 *
 * Function: mapTildeParabolicTest
 *
 *
 * This test uses a form for mapObj(b) that is a parabola: 
 *
 *                                    2
 *     mapObj(b) =  log(2 PI)  +  b      .
 *                                 (1)
 *
 * It accomplishes this by setting 
 *
 *     y     =  ( 1 )   ,
 *
 *     f(b)  =  ( 1 )   ,
 *
 *     R(b)  =  ( 1 )   ,
 *
 *     D(b)  =  ( 1/2 )   .
 *
 *************************************************************************/

class UserModelMapTildeParabolicTest : public SpkModel<double>
{
    valarray<double> _b;
    const int _nB;
    const int _nY;
public:
    UserModelMapTildeParabolicTest(int nB, int nY)
      :
    _nB(nB), _nY(nY)
    {};    
    ~UserModelMapTildeParabolicTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        assert(bval.size() == _nB);
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(b) = [ 1 ]
      //
      ret.resize(_nY,1);
      ret[0] = 1.0;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      //     f_b(b) = [ 0 ]
      //
      ret.resize(_nY, _nB);
      ret[0] = 0.0;
      return false;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      //     R(b)  =  [ 1 ]   .
      //
      ret.resize(_nY, _nY);
      ret[0] = 1.0;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      //     R_b(b)  =  [ 0 ]   .
      //
      ret.resize(_nY * _nY, _nB);
      ret[0] = 0.0;
      return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      //     R(b)^-1  =  [ 1 ]   .
      //
      ret.resize(_nY, _nY);
      ret[0] = 1.0;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      //     R^(-1)_b(b)  =  [ 0 ]   .
      //
      ret.resize(_nY * _nY, _nB);
      ret[0] = 0.0;
      return false;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
      //
      // D(alp) = [ 0,5 ]
      //
      ret.resize( _nB, _nB );
      ret[0] = 0.5;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      //
      // D(alp) = [ 2.0 ]
      //
      ret.resize( _nB, _nB );
      ret[0] = 2.0;
    }
};

void mapTildeTest::mapTildeParabolicTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  bool ok;

  const int nY = 1;
  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelMapTildeParabolicTest model(nB, nY);

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------


  DoubleMatrix dvecY( nY, 1 );
  dvecY.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------


  DoubleMatrix dmatD( nB, nB );
  dmatD.fill( 0.5 );

  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBIn  ( nB, 1 );
  DoubleMatrix dvecBOut ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  dvecBLow .fill( -1.0 );
  dvecBUp  .fill( +1.0 );
  dvecBIn  .fill(  0.5 );
  dvecBStep.fill(  0.001 );


  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  DoubleMatrix drowMapObj_bOut  ( 1, nB );
  DoubleMatrix dmatMapObj_b_bOut( nB, nB );

  DoubleMatrix dvecNormOut( nB, 1 );


  //------------------------------------------------------------
  // Remaining inputs to mapTilde.
  //------------------------------------------------------------

  double epsilon  = 1.e-6; 
  int nMaxIter    = 40; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  bool          withD     = true;

  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------
  try{
      mapTilde(
          model,
          dvecY, 
          optimizer,
          dvecBIn, 
          dvecBLow, 
          dvecBUp, 
          dvecBOut, 
          dvecBStep, 
          dvecNormOut, 
          &drowMapObj_bOut, 
          &dmatMapObj_b_bOut,
          withD
          );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  //------------------------------------------------------------
  // Compute MapObj(b) at bOut.
  //------------------------------------------------------------

  double dMapObjOut;

  // Compute and then set the first derivative of the objective
  // function at the final b value, if necessary.
  if ( ok )
  {
    DoubleMatrix* pdmatNull = 0;

    bool okMapObj;
    try{
        mapObj( model, 
                dvecY, 
                dvecBOut,
               &dMapObjOut,
                pdmatNull,
                withD,
                false,
                NULL
                );
        okMapObj = true;
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  DoubleMatrix dvecBKnown( nB, 1 );
  dvecBKnown.fill( 0.0 );

  // Compute mapObj(bKnown).  
  double dMapObjKnown = 0.5 * ( log( PI ) + log( 2.0 * PI ) );

  DoubleMatrix dmatMapObj_b_bKnown( nB, nB );
  dmatMapObj_b_bKnown.fill( 2.0 );

  
  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  CPPUNIT_ASSERT(ok);
  CPPUNIT_ASSERT(isConverged(epsilon, 
              dvecBLow, 
              dvecBUp, 
              dvecBOut, 
              dvecBKnown ));
  
}

/*************************************************************************
 *
 * Function: mapTildeQuadraticTest
 *
 *
 * This test uses a form for mapObj(b) that is quadratic in b: 
 *
 *                  1             
 *     mapObj(b) =  -  ( nY + nB )  log(2 PI)
 *                  2  
 * 
 *                         nB    -                         -
 *                     1  ----  |              2         2  |
 *                  +  -  >     |  [ j - b    ]   +  b      |  ,
 *                     2  ----  |         (j)         (j)   |
 *                        j = 1  -                         -
 *
 * where
 *
 *     nY  =  number of elements in y,
 *
 *     nB  =  number of elements in b,
 *
 * and
 *
 *     nB  <  nY  .
 *
 *
 * It accomplishes this by setting 
 *
 *                                   T
 *     y     =  ( 1, 2, 3, ... , nY )   ,
 *
 *                                                                   T
 *     f(b)  =  ( b   , b   , ... , b    , nB + 1, nB + 2, ... , nY )   ,
 *                 (1)   (2)         (nB)
 *
 *     R(b)  =  I     ,
 *               nY
 *
 *     D(b)  =  I     ,
 *               nB
 *
 * where
 *
 *     I   =  an m by m identity matrix.
 *      m
 *
 *************************************************************************/

class UserModelMapTildeQuadraticTest : public SpkModel<double>
{
    valarray<double> _b;
    int _i;
    const int _nB;
    const int _nY;
public:
    UserModelMapTildeQuadraticTest(int nB, int nY)
      : _nB(nB), _nY(nY), _b(nB)
    {};    
    ~UserModelMapTildeQuadraticTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      int i;
      //
      // Calculates
      //                                                                   T
      //     f(b)  =  ( b   , b   , ... , b    , nB + 1, nB + 2, ... , nY )   .
      //                 (1)   (2)         (nB)
      //
      ret.resize(_nY);
      for ( i = 0; i < _nB; i++ )
      {
        ret[i] = _b[i];
      }
      for ( i = _nB; i < _nY; i++ )
      {
        ret[i] = i + 1;
      }
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // Calculates
      //
      //                 -        -
      //                | I      0 |
      //     f_b(b)  =  |  nB      |  ,
      //                |          |
      //                | 0      0 |
      //                 -        -
      //
      // where
      //
      //     nB  =  number of elements in b.
      //
      ret.resize(_nY * _nB);
      for( int j=0; j<_nY*_nB; j++ )
        ret[j] = 0.0;

      for( int i=0; i<_nB; i++ )
      {
        ret[ i + i * _nY ] = 1.0;
      }
      return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     R(b) = I    ,
      //             nY
      // where
      //
      //     nY  =  number of elements in y.
      //
      ret.resize(_nY * _nY);
      for( int j=0; j<_nY; j++ )
      {
        for( int i=0; i<_nY; i++ )
        {
          if( i==j )
            ret[i+j*_nY] = 1.0;
          else
            ret[i+j*_nY] = 0.0;
        }
      }
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     R_b(b) = 0
      //
      ret.resize( _nY * _nY * _nB );
      for( int i=0; i< _nY*_nY*_nB; i++ )
        ret[i] = 0.0;
      return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     R(b)^-1 = I    ,
      //                nY
      // where
      //
      //     nY  =  number of elements in y.
      //
      ret.resize(_nY * _nY);
      for( int j=0; j<_nY; j++ )
      {
        for( int i=0; i<_nY; i++ )
        {
          if( i==j )
            ret[i+j*_nY] = 1.0;
          else
            ret[i+j*_nY] = 0.0;
        }
      }    
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     R^(-1)_b(b) = 0
      //
      ret.resize( _nY * _nY * _nB );
      for( int i=0; i< _nY*_nY*_nB; i++ )
        ret[i] = 0.0;
      return false;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     D(alp) = I    ,
      //               nB
      // where
      //
      //     nB  =  number of individual parameters.
      //
      ret.resize(_nB * _nB);
      for( int j=0; j<_nB; j++ )
      {
        for( int i=0; i<_nB; i++ )
        {
          ret[i+j*_nB] = ( i==j? 1.0 : 0.0 );
        }
      }
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      //
      // Calculates 
      //
      //     D(alp)^-1 = I    ,
      //                  nB
      // where
      //
      //     nB  =  number of individual parameters.
      //
      ret.resize(_nB * _nB);
      for( int j=0; j<_nB; j++ )
      {
        for( int i=0; i<_nB; i++ )
        {
          ret[i+j*_nB] = ( i==j? 1.0 : 0.0 );
        }
      }
    }
};


void mapTildeTest::mapTildeQuadraticTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  bool ok;

  int i;

  const int nB = 25;
  const int nY = 50;
  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelMapTildeQuadraticTest model(nB, nY);


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();
  for ( i = 0; i < nY; i++ )
  {
    pdYData[i] = i + 1;
  }


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------
  assert( nB < nY );

  DoubleMatrix dmatD = identity( nB );

  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBIn  ( nB, 1 );
  DoubleMatrix dvecBOut ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  // This is the known value for the (nB - 1)th element of the
  // b value that minimizes the objective function.
  assert( nB > 1 );
  double val = 0.5 * ( nB - 1 );

  dvecBLow .fill( -val );
  dvecBUp  .fill( +val );
  dvecBIn  .fill( 0.0 );
  dvecBStep.fill( 0.001 );

  double* pdBLowData = dvecBLow.data();
  double* pdBUpData  = dvecBUp .data();
  double* pdBOutData = dvecBOut.data();


  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  DoubleMatrix drowMapObj_bOut  ( 1, nB );
  DoubleMatrix dmatMapObj_b_bOut( nB, nB );

  DoubleMatrix dvecNormOut( nB, 1 );


  //------------------------------------------------------------
  // Remaining inputs to mapTilde.
  //------------------------------------------------------------

  double epsilon  = 1.e-6; 
  int nMaxIter    = 40; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  bool          withD     = true;

  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------
  try{
      mapTilde(  
          model,
          dvecY, 
          optimizer,
          dvecBIn, 
          dvecBLow, 
          dvecBUp, 
          dvecBOut, 
          dvecBStep, 
          dvecNormOut, 
          &drowMapObj_bOut, 
          &dmatMapObj_b_bOut,
          withD
          );
    ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }

  //------------------------------------------------------------
  // Compute MapObj(b) at bOut.
  //------------------------------------------------------------

  double dMapObjOut;

  if ( ok )
  {
    DoubleMatrix* pdmatNull = 0;

    bool okMapObj;
    try{
        mapObj( model, 
                dvecY, 
                dvecBOut,
               &dMapObjOut,
                pdmatNull,
                withD,
                false,
                NULL
                );
        okMapObj = true;
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  DoubleMatrix dvecBKnown( nB, 1 );
  double* pdBKnownData = dvecBKnown.data();

  // Compute the bKnown and mapObj(bKnown).  Note that the last 
  // element of bKnown is constrained by its upper bound, i.e.,
  // it is not equal to the last element of bTrue.
  double dMapObjKnown = 0.5 * ( nY + nB ) * log( 2.0 * PI );
  for ( i = 0; i < nB - 1; i++ )
  {
    pdBKnownData[i]  = 0.5  * ( i + 1 );
    dMapObjKnown    += 0.25 * ( i + 1 ) * ( i + 1 );
  }
  pdBKnownData[nB - 1]  = pdBUpData[nB - 1];
  dMapObjKnown         += 0.25 * ( nB * nB + 1 );

  DoubleMatrix dmatMapObj_b_bKnown = identity( nB );
  dmatMapObj_b_bKnown = mulByScalar( dmatMapObj_b_bKnown, 2.0);

  
  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  CPPUNIT_ASSERT(ok);
  CPPUNIT_ASSERT(isConverged(epsilon, 
              dvecBLow, 
              dvecBUp, 
              dvecBOut, 
              dvecBKnown ));
  
}

/*************************************************************************
 *
 * Function: isConverged
 *
 *************************************************************************/

bool
 mapTildeTest::isConverged( 
                       double epsilon,
                       const DoubleMatrix& dvecBLow,
                       const DoubleMatrix& dvecBUp,
                       const DoubleMatrix& dvecBOut,
                       const DoubleMatrix& dvecBKnown
                      )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nB = dvecBOut.nr();

  const double* pdBLowData   = dvecBLow  .data();
  const double* pdBUpData    = dvecBUp   .data();
  const double* pdBOutData   = dvecBOut  .data();
  const double* pdBKnownData = dvecBKnown.data();


  //------------------------------------------------------------
  // Check the final parameter value.
  //------------------------------------------------------------

  // Check to see if any elements of bOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(bOut - bKnown) <= epsilon (bUp - bLow)
  //
  bool isConverged = true;
  for (int i = 0; i < nB; i++ )
  {
    if ( fabs(pdBOutData[i] - pdBKnownData[i]) > 
            epsilon * (pdBUpData[i] - pdBLowData[i]) )
    {
      isConverged = false;
    }
  }
  return isConverged;  
}


