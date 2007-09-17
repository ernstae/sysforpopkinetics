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
 * File: mapObjTest.cpp
 *
 *
 * Unit test for mapObj(), class MapObj and class MapObj_b
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/mapObj.h"
#include "mapObjTest.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/matmax.h"
#include "../../../spk/matabs.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

void mapObjTest::setUp()
{
    // initializations
}
void mapObjTest::tearDown()
{
    // clean up
}

Test* mapObjTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("mapObjTest");

    suiteOfTests->addTest(new TestCaller<mapObjTest>("testMapObj", &mapObjTest::testMapObj));
    suiteOfTests->addTest(new TestCaller<mapObjTest>("testNonzeroBMean", &mapObjTest::testNonzeroBMean));
    suiteOfTests->addTest(new TestCaller<mapObjTest>("testFunctionObjects", &mapObjTest::testFunctionObjects));

    return suiteOfTests;
}

class UserModelMapObjTest : public SpkModel<double>
{
    valarray<double> _b;
    int          _i;
    const int _nB;
    const int _nY;
public:
  UserModelMapObjTest(int nB, int nY)
    : _nB(nB), _nY(nY), _b(nB)
  {};
    ~UserModelMapObjTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
      assert( bval.size() == _nB );
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(b) = [ b(2) ]
      //        [ b(2) ]
      //
      ret.resize(_nY);
      ret[0] = _b[1];
      ret[1] = _b[1];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // f_b(b) = [ 0  1 ]
      //          [ 0  1 ]
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
      //
      // R(b) = [ b(1)  0   ]
      //        [  0   b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = _b[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = _b[0];
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // R_b(b) = [ 1  0 ]
      //          [ 0  0 ]
      //          [ 0  0 ]
      //          [ 1  0 ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // R(b)^-1 = [ 1 / b(1)      0   ]
      //           [  0       1 / b(1) ]
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
      // R^(-1)_b(b) = [ -1 / b(1)^2  0 ]
      //               [ 0            0 ]
      //               [ 0            0 ]
      //               [ -1 / b(1)^2  0 ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / _b[0];
      ret[3] = -1.0 / _b[0];
      return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
       //
       // D(alp) = [ 1  0 ]
       //          [ 0  1 ]
       //
       ret.resize(_nB * _nB);
       ret[0] = 1.0;
       ret[1] = 0.0;
       ret[2] = 0.0;
       ret[3] = 1.0;
    }
};

//
// Correspondence:
//
//    mapObj               elsq/elsq_x
//       y                     z
//       f                     h
//       f_b                   h_x
//       R                     Q
//       Rinv                  Qinv
//       R_b                   Q_x
//
void mapObjTest::testMapObj(){

    const int nB = 2;
    const int nY = 2;
    UserModelMapObjTest model( nB, nY );
    DoubleMatrix b(nB,1); b.fill(2.0);
    DoubleMatrix y(nY,1); y.fill(1.0);

    DoubleMatrix mapObj_bOut(1, nB);
    DoubleMatrix dmatCorrectObj_bAns(1,2);
    double       mapObjOut;
    double       *pdMapObj_bOut;

    try{
        mapObj( model, y, b, &mapObjOut, 0, true, false, NULL );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    try{
        mapObj( model, y, b, 0, &mapObj_bOut, true, false, NULL );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    pdMapObj_bOut = mapObj_bOut.data();

    // For this test,
    //
    //     MapObj(b) = (1/2) log{[2 pi b(1)]^2} + [1 - b(2)]^2 / b(1)
    //               + (1/2) log{[2 pi]^2}      + (1/2) [b(1)^2 + b(2)^2]
    //
    // The gradient of MapObj(b) is equal to
    //
    //         / 1 / b(1) - [1 - b(2)]^2 / b(1)^2 + b(1) \
    //         |                                         |
    //         \     - 2 [1 - b(2)] / b(1) + b(2)        /
    //
    // If all the components of b are two,
    //
    //     MapObj(b)   = log(4 pi) + log(2 pi) + 1 / 2 + 4
    //                 = log(8 pi^2) + 4.5
    // 
    //     MapObj_b(b) = [ 1 / 2 - 1 / 4 + 2 , 1 + 2  ]
    //                 = [ 2.25 , 3 ]
    //
    CPPUNIT_ASSERT_DOUBLES_EQUAL(mapObjOut, 
		       8.868901313378768, 
		       fabs(mapObjOut - 8.868901313378768) / fabs(mapObjOut) * DBL_EPS_EQUAL_MULT );
    CPPUNIT_ASSERT_DOUBLES_EQUAL(pdMapObj_bOut[0], 
		       2.25, 
		       fabs(pdMapObj_bOut[0] - 2.25) / fabs(pdMapObj_bOut[0]) * DBL_EPS_EQUAL_MULT );
    CPPUNIT_ASSERT_DOUBLES_EQUAL(pdMapObj_bOut[1], 
		       3.0, 
		       fabs(pdMapObj_bOut[1] - 3.0) / fabs(pdMapObj_bOut[1]) * DBL_EPS_EQUAL_MULT );
}

//
// Correspondence:
//
//    mapObj               elsq/elsq_x
//       y                     z
//       f                     h
//       f_b                   h_x
//       R                     Q
//       Rinv                  Qinv
//       R_b                   Q_x
//
void mapObjTest::testNonzeroBMean(){

    const int nB = 2;
    const int nY = 2;
    UserModelMapObjTest model( nB, nY );
    DoubleMatrix b(nB,1); b.fill(2.0);
    DoubleMatrix y(nY,1); y.fill(1.0);

    // Set the mean value for b equal to a value other than zero.
    DoubleMatrix bMean(b);

    DoubleMatrix mapObj_bOut(1, nB);
    DoubleMatrix dmatCorrectObj_bAns(1,2);
    double       mapObjOut;
    double       *pdMapObj_bOut;

    try{
        mapObj( model, y, b, &mapObjOut, 0, true, false, NULL );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }
    try{
        mapObj( model, y, b, 0, &mapObj_bOut, true, false, NULL );
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    pdMapObj_bOut = mapObj_bOut.data();

    // For this test,
    //
    //     MapObj(b) = (1/2) log{[2 pi b(1)]^2} + [1 - b(2)]^2 / b(1)
    //               + (1/2) log{[2 pi]^2}      + (1/2) [(bMean(1)-b(1))^2 + (bMean(2)-b(2))^2]
    //
    // The gradient of MapObj(b) is equal to
    //
    //         / 1 / b(1) - [1 - b(2)]^2 / b(1)^2 - (bMean(1)-b(1)) \
    //         |                                                    |
    //         \     - 2 [1 - b(2)] / b(1) - (bMean(2)-b(2))        /
    //
    // If all the components of b and bMean are two,
    //
    //     MapObj(b)   = log(4 pi) + log(2 pi) + 1 / 2
    //                 = log(8 pi^2) + 0.5
    // 
    //     MapObj_b(b) = [ 1 / 2 - 1 / 4, 1 ]
    //                 = [ 0.25 , 1 ]
    //
    CPPUNIT_ASSERT_DOUBLES_EQUAL(mapObjOut, 
		       4.868901313378768, 
		       fabs(mapObjOut - 4.868901313378768) / fabs(mapObjOut) * DBL_EPS_EQUAL_MULT );
    CPPUNIT_ASSERT_DOUBLES_EQUAL(pdMapObj_bOut[0], 
		       0.25, 
		       fabs(pdMapObj_bOut[0] - 0.25) / fabs(pdMapObj_bOut[0]) * DBL_EPS_EQUAL_MULT );
    CPPUNIT_ASSERT_DOUBLES_EQUAL(pdMapObj_bOut[1], 
		       1.0, 
		       fabs(pdMapObj_bOut[1] - 1.0) / fabs(pdMapObj_bOut[1]) * DBL_EPS_EQUAL_MULT );
}

void mapObjTest::testFunctionObjects()
{
    const int nB = 2;
    const int nY = 2;
    UserModelMapObjTest model( nB, nY );
    DoubleMatrix b(nB,1);  b.fill(2.0);
    DoubleMatrix y(nY, 1); y.fill(1.0);
    DoubleMatrix N(1,1); N.data()[0] = nY;
    bool  withD;
    bool  isFO;

    double       mapObjOutExpected;
    DoubleMatrix mapObj_bOutExpected(1, nB);

    DoubleMatrix mapObjOutActual    (1, 1);
    DoubleMatrix mapObj_bOutActual  (1, nB);

    DoubleMatrix *pN;

    for( int j=0; j<2; j++ )
    {
      switch( j )
      {
        case 0: withD = false;
          break;
        case 1: withD = true;
          break;
        default:
          CPPUNIT_ASSERT( false );
      }
      for( int i=0; i<2; i++ )
      {
        switch( i )
        {
          case 0: isFO = true; pN = &N;
            break;
          case 1: isFO = false; pN = NULL;
            break;
          default:
            CPPUNIT_ASSERT( false );
        }
      

        try{
            MapObj<DoubleMatrix> mapOb( &model, y, withD, isFO, pN );
            mapObjOutActual = mapOb( b );
        }
        catch(...)
        {
            CPPUNIT_ASSERT( false );
        }
        try{
            MapObj_b<DoubleMatrix> map_bOb( &model, y, withD, isFO, pN );
            mapObj_bOutActual = map_bOb( b );
        }
        catch(...)
        {
            CPPUNIT_ASSERT( false );
        }
        try{
            mapObj( model, y, b, &mapObjOutExpected, &mapObj_bOutExpected, withD, isFO, pN );
        }
        catch(...)
        {
            CPPUNIT_ASSERT( false );
        }

        CPPUNIT_ASSERT_EQUAL( mapObjOutExpected, mapObjOutActual.data()[0] );
        CPPUNIT_ASSERT_EQUAL( mapObj_bOutExpected.data()[0], mapObj_bOutActual.data()[0] );
        CPPUNIT_ASSERT_EQUAL( mapObj_bOutExpected.data()[1], mapObj_bOutActual.data()[1] );
      }
    }
}

