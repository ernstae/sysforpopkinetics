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
 * File: indResidualsTest.cpp
 *
 *
 * Unit test for the function indResiduals.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/identity.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/indResiduals.h"
#include "../../../spk/multiply.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/transpose.h"
#include "indResidualsTest.h"

using namespace std;
using namespace CppUnit;


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: ThreeDataValuesModel
  //             
  //**********************************************************************

  class ThreeDataValuesModel : public SpkModel<double>
  {
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
  public:
    ThreeDataValuesModel(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~ThreeDataValuesModel(){};
  private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
      assert(aval.size() == _nA);
        _a = aval;
    }
    void doSetIndPar(const  valarray<double>& bval)
    {
      assert(bval.size() == _nB);
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        // Create an arbitrary Cholesky factor that will be used to ensure that
        // D is positive definite.
        //
        //     DChol  =   /   1.2    0.0    0.0   0.0  \
        //                |   2.2    3.5    0.0   0.0  |
        //                |   4.2    5.5    6.7   0.0  |  .
        //                \   7.2    8.5    9.7  10.0  /
        //
        valarray<double> DChol( _nB * _nB );
        DChol = 0.0;
        DChol[0 + 0 * _nB ] =  1.2;
        DChol[1 + 0 * _nB ] =  2.2;
        DChol[2 + 0 * _nB ] =  4.2;
        DChol[3 + 0 * _nB ] =  7.2;
        DChol[1 + 1 * _nB ] =  3.5;
        DChol[2 + 1 * _nB ] =  5.5;
        DChol[3 + 1 * _nB ] =  8.5;
        DChol[2 + 2 * _nB ] =  6.7;
        DChol[3 + 2 * _nB ] =  9.7;
        DChol[3 + 3 * _nB ] = 10.0;

        valarray<double> DCholTrans( _nB * _nB );
        DCholTrans = transpose( DChol, _nB );

        //
        //                           T
        //     D(alp) =  DChol  DChol   .
        //
        ret.resize(_nB * _nB);
        ret = multiply( DChol, _nB, DCholTrans, _nB );
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        //    D_alp(alp) = 0  .
        //
        ret.resize(_nB * _nB * _nA);
        ret = 0.0;
        return false;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        //                 /  exp[b(1)] \ 
        //     f(alp, b) = |  exp[b(1)] |  .
        //                 \  exp[b(1)] / 
        //
        ret.resize(_nYi);
        ret = exp( _b[0] );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        //     f_alp(alp, b) = 0  .
        //
        ret.resize(_nYi * _nA);
        ret = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        //                   /  exp[b(1)]     0     0     0  \ 
        //     f_b(alp, b) = |  exp[b(1)]     0     0     0  |  .
        //                   \  exp[b(1)]     0     0     0  / 
        //
        ret.resize(_nYi * _nB);
        ret = 0.0;
        ret[0] = exp( _b[0] );
        ret[1] = exp( _b[0] );
        ret[2] = exp( _b[0] );
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        //                 /  alp(1) exp[b(1)]        0                          0  \ 
        //     R(alp, b) = |  0                 alp(1) exp[b(1)]                 0  |  .
        //                 \  0                       0           alp(1) exp[b(1)]  / 
        //
        ret.resize(_nYi * _nYi);
        ret = 0.0;
        ret[0] = _a[0] * exp( _b[0] );
        ret[4] = _a[0] * exp( _b[0] );
        ret[8] = _a[0] * exp( _b[0] );
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        //                     /   exp[b(1)]          0  \ 
        //                     |   0                  0  | 
        //                     |   0                  0  | 
        //                     |   0                  0  | 
        //     R_alp(alp, b) = |   exp[b(1)]          0  |   .
        //                     |   0                  0  | 
        //                     |   0                  0  | 
        //                     |   0                  0  | 
        //                     \   exp[b(1)]          0  / 
        //
        ret.resize(_nYi * _nYi * _nA);
        ret = 0.0;
        ret[0] = exp( _b[0] );
        ret[4] = exp( _b[0] );
        ret[8] = exp( _b[0] );
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        //                   /   alp(1) exp[b(1)]         0          0          0  \ 
        //                   |   0                        0          0          0  | 
        //                   |   0                        0          0          0  | 
        //                   |   0                        0          0          0  | 
        //     R_b(alp, b) = |   alp(1) exp[b(1)]         0          0          0  |   .
        //                   |   0                        0          0          0  | 
        //                   |   0                        0          0          0  | 
        //                   |   0                        0          0          0  | 
        //                   \   alp(1) exp[b(1)]         0          0          0  / 
        //
        ret.resize(_nYi * _nYi * _nB);
        ret = 0.0;
        ret[0] = _a[0] * exp( _b[0] );
        ret[4] = _a[0] * exp( _b[0] );
        ret[8] = _a[0] * exp( _b[0] );
        return true;
    }   
  };

} // [End: unnamed namespace]


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void indResidualsTest::setUp()
{
  // initializations
}
void indResidualsTest::tearDown()
{
  // clean up
}

Test* indResidualsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "indResidualsTest" );

  suiteOfTests->addTest( new TestCaller<indResidualsTest>(
    "threeDataValuesTest", &indResidualsTest::threeDataValuesTest ) );

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: threeDataValuesTest
 *
 *
 * This test checks the case where the individual has three data values.
 *
 *************************************************************************/

void indResidualsTest::threeDataValuesTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Number of measurements.
  const int nY = 3;

  // Number of population parameters.
  const int nAlp = 2;

  // Number of individual parameters.
  const int nB = 4;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ThreeDataValuesModel model( nAlp, nB, nY );


  //------------------------------------------------------------
  // Quantities related to the population parameters.
  //------------------------------------------------------------

  valarray<double> alp( nAlp );

  alp[ 0 ] = 10.0;
  alp[ 1 ] = 25.0;

  // Set the value for the population parameter.
  model.setPopPar( alp );
  

  //------------------------------------------------------------
  // Quantities related to the individual parameters.
  //------------------------------------------------------------

  valarray<double> b( 1., nB );


  //------------------------------------------------------------
  // Quantities related to the data vector.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> y( nY );

  int j;

  // Set the data values equal to the model for mean f(alp, b)
  // plus a small residual value.
  for ( j = 0; j < nY; j++ )
  {
    y[j] = exp( b[0] ) + pow( -1.0, j ) * ( j + 1 ) * 0.01;
  }


  //------------------------------------------------------------
  // Quantities related to the residuals.
  //------------------------------------------------------------

  valarray<double> indPredOut  ( nY );
  valarray<double> indResOut   ( nY );
  valarray<double> indResWtdOut( nY );

  valarray<double> indParResOut   ( nB );
  valarray<double> indParResWtdOut( nB );


  //------------------------------------------------------------
  // Calculate the residuals.
  //------------------------------------------------------------

  try
  {
    indResiduals(
      model, 
      y,
      b,            
      &indPredOut,
      &indResOut,
      &indResWtdOut,
      &indParResOut,
      &indParResWtdOut );
  }
  catch( const SpkException& e )
  {
    cerr << e << endl;
    CPPUNIT_ASSERT_MESSAGE( "indResiduals failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "indResiduals failed for unknown reasons!", false);
  }

  // [Remove]==============================================
  /*
  cout << endl;
  cout << endl;
  cout << "=======================" << endl;
  cout << "indPredOut = " << endl;
  printInMatrix( indPredOut, 1 );
  cout << "indResOut = " << endl;
  printInMatrix( indResOut, 1 );
  cout << "indResWtdOut = " << endl;
  printInMatrix( indResWtdOut, 1 );
  cout << "indParResOut = " << endl;
  printInMatrix( indParResOut, 1 );
  cout << "indParResWtdOut = " << endl;
  printInMatrix( indParResWtdOut, 1 );
  cout << "-----------------------" << endl;
  */
  // [Remove]==============================================


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // The predicted values are calculated as follows:
  //
  //     pred  =  f (alp, b )  .
  //
  // For this test,
  //
  //     pred     =  exp( b(1) )  .
  //         (j)
  //
  for ( j = 0; j < nY; j++ )
  {
    CPPUNIT_ASSERT( isDblEpsEqual( exp( b[0] ), indPredOut[j], fabs( indPredOut[j] ) ) );
  }
      
  // The residuals are calculated as follows:
  //
  //     res  =  y  -  pred  .
  //
  for ( j = 0; j < nY; j++ )
  {
    CPPUNIT_ASSERT( isDblEpsEqual( y[j] - indPredOut[j], indResOut[j], fabs( indResOut[j] ) ) );
  }
      
  // The weighted residuals are calculated as follows:
  //
  //                    -1/2
  //     resWtd  =  R(b)      *  res   .
  //
  // The known values that appear here were calculated using the
  // Octave code that appears at the end of this function.
  CPPUNIT_ASSERT( isDblEpsEqual(  0.00191801835541641, indResWtdOut[0], fabs( indResWtdOut[0] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( -0.00383603671083290, indResWtdOut[1], fabs( indResWtdOut[1] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual(  0.00575405506624931, indResWtdOut[2], fabs( indResWtdOut[2] ) ) );
      
  // The individual parameter residuals are calculated as follows:
  //
  //     bWtd  =  - b  .
  //
  CPPUNIT_ASSERT( isDblEpsEqual( - b[0], indParResOut[0], fabs( indParResOut[0] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( - b[1], indParResOut[1], fabs( indParResOut[1] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( - b[2], indParResOut[2], fabs( indParResOut[2] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( - b[3], indParResOut[3], fabs( indParResOut[3] ) ) );
      
  // The weighted individual parameter residuals are calculated as follows:
  //
  //                      -1/2
  //     bWtd  =  D(alpha)      *  ( - b )  .
  //
  // The known values that appear here were calculated using the
  // Octave code that appears at the end of this function.
  // 
  // Note that the scales have been increased slightly for these
  // comparisons.
  CPPUNIT_ASSERT( isDblEpsEqual( -0.87351003662302229, indParResWtdOut[0],  10.0 * fabs( indParResWtdOut[0] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( -0.17854871318119675, indParResWtdOut[1],  10.0 * fabs( indParResWtdOut[1] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( -0.05911120680695928, indParResWtdOut[2],  10.0 * fabs( indParResWtdOut[2] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( -0.00178338758982985, indParResWtdOut[3], 100.0 * fabs( indParResWtdOut[3] ) ) );
      
  // The known values were calculated using the following Octave code.
  //
  //~~~~~~~~~~~~~~~<Begin Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  //     #--------------------------------------------------------------------
  //     #
  //     # Preliminaries.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     format long
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # Calculate the weighted residuals.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     nY = 3
  //     nB = 4
  //     
  //     a = 10.0
  //     
  //     b = ones( nB, 1 )
  //           
  //     y = [ exp( b(1) ) + 0.01;
  //           exp( b(1) ) - 0.02;
  //           exp( b(1) ) + 0.03 ]
  //     
  //     f = exp( b(1) ) * ones( nY, 1 )
  //     
  //     R = a * exp( b(1) ) * eye( nY )
  //     
  //     cov = R
  //     
  //     resWtd = sqrtm( inverse( cov ) ) * ( y - f )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # Calculate the weighted individual parameter residuals.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     # Create an arbitrary Cholesky factor that will be used to ensure that
  //     # D will be positive definite.
  //     DChol = [ 1.2,    0.0,    0.0,   0.0;
  //               2.2,    3.5,    0.0,   0.0;
  //               4.2,    5.5,    6.7,   0.0;
  //               7.2,    8.5,    9.7,  10.0 ]
  //     
  //     D = DChol * DChol'
  //     
  //     cov = D
  //     
  //     bWtd = sqrtm( inverse( cov ) ) * ( 0.0 * ones( nB, 1 ) - b )
  //
  //~~~~~~~~~~~~~~~<End Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}

