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
 * File: popResidualsTest.cpp
 *
 *
 * Unit test for the function popResiduals.
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
#include "../../../spk/multiply.h"
#include "../../../spk/Objective.h"
#include "../../../spk/popResiduals.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/transpose.h"
#include "popResidualsTest.h"

using namespace std;
using namespace CppUnit;


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: ThreeDataValuesPerIndModel
  //             
  //**********************************************************************

  class ThreeDataValuesPerIndModel : public SpkModel<double>
  {
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
  public:
    ThreeDataValuesPerIndModel(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~ThreeDataValuesPerIndModel(){};
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

void popResidualsTest::setUp()
{
  // initializations
}
void popResidualsTest::tearDown()
{
  // clean up
}

Test* popResidualsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "popResidualsTest" );

  suiteOfTests->addTest( new TestCaller<popResidualsTest>(
    "firstOrderTest", &popResidualsTest::firstOrderTest ) );

  suiteOfTests->addTest( new TestCaller<popResidualsTest>(
    "expectedHessianTest", &popResidualsTest::expectedHessianTest ) );

  suiteOfTests->addTest( new TestCaller<popResidualsTest>(
    "modifiedLaplaceTest", &popResidualsTest::modifiedLaplaceTest ) );

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: firstOrderTest
 *
 *************************************************************************/

void popResidualsTest::firstOrderTest()
{
  threeDataValuesPerIndTest( FIRST_ORDER );
  threeDataValuesPerInd_usesBCheckTest( FIRST_ORDER );
}


/*************************************************************************
 *
 * Function: expectedHessianTest
 *
 *************************************************************************/

void popResidualsTest::expectedHessianTest()
{
  threeDataValuesPerIndTest( EXPECTED_HESSIAN );
  threeDataValuesPerInd_usesBCheckTest( EXPECTED_HESSIAN );
}


/*************************************************************************
 *
 * Function: modifiedLaplaceTest
 *
 *************************************************************************/

void popResidualsTest::modifiedLaplaceTest()
{
  threeDataValuesPerIndTest( MODIFIED_LAPLACE );
  threeDataValuesPerInd_usesBCheckTest( MODIFIED_LAPLACE );
}


/*************************************************************************
 *
 * Function: threeDataValuesPerIndTest
 *
 *
 * This test checks the case where each individual in the population
 * has three data values.
 *
 *************************************************************************/

void popResidualsTest::threeDataValuesPerIndTest( enum Objective whichObjective )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Number of individuals.
  const int nInd = 4;

  // Number of measurements per individual.
  const int nY_i = 3;

  // Number of measurements in total.
  const int nY = nInd * nY_i;

  const int nAlp = 2;

  const int nB = 4;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ThreeDataValuesPerIndModel model( nAlp, nB, nY_i );


  //------------------------------------------------------------
  // Quantities related to the population parameters.
  //------------------------------------------------------------

  valarray<double> alp( nAlp );

  alp[ 0 ] = 10.0;
  alp[ 1 ] = 25.0;
  

  //------------------------------------------------------------
  // Quantities related to the individual parameters.
  //------------------------------------------------------------

  valarray<double> bAll( 1., nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the data vector.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( nY_i, nInd );

  int i;
  int j;

  // Set the data values equal to the model for mean f( alp, b_i )
  // plus a small residual value.
  for ( i = 0; i < nInd; i++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      Y[i * nY_i + j] = exp( bAll[0] ) + pow( -1.0, j ) * ( j + 1 ) * 0.01;
    }
  }


  //------------------------------------------------------------
  // Quantities related to the population residuals.
  //------------------------------------------------------------

  valarray<double> popPredOut  ( nY );
  valarray<double> popResOut   ( nY );
  valarray<double> popResWtdOut( nY );

  valarray<double> popIndParResOut   ( nB * nInd );
  valarray<double> popIndParResWtdOut( nB * nInd );


  //------------------------------------------------------------
  // Calculate the population residuals.
  //------------------------------------------------------------

  try
  {
    popResiduals(
      model, 
      whichObjective,
      N,
      Y,
      alp,
      bAll,            
      &popPredOut,
      &popResOut,
      &popResWtdOut,
      &popIndParResOut,
      &popIndParResWtdOut );
  }
  catch( const SpkException& e )
  {
    cerr << e << endl;
    CPPUNIT_ASSERT_MESSAGE( "popResiduals failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "popResiduals failed for unknown reasons!", false);
  }

  // [Remove]==============================================
  /*
  cout << endl;
  cout << endl;
  cout << "=======================" << endl;
  cout << "objective = " << whichObjective << endl;
  cout << "popPredOut = " << endl;
  printInMatrix( popPredOut, 1 );
  cout << "popResOut = " << endl;
  printInMatrix( popResOut, 1 );
  cout << "popResWtdOut = " << endl;
  printInMatrix( popResWtdOut, 1 );
  cout << "popIndParResOut = " << endl;
  printInMatrix( popIndParResOut, 1 );
  cout << "popIndParResWtdOut = " << endl;
  printInMatrix( popIndParResWtdOut, 1 );
  cout << "-----------------------" << endl;
  */
  // [Remove]==============================================


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // The predicted values are calculated as follows:
  //
  //     pred   =  f (alp, b )  -  d  f (alp, b )  b   .
  //         i      i       i       b  i       i    i 
  //
  // For this test,
  //
  //     pred      =  ( 1 - b(1) ) * exp( b(1) )  .
  //         i(j)
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     pred      =  1  .
      //         i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 1.0, popPredOut[i * nY_i + 0], fabs( popPredOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 1.0, popPredOut[i * nY_i + 1], fabs( popPredOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 1.0, popPredOut[i * nY_i + 2], fabs( popPredOut[i * nY_i + 2] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero.
      //
      //     pred      =  ( 1 - 1 ) * exp( 1 )
      //         i(j)
      //
      //               =  0  .
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popPredOut[i * nY_i + 0], fabs( popPredOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popPredOut[i * nY_i + 1], fabs( popPredOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popPredOut[i * nY_i + 2], fabs( popPredOut[i * nY_i + 2] ) ) );
    }
  }
      
  // The residuals are calculated as follows:
  //
  //     res   =  y   -  pred   .
  //        i      i         i
  //
  for ( i = 0; i < nInd; i++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      CPPUNIT_ASSERT( isDblEpsEqual( Y[i * nY_i + j] - popPredOut[i * nY_i + j],
        popResOut[i * nY_i + j], fabs( popResOut[i * nY_i + j] ) ) );
    }
  }
      
  // The weighted residuals are calculated as follows:
  //
  //                  -1/2
  //     wres   =  cov      *  res   .
  //         i        i           i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     cov   =  R (alp, 0 )
      //        i      i
      //                                                 T
      //        +  d  f (alp, 0 )  D(alp)  d  f (alp, 0 )   .
      //            b  i                    b  i
      // 
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual( 0.456885896444145, popResWtdOut[i * nY_i + 0], fabs( popResWtdOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.447399063463640, popResWtdOut[i * nY_i + 1], fabs( popResWtdOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.463210451764482, popResWtdOut[i * nY_i + 2], fabs( popResWtdOut[i * nY_i + 2] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero, i.e.,
      //
      //     cov   =  R (alp, b )
      //        i      i       i
      //                                                 T
      //        +  d  f (alp, b )  D(alp)  d  f (alp, b )   .
      //            b  i       i            b  i       i
      // 
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual( 0.355086538546659, popResWtdOut[i * nY_i + 0], fabs( popResWtdOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.349332483480409, popResWtdOut[i * nY_i + 1], fabs( popResWtdOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.358922575257491, popResWtdOut[i * nY_i + 2], fabs( popResWtdOut[i * nY_i + 2] ) ) );
    }
  }
      
  // The individual parameter residuals are calculated as follows:
  //
  //     bRes   =  - b   .
  //         i        i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     bRes      =  0  .
      //         i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[0], fabs( popIndParResOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[1], fabs( popIndParResOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[2], fabs( popIndParResOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[3], fabs( popIndParResOut[3] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero, i.e.,
      //
      //     bRes      =  - 1  .
      //         i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[0], fabs( popIndParResOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[1], fabs( popIndParResOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[2], fabs( popIndParResOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[3], fabs( popIndParResOut[3] ) ) );
    }
  }
      
  // The weighted individual parameter residualss are calculated as follows:
  //
  //                          -1/2
  //     bResWtd   =  D(alpha)      *  ( - b  )   .
  //            i                           i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     bResWtd      =  0  .
      //            i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[0], fabs( popIndParResWtdOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[1], fabs( popIndParResWtdOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[2], fabs( popIndParResWtdOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[3], fabs( popIndParResWtdOut[3] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero.
      //
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      // 
      // Note that the scales have been increased slightly for these
      // comparisons.
      CPPUNIT_ASSERT( isDblEpsEqual( -0.87351003662302229, popIndParResWtdOut[0],  10.0 * fabs( popIndParResWtdOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.17854871318119675, popIndParResWtdOut[1],  10.0 * fabs( popIndParResWtdOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.05911120680695928, popIndParResWtdOut[2],  10.0 * fabs( popIndParResWtdOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.00178338758982985, popIndParResWtdOut[3], 100.0 * fabs( popIndParResWtdOut[3] ) ) );
    }
  }
      
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
  //     # Common quantities.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     nY = 3
  //     nB = 4
  //     
  //     a = 10.0
  //     
  //     bOrig = ones( nB, 1 )
  //           
  //     y = [ exp( bOrig(1) ) + 0.01;
  //           exp( bOrig(1) ) - 0.02;
  //           exp( bOrig(1) ) + 0.03 ]
  //     
  //     # Create an arbitrary Cholesky factor that will be used to 
  //     # ensure that D will be positive definite.
  //     DChol = [ 1.2,    0.0,    0.0,   0.0;
  //               2.2,    3.5,    0.0,   0.0;
  //               4.2,    5.5,    6.7,   0.0;
  //               7.2,    8.5,    9.7,  10.0 ]
  //     
  //     D = DChol * DChol'
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # First order (FO) values.
  //     #
  //     # Note: this is just a copy of the Laplace or Expected Hessian (FOCE)
  //     # calculation with b equal to zero.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     #--------------------------------------------------------------------
  //     # Calculate the predicted values, residuals, and weighted residuals.
  //     #--------------------------------------------------------------------
  //     
  //     b = 0.0 * ones( nB, 1 )
  //           
  //     f = exp( b(1) ) * ones( nY, 1 )
  //     
  //     f_b = [ f, 0.0 * ones( nB - 1, nB - 1 ) ]
  //     
  //     pred = f - f_b * b
  //     
  //     R = a * exp( b(1) ) * eye( nY )
  //     
  //     cov = R + f_b * D * f_b'
  //     
  //     resWtd = sqrtm( inverse( cov ) ) * ( y - pred )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     # Calculate the weighted individual parameters.
  //     #--------------------------------------------------------------------
  //     
  //     cov = D
  //     
  //     bWtd = sqrtm( inverse( cov ) ) * ( 0.0 * ones( nB, 1 ) - b )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # Laplace or Expected Hessian (FOCE) values.
  //     #
  //     # Note: this is just a copy of the FO calculation with nonzero b.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     #--------------------------------------------------------------------
  //     # Calculate the predicted values, residuals, and weighted residuals.
  //     #--------------------------------------------------------------------
  //     
  //     b = bOrig
  //     
  //     f = exp( b(1) ) * ones( nY, 1 )
  //     
  //     f_b = [ f, 0.0 * ones( nB - 1, nB - 1 ) ]
  //     
  //     pred = f - f_b * b
  //     
  //     R = a * exp( b(1) ) * eye( nY )
  //     
  //     cov = R + f_b * D * f_b'
  //     
  //     resWtd = sqrtm( inverse( cov ) ) * ( y - pred )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     # Calculate the weighted individual parameters.
  //     #--------------------------------------------------------------------
  //     
  //     cov = D
  //     
  //     bWtd = sqrtm( inverse( cov ) ) * ( 0.0 * ones( nB, 1 ) - b )
  //
  //~~~~~~~~~~~~~~~<End Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}


/*************************************************************************
 *
 * Function: threeDataValuesPerInd_usesBCheckTest
 *
 *
 * This test checks the case where each individual in the population
 * has three data values and the value for bCheck_i is used to
 * calculate the residuals.
 *
 *************************************************************************/

void popResidualsTest::threeDataValuesPerInd_usesBCheckTest( enum Objective whichObjective )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Number of individuals.
  const int nInd = 4;

  // Number of measurements per individual.
  const int nY_i = 3;

  // Number of measurements in total.
  const int nY = nInd * nY_i;

  const int nAlp = 2;

  const int nB = 4;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ThreeDataValuesPerIndModel model( nAlp, nB, nY_i );


  //------------------------------------------------------------
  // Quantities related to the population parameters.
  //------------------------------------------------------------

  valarray<double> alp( nAlp );

  alp[ 0 ] = 10.0;
  alp[ 1 ] = 25.0;
  

  //------------------------------------------------------------
  // Quantities related to the individual parameters.
  //------------------------------------------------------------

  valarray<double> bAll( 1., nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the data vector.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( nY_i, nInd );

  int i;
  int j;

  // Set the data values equal to the model for mean f( alp, b_i )
  // plus a small residual value.
  for ( i = 0; i < nInd; i++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      Y[i * nY_i + j] = exp( bAll[0] ) + pow( -1.0, j ) * ( j + 1 ) * 0.01;
    }
  }


  //------------------------------------------------------------
  // Quantities related to the population residuals.
  //------------------------------------------------------------

  valarray<double> popPredOut  ( nY );
  valarray<double> popResOut   ( nY );
  valarray<double> popResWtdOut( nY );

  valarray<double> popIndParResOut   ( nB * nInd );
  valarray<double> popIndParResWtdOut( nB * nInd );


  //------------------------------------------------------------
  // Calculate the population residuals.
  //------------------------------------------------------------

  // Set this so that the residuals will be calculated using bCheck_i,
  // which is the minimizer of the Map Bayesian objective MapObj(b)
  // using the first order (FO) model.
  bool calcFoModelMinimizer = true;

  try
  {
    popResiduals(
      model, 
      whichObjective,
      N,
      Y,
      alp,
      bAll,            
      &popPredOut,
      &popResOut,
      &popResWtdOut,
      &popIndParResOut,
      &popIndParResWtdOut,
      calcFoModelMinimizer );
  }
  catch( const SpkException& e )
  {
    cerr << e << endl;
    CPPUNIT_ASSERT_MESSAGE( "popResiduals failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "popResiduals failed for unknown reasons!", false);
  }

  // [Remove]==============================================
  /*
  cout << endl;
  cout << endl;
  cout << "=======================" << endl;
  cout << "objective = " << whichObjective << endl;
  cout << "popPredOut = " << endl;
  printInMatrix( popPredOut, 1 );
  cout << "popResOut = " << endl;
  printInMatrix( popResOut, 1 );
  cout << "popResWtdOut = " << endl;
  printInMatrix( popResWtdOut, 1 );
  cout << "popIndParResOut = " << endl;
  printInMatrix( popIndParResOut, 1 );
  cout << "popIndParResWtdOut = " << endl;
  printInMatrix( popIndParResWtdOut, 1 );
  cout << "-----------------------" << endl;
  */
  // [Remove]==============================================


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // The predicted values are calculated as follows:
  //
  //     pred   =  f (alp, b )  -  d  f (alp, b )  bCheck   .
  //         i      i       i       b  i       i         i 
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     pred   =  f (alp, 0 )  -  d  f (alp, 0 )  bCheck   .
      //         i      i               b  i                 i 
      //
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual( 0.607592903253869, popPredOut[i * nY_i + 0], fabs( popPredOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.607592903253869, popPredOut[i * nY_i + 1], fabs( popPredOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.607592903253869, popPredOut[i * nY_i + 2], fabs( popPredOut[i * nY_i + 2] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero.
      //
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual( 2.71531787757746, popPredOut[i * nY_i + 0], fabs( popPredOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 2.71531787757746, popPredOut[i * nY_i + 1], fabs( popPredOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 2.71531787757746, popPredOut[i * nY_i + 2], fabs( popPredOut[i * nY_i + 2] ) ) );
    }
  }
      
  // The residuals are calculated as follows:
  //
  //     res   =  y   -  pred   .
  //        i      i         i
  //
  for ( i = 0; i < nInd; i++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      CPPUNIT_ASSERT( isDblEpsEqual( Y[i * nY_i + j] - popPredOut[i * nY_i + j],
        popResOut[i * nY_i + j], fabs( popResOut[i * nY_i + j] ) ) );
    }
  }
      
  // The weighted residuals are calculated as follows:
  //
  //                  -1/2
  //     wres   =  cov      *  res   .
  //         i        i           i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     cov   =  R (alp, 0 )
      //        i      i
      //                                                 T
      //        +  d  f (alp, 0 )  D(alp)  d  f (alp, 0 )   .
      //            b  i                    b  i
      // 
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual( 0.560582693928111, popResWtdOut[i * nY_i + 0], fabs( popResWtdOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.551095860947606, popResWtdOut[i * nY_i + 1], fabs( popResWtdOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.566907249248448, popResWtdOut[i * nY_i + 2], fabs( popResWtdOut[i * nY_i + 2] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero, i.e.,
      //
      //     cov   =  R (alp, b )
      //        i      i       i
      //                                                 T
      //        +  d  f (alp, b )  D(alp)  d  f (alp, b )   .
      //            b  i       i            b  i       i
      // 
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      CPPUNIT_ASSERT( isDblEpsEqual(  0.00189204034565286, popResWtdOut[i * nY_i + 0], fabs( popResWtdOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.00386201472059646, popResWtdOut[i * nY_i + 1], fabs( popResWtdOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual(  0.00572807705648576, popResWtdOut[i * nY_i + 2], fabs( popResWtdOut[i * nY_i + 2] ) ) );
    }
  }
      
  // The individual parameter residuals are calculated as follows:
  //
  //     bRes   =  - b   .
  //         i        i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     bRes      =  0  .
      //         i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[0], fabs( popIndParResOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[1], fabs( popIndParResOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[2], fabs( popIndParResOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResOut[3], fabs( popIndParResOut[3] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero, i.e.,
      //
      //     bRes      =  - 1  .
      //         i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[0], fabs( popIndParResOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[1], fabs( popIndParResOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[2], fabs( popIndParResOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -1, popIndParResOut[3], fabs( popIndParResOut[3] ) ) );
    }
  }
      
  // The weighted individual parameter residualss are calculated as follows:
  //
  //                          -1/2
  //     bResWtd   =  D(alpha)      *  ( - b  )   .
  //            i                           i
  //
  for ( i = 0; i < nInd; i++ )
  {
    if ( whichObjective == FIRST_ORDER || whichObjective == NAIVE_FIRST_ORDER  )
    {
      // For the first order objectives the individual parameters 
      // are all set equal to zero, i.e.,
      //
      //     bResWtd      =  0  .
      //            i(j)
      //
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[0], fabs( popIndParResWtdOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[1], fabs( popIndParResWtdOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[2], fabs( popIndParResWtdOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.0, popIndParResWtdOut[3], fabs( popIndParResWtdOut[3] ) ) );
    }
    else
    {
      // For the Laplace and Expected Hessian objectives the
      // individual parameters are not set equal to zero.
      //
      // The known values that appear here were calculated using the
      // Octave code that appears at the end of this function.
      // 
      // Note that the scales have been increased slightly for these
      // comparisons.
      CPPUNIT_ASSERT( isDblEpsEqual( -0.87351003662302229, popIndParResWtdOut[0],  10.0 * fabs( popIndParResWtdOut[0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.17854871318119675, popIndParResWtdOut[1],  10.0 * fabs( popIndParResWtdOut[1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.05911120680695928, popIndParResWtdOut[2],  10.0 * fabs( popIndParResWtdOut[2] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( -0.00178338758982985, popIndParResWtdOut[3], 100.0 * fabs( popIndParResWtdOut[3] ) ) );
    }
  }
      
  // The known values were calculated using the following Octave code.
  //
  //~~~~~~~~~~~~~~~<Begin Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  //    #--------------------------------------------------------------------
  //    #
  //    # Preliminaries.
  //    #
  //    #--------------------------------------------------------------------
  //    
  //    format long
  //    
  //    
  //    #--------------------------------------------------------------------
  //    #
  //    # Common quantities.
  //    #
  //    #--------------------------------------------------------------------
  //    
  //    nY = 3
  //    nB = 4
  //    
  //    a = 10.0
  //    
  //    bOrig = ones( nB, 1 )
  //          
  //    y = [ exp( bOrig(1) ) + 0.01;
  //          exp( bOrig(1) ) - 0.02;
  //          exp( bOrig(1) ) + 0.03 ]
  //    
  //    # Create an arbitrary Cholesky factor that will be used to 
  //    # ensure that D will be positive definite.
  //    DChol = [ 1.2,    0.0,    0.0,   0.0;
  //              2.2,    3.5,    0.0,   0.0;
  //              4.2,    5.5,    6.7,   0.0;
  //              7.2,    8.5,    9.7,  10.0 ]
  //    
  //    D = DChol * DChol'
  //    
  //    
  //    #--------------------------------------------------------------------
  //    #
  //    # First order (FO) values.
  //    #
  //    # Note: this is just a copy of the Laplace or Expected Hessian (FOCE)
  //    # calculation with b equal to zero.
  //    #
  //    #--------------------------------------------------------------------
  //    
  //    #--------------------------------------------------------------------
  //    # Calculate the predicted values, residuals, and weighted residuals.
  //    #--------------------------------------------------------------------
  //    
  //    b = 0.0 * ones( nB, 1 )
  //    
  //    f = exp( b(1) ) * ones( nY, 1 )
  //    
  //    f_b = [ f, 0.0 * ones( nB - 1, nB - 1 ) ]
  //    
  //    R = a * exp( b(1) ) * eye( nY )
  //    
  //    bCheck = inverse( f_b' * inverse( R ) * f_b + D ) * f_b' * inverse( R ) * ( y - f )
  //          
  //    pred = f - f_b * bCheck
  //    
  //    cov = R + f_b * D * f_b'
  //    
  //    resWtd = sqrtm( inverse( cov ) ) * ( y - pred )
  //    
  //    
  //    #--------------------------------------------------------------------
  //    # Calculate the weighted individual parameters.
  //    #--------------------------------------------------------------------
  //    
  //    cov = D
  //    
  //    bWtd = sqrtm( inverse( cov ) ) * ( 0.0 * ones( nB, 1 ) - b )
  //    
  //    
  //    #--------------------------------------------------------------------
  //    #
  //    # Laplace or Expected Hessian (FOCE) values.
  //    #
  //    # Note: this is just a copy of the FO calculation with nonzero b.
  //    #
  //    #--------------------------------------------------------------------
  //    
  //    #--------------------------------------------------------------------
  //    # Calculate the predicted values, residuals, and weighted residuals.
  //    #--------------------------------------------------------------------
  //    
  //    b = bOrig
  //    
  //    f = exp( b(1) ) * ones( nY, 1 )
  //    
  //    f_b = [ f, 0.0 * ones( nB - 1, nB - 1 ) ]
  //    
  //    R = a * exp( b(1) ) * eye( nY )
  //    
  //    bCheck = inverse( f_b' * inverse( R ) * f_b + D ) * f_b' * inverse( R ) * ( y - f )
  //          
  //    pred = f - f_b * bCheck
  //    
  //    cov = R + f_b * D * f_b'
  //    
  //    resWtd = sqrtm( inverse( cov ) ) * ( y - pred )
  //    
  //    
  //    #--------------------------------------------------------------------
  //    # Calculate the weighted individual parameters.
  //    #--------------------------------------------------------------------
  //    
  //    cov = D
  //    
  //    bWtd = sqrtm( inverse( cov ) ) * ( 0.0 * ones( nB, 1 ) - b )
  //
  //~~~~~~~~~~~~~~~<End Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

