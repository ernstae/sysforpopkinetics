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
#include <spk/identity.h>
#include <spk/isDblEpsEqual.h>
#include <spk/Objective.h>
#include <spk/popResiduals.h>
#include <spk/SpkModel.h>
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

  class ThreeDataValuesPerIndModel : public SpkModel
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
        //
        //     D(alp) = I  .
        //
        ret.resize(_nB * _nB);
        identity( _nB, ret );
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
        //                   /  exp[b(1)] \ 
        //     f_b(alp, b) = |  exp[b(1)] |  .
        //                   \  exp[b(1)] / 
        //
        ret.resize(_nYi * _nB);
        ret = exp( _b[0] );
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
        //                   /   alp(1) exp[b(1)]          0  \ 
        //                   |   0                         0  | 
        //                   |   0                         0  | 
        //                   |   0                         0  | 
        //     R_b(alp, b) = |   alp(1) exp[b(1)]          0  |   .
        //                   |   0                         0  | 
        //                   |   0                         0  | 
        //                   |   0                         0  | 
        //                   \   alp(1) exp[b(1)]          0  / 
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
}


/*************************************************************************
 *
 * Function: expectedHessianTest
 *
 *************************************************************************/

void popResidualsTest::expectedHessianTest()
{
  threeDataValuesPerIndTest( EXPECTED_HESSIAN );
}


/*************************************************************************
 *
 * Function: modifiedLaplaceTest
 *
 *************************************************************************/

void popResidualsTest::modifiedLaplaceTest()
{
  threeDataValuesPerIndTest( MODIFIED_LAPLACE );
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

  const int nB = 1;


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

  valarray<double> popPredOut( nY );
  valarray<double> popResOut ( nY );
  valarray<double> popWresOut( nY );


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
      &popWresOut );
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
  cout << "popWresOut = " << endl;
  printInMatrix( popWresOut, 1 );
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
      CPPUNIT_ASSERT( isDblEpsEqual( 0.479468726915712, popWresOut[i * nY_i + 0], fabs( popWresOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.469981893935207, popWresOut[i * nY_i + 1], fabs( popWresOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.485793282236049, popWresOut[i * nY_i + 2], fabs( popWresOut[i * nY_i + 2] ) ) );
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
      CPPUNIT_ASSERT( isDblEpsEqual( 0.388534872659817, popWresOut[i * nY_i + 0], fabs( popWresOut[i * nY_i + 0] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.382780817593567, popWresOut[i * nY_i + 1], fabs( popWresOut[i * nY_i + 1] ) ) );
      CPPUNIT_ASSERT( isDblEpsEqual( 0.392370909370650, popWresOut[i * nY_i + 2], fabs( popWresOut[i * nY_i + 2] ) ) );
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
  //     nB = 1
  //     
  //     a  = 10.0
  //     
  //     bOrig = 1.0
  //     
  //     y  =  [ exp( bOrig ) + 0.01;
  //             exp( bOrig ) - 0.02;
  //             exp( bOrig ) + 0.03 ]
  //     
  //     D  = eye( nB )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # First order (FO) weighted residuals.
  //     #
  //     # Note: this is just a copy of the Laplace or Expected Hessian (FOCE)
  //     # calculation with b equal to zero.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     b  = 0.0
  //     
  //     f  =  exp( b ) * ones( nY, 1 )
  //     
  //     f_b  =  f
  //     
  //     pred  =  f - f_b * b
  //     
  //     R  =  a * exp( b ) * eye( nY )
  //     
  //     cov = R + f_b * D * f_b'
  //     
  //     wres = sqrtm( inverse( cov ) ) * ( y - pred )
  //     
  //     
  //     #--------------------------------------------------------------------
  //     #
  //     # Laplace or Expected Hessian (FOCE) weighted residuals.
  //     #
  //     # Note: this is just a copy of the FO calculation with nonzero b.
  //     #
  //     #--------------------------------------------------------------------
  //     
  //     b  = bOrig
  //     
  //     f  =  exp( b ) * ones( nY, 1 )
  //     
  //     f_b  =  f
  //     
  //     pred  =  f - f_b * b
  //     
  //     R  =  a * exp( b ) * eye( nY )
  //     
  //     cov = R + f_b * D * f_b'
  //     
  //     wres = sqrtm( inverse( cov ) ) * ( y - pred )
  //
  //~~~~~~~~~~~~~~~<End Octave Code>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

