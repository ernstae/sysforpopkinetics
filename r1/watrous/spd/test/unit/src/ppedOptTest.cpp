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
 * File: ppedOptTest.cpp
 *
 *
 * This class performs the unit test for the function ppedOpt.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPD test suite header files.
#include "ppedOptTest.h"

// SPD library header files.
#include <spd/ppedOpt.h>
#include <spd/SpdModel.h>
#include <spd/DoubleMatrix.h>
#include <spd/SpkException.h>
#include <spd/SpkValarray.h>
#include <spd/mulByScalar.h>
#include <spd/subtract.h>
#include <spd/DBL_EPS_EQUAL_MULT.h>
#include <spd/isDblEpsEqual.h>
#include <spd/isDmatEpsEqual.h>
#include <spd/pi.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iostream>
#include <string>
#include <cmath>
#include <cfloat>

using namespace CppUnit;
using SPK_VA::valarray;
using std::string;


/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  void preTestPrinting( string title );

  void postTestPrinting( bool ok );

  bool isAbsDiffEqual( double x, double y, double maxDiff );

  bool isAbsDiffEqual( const DoubleMatrix &dmatX, 
                       const DoubleMatrix &dmatY, 
                       double maxDiff );

  bool absDiffEqualTest( double x, 
                         double y, 
                         double maxDiff,
                         string testName,
                         string xName,
                         string yName );

  bool absDiffEqualTest( const DoubleMatrix &dmatX, 
                         const DoubleMatrix &dmatY, 
                         double maxDiff,
                         string testName,
                         string xName,
                         string yName );

  bool isRelTolEqual( double x, double y, double tol );

  bool isRelTolEqual( const DoubleMatrix &dmatX, 
                      const DoubleMatrix &dmatY, 
                      double tol );

  bool relTolEqualTest( double x, 
                        double y, 
                        double tol,
                        string testName,
                        string xName,
                        string yName );

  bool relTolEqualTest( const DoubleMatrix &dmatX, 
                        const DoubleMatrix &dmatY, 
                        double tol,
                        string testName,
                        string xName,
                        string yName );

  bool epsEqualTest(  double x, 
                      double y, 
                      double scale,
                      string testName,
                      string xName,
                      string yName );

  bool epsEqualTest(  const DoubleMatrix &dmatX, 
                      const DoubleMatrix &dmatY, 
                      const DoubleMatrix &dmatScale, 
                      string testName,
                      string xName,
                      string yName );

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local Class Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Class: ScalarModel
 *
 *
 * All of the parameter vectors, data mean models, and covariance
 * matrix models for this class are scalars.
 *
 *************************************************************************/

class ScalarModel : public SpdModel
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  ScalarModel( const valarray<int>& nX_iIn,
               int                  nXCommonIn )
    :
    SpdModel   ( nX_iIn, nXCommonIn, 1 ),
    nInd       ( nX_iIn.size() ),
    nX         ( nX_iIn.sum() + nXCommonIn ),
    nXCommon   ( nXCommonIn ),
    nAlp       ( 1 ),
    nB_i       ( 1 ),
    nY_i       ( 1 ),
    nX_i       ( nX_iIn ),
    desParCurr ( nX ),
    popParCurr ( nAlp ),
    indParCurr ( nB_i )
  {
    assert( nInd     >= 0 );
    assert( nX       >= 0 );
    assert( nXCommon >= 0 );

    // Check that the number of individual plus common design
    // parameters is never more than one for any individual.
    for ( int i = 0; i < nInd; i++ )
    {
      assert( nX_i[ i ] + nXCommon == 1 );
    }
  }

private:
  // A default constructor should never be called for this class.
  // This default constructor is declared private and not defined
  // in ScalarModel.cpp so it won't be called.
  ScalarModel();

public:
  ~ScalarModel() {}


  //------------------------------------------------------------
  // Miscellaneous member variables.
  //------------------------------------------------------------

private:
  const int nInd;

  const int nX;
  const int nXCommon;

  const int nAlp;
  const int nB_i;
  const int nY_i;

  valarray<int> nX_i;

  // The combination of parameters that each individuals' 
  // model functions depend on is denoted 
  //               -          -
  //    chi_i  =  |  x_i       |  .
  //              |  x_common  |
  //               -          -
  // This keeps track of the number of elements in chi_i.
  int nChi_i;


  //------------------------------------------------------------
  // State variables.
  //------------------------------------------------------------

private:
  int iCurr;

  valarray<double> desParCurr;
  valarray<double> popParCurr;
  valarray<double> indParCurr;


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

private:
  void doSelectIndividual ( int base0 )
  {
    iCurr = base0;

    // Sum the number design parameters for this individual plus
    // the number common to all of the individuals.
    nChi_i = nX_i[ iCurr ] + nXCommon;
  }

  void doSetDesPar( const valarray<double>& inVA )
  {
    desParCurr = inVA;
    assert( desParCurr.size() == nX );
  }

  void doSetPopPar( const valarray<double>& inVA )
  {
    popParCurr = inVA;
    assert( popParCurr.size() == nAlp );
  }

  void doSetIndPar( const valarray<double>& inVA )
  {
    indParCurr = inVA;
    assert( indParCurr.size() == nB_i );
  }


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

  void doDataMean( valarray<double>& ret ) const
  {
    // Set
    //
    //     f  =  [ alp[0] ]  .
    //
    ret.resize( nY_i );
    ret = popParCurr[0];
  }
  bool doDataMean_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_chi  =  [ 0 ]  .
    //
    ret.resize( nY_i * nChi_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_alp  =  [ 1 ]  .
    //
    ret.resize( nY_i * nAlp );
    ret = 1.0;

    return true;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_indPar_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b_chi  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i * nChi_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_indPar_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b_alp  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doDataVariance( valarray<double>& ret ) const
  {
    // Set
    //            -             -
    //           |       1       |
    //     R  =  |  -----------  |  .
    //           |           2   |
    //           |   1 - x[0]    |
    //            -             -
    //
    ret.resize( nY_i * nY_i );
    ret = 1.0 / ( 1.0 - desParCurr[0] * desParCurr[0] );
  }
  bool doDataVariance_desPar( valarray<double>& ret ) const
  {
    // Set
    //                -                   -
    //               |       2 x[0]        |
    //     R_chi  =  |  -----------------  |  .
    //               |              2  2   |
    //               |   (  1 - x[0]  )    |
    //                -                   -
    //
    ret.resize( nY_i * nY_i * nChi_i );
    double term = 1.0 - desParCurr[0] * desParCurr[0];
    ret = 2.0 * desParCurr[0] / ( term * term );

    return desParCurr[0] != 0.0 ? true : false;
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     R_alp  =  [ 0 ]  .
    //
    ret.resize( nY_i * nY_i * nAlp );
    ret = 0.0;

    return false;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     R_b  =  [ 0 ]  .
    //
    ret.resize( nY_i * nY_i * nB_i );
    ret = 0.0;

    return false;
  }
  void doDataVarianceInv( valarray<double>& ret ) const
  {
    // Set
    //
    //      -1             2
    //     R    =  1 - x[0]   .
    //
    ret.resize( nY_i * nY_i );
    ret = 1.0 - desParCurr[0] * desParCurr[0];
  }
  bool doDataVarianceInv_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_chi  =  - 2 x[0]  .
    //
    ret.resize( nY_i * nY_i * nChi_i );
    ret = -2.0 * desParCurr[0];

    return desParCurr[0] != 0.0 ? true : false;
  }
  bool doDataVarianceInv_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_alp  =  0  .
    //
    ret.resize( nY_i * nY_i * nAlp );
    ret = 0.0;

    return false;
  }
  bool doDataVarianceInv_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_b  =  0  .
    //
    ret.resize( nY_i * nY_i * nB_i );
    ret = 0.0;

    return false;
  }   
  void doIndParVariance( valarray<double>& ret ) const
  {
    // Set
    //
    //     D  =  [ 1 ]  .
    //
    ret.resize( nB_i * nB_i );
    ret = 1.0;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     D_alp  =  [ 0 ]  .
    //
    ret.resize( nB_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doIndParVarianceInv( valarray<double>& ret ) const
  {
    // Set
    //
    //      -1
    //     D    =  [ 1 ]  .
    //
    ret.resize( nB_i * nB_i );
    ret = 1.0;
  }
  bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( D   )_alp  =  0  .
    //
    ret.resize( nB_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doPopParPrior( double& ret ) const
  {
    // Set
    //                     2
    //            -[ alp[0]  ]
    //     p  =  e              .
    //
    ret = exp( - popParCurr[0] * popParCurr[0] );
  }
  bool doPopParPrior_popPar( valarray<double>& ret ) const
  {
    // Set
    //                                    2
    //                           -[ alp[0]  ]
    //     p_alp  =  - 2 alp[0] e              .
    //
    ret.resize( nAlp );
    ret = -2.0 * popParCurr[0] * exp( - popParCurr[0] * popParCurr[0] );

    if ( popParCurr[0] )
    {
      return true;
    }
    else
    {
      return false;
    }
  }

};

/*************************************************************************
 *
 * Class: OneExponentialModel
 *
 *
 * The data mean model for this class is a single exponential.  
 * This test is based on Example 1.1 in Marco Foracchia's master
 * thesis (pp. 115-22).
 *
 *************************************************************************/

class OneExponentialModel : public SpdModel
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  OneExponentialModel( const valarray<int>& nX_iIn,
                       int                  nXCommonIn )
    :
    SpdModel   ( nX_iIn, nXCommonIn, 2 ),
    nInd       ( nX_iIn.size() ),
    nX         ( nX_iIn.sum() + nXCommonIn ),
    nXCommon   ( nXCommonIn ),
    nAlp       ( 1 ),
    nB_i       ( 1 ),
    nY_i       ( 1 ),
    sigma      ( 3.0 ),
    nX_i       ( nX_iIn ),
    desParCurr ( nX ),
    popParCurr ( nAlp ),
    indParCurr ( nB_i )
  {
    assert( nInd     >= 0 );
    assert( nX       >= 0 );
    assert( nXCommon >= 0 );

    // Check that the number of individual plus common design
    // parameters is never more than one for any individual.
    for ( int i = 0; i < nInd; i++ )
    {
      assert( nX_i[ i ] + nXCommon == 2 );
    }
  }

private:
  // A default constructor should never be called for this class.
  // This default constructor is declared private and not defined
  // in OneExponentialModel.cpp so it won't be called.
  OneExponentialModel();

public:
  ~OneExponentialModel() {}


  //------------------------------------------------------------
  // Miscellaneous member variables.
  //------------------------------------------------------------

private:
  const int nInd;

  const int nX;
  const int nXCommon;

  const int nAlp;
  const int nB_i;
  const int nY_i;

  valarray<int> nX_i;

  // The combination of parameters that each individuals' 
  // model functions depend on is denoted 
  //               -          -
  //    chi_i  =  |  x_i       |  .
  //              |  x_common  |
  //               -          -
  // This keeps track of the number of elements in chi_i.
  int nChi_i;

  // Variance of the data values that is common to all individuals
  // and all data values.
  const double sigma;


  //------------------------------------------------------------
  // State variables.
  //------------------------------------------------------------

private:
  int iCurr;

  valarray<double> desParCurr;
  valarray<double> popParCurr;
  valarray<double> indParCurr;


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

private:
  void doSelectIndividual ( int base0 )
  {
    iCurr = base0;

    // Sum the number design parameters for this individual plus
    // the number common to all of the individuals.
    nChi_i = nX_i[ iCurr ] + nXCommon;
  }

  void doSetDesPar( const valarray<double>& inVA )
  {
    desParCurr = inVA;
    assert( desParCurr.size() == nX );
  }

  void doSetPopPar( const valarray<double>& inVA )
  {
    popParCurr = inVA;
    assert( popParCurr.size() == nAlp );
  }

  void doSetIndPar( const valarray<double>& inVA )
  {
    indParCurr = inVA;
    assert( indParCurr.size() == nB_i );
  }


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

  void doDataMean( valarray<double>& ret ) const
  {
    // Set
    //
    //     f  =  [ alp[0] ]  .
    //
    ret.resize( nY_i );
    ret = popParCurr[0];
  }
  bool doDataMean_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_chi  =  [ 0 ]  .
    //
    ret.resize( nY_i * nChi_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_alp  =  [ 1 ]  .
    //
    ret.resize( nY_i * nAlp );
    ret = 1.0;

    return true;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_indPar_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b_chi  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i * nChi_i );
    ret = 0.0;

    return false;
  }
  bool doDataMean_indPar_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     f_b_alp  =  [ 0 ]  .
    //
    ret.resize( nY_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doDataVariance( valarray<double>& ret ) const
  {
    // Set
    //            -             -
    //           |       1       |
    //     R  =  |  -----------  |  .
    //           |           2   |
    //           |   1 - x[0]    |
    //            -             -
    //
    ret.resize( nY_i * nY_i );
    ret = 1.0 / ( 1.0 - desParCurr[0] * desParCurr[0] );
  }
  bool doDataVariance_desPar( valarray<double>& ret ) const
  {
    // Set
    //                -                   -
    //               |       2 x[0]        |
    //     R_chi  =  |  -----------------  |  .
    //               |              2  2   |
    //               |   (  1 - x[0]  )    |
    //                -                   -
    //
    ret.resize( nY_i * nY_i * nChi_i );
    double term = 1.0 - desParCurr[0] * desParCurr[0];
    ret = 2.0 * desParCurr[0] / ( term * term );

    return desParCurr[0] != 0.0 ? true : false;
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     R_alp  =  [ 0 ]  .
    //
    ret.resize( nY_i * nY_i * nAlp );
    ret = 0.0;

    return false;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     R_b  =  [ 0 ]  .
    //
    ret.resize( nY_i * nY_i * nB_i );
    ret = 0.0;

    return false;
  }
  void doDataVarianceInv( valarray<double>& ret ) const
  {
    // Set
    //
    //      -1             2
    //     R    =  1 - x[0]   .
    //
    ret.resize( nY_i * nY_i );
    ret = 1.0 - desParCurr[0] * desParCurr[0];
  }
  bool doDataVarianceInv_desPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_chi  =  - 2 x[0]  .
    //
    ret.resize( nY_i * nY_i * nChi_i );
    ret = -2.0 * desParCurr[0];

    return desParCurr[0] != 0.0 ? true : false;
  }
  bool doDataVarianceInv_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_alp  =  0  .
    //
    ret.resize( nY_i * nY_i * nAlp );
    ret = 0.0;

    return false;
  }
  bool doDataVarianceInv_indPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( R   )_b  =  0  .
    //
    ret.resize( nY_i * nY_i * nB_i );
    ret = 0.0;

    return false;
  }   
  void doIndParVariance( valarray<double>& ret ) const
  {
    // Set
    //
    //     D  =  [ 1 ]  .
    //
    ret.resize( nB_i * nB_i );
    ret = 1.0;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //     D_alp  =  [ 0 ]  .
    //
    ret.resize( nB_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doIndParVarianceInv( valarray<double>& ret ) const
  {
    // Set
    //
    //      -1
    //     D    =  [ 1 ]  .
    //
    ret.resize( nB_i * nB_i );
    ret = 1.0;
  }
  bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
  {
    // Set
    //
    //        -1
    //     ( D   )_alp  =  0  .
    //
    ret.resize( nB_i * nB_i * nAlp );
    ret = 0.0;

    return false;
  }
  void doPopParPrior( double& ret ) const
  {
    // Set
    //                     2
    //            -[ alp[0]  ]
    //     p  =  e              .
    //
    ret = exp( - popParCurr[0] * popParCurr[0] );
  }
  bool doPopParPrior_popPar( valarray<double>& ret ) const
  {
    // Set
    //                                    2
    //                           -[ alp[0]  ]
    //     p_alp  =  - 2 alp[0] e              .
    //
    ret.resize( nAlp );
    ret = -2.0 * popParCurr[0] * exp( - popParCurr[0] * popParCurr[0] );

    if ( popParCurr[0] )
    {
      return true;
    }
    else
    {
      return false;
    }
  }

};

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: setup
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void ppedOptTest::setUp()
{
  // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void ppedOptTest::tearDown()
{
  // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* ppedOptTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite;

  suiteOfTests->addTest( new TestCaller<ppedOptTest>(
    "scalarModelNoXCommonTest", &ppedOptTest::scalarModelNoXCommonTest ) );

  suiteOfTests->addTest( new TestCaller<ppedOptTest>(
    "scalarModelOneXCommonTest", &ppedOptTest::scalarModelOneXCommonTest ) );

  suiteOfTests->addTest( new TestCaller<ppedOptTest>(
    "singleExponentialModelNoXCommonTest", &ppedOptTest::singleExponentialModelNoXCommonTest ) );

  suiteOfTests->addTest( new TestCaller<ppedOptTest>(
    "singleExponentialModelOneXCommonTest", &ppedOptTest::singleExponentialModelOneXCommonTest ) );

  suiteOfTests->addTest( new TestCaller<ppedOptTest>(
    "singleExponentialModelTwoXCommonTest", &ppedOptTest::singleExponentialModelTwoXCommonTest ) );

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: scalarModelNoXCommonTest
 *
 *************************************************************************/

void ppedOptTest::scalarModelNoXCommonTest()
{
  int nInd     = 1;
  int nXPerInd = 1;
  int nXCommon = 0;

  scalarModelTest( nInd, nXPerInd, nXCommon );
}


/*************************************************************************
 *
 * Function: scalarModelOneXCommonTest
 *
 *************************************************************************/

void ppedOptTest::scalarModelOneXCommonTest()
{
  int nInd     = 1;
  int nXPerInd = 0;
  int nXCommon = 1;

  scalarModelTest( nInd, nXPerInd, nXCommon );
}


/*************************************************************************
 *
 * Function: scalarModelTest
 *
 *
 * All of the parameter vectors, data mean models, and covariance
 * matrix models for this test are scalars.
 *
 *
 * Arguments
 * ---------
 *
 * nInd
 *
 * Number of individuals.
 *
 * 
 * nXPerInd
 *
 * Number of design parameters for each individual.
 *
 *
 * nXCommon
 *
 * Number of design parameters common to all of the individuals. 
 *
 *************************************************************************/

void ppedOptTest::scalarModelTest( int nInd, int nXPerInd, int nXCommon )
{
  //------------------------------------------------------------
  // Quantities related to the design parameters.
  //------------------------------------------------------------

  // Number of design parameters for each individual.
  valarray<int> nX_i( nXPerInd, nInd );

  // Total number of design parameters.
  const int nX = nXPerInd * nInd + nXCommon;


  //------------------------------------------------------------
  // Quantities related to the fixed population parameters.
  //------------------------------------------------------------

  const int nAlp = 1;


  //------------------------------------------------------------
  // Quantities related to the random population parameters.
  //------------------------------------------------------------

  const int nB_i = 1;


  //------------------------------------------------------------
  // Quantities related to the model.
  //------------------------------------------------------------

  ScalarModel model( nX_i, nXCommon );


  //-----------------------------------------------------------
  // Quantities related to the optimization problems.
  //-----------------------------------------------------------

  DoubleMatrix dvecXLow ( nX, 1 );
  DoubleMatrix dvecXUp  ( nX, 1 );
  DoubleMatrix dvecXIn  ( nX, 1 );
  DoubleMatrix dvecXOut ( nX, 1 );
  DoubleMatrix dvecXStep( nX, 1 );

  // Initial values and step sizes for the design parameters.
  dvecXIn  .fill( 0.5 );
  dvecXStep.fill( 0.001 );

  // Bounds for the design parameters.
  mulByScalar( dvecXIn, -1.25, dvecXLow );
  mulByScalar( dvecXIn, +1.25, dvecXUp );

  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  // Initial values and step sizes for the fixed population parameters.
  dvecAlpIn  .fill( 0.3 );
  dvecAlpStep.fill( 0.001 );

  // Bounds for the fixed population parameters.
  mulByScalar( dvecAlpIn, -1.25, dvecAlpLow );
  mulByScalar( dvecAlpIn, +1.25, dvecAlpUp );

  // Relative solution error.
  double xEpsilon   = 1.0e-4;
  double alpEpsilon = 1.0e-4;

  // Maximum number of iterations.
  int nXMaxIter   = 50;
  int nAlpMaxIter = 50;

  // Amount of printing during optimization problems.
  int xLevel   = 1;
  int alpLevel = 0;

  // Set the optimizer controls.
  Optimizer xOptimizer(   xEpsilon,   nXMaxIter,   xLevel );
  Optimizer alpOptimizer( alpEpsilon, nAlpMaxIter, alpLevel );


  //------------------------------------------------------------
  // Quantities related to the population expected determinant criterion.
  //------------------------------------------------------------

  double dPhiOut;

  DoubleMatrix drowPhi_xOut  ( 1,  nX );
  DoubleMatrix dmatPhi_x_xOut( nX, nX );


  //-----------------------------------------------------------
  // Optimize the population expected determinant criterion.
  //-----------------------------------------------------------

  try
  {
    ppedOpt( 
      model,
      xOptimizer,
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      dvecXStep,
      alpOptimizer,
      dvecAlpLow,
      dvecAlpUp,
      dvecAlpIn,
      &dvecAlpOut,
      dvecAlpStep,
      &dPhiOut,
      &drowPhi_xOut,
      &dmatPhi_x_xOut );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected exception occurred in ppedOpt.",
      false );
  }


  //-----------------------------------------------------------
  // Calculate the known values.
  //-----------------------------------------------------------

  double dPhiKnown;

  DoubleMatrix dvecXHat        ( nX,   1 );
  DoubleMatrix dvecAlpHat      ( nAlp, 1 );
  DoubleMatrix drowPhi_xKnown  ( 1,    nX );
  DoubleMatrix dmatPhi_x_xKnown( nX,   nX );

  // For this test the prior distribution of the fixed effects is
  //
  //                        2
  //    p(alp)  =  exp( -alp  )  ,
  //
  // and the determinant of the expected Hessian, i.e., the Fisher
  // information matrix, is
  //
  //                                2
  //    det[ H(x, alp) ]  =  ( 1 - x  )  .
  //
  // Therefore, the criterion for this test is 
  //
  //                +infinity
  //               /\
  //               \                      
  //    phi(x)  =   \        p(alp) det[ HTilde(x, alp) ] dalp
  //               \/
  //                -infinity
  //
  //                 1/2         2
  //            =  pi     ( 1 - x  )  ,
  //
  // and the negative log of this integrand is
  //
  //                             -                               -
  //                            |                                 |
  //    Lambda(x, alp) = - log  |  p(alp)  det[ HTilde(x, alp) ]  |
  //                            |                                 |
  //                             -                               -
  //
  //                         2        2
  //                   =  alp  ( 1 - x  )  .
  //

  // Set the maximizer of phi(x).
  dvecXHat.fill( 0.0 );

  // Set the minimizer of Lambda(x, alp) with respect to alp.
  dvecAlpHat.fill( 0.0 );

  double* pdXHat = dvecXHat.data();

  // Set the criterion and its derivatives at the maximizer.
  dPhiKnown  = sqrt( PI ) * ( 1.0 - pdXHat[0] * pdXHat[0] );
  drowPhi_xKnown  .fill( -2.0 * sqrt( PI ) * pdXHat[0] );
  dmatPhi_x_xKnown.fill( -2.0 * sqrt( PI ) );


  //-----------------------------------------------------------
  // Check the results.
  //-----------------------------------------------------------

  // Check to see if any elements of xOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(xOut - xHat) <= xEpsilon * (xUp - xLow)  .
  //
  // Since epsEqualTest (via isDmatEpsEqual) checks to see if 
  // 
  //      abs(xOut - xHat) <  scale * DBL_EPS_EQUAL_MULT * DBL_EPSILON   ,
  // 
  // the scale for this test is 
  // 
  //      scale  = [ xEpsilon / (DBL_EPS_EQUAL_MULT * DBL_EPSILON) ] * (xUp - xLow)  .
  // 
  DoubleMatrix dvecXScale( nX, 1 );
  dvecXScale = mulByScalar( subtract( dvecXUp, dvecXLow ), 
    xEpsilon / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dvecXOut not equal to dvecXHat",
    epsEqualTest(
      dvecXOut,
      dvecXHat,
      dvecXScale,
      "ppedOptTest", 
      "xOut",
      "x (analytic)" ) );

  // Check to see if any elements of alpOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(alpOut - alpHat) <= alpEpsilon * (alpUp - alpLow)  .
  //
  // Since epsEqualTest (via isDmatEpsEqual) checks to see if 
  // 
  //      abs(alpOut - alpHat) <  scale * DBL_EPS_EQUAL_MULT * DBL_EPSILON   ,
  // 
  // the scale for this test is 
  // 
  //      scale  = [ alpEpsilon / (DBL_EPS_EQUAL_MULT * DBL_EPSILON) ] * (alpUp - alpLow)  .
  // 
  DoubleMatrix dvecAlpScale( nAlp, 1 );
  dvecAlpScale = mulByScalar( subtract( dvecAlpUp, dvecAlpLow ), 
    alpEpsilon / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dvecAlpOut not equal to dvecAlpHat",
    epsEqualTest(
      dvecAlpOut,
      dvecAlpHat,
      dvecAlpScale,
      "ppedOptTest", 
      "alpOut",
      "alp (analytic)" ) );

  double tol = 1.0e-10;

  CPPUNIT_ASSERT_MESSAGE(
    "dPhiOut not equal to dPhiKnown",
    relTolEqualTest(
      dPhiOut,
      dPhiKnown,
      tol,
      "ppedOptTest",
      "phiOut",
      "phi (analytic)" ) );

  tol = xEpsilon;

  CPPUNIT_ASSERT_MESSAGE(
    "drowPhi_xOut not equal to drowPhi_xKnown",
    absDiffEqualTest(
      drowPhi_xOut,
      drowPhi_xKnown,
      tol,
      "ppedOptTest",
      "phi_xOut",
      "phi_x (analytic)" ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dmatPhi_x_xOut not equal to dmatPhi_x_xKnown",
    relTolEqualTest(
      dmatPhi_x_xOut,
      dmatPhi_x_xKnown,
      tol,
      "ppedOptTest",
      "phi_x_xOut",
      "phi_x_x (analytic)" ) );

}


/*************************************************************************
 *
 * Function: singleExponentialModelNoXCommonTest
 *
 *************************************************************************/

void ppedOptTest::singleExponentialModelNoXCommonTest()
{
  int nInd     = 1;
  int nXPerInd = 2;
  int nXCommon = 0;

  singleExponentialModelTest( nInd, nXPerInd, nXCommon );
}


/*************************************************************************
 *
 * Function: singleExponentialModelOneXCommonTest
 *
 *************************************************************************/

void ppedOptTest::singleExponentialModelOneXCommonTest()
{
  int nInd     = 1;
  int nXPerInd = 1;
  int nXCommon = 1;

  singleExponentialModelTest( nInd, nXPerInd, nXCommon );
}


/*************************************************************************
 *
 * Function: singleExponentialModelTwoXCommonTest
 *
 *************************************************************************/

void ppedOptTest::singleExponentialModelTwoXCommonTest()
{
  int nInd     = 1;
  int nXPerInd = 0;
  int nXCommon = 2;

  singleExponentialModelTest( nInd, nXPerInd, nXCommon );
}


/*************************************************************************
 *
 * Function: singleExponentialModelTest
 *
 *
 * The data mean model for this test is a single exponential.  
 * This test is based on Example 1.1 in Marco Foracchia's master
 * thesis (pp. 115-22).
 *
 *
 * Arguments
 * ---------
 *
 * nInd
 *
 * Number of individuals.
 *
 * 
 * nXPerInd
 *
 * Number of design parameters for each individual.
 *
 *
 * nXCommon
 *
 * Number of design parameters common to all of the individuals. 
 *
 *************************************************************************/

void ppedOptTest::singleExponentialModelTest( int nInd, int nXPerInd, int nXCommon )
{
  //------------------------------------------------------------
  // Quantities related to the design parameters.
  //------------------------------------------------------------

  // Number of design parameters for each individual.
  valarray<int> nX_i( nXPerInd, nInd );

  // Total number of design parameters.
  const int nX = nXPerInd * nInd + nXCommon;


  //------------------------------------------------------------
  // Quantities related to the fixed population parameters.
  //------------------------------------------------------------

  const int nAlp = 1;


  //------------------------------------------------------------
  // Quantities related to the random population parameters.
  //------------------------------------------------------------

  const int nB_i = 1;


  //------------------------------------------------------------
  // Quantities related to the model.
  //------------------------------------------------------------

  OneExponentialModel model( nX_i, nXCommon );


  //-----------------------------------------------------------
  // Quantities related to the optimization problems.
  //-----------------------------------------------------------

  DoubleMatrix dvecXLow ( nX, 1 );
  DoubleMatrix dvecXUp  ( nX, 1 );
  DoubleMatrix dvecXIn  ( nX, 1 );
  DoubleMatrix dvecXOut ( nX, 1 );
  DoubleMatrix dvecXStep( nX, 1 );

  // Initial values and step sizes for the design parameters.
  dvecXIn  .fill( 0.5 );
  dvecXStep.fill( 0.001 );

  // Bounds for the design parameters.
  mulByScalar( dvecXIn, -1.25, dvecXLow );
  mulByScalar( dvecXIn, +1.25, dvecXUp );

  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  // Initial values and step sizes for the fixed population parameters.
  dvecAlpIn  .fill( 0.3 );
  dvecAlpStep.fill( 0.001 );

  // Bounds for the fixed population parameters.
  mulByScalar( dvecAlpIn, -1.25, dvecAlpLow );
  mulByScalar( dvecAlpIn, +1.25, dvecAlpUp );

  // Relative solution error.
  double xEpsilon   = 1.0e-4;
  double alpEpsilon = 1.0e-4;

  // Maximum number of iterations.
  int nXMaxIter   = 50;
  int nAlpMaxIter = 50;

  // Amount of printing during optimization problems.
  int xLevel   = 1;
  int alpLevel = 0;

  // Set the optimizer controls.
  Optimizer xOptimizer(   xEpsilon,   nXMaxIter,   xLevel );
  Optimizer alpOptimizer( alpEpsilon, nAlpMaxIter, alpLevel );


  //------------------------------------------------------------
  // Quantities related to the population expected determinant criterion.
  //------------------------------------------------------------

  double dPhiOut;

  DoubleMatrix drowPhi_xOut  ( 1,  nX );
  DoubleMatrix dmatPhi_x_xOut( nX, nX );


  //-----------------------------------------------------------
  // Optimize the population expected determinant criterion.
  //-----------------------------------------------------------

  try
  {
    ppedOpt( 
      model,
      xOptimizer,
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      dvecXStep,
      alpOptimizer,
      dvecAlpLow,
      dvecAlpUp,
      dvecAlpIn,
      &dvecAlpOut,
      dvecAlpStep,
      &dPhiOut,
      &drowPhi_xOut,
      &dmatPhi_x_xOut );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected exception occurred in ppedOpt.",
      false );
  }


  //-----------------------------------------------------------
  // Calculate the known values.
  //-----------------------------------------------------------

  double dPhiKnown;

  DoubleMatrix dvecXHat        ( nX,   1 );
  DoubleMatrix dvecAlpHat      ( nAlp, 1 );
  DoubleMatrix drowPhi_xKnown  ( 1,    nX );
  DoubleMatrix dmatPhi_x_xKnown( nX,   nX );

  // For this test the prior distribution of the fixed effects is
  //
  //                        2
  //    p(alp)  =  exp( -alp  )  ,
  //
  // and the determinant of the expected Hessian, i.e., the Fisher
  // information matrix, is
  //
  //                                2
  //    det[ H(x, alp) ]  =  ( 1 - x  )  .
  //
  // Therefore, the criterion for this test is 
  //
  //                +infinity
  //               /\
  //               \                      
  //    phi(x)  =   \        p(alp) det[ HTilde(x, alp) ] dalp
  //               \/
  //                -infinity
  //
  //                 1/2         2
  //            =  pi     ( 1 - x  )  ,
  //
  // and the negative log of this integrand is
  //
  //                             -                               -
  //                            |                                 |
  //    Lambda(x, alp) = - log  |  p(alp)  det[ HTilde(x, alp) ]  |
  //                            |                                 |
  //                             -                               -
  //
  //                         2        2
  //                   =  alp  ( 1 - x  )  .
  //

  // Set the maximizer of phi(x).
  dvecXHat.fill( 0.0 );

  // Set the minimizer of Lambda(x, alp) with respect to alp.
  dvecAlpHat.fill( 0.0 );

  double* pdXHat = dvecXHat.data();

  // Set the criterion and its derivatives at the maximizer.
  dPhiKnown  = sqrt( PI ) * ( 1.0 - pdXHat[0] * pdXHat[0] );
  drowPhi_xKnown  .fill( -2.0 * sqrt( PI ) * pdXHat[0] );
  dmatPhi_x_xKnown.fill( -2.0 * sqrt( PI ) );


  //-----------------------------------------------------------
  // Check the results.
  //-----------------------------------------------------------

  // Check to see if any elements of xOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(xOut - xHat) <= xEpsilon * (xUp - xLow)  .
  //
  // Since epsEqualTest (via isDmatEpsEqual) checks to see if 
  // 
  //      abs(xOut - xHat) <  scale * DBL_EPS_EQUAL_MULT * DBL_EPSILON   ,
  // 
  // the scale for this test is 
  // 
  //      scale  = [ xEpsilon / (DBL_EPS_EQUAL_MULT * DBL_EPSILON) ] * (xUp - xLow)  .
  // 
  DoubleMatrix dvecXScale( nX, 1 );
  dvecXScale = mulByScalar( subtract( dvecXUp, dvecXLow ), 
    xEpsilon / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dvecXOut not equal to dvecXHat",
    epsEqualTest(
      dvecXOut,
      dvecXHat,
      dvecXScale,
      "ppedOptTest", 
      "xOut",
      "x (analytic)" ) );

  // Check to see if any elements of alpOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(alpOut - alpHat) <= alpEpsilon * (alpUp - alpLow)  .
  //
  // Since epsEqualTest (via isDmatEpsEqual) checks to see if 
  // 
  //      abs(alpOut - alpHat) <  scale * DBL_EPS_EQUAL_MULT * DBL_EPSILON   ,
  // 
  // the scale for this test is 
  // 
  //      scale  = [ alpEpsilon / (DBL_EPS_EQUAL_MULT * DBL_EPSILON) ] * (alpUp - alpLow)  .
  // 
  DoubleMatrix dvecAlpScale( nAlp, 1 );
  dvecAlpScale = mulByScalar( subtract( dvecAlpUp, dvecAlpLow ), 
    alpEpsilon / ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dvecAlpOut not equal to dvecAlpHat",
    epsEqualTest(
      dvecAlpOut,
      dvecAlpHat,
      dvecAlpScale,
      "ppedOptTest", 
      "alpOut",
      "alp (analytic)" ) );

  double tol = 1.0e-10;

  CPPUNIT_ASSERT_MESSAGE(
    "dPhiOut not equal to dPhiKnown",
    relTolEqualTest(
      dPhiOut,
      dPhiKnown,
      tol,
      "ppedOptTest",
      "phiOut",
      "phi (analytic)" ) );

  tol = xEpsilon;

  CPPUNIT_ASSERT_MESSAGE(
    "drowPhi_xOut not equal to drowPhi_xKnown",
    absDiffEqualTest(
      drowPhi_xOut,
      drowPhi_xKnown,
      tol,
      "ppedOptTest",
      "phi_xOut",
      "phi_x (analytic)" ) );

  CPPUNIT_ASSERT_MESSAGE(
    "dmatPhi_x_xOut not equal to dmatPhi_x_xKnown",
    relTolEqualTest(
      dmatPhi_x_xOut,
      dmatPhi_x_xKnown,
      tol,
      "ppedOptTest",
      "phi_x_xOut",
      "phi_x_x (analytic)" ) );

}


/*========================================================================
 *
 *
 * Local Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: preTestPrinting
 *
 *************************************************************************/

void preTestPrinting( string title )
{
  using namespace std;
  
  string line = "====================================================";
  
  cout << line << endl;
  cout << title << endl;
  cout << line << endl;
}
  


/*************************************************************************
 *
 * Function: postTestPrinting
 *
 *************************************************************************/

void postTestPrinting( bool ok )
{
  using namespace std;
  
  if ( ok )
  {
    cout << "Test Succeeded:  All values are equal (within tolerance).";
  }
  else
  {
    cout << "Test Failed:  All values are not equal (within tolerance).";
  }

  cout << endl << endl;
}
  

/*************************************************************************
 *
 * Function: isAbsDiffEqual  (double version)
 *
 *************************************************************************/

bool isAbsDiffEqual( double x, double y, double maxDiff )
{
  if ( fabs( x - y ) <= maxDiff )
  {
    return true;
  }
  else 
  {
    return false;
  }
}
  

/*************************************************************************
 *
 * Function: isAbsDiffEqual  (DoubleMatrix version)
 *
 *************************************************************************/

bool isAbsDiffEqual( const DoubleMatrix &dmatX, 
                     const DoubleMatrix &dmatY, 
                     double maxDiff )
{
  assert( dmatX.nr() == dmatY.nr() );
  assert( dmatX.nc() == dmatY.nc() );
  
  const double *x = dmatX.data();
  const double *y = dmatY.data();

  for( int i = 0; i < dmatX.nr() * dmatX.nc(); i++ )
  {
    if( !isAbsDiffEqual( x[i], y[i], maxDiff ) )
    {
      return false;
    }
  }
  return true;
}


/*************************************************************************
 *
 * Function: absDiffEqualTest  (double version)
 *
 *************************************************************************/

bool absDiffEqualTest( double x, 
                       double y, 
                       double maxDiff,
                       string testName,
                       string xName,
                       string yName )
{
  using namespace std;

  if ( isAbsDiffEqual( x, y, maxDiff ) )
  {
      return true;
  }
  else
  {
    cout << "---<< TEST: " << testName << " >>>---" << endl;
    cout << "[Values are not equal up to maximum absolute difference]" << endl;
    cout << endl;
    cout << xName << " = " << x << endl;
    cout << endl;
    cout << yName << " = " << y << endl;
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: absDiffEqualTest  (DoubleMatrix version)
 *
 *************************************************************************/

bool absDiffEqualTest( const DoubleMatrix &dmatX, 
                       const DoubleMatrix &dmatY, 
                       double maxDiff,
                       string testName,
                       string xName,
                       string yName )
{
  using namespace std;

  if ( isAbsDiffEqual( dmatX, dmatY, maxDiff ) )
  {
      return true;
  }
  else
  {
    cout << "*************************************************" << endl;
    cout <<  testName << endl;
    cout << "*************************************************" << endl;
    cout << "[Values are not equal up to maximum absolute difference]" << endl;
    cout << endl;
    cout << xName << " = " << endl;
    dmatX.print();
    cout << endl;
    cout << yName << " = " << endl;
    dmatY.print();
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: isRelTolEqual  (double version)
 *
 *************************************************************************/

bool isRelTolEqual( double x, double y, double tol )
{
  double scale = fabs( x ) > fabs( y ) ? fabs( x ) : fabs( y );

  if ( fabs( x - y ) <= tol * scale )
  {
    return true;
  }
  else 
  {
    return false;
  }
}
  

/*************************************************************************
 *
 * Function: isRelTolEqual  (DoubleMatrix version)
 *
 *************************************************************************/

bool isRelTolEqual( const DoubleMatrix &dmatX, 
                    const DoubleMatrix &dmatY, 
                    double tol )
{
  assert( dmatX.nr() == dmatY.nr() );
  assert( dmatX.nc() == dmatY.nc() );
  
  const double *x = dmatX.data();
  const double *y = dmatY.data();

  for( int i = 0; i < dmatX.nr() * dmatX.nc(); i++ )
  {
    if( !isRelTolEqual( x[i], y[i], tol ) )
    {
      return false;
    }
  }
  return true;
}


/*************************************************************************
 *
 * Function: relTolEqualTest  (double version)
 *
 *************************************************************************/

bool relTolEqualTest( double x, 
                      double y, 
                      double tol,
                      string testName,
                      string xName,
                      string yName )
{
  using namespace std;

  if ( isRelTolEqual( x, y, tol ) )
  {
      return true;
  }
  else
  {
    cout << "---<< TEST: " << testName << " >>>---" << endl;
    cout << "[Values are not equal up to relative tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << x << endl;
    cout << endl;
    cout << yName << " = " << y << endl;
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: relTolEqualTest  (DoubleMatrix version)
 *
 *************************************************************************/

bool relTolEqualTest( const DoubleMatrix &dmatX, 
                      const DoubleMatrix &dmatY, 
                      double tol,
                      string testName,
                      string xName,
                      string yName )
{
  using namespace std;

  if ( isRelTolEqual( dmatX, dmatY, tol ) )
  {
      return true;
  }
  else
  {
    cout << "*************************************************" << endl;
    cout <<  testName << endl;
    cout << "*************************************************" << endl;
    cout << "[Values are not equal up to relative tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << endl;
    dmatX.print();
    cout << endl;
    cout << yName << " = " << endl;
    dmatY.print();
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: epsEqualTest  (double version)
 *
 *************************************************************************/

bool epsEqualTest(  double x, 
                    double y, 
                    double scale,
                    string testName,
                    string xName,
                    string yName )
{
  using namespace std;

  if ( isDblEpsEqual( x, y, scale ) )
  {
      return true;
  }
  else
  {
    cout << "---<< TEST: " << testName << " >>>---" << endl;
    cout << "[Values are not equal up to epsilon tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << x << endl;
    cout << endl;
    cout << yName << " = " << y << endl;
    cout << endl;
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: epsEqualTest  (DoubleMatrix version)
 *
 *************************************************************************/

bool epsEqualTest(  const DoubleMatrix &dmatX, 
                    const DoubleMatrix &dmatY, 
                    const DoubleMatrix &dmatScale, 
                    string testName,
                    string xName,
                    string yName )
{
  using namespace std;

  if ( isDmatEpsEqual( dmatX, dmatY, dmatScale ) )
  {
      return true;
  }
  else
  {
    cout << "*************************************************" << endl;
    cout <<  testName << endl;
    cout << "*************************************************" << endl;
    cout << "[Values are not equal up to epsilon tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << endl;
    dmatX.print();
    cout << endl;
    cout << yName << " = " << endl;
    dmatY.print();
    cout << endl;

    return false;
  }
}


} // [End: unnamed namespace]
