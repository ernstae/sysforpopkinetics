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
 * File: quasiNewtonAnyBoxTest.cpp
 *
 *
 * Unit test for the function quasiNewtonAnyBox.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK test suite header files.
#include "quasiNewtonAnyBoxTest.h"

// SPK library header files.
#include "../../../spk/quasiNewtonAnyBox.h"
#include "../../../spk/DoubleMatrix.h"

// SPK optimizer header files.
#include <QN01Box/QuasiNewton01Box.h>
#include <QN01Box/MaxAbs.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <string>
#include <cmath>
#include <iostream>

using namespace CppUnit;
using std::string;


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class: SimpleQuadratic
  //
  //
  // Evaluates the following objective function and/or its gradient:
  //
  //               2
  //     f(x)  =  x     , where x = [ x    ] .
  //               (1)                 (1)
  //
  //**********************************************************************

  class SimpleQuadratic : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    SimpleQuadratic( int nXIn )
      :
      nX  ( nXIn )
    {
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nX;

    DoubleMatrix dvecXCurr;
    double* pdXCurrData;


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the objective function f(x).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecXIn, double* pdFOut )
    {
      // Set the current value for x.
      dvecXCurr = dvecXIn;

      pdXCurrData = dvecXCurr.data();
      assert( dvecXCurr.nr() == nX );
      assert( dvecXCurr.nc() == 1 );

      // Set the objective.
      *pdFOut = pow( pdXCurrData[0], 2.0 );
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the objective function f_x(x).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowF_xOut ) const
    {
      double* pdF_xOutData = pdrowF_xOut->data();
      assert( pdrowF_xOut->nr() == 1 );
      assert( pdrowF_xOut->nc() == nX );

      // Set the gradient of the objective.
      pdF_xOutData[0] = 2.0 * pdXCurrData[0];
    }

  };


  //**********************************************************************
  //
  // Class: SimpleFourthOrder
  //
  //
  // Evaluates the following objective function and/or its gradient:
  //
  //               4      2
  //     f(x)  =  x    + x   / 100 , where x = [ x    ] .
  //               (1)    (1)                     (1)
  //
  //**********************************************************************

  class SimpleFourthOrder : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    SimpleFourthOrder( int nXIn )
      :
      nX  ( nXIn )
    {
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nX;

    DoubleMatrix dvecXCurr;
    double* pdXCurrData;


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the objective function f(x).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecXIn, double* pdFOut )
    {
      // Set the current value for x.
      dvecXCurr = dvecXIn;

      pdXCurrData = dvecXCurr.data();
      assert( dvecXCurr.nr() == nX );
      assert( dvecXCurr.nc() == 1 );

      // Set the objective.
      double x = pdXCurrData[0];
      *pdFOut = pow( x, 4.0 ) + pow( x, 2.0) / 100.;
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the objective function f_x(x).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowF_xOut ) const
    {
      double* pdF_xOutData = pdrowF_xOut->data();
      assert( pdrowF_xOut->nr() == 1 );
      assert( pdrowF_xOut->nc() == nX );

      // Set the gradient of the objective.
      double x = pdXCurrData[0];
      pdF_xOutData[0] = 4.0 * pow( x, 3.0 ) + 2.0 * x / 100.;
    }

  };


  //**********************************************************************
  //
  // Class: FourParamQuadratic
  //
  //
  // Evaluates the following objective function and/or its gradient:
  //
  //
  //                  N
  //              1  ----     2                 i      2
  //    f(x)  =   -  >       i   [ x     +  (-1)  * i ]   ,
  //              2  ----           (i)
  //                 i = 1
  //
  // subject to the constraint that
  //
  //    - (N - 1)  <  x     <  + (N - 1)  , for all i,
  //                   (i)
  //
  // and
  //
  //    x = [ x   , x   , x    , x    ]  .
  //           (1)   (2)   (3)    (4)
  //
  //
  // The i-th element of the gradient is given by
  //             
  //                 2                 i      
  //    g   (x)  =  i   [ x     +  (-1)  * i ]  .
  //     (i)               (i)
  //
  // For this test, 
  //
  //    N = 4 
  //
  // and the minimizer of the objective function is
  //
  //    ^
  //    x = [ 1, -2, 3, -3 ]  .
  //             
  //**********************************************************************

  class FourParamQuadratic : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    FourParamQuadratic( int nXIn )
      :
      nX  ( nXIn )
    {
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nX;

    DoubleMatrix dvecXCurr;
    double* pdXCurrData;


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the objective function f(x).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecXIn, double* pdFOut )
    {
      // Set the current value for x.
      dvecXCurr = dvecXIn;

      pdXCurrData = dvecXCurr.data();
      assert( dvecXCurr.nr() == nX );
      assert( dvecXCurr.nc() == 1 );

      // Set the objective.
      double total = 0.0;
      int i;
      for ( i = 1; i <= nX; i++ )
      {
        total += i * i * pow( pdXCurrData[i - 1] + pow( -1.0, i ) * i, 2.0 );
      }
      *pdFOut = 0.5 * total;
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the objective function f_x(x).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowF_xOut ) const
    {
      double* pdF_xOutData = pdrowF_xOut->data();
      assert( pdrowF_xOut->nr() == 1 );
      assert( pdrowF_xOut->nc() == nX );

      // Set the gradient of the objective.
      int i;
      for ( i = 1; i <= nX; i++ )
      {
        pdF_xOutData[i - 1] = i * i * ( pdXCurrData[i - 1] + pow( -1.0, i ) * i );
      }
    }

  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::setUp()
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

void quasiNewtonAnyBoxTest::tearDown()
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

Test* quasiNewtonAnyBoxTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "quasiNewtonAnyBoxTest" );

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "simpleQuadratic_isWithinTolTest", 
    &quasiNewtonAnyBoxTest::simpleQuadratic_isWithinTolTest));

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "simpleQuadratic_nonzeroGradTest", 
    &quasiNewtonAnyBoxTest::simpleQuadratic_nonzeroGradTest));

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "simpleQuadratic_equalBoundsTest", 
    &quasiNewtonAnyBoxTest::simpleQuadratic_equalBoundsTest));

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "simpleFourthOrder_isWithinTolTest", 
    &quasiNewtonAnyBoxTest::simpleFourthOrder_isWithinTolTest));

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "fourParamQuadratic_isWithinTolTest", 
    &quasiNewtonAnyBoxTest::fourParamQuadratic_isWithinTolTest));

  suiteOfTests->addTest(new TestCaller<quasiNewtonAnyBoxTest>(
    "fourParamQuadratic_equalBoundsTest", 
    &quasiNewtonAnyBoxTest::fourParamQuadratic_equalBoundsTest));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: simpleQuadratic_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect convergence for this objective function.
 * 
 * Note:  with this objective function and set of inputs the sequential
 * quadratic programming (SQP) optimizer from NAG (nag_opt_nlp) was not
 * able to detect convergence.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::simpleQuadratic_isWithinTolTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 1;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  pdXLowData[0] = -1.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  SimpleQuadratic objective( nX );

  // Set the optimizer control information.
  double epsilon  = 1.e-7;
  int nMaxIter    = 50; 
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( SpkException& e )
  {
    cout <<"Error: The following exception(s) occured in quasiNewtonAnyBox:" << endl;
    cout << e;

    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected SpkException occurred in quasiNewtonAnyBox.",
      false );
  }
  catch( const std::exception& stde )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected standard exception occurred in quasiNewtonAnyBox.",
      false );
  }  
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected and unknown exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  double fKnown;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1,  nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter and then calculate
  // the objective function,
  //
  //               2
  //     f(x)  =  x     , where x = [ x    ] .
  //               (1)                 (1)
  //
  // and its gradient.
  pdXKnownData[0]   = 0.0;
  fKnown            = pdXKnownData[0] * pdXKnownData[0];
  pdF_xKnownData[0] = 2.0 * pdXKnownData[0];


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: simpleQuadratic_nonzeroGradTest
 *
 *
 * The goal of this test is to check that the value for the gradient
 * of the objective function at an optimization parameter value for
 * which the gradient is not equal to zero.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::simpleQuadratic_nonzeroGradTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 1;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  pdXLowData[0] = -1.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  SimpleQuadratic objective( nX );

  // Set the number of iterations equal to zero so that the input
  // value for x will be accepted as the output value. This should
  // make the gradient be nonzero when it is evaluated.
  int nMaxIter = 0; 

  // Set the optimizer control information.
  double epsilon  = 1.e-7;
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  double fKnown;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1, nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter equal to the input
  // value and then calculate the objective function,
  //
  //               2
  //     f(x)  =  x     , where x = [ x    ] .
  //               (1)                 (1)
  //
  // and its gradient.
  pdXKnownData[0]   = pdXInData[0];
  fKnown            = pdXKnownData[0] * pdXKnownData[0];
  pdF_xKnownData[0] = 2.0 * pdXKnownData[0];


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: simpleQuadratic_equalBoundsTest
 *
 *
 * The goal of this test is to check that the optimizer can handle the
 * case where the parameter's lower and upper bounds are equal.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::simpleQuadratic_equalBoundsTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 1;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  double xFixed = -25.0;

  pdXLowData[0] = xFixed;
  pdXUpData[0]  = xFixed;
  pdXInData[0]  = xFixed;


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  SimpleQuadratic objective( nX );

  // Set the optimizer control information.
  double epsilon  = 1.e-7;
  int nMaxIter    = 50; 
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( SpkException& e )
  {
    cout <<"Error: The following exception(s) occured in quasiNewtonAnyBox:" << endl;
    cout << e;

    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected SpkException occurred in quasiNewtonAnyBox.",
      false );
  }
  catch( const std::exception& stde )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected standard exception occurred in quasiNewtonAnyBox.",
      false );
  }  
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected and unknown exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  double fKnown;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1,  nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter and then calculate
  // the objective function,
  //
  //               2
  //     f(x)  =  x     , where x = [ x    ] .
  //               (1)                 (1)
  //
  // and its gradient, which is equal to zero for this test since
  // the lower and upper bounds are equal.
  pdXKnownData[0]   = xFixed;
  fKnown            = pdXKnownData[0] * pdXKnownData[0];
  pdF_xKnownData[0] = 0.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: simpleFourthOrder_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect convergence for this objective function.
 * 
 * Note:  with this objective function and set of inputs the sequential
 * quadratic programming (SQP) optimizer from NAG (nag_opt_nlp) was not
 * able to detect convergence.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::simpleFourthOrder_isWithinTolTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 1;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  pdXLowData[0] = -1.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  SimpleFourthOrder objective( nX );

  // Set the optimizer control information.
  double epsilon  = 1.e-4;
  int nMaxIter    = 50; 
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( SpkException& e )
  {
    cout <<"Error: The following exception(s) occured in quasiNewtonAnyBox:" << endl;
    cout << e;

    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected SpkException occurred in quasiNewtonAnyBox.",
      false );
  }
  catch( const std::exception& stde )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected standard exception occurred in quasiNewtonAnyBox.",
      false );
  }  
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected and unknown exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  double fKnown;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1,  nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter and then calculate
  // the objective function,
  //
  //               4      2
  //     f(x)  =  x    + x   / 100 , where x = [ x    ] .
  //               (1)    (1)                     (1)
  //
  // and its gradient.
  pdXKnownData[0]   = 0.0;
  fKnown            = 0.0;
  pdF_xKnownData[0] = 0.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect convergence for this objective function.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::fourParamQuadratic_isWithinTolTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 4;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  int i;
  for ( i = 0; i < nX; i++ )
  {
    pdXLowData[i] = -(nX - 1.0);
    pdXUpData[i]  = +(nX - 1.0);

    pdXInData[i]  = 0.0;
  }


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  FourParamQuadratic objective( nX );

  // Set the optimizer control information.
  double epsilon  = 1.e-6;
  int nMaxIter    = 50; 
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( SpkException& e )
  {
    cout <<"Error: The following exception(s) occured in quasiNewtonAnyBox:" << endl;
    cout << e;

    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected SpkException occurred in quasiNewtonAnyBox.",
      false );
  }
  catch( const std::exception& stde )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected standard exception occurred in quasiNewtonAnyBox.",
      false );
  }  
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected and unknown exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known value for the objective function.
  double fKnown = 8.0;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1,  nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter.
  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] =  3.0;
  pdXKnownData[3] = -3.0;

  // Calculate the known values for the elements of the gradient,
  //
  //                   2                 i      
  //     g   (x)  =   i   [ x     +  (-1)  * i ]  ,
  //      (i)                (i)
  //
  // where i = 1, 2, ..., nX.
  //
  for ( i = 1; i <= nX; i++ )
  {
    pdF_xKnownData[i - 1] = i * i * ( pdXKnownData[i - 1] + pow( -1.0, i ) * i );
  }


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_equalBoundsTest
 *
 *
 * The goal of this test is to check that the optimizer can handle the
 * case where the parameter's lower and upper bounds are equal.
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::fourParamQuadratic_equalBoundsTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nX = 4;


  //------------------------------------------------------------
  // Quantities related to the objective function parameter.
  //------------------------------------------------------------

  DoubleMatrix dvecXLow( nX, 1 );
  DoubleMatrix dvecXUp(  nX, 1 );
  DoubleMatrix dvecXIn(  nX, 1 );
  DoubleMatrix dvecXOut( nX, 1 );

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  int i;
  for ( i = 0; i < nX; i++ )
  {
    pdXLowData[i] = -(nX - 1.0);
    pdXUpData[i]  = +(nX - 1.0);

    pdXInData[i]  = 0.0;
  }

  double xFixed = 10.0;

  // Reset the bounds for a particular element of x so that they are
  // fixed to this value.
  pdXLowData[2] = xFixed;
  pdXUpData[2]  = xFixed;
  pdXInData[2]  = xFixed;


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  FourParamQuadratic objective( nX );

  // Set the optimizer control information.
  double epsilon  = 1.e-6;
  int nMaxIter    = 50; 
  int level       = 0;
  Optimizer optInfo( epsilon, nMaxIter, level );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try
  {
    quasiNewtonAnyBox(
      objective,
      optInfo, 
      dvecXLow,
      dvecXUp,
      dvecXIn,
      &dvecXOut,
      &fOut,
      &drowF_xOut );
  }
  catch( SpkException& e )
  {
    cout <<"Error: The following exception(s) occured in quasiNewtonAnyBox:" << endl;
    cout << e;

    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected SpkException occurred in quasiNewtonAnyBox.",
      false );
  }
  catch( const std::exception& stde )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected standard exception occurred in quasiNewtonAnyBox.",
      false );
  }  
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE(
      "An unexpected and unknown exception occurred in quasiNewtonAnyBox.",
      false );
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known value for the objective function.
  double fKnown = 228.5;

  DoubleMatrix dvecXKnown(   nX, 1 );
  DoubleMatrix drowF_xKnown( 1,  nX );

  double* pdXKnownData   = dvecXKnown.data();
  double* pdF_xKnownData = drowF_xKnown.data();

  // Set the known value for the parameter.
  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] = xFixed;
  pdXKnownData[3] = -3.0;

  // Calculate the known values for the elements of the gradient,
  //
  //                   2                 i      
  //     g   (x)  =   i   [ x     +  (-1)  * i ]  ,
  //      (i)                (i)
  //
  // where i = 1, 2, ..., nX.
  //
  for ( i = 1; i <= nX; i++ )
  {
    pdF_xKnownData[i - 1] = i * i * ( pdXKnownData[i - 1] + pow( -1.0, i ) * i );
  }

  // Reset this element of the gradient equal to zero since
  // its lower and upper bounds are equal.
  pdF_xKnownData[2] = 0.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(
    fOut,
    fKnown,
    drowF_xOut,
    drowF_xKnown,
    epsilon,
    dvecXLow,
    dvecXUp,
    dvecXOut,
    dvecXKnown );
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void quasiNewtonAnyBoxTest::doTheTest(
  double               fOut,
  double               fKnown,
  const DoubleMatrix&  drowF_xOut,
  const DoubleMatrix&  drowF_xKnown,
  double               epsilon,
  const DoubleMatrix&  dvecXLow,
  const DoubleMatrix&  dvecXUp,
  const DoubleMatrix&  dvecXOut,
  const DoubleMatrix&  dvecXKnown )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;

  bool ok;

  /*
  cout << endl;

  cout << "xOut = " << endl;
  dvecXOut.print(); 
  cout << "xKnown = " << endl;
  dvecXKnown.print(); 
  cout << endl;

  cout << "fOut   = " << fOut << endl;
  cout << "fKnown = " << fKnown << endl;
  cout << endl;

  cout << "f_xOut  = " << endl;
  drowF_xOut.print();
  cout << "f_xKnown  = " << endl;
  drowF_xKnown.print();
  cout << endl;
  */


  //------------------------------------------------------------
  // Check the parameter values.
  //------------------------------------------------------------

  const double* pdXLowData    = dvecXLow  .data();
  const double* pdXUpData     = dvecXUp   .data();
  const double* pdXOutData    = dvecXOut  .data();
  const double* pdXKnownData  = dvecXKnown.data();

  int nX = dvecXOut.nr();

  // Check to see if any elements of xOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(xOut - xKnown) <= epsilon (xUp - xLow)
  //
  ok = true;
  for ( i = 0; i < nX; i++ )
  {
    ok &= fabs( pdXOutData[i] - pdXKnownData[i] )
          <= epsilon * ( pdXUpData[i] - pdXLowData[i] );
  }

  CPPUNIT_ASSERT_MESSAGE( 
    "The final and known parameter values do not agree.",
    ok );


  //------------------------------------------------------------
  // Check the objective function values.
  //------------------------------------------------------------

  double tol = 1.0e-9;

  if ( fKnown != 0.0 )
  {
    ok &= fabs( ( fOut - fKnown ) / fKnown ) < tol;
  }
  else
  {
    ok &= fabs( fOut - fKnown ) < tol;
  }

  CPPUNIT_ASSERT_MESSAGE( 
    "The final and known objective function values do not agree.",
    ok );


  //------------------------------------------------------------
  // Check the objective function gradient values.
  //------------------------------------------------------------

  tol = 5.0e-5;

  const double* pdF_xOutData   = drowF_xOut.data();
  const double* pdF_xKnownData = drowF_xKnown.data();

  ok = true;
  for ( i = 0; i < nX; i++ )
  {
    if ( pdF_xKnownData[i] != 0.0 )
    {
      ok &= fabs( ( pdF_xOutData[i] - pdF_xKnownData[i] ) 
              / pdF_xKnownData[i] ) < tol;
    }
    else
    {
      ok &= fabs( pdF_xOutData[i] - pdF_xKnownData[i] ) < tol;
    }
  }

  CPPUNIT_ASSERT_MESSAGE( 
    "The final and known gradient values do not agree.",
    ok );

}

