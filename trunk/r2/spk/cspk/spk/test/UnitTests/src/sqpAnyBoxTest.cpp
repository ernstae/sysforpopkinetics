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
 * File: sqpAnyBoxTest.cpp
 *
 *
 * Unit test for the function sqpAnyBox.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/
// Standard CppUnit headers 
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Headers specific to this test
#include <iostream>
#include <iomanip>
#include <cassert>
#include <string>
#include <cmath>
#include <spk/sqpAnyBox.h>
#include <spk/DoubleMatrix.h>

#include "sqpAnyBoxTest.h"

using namespace CppUnit;

void sqpAnyBoxTest::setUp()
{
    // initializations
}
void sqpAnyBoxTest::tearDown()
{
    // clean up
}

Test* sqpAnyBoxTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite( "sqpAnyBoxTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "fvalInfoTest", 
                         &sqpAnyBoxTest::fvalInfoTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "simpleQuadratic_isWithinTolTest", 
                         &sqpAnyBoxTest::simpleQuadratic_isWithinTolTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "simpleFourthOrder_isWithinTolTest", 
                         &sqpAnyBoxTest::simpleFourthOrder_isWithinTolTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "simpleQuadratic_EqualBoundsTest", 
                         &sqpAnyBoxTest::simpleQuadratic_EqualBoundsTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "fourParamQuadratic_SpecExampleTest", 
                         &sqpAnyBoxTest::fourParamQuadratic_SpecExampleTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "fourParamQuadratic_EqualBoundsTest", 
                         &sqpAnyBoxTest::fourParamQuadratic_EqualBoundsTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "fourParamQuadratic_isWithinTolTest", 
                         &sqpAnyBoxTest::fourParamQuadratic_isWithinTolTest));
    suiteOfTests->addTest(new TestCaller<sqpAnyBoxTest>(
                         "fourParamQuadratic_RelParPrecTest", 
                         &sqpAnyBoxTest::fourParamQuadratic_RelParPrecTest));

    return suiteOfTests;
}

/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using std::string;
using sqpanybox::FVAL_PROTOTYPE;


/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Local Variable Declarations
 *------------------------------------------------------------------------*/

static double checkRelParPrecScale = 1.0e-10;


/*************************************************************************
 *
 * Function: fvalInfoObjective
 *
 *
 * Description
 * -----------
 *
 * This is a dummy objective function used by fvalInfoTest.
 * 
 * 
 * Arguments
 * ---------
 *
 * See the specification for sqpAnyBox where the arguments for the 
 * objective function, fval, are described.
 *
 *************************************************************************/

static void fvalInfoObjective( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  // Reset the value pointed to by pFvalInfo.
  int* pTest = (int*) pFvalInfo;
  *pTest = -1;
  
  // Set the objective function to be a constant.
  if ( pdFOut ) 
  {
    *pdFOut = 1.0;
  }

  // Set the gradient to be zero.
  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();
    int nGOutCols = pdrowGOut->nc();
    assert( nGOutCols == 1 );

    pdGOutData[0] = 0.0;
  }  
}


/*************************************************************************
 *
 * Function: fvalInfoTest
 *
 *
 * The goal of this test is to check that information can be passed 
 * to fval using pFvalInfo.
 *
 *************************************************************************/

void sqpAnyBoxTest::fvalInfoTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 1;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  pdXLowData[0] =  0.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;

  double epsilon  = 1.e-5;
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );

  int test = 1;
  void* pFvalInfo = &test;

  try{
      sqpAnyBox( fvalInfoObjective, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }
   
  CPPUNIT_ASSERT_EQUAL(-1, test);
}


/*************************************************************************
 *
 * Function: simpleQuadratic
 *
 *
 * Description
 * -----------
 *
 * Evaluates the following objective function and/or its gradient:
 *
 *           2
 *   f(x) = x    , where x = [ x    ] .
 *           (1)                (1)
 * 
 * 
 * Arguments
 * ---------
 *
 * See the specification for sqpAnyBox where the arguments for the 
 * objective function, fval, are described.
 *
 *************************************************************************/

void simpleQuadratic( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  // Updated 1-8-01 Alyssa
  // fixed for const correctness
  const double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();
  assert( nXRows == 1 );

  if ( pdFOut ) 
  {
    // Set the objective function value.
    *pdFOut = pow( pdXData[0], 2.0 );
  }

  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();
    int nGOutCols = pdrowGOut->nc();
    assert( nGOutCols == 1 );

    // Set the elements of the gradient.
    pdGOutData[0] = 2.0 * pdXData[0];
  }
}


/*************************************************************************
 *
 * Function: simpleQuadratic_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect convergence for those cases where nag_opt_nlp cannot.
 *
 *************************************************************************/

void sqpAnyBoxTest::simpleQuadratic_isWithinTolTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 1;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  pdXLowData[0] = -1.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;

  double epsilon  = 1.e-7;
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( simpleQuadratic, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }

  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // But, by calling the function isWithinTol, sqpAnyBox should 
  // determine that the sequence of y values really has converged .
  bool okDesired = true;

  double fKnown = 0.0;

  pdXKnownData[0] = 0.0;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: simpleQuadratic_EqualBoundsTest
 *
 *************************************************************************/

void sqpAnyBoxTest::simpleQuadratic_EqualBoundsTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 1;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  double xFixed = -25.0;

  pdXLowData[0] = xFixed;
  pdXUpData[0]  = xFixed;

  pdXInData[0]  = xFixed;

  double epsilon  = 1.e-5;
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( simpleQuadratic, pFvalInfo,optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }

  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // But, by calling the function isWithinTol, sqpAnyBox should 
  // determine that the sequence of y values really has converged .
  bool okDesired = true;

  double fKnown = xFixed * xFixed;

  pdXKnownData[0] = xFixed;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: simpleFourthOrder
 *
 *
 * Description
 * -----------
 *
 * Evaluates the following objective function and/or its gradient:
 *
 *           4
 *   f(x) = x    , where x = [ x    ] .
 *           (1)                (1)
 * 
 * 
 * Arguments
 * ---------
 *
 * See the specification for sqpAnyBox where the arguments for the 
 * objective function, fval, are described.
 *
 *************************************************************************/

void simpleFourthOrder( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  // Updated 1-8-01 Alyssa
  // fixed for const correctness
  const double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();
  assert( nXRows == 1 );

  if ( pdFOut ) 
  {
    // Set the objective function value.
    *pdFOut = pow( pdXData[0], 4.0 );
  }

  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();
    int nGOutCols = pdrowGOut->nc();
    assert( nGOutCols == 1 );

    // Set the elements of the gradient.
    pdGOutData[0] = 4.0 * pow( pdXData[0], 3.0 );
  }
}


/*************************************************************************
 *
 * Function: simpleFourthOrder_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect when the optimization has not convergenced.
 *
 *************************************************************************/

void sqpAnyBoxTest::simpleFourthOrder_isWithinTolTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 1;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  pdXLowData[0] = -1.0;
  pdXUpData[0]  =  1.0;

  pdXInData[0]  = 0.5;

  double epsilon  = 1.e-7;
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( simpleFourthOrder, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      ok = false;
      CPPUNIT_ASSERT_MESSAGE( "sqpAnyBox should have returned false", true );
  }

  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // It should also cause the function isWithinTol to return false,
  // because the Hessian (i.e., the second derivative) for this
  // objective function is 
  //
  //                    2
  //     f_x_x(x) = 12 x  , 

  // which blows up at the solution, x = 0.  That means that the
  // estimated distance to the true solution that is calculated by
  // isWithinTol will be very large and thus the tolerance will
  // will not be satisfied.
  bool okDesired = false;

  double fKnown = 0.0;

  pdXKnownData[0] = 0.0;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadratic
 *
 *
 * Description
 * -----------
 *
 * Evaluates the objective function:
 *
 *                N
 *            1  ----     2                 i      2
 *    f(x) =  -  >       i   [ x     +  (-1)  * i ]
 *            2  ----           (i)
 *               i = 1
 *
 * and/or its gradient:
 *
 *             
 *              2                 i      
 *    g(x) =   i   [ x     +  (-1)  * i ]
 *                    (i)
 *             
 *
 * subject to the constraint that
 *
 *    - (N - 1)  <  x     <  + (N - 1)  , for all i,
 *                   (i)
 *
 * where 
 *
 *    N = 4 
 *
 * and
 *
 *    x = [ x   , x   , x    , x    ] .
 *           (1)   (2)   (3)    (4)
 *
 *
 * Arguments
 * ---------
 *
 * See the specification for sqpAnyBox where the arguments for the 
 * objective function, fval, are described.
 *
 *************************************************************************/

static void fourParamQuadratic( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  int nObjPars = 4;
  
  // Updated 1-8-01 Alyssa
  // fixed for const correctness
  const double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();
  assert( nXRows == nObjPars );

  int i, j;
  double tot;

  if ( pdFOut ) 
  {
    // Set the objective function value.
    tot = 0.0;
    for ( j = 0; j < nObjPars; j++ )
    {
      i = j + 1;
      tot += pow(i, 2.0) * pow( pdXData[j] + pow( -1.0, i ) * i, 2.0 );
    }
    *pdFOut = 0.5 * tot;
  }

  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();
    int nGOutCols = pdrowGOut->nc();
    assert( nGOutCols == nObjPars );

    // Set the elements of the gradient.
    for ( j = 0; j < nObjPars; j++ )
    {
      i = j + 1;
      pdGOutData[j] =  pow(i, 2.0) * ( pdXData[j] + pow( -1.0, i ) * i );
    }
  }
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_SpecExampleTest
 *
 *
 * This test corresponds to the example from the sqpAnyBox specification.
 *
 *************************************************************************/

void sqpAnyBoxTest::fourParamQuadratic_SpecExampleTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 4;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  int i;
  for ( i = 0; i < nObjPars; i++ )
  {
    pdXLowData[i] = -(nObjPars - 1.0);
    pdXUpData[i]  = +(nObjPars - 1.0);

    pdXInData[i]  = 0.0;
  }

  double epsilon  = 1.e-5; 
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  DoubleMatrix f_xOut(1, nObjPars);
  DoubleMatrix f_xKnown(1, nObjPars);
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( fourParamQuadratic, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, &f_xOut );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  bool okDesired = true;

  double fKnown = 8.0;

  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] =  3.0;
  pdXKnownData[3] = -3.0;

/*              2                 i      
 *    g(x) =   i   [ x     +  (-1)  * i ]
 *                    (i)
 */
  for( i=1; i<=nObjPars; i++)
  {
      f_xKnown.data()[i-1] = i*i * ( pdXKnownData[i-1] + pow(-1.0, i) * i );
  }


  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_EqualBoundsTest
 *
 *************************************************************************/

void sqpAnyBoxTest::fourParamQuadratic_EqualBoundsTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 4;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  for ( int i = 0; i < nObjPars; i++ )
  {
    pdXLowData[i] = -(nObjPars - 1.0);
    pdXUpData[i]  = +(nObjPars - 1.0);

    pdXInData[i]  = 0.0;
  }

  double xFixed = 10.0;

  // Reset the bounds for a particular element of x so that they are
  // fixed to this value.
  pdXLowData[2] = xFixed;
  pdXUpData[2]  = xFixed;
  pdXInData[2]  = xFixed;

  double epsilon  = 1.e-5; 
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( fourParamQuadratic, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // But, by calling the function isWithinTol, sqpAnyBox should 
  // determine that the sequence of y values really has converged .
  bool okDesired = true;

  double fKnown = 228.5;

  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] = xFixed;
  pdXKnownData[3] = -3.0;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_isWithinTolTest
 *
 *
 * The goal of this test is to check that the function isWithinTol
 * can detect convergence for those cases where nag_opt_nlp cannot.
 *
 *************************************************************************/

void sqpAnyBoxTest::fourParamQuadratic_isWithinTolTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 4;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  for ( int i = 0; i < nObjPars; i++ )
  {
    pdXLowData[i] = -(nObjPars - 1.0);
    pdXUpData[i]  = +(nObjPars - 1.0);

    pdXInData[i]  = 0.0;
  }

  double xFixed = 10.0;

  // Reset the bounds for a particular element of x so that they are
  // fixed to this value.
  pdXLowData[2] = xFixed;
  pdXUpData[2]  = xFixed;
  pdXInData[2]  = xFixed;

  double epsilon  = 1.e-5; 
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( fourParamQuadratic, pFvalInfo, optimizer, 
        dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, 0 );
    ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // But, by calling the function isWithinTol, sqpAnyBox should 
  // determine that the sequence of y values really has converged .
  bool okDesired = true;

  double fKnown = 228.5;

  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] = xFixed;
  pdXKnownData[3] = -3.0;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: fourParamQuadraticWithScale
 *
 *
 * Description
 * -----------
 *
 * (Note: this is just the objective function fourParamQuadratic 
 * multiplied by a constant scale factor).
 *
 * Evaluates the objective function:
 *
 *                      N
 *                  1  ----     2                 i      2
 *    f(x) =  C  *  -  >       i   [ x     +  (-1)  * i ]
 *                  2  ----           (i)
 *                     i = 1
 *
 * and/or its gradient:
 *
 *             
 *                   2                 i      
 *    g(x) =  C  *  i   [ x     +  (-1)  * i ]
 *                         (i)
 *             
 *
 * subject to the constraint that
 *
 *    - (N - 1)  <  x     <  + (N - 1)  , for all i,
 *                   (i)
 *
 * where 
 *
 *    C = checkRelParPrecScale , 
 *
 *    N = 4 , 
 *
 * and
 *
 *    x = [ x   , x   , x    , x    ] .
 *           (1)   (2)   (3)    (4)
 *
 *
 * Arguments
 * ---------
 *
 * See the specification for sqpAnyBox where the arguments for the 
 * objective function, fval, are described.
 *
 *************************************************************************/

static void fourParamQuadraticWithScale( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  int nObjPars = 4;
  
  // Updated 1-8-01 Alyssa
  // fixed for const correctness
  const double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();
  assert( nXRows == nObjPars );

  int i, j;
  double tot;

  if ( pdFOut ) 
  {
    // Set the objective function value.
    tot = 0.0;
    for ( j = 0; j < nObjPars; j++ )
    {
      i = j + 1;
      tot += pow(i, 2.0) * pow( pdXData[j] + pow( -1.0, i ) * i, 2.0 );
    }
    *pdFOut = checkRelParPrecScale * 0.5 * tot;
  }

  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();
    int nGOutCols = pdrowGOut->nc();
    assert( nGOutCols == nObjPars );

    // Set the elements of the gradient.
    for ( j = 0; j < nObjPars; j++ )
    {
      i = j + 1;
      pdGOutData[j] =  checkRelParPrecScale * pow(i, 2.0) * 
                          ( pdXData[j] + pow( -1.0, i ) * i );
    }
  }
}


/*************************************************************************
 *
 * Function: fourParamQuadratic_RelParPrecTest
 *
 *
 * The goal of this test is to check that nag_opt_nlp, the NAG optimizer 
 * called by sqpAnyBox, uses relative parameter value tolerance, i.e.
 * that the precision of the final parameter values does not depend on the
 * magnitude of the objective function.
 *
 * To check this, this test uses an objective function which is 
 * equivalent to the function fourParamQuadratic multiplied by a 
 * small scale factor, checkRelParPrecScale.  
 *
 *************************************************************************/

void sqpAnyBoxTest::fourParamQuadratic_RelParPrecTest()
{
  using namespace std;

  bool ok;

  int nObjPars = 4;

  DoubleMatrix dvecXLow   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXUp    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXIn    = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXOut   = DoubleMatrix(nObjPars, 1);
  DoubleMatrix dvecXKnown = DoubleMatrix(nObjPars, 1);

  double* pdXLowData   = dvecXLow  .data();
  double* pdXUpData    = dvecXUp   .data();
  double* pdXInData    = dvecXIn   .data();
  double* pdXOutData   = dvecXOut  .data();
  double* pdXKnownData = dvecXKnown.data();

  for ( int i = 0; i < nObjPars; i++ )
  {
    pdXLowData[i] = -(nObjPars - 1.0);
    pdXUpData[i]  = +(nObjPars - 1.0);

    pdXInData[i]  = 0.0;
  }

  double epsilon  = 1.e-5;
  int nMaxIter    = 50; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );
  void* pFvalInfo = 0;

  try{
      sqpAnyBox( fourParamQuadraticWithScale, pFvalInfo, 
        optimizer, dvecXLow, dvecXUp, dvecXIn, 
        &dvecXOut, &fOut, 0 );
      ok = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }

  // This problem will cause nag_opt_nlp (the optimizer called by 
  // sqpAnyBox) to return with fail.code equal to NW_NOT_CONVERGED. 
  // But, by calling the function isWithinTol, sqpAnyBox should 
  // determine that the sequence of y values really has converged .
  bool okDesired = true;

  // If nag_opt_nlp uses absolute objective function tolerance,
  // then it won't be able to reach the known value because it
  // is too small.  If nag_opt_nlp uses relative parameter 
  // tolerance, it shouldn't matter that the objective is small,
  // i.e., it should return a value that is close to fKnown.
  double fKnown = checkRelParPrecScale * 8.0;

  pdXKnownData[0] =  1.0;
  pdXKnownData[1] = -2.0;
  pdXKnownData[2] =  3.0;
  pdXKnownData[3] = -3.0;

  doTheTest( ok, okDesired, fOut, fKnown, epsilon, 
    dvecXLow, dvecXUp, dvecXOut, dvecXKnown );
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void sqpAnyBoxTest::doTheTest( bool okReturned,
                       bool okDesired,
                       double fOut,
                       double fKnown,
                       double epsilon,
                       const DoubleMatrix& dvecXLow,
                       const DoubleMatrix& dvecXUp,
                       const DoubleMatrix& dvecXOut,
                       const DoubleMatrix& dvecXKnown
                      )
{
  using namespace std;

  //------------------------------------------------------------
  // Check to see if the optimization completed sucessfully.
  //------------------------------------------------------------

  CPPUNIT_ASSERT ( okReturned == okDesired );

  //------------------------------------------------------------
  // Check the final parameter value.
  //------------------------------------------------------------

  const double* pdXLowData    = dvecXLow  .data();
  const double* pdXUpData     = dvecXUp   .data();
  const double* pdXOutData    = dvecXOut  .data();
  const double* pdXKnownData  = dvecXKnown.data();

  int nObjPars = dvecXOut.nr();

  // Check to see if any elements of xOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(xOut - xKnown) <= epsilon (xUp - xLow)
  //
  if(okReturned)
  {
      for (int i = 0; i < nObjPars; i++ )
      {
        CPPUNIT_ASSERT_MESSAGE( 
	   "fabs(pdXOutData[i] - pdXKnownData[i]) <=  epsilon * (pdXUpData[i] - pdXLowData[i])", 
	   fabs(pdXOutData[i] - pdXKnownData[i]) <=  epsilon * (pdXUpData[i] - pdXLowData[i]) );
      }
  }
}

