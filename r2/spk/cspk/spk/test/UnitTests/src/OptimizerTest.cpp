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
 * File: OptimizerTest.cpp
 *
 *
 * Unit test for the Optimizer class.
 *
 * Author: Jiaji Du
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <string>
#include <cstdio>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/Optimizer.h"
#include "../../../spk/quasiNewtonAnyBox.h"
#include "OptimizerTest.h"

using namespace CppUnit;


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
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
    FourParamQuadratic( int nXIn, Optimizer* pOptInfoIn )
      :
      nX       ( nXIn ),
      pOptInfo ( pOptInfoIn )
    {
      // Give the optimizer controller a pointer to this objective.
      pOptInfo->setObjFunc( this );
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int    nX;
    Optimizer*   pOptInfo;
    DoubleMatrix dvecXCurr;
    double*      pdXCurrData;


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


    //**********************************************************
    // 
    // Function: readRestartInfoFromFile
    //
    //
    // Reads any information from the restart file that is required by
    // the objective function.
    //
    //**********************************************************

    virtual void readRestartInfoFromFile() const
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace std;


      //--------------------------------------------------------
      // Read scalar values from the restart file.
      //--------------------------------------------------------

      bool boolTrueValue;
      pOptInfo->getValue( "boolTrueValue",  boolTrueValue );
      CPPUNIT_ASSERT_MESSAGE(
        "The true bool value from the restart file is not true.",
        boolTrueValue );

      bool boolFalseValue;
      pOptInfo->getValue( "boolFalseValue",  boolFalseValue );
      CPPUNIT_ASSERT_MESSAGE(
        "The false bool value from the restart file is not false.",
        !boolFalseValue );

      double doubleValue;
      pOptInfo->getValue( "doubleValue",  doubleValue );
      CPPUNIT_ASSERT_MESSAGE(
        "The double value from the restart file is not correct.",
        doubleValue == -3.830057027409275e+123 );

      int intValue;
      pOptInfo->getValue( "intValue",  intValue );
      CPPUNIT_ASSERT_MESSAGE(
        "The int value from the restart file is not correct.",
        intValue == 833455 );


      //--------------------------------------------------------
      // Read arrays of values from the restart file.
      //--------------------------------------------------------

      int i;

      const int nBoolTrue = 5;
      bool* boolTrueArray = new bool[ nBoolTrue ];
      pOptInfo->getArray( "boolTrueArray", nBoolTrue, boolTrueArray );
      for ( i = 0; i < nBoolTrue; i++ )
      {
        CPPUNIT_ASSERT_MESSAGE(
          "The array of true bool values from the restart file is not all true.",
          boolTrueArray[i] == boolTrueValue );
      }
      delete[] boolTrueArray;

      const int nBoolFalse = 4;
      bool* boolFalseArray = new bool[ nBoolFalse ];
      pOptInfo->getArray( "boolFalseArray", nBoolFalse, boolFalseArray );
      for ( i = 0; i < nBoolFalse; i++ )
      {
        CPPUNIT_ASSERT_MESSAGE(
          "The array of false bool values from the restart file is not all false.",
          boolFalseArray[i] == boolFalseValue );
      }
      delete[] boolFalseArray;

      const int nDouble = 3;
      double* doubleArray = new double[ nDouble ];
      pOptInfo->getArray( "doubleArray", nDouble, doubleArray );
      for ( i = 0; i < nDouble; i++ )
      {
        CPPUNIT_ASSERT_MESSAGE(
          "The array of double values from the restart file is not correct.",
          doubleArray[i] == doubleValue );
      }
      delete[] doubleArray;

      const int nInt = 2;
      int* intArray = new int[ nInt ];
      pOptInfo->getArray( "intArray", nInt, intArray );
      for ( i = 0; i < nInt; i++ )
      {
        CPPUNIT_ASSERT_MESSAGE(
          "The array of int values from the restart file is not correct.",
          intArray[i] == intValue );
      }
      delete[] intArray;
    }


    //**********************************************************
    // 
    // Function: writeRestartInfoToFile
    //
    //
    // Writes any information to the restart file that is required by
    // the objective function.
    //
    //**********************************************************

    virtual void writeRestartInfoToFile() const
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace std;


      //--------------------------------------------------------
      // Read scalar values from the restart file.
      //--------------------------------------------------------

      const bool boolTrueValue = true;
      pOptInfo->writeValue( "boolTrueValue",  boolTrueValue );

      const bool boolFalseValue = false;
      pOptInfo->writeValue( "boolFalseValue",  boolFalseValue );

      const double doubleValue = -3.830057027409275e+123;
      pOptInfo->writeValue( "doubleValue",  doubleValue );

      const int intValue = 833455;
      pOptInfo->writeValue( "intValue",  intValue );


      //--------------------------------------------------------
      // Write arrays of values from the restart file.
      //--------------------------------------------------------

      int i;

      const int nBoolTrue = 5;
      bool* boolTrueArray = new bool[ nBoolTrue ];
      for ( i = 0; i < nBoolTrue; i++ )
      {
        boolTrueArray[i] = boolTrueValue;
      }
      pOptInfo->writeArray( "boolTrueArray", nBoolTrue, boolTrueArray );
      delete[] boolTrueArray;

      const int nBoolFalse = 4;
      bool* boolFalseArray = new bool[ nBoolFalse ];
      for ( i = 0; i < nBoolFalse; i++ )
      {
        boolFalseArray[i] = boolFalseValue;
      }
      pOptInfo->writeArray( "boolFalseArray", nBoolFalse, boolFalseArray );
      delete[] boolFalseArray;

      const int nDouble = 3;
      double* doubleArray = new double[ nDouble ];
      for ( i = 0; i < nDouble; i++ )
      {
        doubleArray[i] = doubleValue;
      }
      pOptInfo->writeArray( "doubleArray", nDouble, doubleArray );
      delete[] doubleArray;

      const int nInt = 2;
      int* intArray = new int[ nInt ];
      for ( i = 0; i < nInt; i++ )
      {
        intArray[i] = intValue;
      }
      pOptInfo->writeArray( "intArray", nInt, intArray );
      delete[] intArray;
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

void OptimizerTest::setUp()
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

void OptimizerTest::tearDown()
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

Test* OptimizerTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("OptimizerTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<OptimizerTest>("basicTest", &OptimizerTest::basicTest));
  
  suiteOfTests->addTest(new TestCaller<OptimizerTest>("restartFileTest", &OptimizerTest::restartFileTest));
  
  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: basicTest
 *
 *
 * This test checks the member functions of the Optimizer class that
 * are not related to restart files.
 *
 *************************************************************************/

void OptimizerTest::basicTest()
{
  using namespace std;
  
  int    nSet               = 2;
  size_t bSet               = 9;
  double rSet               = 5.2;
  double fSet               = 7.5;
  double xSet[]             = { 1.0, 2.0 };
  double gSet[]             = { 1.0, 2.0 };
  double hSet[]             = { 1.0, 2.0, 3.0, 4.0 };
  int    mSet               = 3;
  double lowSet[]           = { -10.0, +20.0, -30.0 };
  double upSet[]            = { +10.0, +20.0, +30.0 };
  int    posSet[]           = { 0, 2 };
  int    acceptStepCountSet = 23;

  // basic constructor
  Optimizer opt1( .001, 10, 1 );
  // setStateInfo
  opt1.setStateInfo(
    nSet, bSet, rSet, fSet, xSet, gSet, hSet,
    mSet, lowSet, upSet, posSet,
    acceptStepCountSet );

  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt1.getEpsilon(), .001 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt1.getNMaxIter(), 10 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt1.getLevel(), 1 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt1.getNIterCompleted(), 0 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( !opt1.getIsTooManyIter() );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( !opt1.getIsWarmStartPossible() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt1.getIsWarmStart() );
  // setEpsilon
  opt1.setEpsilon( .01 );
  CPPUNIT_ASSERT_EQUAL( opt1.getEpsilon(), .01 );
  // setNMaxIter
  opt1.setNMaxIter( 100 );
  CPPUNIT_ASSERT_EQUAL( opt1.getNMaxIter(), 100 );
  // setLevel
  opt1.setLevel( 2 );
  CPPUNIT_ASSERT_EQUAL( opt1.getLevel(), 2 );
  // setNIterCompleted
  opt1.setNIterCompleted( 20 );
  CPPUNIT_ASSERT_EQUAL( opt1.getNIterCompleted(), 20 );
  // setIsTooManyIter
  opt1.setIsTooManyIter( true );
  CPPUNIT_ASSERT( opt1.getIsTooManyIter() );
  
  // copy constructor
  Optimizer opt2(opt1);

  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt2.getEpsilon(), .01 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt2.getNMaxIter(), 100 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt2.getLevel(), 2 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt2.getNIterCompleted(), 20 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt2.getIsTooManyIter() );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt1.getIsTooManyIter() );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT_EQUAL( opt1.getIsWarmStartPossible(), false );
  // getIsWarmStart
  CPPUNIT_ASSERT_EQUAL( opt1.getIsWarmStart(), false );
  
  // assignment operator
  Optimizer opt3( .01, 1, 0 );
  opt3 = opt2;

  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt3.getEpsilon(), .01 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt3.getNMaxIter(), 100 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt3.getLevel(), 2 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt3.getNIterCompleted(), 20 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt3.getIsTooManyIter() );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( !opt3.getIsWarmStartPossible() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt3.getIsWarmStart() );
  
  // setIsWarmStartPossible
  opt1.setIsWarmStartPossible( true );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( opt1.getIsWarmStartPossible() );
  
  // setIsWarmStart
  opt1.setIsWarmStart( true );
  // getIsWarmStart
  CPPUNIT_ASSERT( opt1.getIsWarmStart() );

  int     nGet               = nSet;
  size_t  bGet;
  double  rGet;
  double  fGet;
  double* xGet               = new double[ nGet ];
  double* gGet               = new double[ nGet ];
  double* hGet               = new double[ nGet * nGet ];
  int     acceptStepCountGet;

  valarray<double> yGet( mSet );

  // getPar
  opt1.getPar( yGet );

  int nObjParFree = 0;
  CPPUNIT_ASSERT_EQUAL( yGet[ 0 ], lowSet[0] + ( upSet[0] - lowSet[0] ) * xSet[nObjParFree++] );
  CPPUNIT_ASSERT_EQUAL( yGet[ 1 ], lowSet[1] );
  CPPUNIT_ASSERT_EQUAL( yGet[ 2 ], lowSet[2] + ( upSet[2] - lowSet[2] ) * xSet[nObjParFree++] );

  // getStateInfo
  opt1.getStateInfo(
    nGet, bGet, rGet, fGet, xGet, gGet, hGet,
    mSet, lowSet, upSet, posSet, 
    acceptStepCountGet );

  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", xGet != 0 );
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", gGet != 0 );
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", hGet != 0 );
  
  CPPUNIT_ASSERT_EQUAL( nGet,                     2 );
  CPPUNIT_ASSERT_EQUAL( static_cast<int>( bGet ), 9 );
  CPPUNIT_ASSERT_EQUAL( rGet,                     5.2 );
  CPPUNIT_ASSERT_EQUAL( fGet,                     7.5 );
  CPPUNIT_ASSERT_EQUAL( xGet[ 0 ],                1.0 );
  CPPUNIT_ASSERT_EQUAL( xGet[ 1 ],                2.0 );
  CPPUNIT_ASSERT_EQUAL( gGet[ 0 ],                1.0 );
  CPPUNIT_ASSERT_EQUAL( gGet[ 1 ],                2.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 0 ],                1.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 1 ],                2.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 2 ],                3.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 3 ],                4.0 );
  CPPUNIT_ASSERT_EQUAL( acceptStepCountGet,       23 );

  // default constructor
  Optimizer opt4;

  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt4.getEpsilon(), 0.001 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt4.getNMaxIter(), 0 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt4.getLevel(), 0 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt4.getNIterCompleted(), 0 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( !opt4.getIsTooManyIter() );
  // getSaveStateAtEndOfOpt
  CPPUNIT_ASSERT( !opt4.getSaveStateAtEndOfOpt() );
  // getThrowExcepIfMaxIter
  CPPUNIT_ASSERT( opt4.getThrowExcepIfMaxIter() );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( !opt4.getIsWarmStartPossible() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt4.getIsWarmStart() );
  // getDidOptFinishOk
  CPPUNIT_ASSERT( !opt4.getDidOptFinishOk() );
  // getIsBeginOfIterStateInfo
  CPPUNIT_ASSERT( !opt4.getIsBeginOfIterStateInfo() );

  delete[] xGet;
  delete[] gGet;
  delete[] hGet;
}


/*************************************************************************
 *
 * Function: restartFileTest
 *
 *
 * This test checks the member functions of the Optimizer class that
 * are related to restart files.
 *
 *************************************************************************/

void OptimizerTest::restartFileTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const int nX = 4;


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
  // Prepare the first optimizer controller.
  //------------------------------------------------------------

  // Set the optimizer control information so that the optimizer will
  // have enough iterations to converge the first time it is called.
  double epsilon = 1.e-6;
  int nMaxIter   = 50; 
  int level      = 0;

  // Instantiate the optimizer controller.
  Optimizer firstOptInfo( epsilon, nMaxIter, level ); 


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  FourParamQuadratic firstObjFunc( nX, &firstOptInfo );

  double fOut;

  DoubleMatrix drowF_xOut( 1, nX );


  //------------------------------------------------------------
  // Call the optimizer for the first time.
  //------------------------------------------------------------

  // Optimize the first objective function.
  try
  {
    quasiNewtonAnyBox(
      firstObjFunc,
      firstOptInfo,
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
  // Prepare the second optimizer controller.
  //------------------------------------------------------------

  // This file will hold the restart information.
  const string restartFile = "OptimizerTest_restart_info.xml";

  // Set these flags so that the restart information will not be read
  // from the restart file, but it will be saved to the file.
  bool readRestartInfo  = false;
  bool writeRestartInfo = true;

  // In order to test handling of the convergence acceptance criteria
  // information, set the number of iterations so that the optimizer
  // will stop one iteration before it converges.
  nMaxIter = firstOptInfo.getNIterCompleted() - 1; 

  // Instantiate the optimizer controller.
  Optimizer secondOptInfo(
    epsilon,
    nMaxIter,
    level, 
    restartFile,
    readRestartInfo,
    writeRestartInfo ); 

  // Set these flags so that no exception will be thrown and so that
  // the state information required for a warm start will be saved.
  secondOptInfo.setThrowExcepIfMaxIter( false );
  secondOptInfo.setSaveStateAtEndOfOpt( true );


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  FourParamQuadratic secondObjFunc( nX, &secondOptInfo );


  //------------------------------------------------------------
  // Call the optimizer for the second time.
  //------------------------------------------------------------

  // Do the second few iterations of the optimization until the
  // maximum number of iterations is reached.
  try
  {
    quasiNewtonAnyBox(
      secondObjFunc,
      secondOptInfo,
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
  // Prepare the third optimizer controller.
  //------------------------------------------------------------

  // Increase the number of iterations so that the optimizer will be
  // able to converge successfully.
  nMaxIter = 50; 

  // Set these flags so that the restart information will be retrieved
  // from the restart file, and so it will be saved to the file.
  readRestartInfo  = true;
  writeRestartInfo = true;

  // Instantiate the optimizer controller.
  Optimizer thirdOptInfo(
    epsilon,
    nMaxIter,
    level,
    restartFile,
    readRestartInfo,
    writeRestartInfo ); 

  // Set these flags so that a warm start will be performed using the
  // state information from the restart file and so that the state
  // information required for a warm start will be saved.
  thirdOptInfo.setIsWarmStart( true );
  thirdOptInfo.setSaveStateAtEndOfOpt( true );


  //------------------------------------------------------------
  // Prepare the remaining inputs to the optimizer.
  //------------------------------------------------------------

  // Construct the objective function.
  FourParamQuadratic thirdObjFunc( nX, &thirdOptInfo );


  //------------------------------------------------------------
  // Call the optimizer for the third time.
  //------------------------------------------------------------

  // Do the rest of the iterations starting where the second call to
  // the optimizer left off.
  try
  {
    quasiNewtonAnyBox(
      thirdObjFunc,
      thirdOptInfo,
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
  // Check the parameter values.
  //------------------------------------------------------------

  // Check to see if any elements of xOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(xOut - xKnown) <= epsilon (xUp - xLow)
  //
  bool ok = true;
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

  tol = 3.0e-5;

  const double* pdF_xOutData   = drowF_xOut.data();

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


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Delete the restart file.
  if ( remove( restartFile.c_str() ) )
  {
    CPPUNIT_ASSERT_MESSAGE( "Unable to delete the restart file.", false );
  }
}
