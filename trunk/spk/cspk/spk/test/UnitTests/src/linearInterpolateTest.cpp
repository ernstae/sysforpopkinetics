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
 * File: linearInterpolateTest.cpp
 *
 *
 * Unit test for the function linearInterpolate.
 *
 * Author: Sachiko Honda, based on Brad Bell's unit test.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "linearInterpolateTest.h"

// SPK library header files.
#include "../../../spk/identity.h"
#include "../../../spk/multiply.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/linearInterpolate.h"

// CppAD header files.
#include <CppAD/CppAD.h>

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
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void linearInterpolateTest::setUp()
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

void linearInterpolateTest::tearDown()
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

Test* linearInterpolateTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "linearInterpolateTest" );

  suiteOfTests->addTest(new TestCaller<linearInterpolateTest>(
    "doTest", 
    &linearInterpolateTest::doTest ));

  suiteOfTests->addTest(new TestCaller<linearInterpolateTest>(
    "equalIndepVarTest", 
    &linearInterpolateTest::equalIndepVarTest ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: doTest
 *
 *
 * This content of this test was written by Brad Bell.
 *
 *************************************************************************/

# include <iostream>
# include <CppAD/NearEqual.h>
# include <CppAD/Runge45.h>
# include <vector>

class Fun {
	const std::vector<double> xdata;
	const std::vector<double> ydata;
public:
	Fun(const std::vector<double> &xdata_, const std::vector<double> &ydata_)
	: xdata(xdata_), ydata(ydata_)
	{ }
	void Ode(double t, const std::vector<double> &z, std::vector<double> &f)
	{	f[0] = linearInterpolate(t, xdata, ydata); }
};

double Z(double t)
{	double z;	
	if( t <= 0. )
		z = 2. - t * t / 2.; 
	else	z = 2. + t * t / 2.; 
	return z;
}
void linearInterpolateTest::doTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  bool   ok = true;
  size_t  n = 3;
  using std::vector;
  
  // data that defines the absolute value function
  vector<double> xdata(n), ydata(n);
  xdata[0] = -2.; ydata[0] = 2.;
  xdata[1] =  0.; ydata[1] = 0.;
  xdata[2] =  2.; ydata[2] = 2.;
  
  // Construct Ode function with copy of data
  Fun F(xdata, ydata);
  
  // Try integration in one step: [-2,+2]
  // This will result in an error estimate > 1e-2
  vector<double> zi(1), zf(1), e(1);
  size_t M  = 1;
  double ti = xdata[0]; 
  double tf = xdata[2];
  zi[0]     = Z(ti);
  zf        = CppAD::Runge45(F, M, ti, tf, zi, e);
  CPPUNIT_ASSERT( e[0] > 1e-2 ); 
  CPPUNIT_ASSERT( ! (CppAD::NearEqual(Z(tf), zf[0], 0., 1e-2 ) ) );
  
  // Break integration into two steps: First step [-2,0]
  // First step has an error estimate < 1e-10
  M     = 1;
  ti    = xdata[0]; 
  tf    = xdata[1];
  zi[0] = Z(ti);
  zf    = CppAD::Runge45(F, M, ti, tf, zi, e);
  CPPUNIT_ASSERT( e[0] <= 1e-10 );
  CPPUNIT_ASSERT( CppAD::NearEqual(Z(tf), zf[0], 0., 1e-10 ) );
  
  // Break integration into two steps: Second step [0,+2]
  // Second step has an error estimate < 1e-10
  ti    = xdata[1]; 
  tf    = xdata[2];
  zi[0] = Z(ti);
  zf    = CppAD::Runge45(F, M, ti, tf, zi, e);
  CPPUNIT_ASSERT( e[0] <= 1e-10 );
  CPPUNIT_ASSERT( CppAD::NearEqual(Z(tf), zf[0], 0., 1e-10 ) );
  
  return;
}


/*************************************************************************
 *
 * Function: equalIndepVarTest
 *
 * See if the linearInterpolate function can detect equal values for
 * the independent variables.
 *
 *************************************************************************/

void linearInterpolateTest::equalIndepVarTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  bool   ok = true;
  size_t  n = 3;
  using std::vector;
  
  // Set the time and insulin values with identical values for two
  // consecutive independent variables.
  vector< CppAD::AD<double> > time(n), insu(n);
  time[0] =  0.; insu[0] = 3.9;
  time[1] =  0.; insu[1] = 3.9;
  time[2] =  2.; insu[2] = 76.2;

  // Set the time for the interpolation.
  CppAD::AD<double> t = 0.0;

  // This will contain the interpolated value for insulin.
  CppAD::AD<double> insuInterp;

  // See if the equal independent variable values are detected.
  bool wasEqualIndepVarDetected = false;
  try
  {
    insuInterp = linearInterpolate( t, time, insu );
  }
  catch ( ... )
  {
    wasEqualIndepVarDetected = true;
  }
  CPPUNIT_ASSERT( wasEqualIndepVarDetected ); 
  
  return;
}
