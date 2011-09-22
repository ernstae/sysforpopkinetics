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
 * File: PpedOptTest.h
 *
 *
 * This class performs the unit test for the function ppedOpt.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PPEDOPTTEST_H
#define PPEDOPTTEST_H

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

class ppedOptTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void scalarModelNoXCommonTest();
  void scalarModelOneXCommonTest();
  void singleExponentialModelNoXCommonTest();
  void singleExponentialModelOneXCommonTest();
  void singleExponentialModelTwoXCommonTest();

  // Utility functions.
  void scalarModelTest( int nInd, int nXPerInd, int nXCommon );
  void singleExponentialModelTest( int nInd, int nXPerInd, int nXCommon );
};

#endif
