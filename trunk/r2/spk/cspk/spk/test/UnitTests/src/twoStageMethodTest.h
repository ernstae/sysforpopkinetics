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
 * File: twoStageMethodTest.h
 *
 *
 * Declare twoStageMethodTest class.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef TWOSTAGEMETHOD_TEST_H
#define TWOSTAGEMETHOD_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include "../../../spk/DoubleMatrix.h"

class twoStageMethodTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void railExampleSTSTest();
  void railExampleITSTest();
  void railExampleGTSTest();

  void railExampleSTS_twoIndWillNotOpt_Test();
  void railExampleITS_twoIndWillNotOpt_Test();
  void railExampleGTS_twoIndWillNotOpt_Test();

  // Helper functions.
  void doTheTest(
    double              epsilon,
    const DoubleMatrix& dvecBLow,
    const DoubleMatrix& dvecBUp,
    const DoubleMatrix& dmatBOut,
    const DoubleMatrix& dmatBKnown,
    const DoubleMatrix& dvecBMeanOut,
    const DoubleMatrix& dvecBMeanKnown,
    const DoubleMatrix& dmatBCovOut,
    const DoubleMatrix& dmatBCovKnown );

} ;

#endif
