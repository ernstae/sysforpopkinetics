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
 * File: quasiNewtonAnyBoxTest.h
 *
 *
 * Unit test for the function quasiNewtonAnyBox.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef QUASINEWTONANYBOX_TEST_H
#define QUASINEWTONANYBOX_TEST_H

#include "../../../spk/DoubleMatrix.h"
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class quasiNewtonAnyBoxTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void simpleQuadratic_isWithinTolTest();
  void simpleQuadratic_nonzeroGradTest();
  void simpleQuadratic_equalBoundsTest();
  void simpleFourthOrder_isWithinTolTest();
  void fourParamQuadratic_isWithinTolTest();
  void fourParamQuadratic_equalBoundsTest();

  // Utility functions.
  void doTheTest(
    double               fOut,
    double               fKnown,
    const DoubleMatrix&  drowF_xOut,
    const DoubleMatrix&  dvecF_xKnown,
    double               epsilon,
    const DoubleMatrix&  dvecXLow,
    const DoubleMatrix&  dvecXUp,
    const DoubleMatrix&  dvecXOut,
    const DoubleMatrix&  dvecXKnown );

};

#endif
