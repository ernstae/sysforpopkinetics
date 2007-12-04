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
 * File: sqpAnyBoxTest.h
 *
 *
 * Declare sqpAnyBoxTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef SQPANYBOX_TEST_H
#define SQPANYBOX_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include <spk/DoubleMatrix.h>

class sqpAnyBoxTest : public CppUnit::TestFixture
{
    void doTheTest( bool okReturned,
                           bool okDesired,
                           double fOut,
                           double fKnown,
                           double epsilon,
                           const DoubleMatrix& dvecXLow,
                           const DoubleMatrix& dvecXUp,
                           const DoubleMatrix& dvecXOut,
                           const DoubleMatrix& dvecXKnown
                          );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void fvalInfoTest();

    void simpleQuadratic_isWithinTolTest();
    void simpleQuadratic_EqualBoundsTest();

    void simpleFourthOrder_isWithinTolTest();

    void fourParamQuadratic_SpecExampleTest();
    void fourParamQuadratic_EqualBoundsTest();
    void fourParamQuadratic_isWithinTolTest();
    void fourParamQuadratic_RelParPrecTest();
};

#endif
