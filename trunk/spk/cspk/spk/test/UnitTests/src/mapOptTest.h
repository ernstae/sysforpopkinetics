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
 * File: mapOptTest.h
 *
 *
 * Declare mapOptTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef MAPOPT_TEST_H
#define MAPOPT_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include "../../../spk/DoubleMatrix.h"

class mapOptTest : public CppUnit::TestFixture
{
    void doTheTest(    double dMapObjOut,
                       double dMapObjKnown,
                       double epsilon,
                       const DoubleMatrix& dvecBLow,
                       const DoubleMatrix& dvecBUp,
                       const DoubleMatrix& dvecBOut,
                       const DoubleMatrix& dvecBKnown,
                       const DoubleMatrix& dmatMapObj_b_bOut,
                       const DoubleMatrix& dmatMapObj_b_bKnown
                      );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void mapOptExampleTest();
    void mapOptExampleExactMatchTest();
    void mapOptNonzeroBMeanTest();
    void mapOptQuadraticTest();
    void mapOptZeroIterationsTest();
};

#endif
