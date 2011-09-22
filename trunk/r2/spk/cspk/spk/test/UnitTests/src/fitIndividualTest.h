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
 * File: fitIndividualTest.h
 *
 *
 * Declare fitIndividualTest class.
 *
 * Author: Jiaji du
 *
 *************************************************************************/

#ifndef FITINDIVIDUAL_TEST_H
#define FITINDIVIDUAL_TEST_H

#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#include "../../../spk/SpkValarray.h"

class fitIndividualTest : public CppUnit::TestFixture
{
    void doTheTest(    double dMapObjOut,
                       double dMapObjKnown,
                       double epsilon,
                       const SPK_VA::valarray<double>& dvecBLow,
                       const SPK_VA::valarray<double>& dvecBUp,
                       const SPK_VA::valarray<double>& dvecBOut,
                       const SPK_VA::valarray<double>& dvecBKnown,
                       const SPK_VA::valarray<double>& dmatMapObj_b_bOut,
                       const SPK_VA::valarray<double>& dmatMapObj_b_bKnown
                      );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void fitIndividualExampleTest();
    void fitIndividualQuadraticTest();
    void fitIndividualZeroIterationsTest();
    void fitIndividualLimitsWarningsTest();
    void fitIndividualOptErrorTest();
};

#endif
