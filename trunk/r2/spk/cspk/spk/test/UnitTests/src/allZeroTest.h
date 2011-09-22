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
 * File: allZero.h
 *
 *
 * Declare a unit test for allZero
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef ALLZERO_TEST_H
#define ALLZERO_TEST_H

#include <iostream>
#include <string>
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "Test.h"


class allZeroTest : public TestCase
{
public: 
    static Test* suite();
    allZeroTest(const std::string &name) : TestCase (name){}
    void setUp();
    void tearDown();

    // add test cases as void member functions
    void allZeroTest::allZeroCase();
    void allZeroTest::someZeroCase();
    void allZeroTest::allNonZeroCase();
};

#endif
