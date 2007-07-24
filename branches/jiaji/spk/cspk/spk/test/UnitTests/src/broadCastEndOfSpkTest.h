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
 * File: broadCastEndOfSpkTest.h
 *
 *
 * Declare broadCastEndOfSpkTest class
 *
 * Author: sachiko honda
 *
 *************************************************************************/

#ifndef BROADCASTENDOFSPK_TEST_H
#define BROADCASTENDOFSPK_TEST_H

#include <iostream>
#include <string>
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "Test.h"


class broadCastEndOfSpkTest : public TestCase
{
public: 
    static Test* suite();
    broadCastEndOfSpkTest(const std::string &name) : TestCase (name){}
    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testBroadcastEndOfSpk();
};

#endif
