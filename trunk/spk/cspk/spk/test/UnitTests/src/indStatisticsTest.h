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
 * File: indStatisticsTest.h
 *
 *
 * Declare indStatisticsTest class.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

#ifndef INDSTATISTICS_TEST_H
#define INDSTATISTICS_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class indStatisticsTest : public CppUnit::TestFixture
{
    void statisticsExampleTest();
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();
};

#endif
