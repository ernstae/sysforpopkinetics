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
 * File: AkronItimesCTest.h
 *
 *
 * test cases for AkronItimesC.
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#ifndef AKRONITIMESC_TEST_H
#define AKRONITIMESC_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#include <vector>
class AkronItimesCTest : public CppUnit::TestFixture
{
    int _n;
    std::vector<int> _rows;
    std::vector<int> _cols;

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testAkronItimesCVal();
    void testAkronItimesCRef();
    void testAkronItimesCValarray();
    void testAkronItimesCValarraySpecExample();
};

#endif
