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
 * File: mulByScalarTest.h
 *
 *
 * Declare mulByScalarTest class.
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#ifndef MULBYSCALAR_TEST
#define MULBYSCALAR_TEST

#include <iostream>
#include <string>
#include <vector>
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class mulByScalarTest : public CppUnit::TestFixture
{
    std::vector<int> _rows;
    std::vector<int> _cols;
    std::vector<DoubleMatrix*> _As;

    int _n;
    double _scalar;
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testMatScalarVal();
    void testMatMatVal();
    void testMatScalarRef();
};

#endif
