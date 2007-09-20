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
#ifndef ISLESSTHANOREQUALTOTEST_H
#define ISLESSTHANOREQUALTOTEST_H

#include <iostream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>


class isLessThanOrEqualToTest : public CppUnit::TestFixture
{
    int * nRows;
    int * nCols;
    int n;
    double pos;
    double zero;
    double neg;
    std::string _id;
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // tests against a scalar
    void allPosAgainstPos();
    void allPosAgainstZero();
    void allPosAgainstNeg();
    void allNegAgainstPos();
    void allNegAgainstZero();
    void allNegAgainstNeg();
    void allZeroAgainstZero();

    // tests against another matrix
    void allPosAgainstAllPos();
    void allPosAgainstAllZero();
    void allPosAgainstAllNeg();
    void allNegAgainstAllPos();
    void allNegAgainstAllZero();
    void allNegAgainstAllNeg();
    void allZeroAgainstAllZero();

};

#endif
