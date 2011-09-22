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
#ifndef ELEMENTWISEANDTEST_H
#define ELEMENTWISEANDTEST_H

#include <iostream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class elementwiseAndTest : public CppUnit::TestFixture
{
    int * nRows;
    int * nCols;
    int n;
    double T;
    double F;
    double neg;
    double pos;

    std::string _id;
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    void allTrueAgainstAllTrue();
    void allTrueAgainstAllFalse();
    void allTrueAgainstAllNeg();
    void allTrueAgainstAllPos();

    void allFalseAgainstAllTrue();
    void allFalseAgainstAllFalse();
    void allFalseAgainstAllNeg();
    void allFalseAgainstAllPos();

    void allNegAgainstAllTrue();
    void allNegAgainstAllFalse();
    void allNegAgainstAllNeg();
    void allNegAgainstAllPos();

    void allPosAgainstAllTrue();
    void allPosAgainstAllFalse();
    void allPosAgainstAllNeg();
    void allPosAgainstAllPos();
};

#endif
