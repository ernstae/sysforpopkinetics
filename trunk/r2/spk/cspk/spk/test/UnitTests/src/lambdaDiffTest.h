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
 * File: lambdaDiffTest.h
 *
 *
 * Declare lambdaDiffTest class
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#ifndef LAMBDADIFF_TEST_H
#define LAMBDADIFF_TEST_H

#include <iostream>
#include <string>
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "Test.h"

#include "DoubleMatrix.h"
#include "SpkModel.h"

class lambdaDiffTest : public TestCase
{

    class UserModelLambdaDiffTest;

    static bool exactDeriv_x(
        SpkModel &model,
        const DoubleMatrix &dvecY,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecB,
        DoubleMatrix *exactLambda_alpOut,
        DoubleMatrix *exactLambda_bOut,
        const bool withD
        );
    static bool exactDeriv_x_alp(
        const DoubleMatrix &dvecY,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecB,
        DoubleMatrix *exactLambda_x_alpOut,
        int   var,
        const bool withD
        );
    static bool exactDeriv_x_b(
        const DoubleMatrix &dvecY,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecB,
        DoubleMatrix *exactLambda_x_bOut,
        int   var,
        const bool withD);

    UserModelLambdaDiffTest *_model;
    DoubleMatrix *_alp;
    DoubleMatrix *_b;
    DoubleMatrix *_y;
    DoubleMatrix *_alpStep;
    DoubleMatrix *_bStep;


public: 
    static Test* suite();
    lambdaDiffTest(const std::string &name) : TestCase (name){}
    void setUp();
    void tearDown();

    // add test cases as void functions
    void testLocalfunctions();
    void testLambda_alp_xWithD();
    void testLambda_alp_xWithoutD();
    void testLambda_b_xWithD();
    void testLambda_b_xWithoutD();
};

#endif
