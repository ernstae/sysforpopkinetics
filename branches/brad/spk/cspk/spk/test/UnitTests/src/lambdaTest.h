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
 * File: lambdaTest.h
 *
 *
 * Declare lambdaTest class
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#ifndef LAMBDA_TEST_H
#define LAMBDA_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkModel.h"


class lambdaTest : public CppUnit::TestFixture
{
    class UserModelLambdaTest;
    UserModelLambdaTest *_model;
    DoubleMatrix *_alp;
    DoubleMatrix *_b;
    DoubleMatrix *_y;

    void test(
        SpkModel<double> &model, 
        const DoubleMatrix &dvecY, 
        const DoubleMatrix &dvecAlp, 
        const DoubleMatrix &dvecB, 
        double lambdaOut, 
        DoubleMatrix &lambda_alpOut, 
        DoubleMatrix &lambda_bOut,
        const bool withD);

    static void setAlp(DoubleMatrix& alp);
    static void setB(DoubleMatrix& b);
    static void setY(DoubleMatrix& y);

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testWithD();
    void testWithoutD();
};

#endif
