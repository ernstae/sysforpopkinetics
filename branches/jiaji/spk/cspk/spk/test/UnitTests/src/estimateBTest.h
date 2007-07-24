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
 * File: estimateBTest.h
 *
 *
 * Declares estimateBTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#ifndef ESTIMATEB_TEST_H
#define ESTIMATEB_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include "../../../spk/SpkModel.h"
#include "../../../spk/DoubleMatrix.h"

class estimateBTest : public CppUnit::TestFixture
{
    class UserModel;
    int _nA;
    int _nB;
    int _nY;

    bool   _blsq;
    double _eps;
    int    _mitr;
    DoubleMatrix *_y;
    DoubleMatrix *_alp;
    DoubleMatrix *_bin;
    DoubleMatrix *_bup;
    DoubleMatrix *_blow;
    DoubleMatrix *_bstep;

    void checkAns(
        const DoubleMatrix& bHatOutT, 
        const DoubleMatrix& bTildeOutT, 
        const DoubleMatrix& bTilde_alpOut);

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testEstimateB();
    void testExample();
};

#endif
