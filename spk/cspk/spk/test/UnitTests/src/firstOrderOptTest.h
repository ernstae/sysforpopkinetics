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
 * File: firstOrderOptTest.h
 *
 *
 * Declare firstOrderOptTest class.
 *
 * Author: Jiaji Du based on Sachiko's ppkaOptTest.h
 *
 *************************************************************************/

#ifndef FIRSTORDEROPT_TEST_H
#define FIRSTORDEROPT_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include <spk/DoubleMatrix.h>

class firstOrderOptTest : public CppUnit::TestFixture
{
    void firstOrderOptExampleTest();
    void firstOrderOptRestartTest();
    void firstOrderOptZeroIterationsTest();
    void doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const double espB,
                       const double espAlp,
                       const DoubleMatrix& dvecAlpLow,
                       const DoubleMatrix& dvecAlpUp,
                       const DoubleMatrix& dvecAlpOut,
                       const DoubleMatrix& dvecAlpHat,
                       const DoubleMatrix& dvecBLow,
                       const DoubleMatrix& dvecBUp,
                       const DoubleMatrix& dmatBOut,
                       const DoubleMatrix& dmatBHat,
                       const DoubleMatrix& drowLTilde_alpOut,
                       const DoubleMatrix& drowLTilde_alpKnown,
                       const DoubleMatrix& dmatLambdaTilde_alpOut,
                       const DoubleMatrix& dmatLambdaTilde_alpKnown,
                       const DoubleMatrix& dmatLTilde_alp_alpOut,
                       const DoubleMatrix& dmatLTilde_alp_alpKnown );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

	void firstOrderTest();

};

#endif
