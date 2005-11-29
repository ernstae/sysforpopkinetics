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
 * File: ppkaOptTest.h
 *
 *
 * Declare ppkaOptTest class.
 *
 * Author: sachiko honda
 *
 *************************************************************************/

#ifndef PPKAOPT_TEST_H
#define PPKAOPT_TEST_H

#include <cppunit/TestFixture.h>

#include "../../../spk/Objective.h"
#include "../../../spk/DoubleMatrix.h"

class ppkaOptTest : public CppUnit::TestFixture
{
    void ppkaOptExampleTest(enum Objective whichObjective);
    void ppkaOptZeroIterationsTest(enum Objective whichObjective);
    void ppkaOptFixedAlpTest(enum Objective whichObjective);
    void doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const double epsB,
					   const double epsAlp,
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
                       const DoubleMatrix& dmatLTilde_alp_alpOut,
                       const DoubleMatrix& dmatLTilde_alp_alpKnown );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    void modifiedLaplaceTest();
    void expectedHessianTest();
	void naiveFirstOrderTest();
    void popAsIndFirstOrderTest();

    };

#endif
