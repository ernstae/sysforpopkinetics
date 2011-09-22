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
 * File: fitPopulationTest.h
 *
 *
 * Declare fitPopulationTest class.
 *
 * Author: Jiaji du
 *
 *************************************************************************/

#ifndef FITPOPULATION_TEST_H
#define FITPOPULATION_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#include "../../../spk/Objective.h"
#include "../../../spk/SpkValarray.h"

class fitPopulationTest : public CppUnit::TestFixture
{
    void fitPopulationExampleTest(enum Objective whichObjective);
    void fitPopulationZeroIterationsTest(enum Objective whichObjective);
    void fitPopulationLimitsWarningsTest(enum Objective whichObjective);
    void fitPopulationIndOptErrorTest(enum Objective whichObjective);
    void fitPopulationPopOptErrorTest(enum Objective whichObjective);
    void fitPopulationRestartTest(enum Objective whichObjective);
    void doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const SPK_VA::valarray<double>& dvecEpsilon,
                       const SPK_VA::valarray<double>& dvecAlpLow,
                       const SPK_VA::valarray<double>& dvecAlpUp,
                       const SPK_VA::valarray<double>& dvecAlpOut,
                       const SPK_VA::valarray<double>& dvecAlpHat,
                       const SPK_VA::valarray<double>& dvecBLow,
                       const SPK_VA::valarray<double>& dvecBUp,
                       const SPK_VA::valarray<double>& dmatBOut,
                       const SPK_VA::valarray<double>& dmatBHat,
                       const SPK_VA::valarray<double>& drowLTilde_alpOut,
                       const SPK_VA::valarray<double>& drowLTilde_alpKnown,
                       const SPK_VA::valarray<double>& dmatLTilde_alp_alpOut,
                       const SPK_VA::valarray<double>& dmatLTilde_alp_alpKnown );
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    void modifiedLaplaceTest();
    void expectedHessianTest();
    void naiveFirstOrderTest();

    };

#endif
