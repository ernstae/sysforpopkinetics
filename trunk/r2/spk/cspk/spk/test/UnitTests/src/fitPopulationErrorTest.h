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
 * File: fitPopulationErrorTest.h
 *
 *
 * Unit test for forcing fitPopulation() to generate errors
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef FITPOPULATIONERROR_TEST_H
#define FITPOPULATIONERROR_TEST_H

#include <cppunit/Test.h>
#include <cppunit/TestFixture.h>

#include <spk/DoubleMatrix.h>
#include <spk/Objective.h>
#include <spk/Optimizer.h>
#include <spk/SpkValarray.h>

namespace fitpopulationerrortest{
    struct Info{
        SPK_VA::valarray<int>    N;

        SPK_VA::valarray<double> Y;
        Optimizer popOptimizer;
        SPK_VA::valarray<double> alpLow;
        SPK_VA::valarray<double> alpUp;
        SPK_VA::valarray<double> alpIn;
        SPK_VA::valarray<double> alpOut;
        SPK_VA::valarray<double> alpStep;
        Optimizer indOptimizer;
        SPK_VA::valarray<double> bLow;
        SPK_VA::valarray<double> bUp;
        SPK_VA::valarray<double> bIn;
        SPK_VA::valarray<double> bOut;
        SPK_VA::valarray<double> bStep;
        double dLTildeOut;
        SPK_VA::valarray<double> lTilde_alpOut;
        SPK_VA::valarray<double> lTilde_alp_alpOut;
    };
}
class fitPopulationErrorTest : public CppUnit::TestFixture
{
    void runFitPopulation(enum Objective whichObjective, fitpopulationerrortest::Info &info);
    void setAllValid(struct fitpopulationerrortest::Info& info);

    void validateAllLegal();

    //void validateObjective();
    void validateN();
    void validateY();
    void validateEpsilon();
    void validateMaxItr();
    void validateLevel();
    void validateAlpLow();
    void validateAlpUp();
    void validateAlpIn();
    void validateAlpStep();

    void validateBLow();
    void validateBUp();
    void validateBIn();
    void validateBStep();

    void tooManyIter();

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void fitPopulationUserInputValidatations();
    void fitPopulationOptimizerStressTest();
};

#endif
