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
 * File: elsq_xTest.h
 *
 *
 * Declare elsq_xTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef ELSQ_X_TEST_H
#define ELSQ_X_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"

class elsq_xTest : public CppUnit::TestFixture
{
    // compare results from running centdiff() to results from directly running the derivative routine
    void testder( double (*pElsq)(const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix & ),
                         const DoubleMatrix (*pElsq_x)( 
                                         const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix &,
                                         const DoubleMatrix & ),
                         const DoubleMatrix &dvecX,
                         const DoubleMatrix &dvecZ,
                         const DoubleMatrix &dvecStepSize );
    void testder( double (*pElsq)(const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> & ),
                         const SPK_VA::valarray<double> (*pElsq_x)( 
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> &,
                                         const SPK_VA::valarray<double> & ),
                         const SPK_VA::valarray<double> &x,
                         const SPK_VA::valarray<double> &z, 
                         const SPK_VA::valarray<double> &stepSize );

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void testElsq_x();
    void testElsq_xValarray();
};

#endif
