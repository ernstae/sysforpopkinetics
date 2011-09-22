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
 * File: AkronBtimesCTest.h
 *
 *
 * Declare AkronBtimesCTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#ifndef AKRONBTIMESC_TEST_H
#define AKRONBTIMESC_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

#pragma warning( disable : 4786 )
#include <vector>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"

class AkronBtimesCTest : public CppUnit::TestFixture
{
    std::vector<int> _rows;
    std::vector<int> _cols;
    const DoubleMatrix kron( const DoubleMatrix &dmatA, const DoubleMatrix &dmatB );
    const SPK_VA::valarray<double> kron( const SPK_VA::valarray<double> &A, int iACols,
                                         const SPK_VA::valarray<double> &B, int iBCols );

public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    void testReturnByValue();
    void testReturnThroughReference();
    void testValarray();
    void testValarraySpecExample();

};

#endif
