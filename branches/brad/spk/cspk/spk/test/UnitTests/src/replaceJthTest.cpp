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
 * File: replaceJthTest.cpp
 *
 *
 * Unit test for replaceJth
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: replaceJthTest
 *
 *
 * Performs the unit test for replaceJth.
 *
 *************************************************************************/
#pragma warning( disable : 4786 )

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/replaceJth.h"
#include "replaceJthTest.h"

using namespace CppUnit;

void replaceJthTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);
}
void replaceJthTest::tearDown()
{
    // clean up
}

Test* replaceJthTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "replaceJthTest" );
  
  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<replaceJthTest>("rangeCheck", &replaceJthTest::rangeCheck));

  return suiteOfTests;
}

void replaceJthTest::rangeCheck()
{
    for( int cnt=0; cnt<_rows.size(); cnt++ )
    {
        int m = _rows[cnt];
        int n = _cols[cnt];

        DoubleMatrix colvec(m,1);
        colvec.fill(1);
        DoubleMatrix original(m,n);


        for( int j=0; j<n; j++ )
        {
            original.fill(0);
            replaceJth(original, j, colvec);
            for( int i=0; i<m*n; i++ )
            {
                if( i>=j*m && i<(j+1)*m )
                    CPPUNIT_ASSERT_EQUAL(1.0, original.data()[i]);
                else
                    CPPUNIT_ASSERT_EQUAL(0.0, original.data()[i]);
            }
            original.fill(0);
        }
    }
}
