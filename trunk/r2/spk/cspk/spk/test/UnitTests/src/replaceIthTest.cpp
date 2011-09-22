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
 * File: replaceIthTest.cpp
 *
 *
 * Test cases for replaceIth()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/replaceIth.h"
#include "replaceIthTest.h"

using namespace CppUnit;

void replaceIthTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);
    _n = _rows.size();
}
void replaceIthTest::tearDown()
{
    // clean up
}

Test* replaceIthTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "replaceIthTest" );
  
  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<replaceIthTest>("testReplaceIth", &replaceIthTest::testReplaceIth));

  return suiteOfTests;
}

void replaceIthTest::testReplaceIth()
{
    for( int j=0; j<_n; j++ )
    {
        const int m = _rows[j];
        const int n = _cols[j];

        DoubleMatrix A(m,n);
        DoubleMatrix b(1,n);
        b.fill(1);

        for(int i=0; i<m; i++)
        {
            A.fill(0);
            replaceIth(A,i,b);
            for( int v=0; v<n; v++ )
            {
                for( int w=0; w<m; w++ )
                {
                    if( w == i )
                    {
                        CPPUNIT_ASSERT_EQUAL(A.data()[w+v*m], 1.0);
                    }
                    else
                    {
                        CPPUNIT_ASSERT_EQUAL(A.data()[w+v*m], 0.0);
                    }
                }
            }
        }
    }
}
