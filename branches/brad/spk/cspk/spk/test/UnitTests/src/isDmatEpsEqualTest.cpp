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
 * File: isDmatEpsEqualTest.cpp
 *
 *
 * Unit test for isDmatEpsEqual.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <cfloat>
#include <cmath>
#include <vector>
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/divByScalar.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "isDmatEpsEqualTest.h"

using namespace CppUnit;

void isDmatEpsEqualTest::setUp()
{
    // initializations
}
void isDmatEpsEqualTest::tearDown()
{
    // clean up
}

Test* isDmatEpsEqualTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "isDmatEpsEqualTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<isDmatEpsEqualTest>( "equalAndNotEqualCase", &isDmatEpsEqualTest::equalAndNotEqualCase ) );

  return suiteOfTests;
}

using namespace std;

void isDmatEpsEqualTest::equalAndNotEqualCase()
{
    std::vector<int> _rows;
    std::vector<int> _cols;
    int _n;

    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);
    _n = _rows.size();

	int iRows, iCols;
	double *pdA, *pdB, *pdS;

	srand( time(0) );
	for( int j=0; j<_n; j++ )
	{
        int i;
		iRows = _rows[j];
		iCols = _cols[j];

		DoubleMatrix A(iRows, iCols);
		DoubleMatrix B(iRows, iCols);
		DoubleMatrix S(iRows, iCols);
		pdA = A.data();
		pdB = B.data();
		pdS = S.data();

		for( i=0; i<iRows*iCols; i++ )
        {
			pdA[i] = i+1;
			pdB[i] = pdA[i]+DBL_EPSILON;
			pdS[i] = pdA[i];
		}

        // should be all true
		if( !isDmatEpsEqual(A,B,S) )
        {
			cerr << "isDmatEpsEqualTest truth test failed. " << __FILE__ << "(" << __LINE__ << ")" << endl;
                cerr << __FILE__ << "(" << __LINE__ << ")" << endl;
                cerr << "A= " << A << endl;
                cerr << "B= " << B << endl;
		}
		CPPUNIT_ASSERT( isDmatEpsEqual(A,B,S) );

		// Inequality tests
		for( i=0; i<iRows*iCols; i++ )
        {
			pdB[i] = pdA[i]*10.0;
		}

        // should be all false
        if( iRows*iCols>0 )
        {
		    if( isDmatEpsEqual(A,B,S) )
            {
			    cerr << "isDmatEpsEqualTest false test failed. " << endl;
                cerr << __FILE__ << "(" << __LINE__ << ")" << endl;
                cerr << "A= " << A << endl;
                cerr << "B= " << B << endl;
		    }
		    CPPUNIT_ASSERT( !isDmatEpsEqual(A,B,S) );
        }
    }

}
