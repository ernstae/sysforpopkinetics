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
 * File: addTest.cpp
 *
 *
 * Test cases for add()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/add.h"
#include "addTest.h"

#include <cstdlib>
#include <iostream>
#include <ctime>
#include <cmath>
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/subtract.h"

using namespace std;
using namespace CppUnit;

void addTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);

    _n = _rows.size();

    DoubleMatrix *A;
    DoubleMatrix *negA;
    DoubleMatrix *B;
    DoubleMatrix *S;

	srand( time(0) );
	for( int j=0; j<_n; j++ )
    {
        A    = new DoubleMatrix(_rows[j], _cols[j]);
        negA = new DoubleMatrix(_rows[j], _cols[j]);
        B    = new DoubleMatrix(_rows[j], _cols[j]);
        S    = new DoubleMatrix(_rows[j], _cols[j]);

        double* dA = A->data();
        double* dNegA = negA->data();
        double* dB = B->data();
        double* dS = S->data();

        for( int i=0; i<_rows[j]*_cols[j]; i++)
        {
		    dA[i]    = rand() / 1000.0;
		    dNegA[i] = -dA[i];
		    dB[i]    = rand() / 1000.0;
		    dS[i]    = (fabs(dA[i]) >= fabs(dB[i])? fabs(dA[i]) : fabs(dB[i]));
        }
        _As.push_back(A);
        _Bs.push_back(B);
        _negAs.push_back(negA);
        _Ss.push_back(S);
	}

}
void addTest::tearDown()
{
    for( int i=0; i<_n; i++ )
    {
        delete _As.at(i);
        delete _Bs.at(i);
        delete _negAs.at(i);
        delete _Ss.at(i);
    }
}

Test* addTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "addTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<addTest>("testAddVal", &addTest::testAddVal));
    suiteOfTests->addTest(new TestCaller<addTest>("testAddRef", &addTest::testAddRef));

    return suiteOfTests;
}

void addTest::testAddVal()
{
    for( int i=0; i<_n; i++ )
    {
		// Computes C = A + B
		DoubleMatrix dmatC = add(*_As.at(i), *_Bs.at(i));
        
		// Computes B = C + (-A)
		DoubleMatrix dmatBB = add(dmatC, *_negAs.at(i));
		
		for(int j=0; j<_As.at(i)->nr()*_As.at(i)->nc(); j++ )
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL( _Bs.at(i)->data()[j], dmatBB.data()[j], _Ss.at(i)->data()[j] );
        }
	};

}
void addTest::testAddRef()
{
    for( int i=0; i<_n; i++ )
    {
		// Computes C = A + B
		DoubleMatrix dmatC = add(*_As.at(i), *_Bs.at(i));
        
		// Computes B = C + (-A)
		DoubleMatrix dmatBB;
        add(dmatC, *_negAs.at(i), dmatBB);
		
		for(int j=0; j<_As.at(i)->nr()*_As.at(i)->nc(); j++ )
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL( _Bs.at(i)->data()[j], dmatBB.data()[j], _Ss.at(i)->data()[j] );
        }
	};
}
