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
/*
 * getColTest.cpp
 *
 * test script for getCol()
 *
 * Author: Sachiko Honda
 */
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/getCol.h"
#include "getColTest.h"

#include <cstdlib>
#include <ctime>
#include <list>
#include "../../../spk/isDmatEpsEqual.h"

using namespace CppUnit;

void getColTest::setUp()
{
    // initializations
}
void getColTest::tearDown()
{
    // clean up
}

Test* getColTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "getColTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<getColTest>("testGetCol", &getColTest::testGetCol));
    //suiteOfTests->addTest(new TestCaller<getColTest>("example", example));

    return suiteOfTests;
}
static DoubleMatrix doIt( const DoubleMatrix &dmatA, const int iJth );

void getColTest::testGetCol()
{
    using namespace std;

    std::vector<int> iRows;
    std::vector<int> iCols;

    iRows.push_back(1); iCols.push_back(1);
    iRows.push_back(1); iCols.push_back(3);
    iRows.push_back(3); iCols.push_back(1);
    iRows.push_back(3); iCols.push_back(3);

	srand(time(0));

    int _n=iRows.size();
	for(int i=0; i<_n; i++ )
    {
		DoubleMatrix dmatA(iRows[i], iCols[i]);
		double* pdA = dmatA.data();
		
		int iJth = rand() % dmatA.nc();

        int j;
		for( j=0; j<iRows[i]*iCols[i]; j++ )
			pdA[j] = rand()/1000.0;
		
		DoubleMatrix dvecJth     = getCol(dmatA,iJth);
		DoubleMatrix dvecTestJth = doIt(dmatA, iJth);

        for( j=0; j<dvecJth.nr()*dvecJth.nc(); j++ )
		    CPPUNIT_ASSERT_EQUAL( dvecJth.data()[j], dvecTestJth.data()[j]);
	}
}
/******************************************
 *  Example in the specification
 ******************************************/
void getColTest::example(){

  using namespace std;

  DoubleMatrix dmatA(3,2),
               dvecB;
  double       *pdA    = dmatA.data();

  // Setting A to a matrix:
  //    [ 1  4 ]
  //    [ 2  5 ]
  //    [ 3  6 ]
  for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
      pdA[i] = i+1;
  }

  // This is a good example
  dvecB = getCol(dmatA, 1);

  cout << "Original matrix A" << endl;
  dmatA.print();

  cout << "1st column of B is: " << endl;
  dvecB.print();

}

static DoubleMatrix doIt( const DoubleMatrix &dmatA, const int iJth )
{
    using namespace std;

	int iRows = dmatA.nr();
	int iCols = dmatA.nc();

	DoubleMatrix  dmatB(iRows,1);
	const double *pdA = dmatA.data();
	double *pdB = dmatB.data();
	list<double> lst;
	int i, end = iRows*(iCols-1);

	for( i=0; i<iRows; i++ ){
		lst.push_back( pdA[i+iRows*iJth] );
	}
	list<double>::iterator p = lst.begin();
	i=0;
	while(p != lst.end()){
		pdB[i] = *p;
		i++;
		p++;
	}
	return dmatB;
}
