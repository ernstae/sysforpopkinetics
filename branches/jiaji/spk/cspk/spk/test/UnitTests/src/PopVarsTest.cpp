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
 * File: PopVarsTest.cpp
 *
 *
 * Unit test for PopVars class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: PopVarsTest
 *
 *
 * Performs the unit test for PopVars class.
 *
 *************************************************************************/
#include <sstream>
#include "../../../spk/PopVars.h"
#include "../../../spk/mulByScalar.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "PopVarsTest.h"

using namespace CppUnit;

void PopVarsTest::setUp()
{
    // initializations
}
void PopVarsTest::tearDown()
{
    // clean up
}

Test* PopVarsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "PopVarsTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<PopVarsTest>( "emptyMatrixCase", &PopVarsTest::emptyMatrixCase ) );


  return suiteOfTests;
}

using namespace std;
void PopVarsTest::emptyMatrixCase()
{
	// Testing with an empty matrix with all true flags
    DoubleMatrix pop1;
    PopVars ttt(pop1, true, true, true);
	PopVars ttf(pop1, true, true, false);
	PopVars tff(pop1, true, false, false);
	PopVars fff(pop1, false, false, false);
	PopVars fft(pop1, false, false, true);
	PopVars ftt(pop1, false, true, true);
	PopVars ftf(pop1, false, true, false);
	PopVars tft(pop1, true, false, true);

	PopVars array[] = {ttt, ttf, tff, fff, fft, ftt, ftf, tft};
	int n = sizeof(array)/sizeof(PopVars);

	// Declare an empty PopVars outside of the loop so that 
	// it gets stuff left over internally after the 1st iteration.
	// It is to see the contents get overwritten completely.
	PopVars temp;
	for( int i=0; i<n; i++ )
	{
		stringstream stream;
		try{
			stream << array[i];
		}
		catch(...)
		{
            cerr << "Control should not reach here" << endl;
			CPPUNIT_ASSERT(false);
		}

		try{
			stream >> temp;
		}
		catch(...)
		{
            cerr << "Control should not reach here" << endl;
			CPPUNIT_ASSERT(false);
		}

		CPPUNIT_ASSERT( !(array[i] != temp) );
    }
}

