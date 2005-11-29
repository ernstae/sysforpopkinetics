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
 * File: BlockAllocTest.cpp
 *
 *
 * Test cases for BlockAlloc()
 *
 * Author: Jiaji Du
 *
 * Extended by Sachiko Honda, 09/26/2002
 *
 *************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/BlockAlloc.h"
#include "BlockAllocTest.h"

using namespace CppUnit;
void BlockAllocTest::setUp()
{
    // initializations
}
void BlockAllocTest::tearDown()
{
    // clean up
}

Test* BlockAllocTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "BlockAllocTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<BlockAllocTest>("testBlockMemoryAlloc", 
							 &BlockAllocTest::testBlockMemoryAlloc));
    suiteOfTests->addTest(new TestCaller<BlockAllocTest>("testInsufficientMemory", 
							 &BlockAllocTest::testInsufficientMemory));


    return suiteOfTests;
}

static bool BlockAlloc_Ok()
{    
    // number of integers required
    int size = 2;

    // number of blocks initialy in use
    int initial = BlockFree(NULL);

    // memory block with space for the integers
    int *i = (int *) BlockAlloc(size * sizeof(int));

    // check the increase in number of block in use
    if( BlockFree(NULL) != initial + 1 )
        return false;

    // write something in that memory
    int capacity = BlockCapacity(i) / sizeof(int);
    if( capacity < size )
        return false;
    int j;
    for(j = 0; j < capacity; j++)
        i[j] = j;

    // check that the memory usage is ok
    for(j = 0; j < capacity; j++)
    {
      if( i[j] != j )
      {
        return false;
      }
    }

    // free the memory block because it will not be referenced again
    // in addition, check that the in use counter is correct
    if( BlockFree(i) != initial )
        return false;

    // test ok
    return true;
}

#include <cmath>
#include <climits>
void BlockAllocTest::testInsufficientMemory()
{
    //
    // Request a large enough to so that there is not sufficient memory
    //
  int nbyte = INT_MAX;

    // memory block with space for the double-precision numbers
    // which occupies twice as much memory as integer.
  double * p = 0;
    try{
      p = (double *) BlockAlloc(nbyte * sizeof(char));
      /*
	if( p == NULL )
	{
	  CPPUNIT_ASSERT_MESSAGE( "Good it detected insufficient memory.", true );
	}
      else
	{
	  BlockFree(p);
	  CPPUNIT_ASSERT_MESSAGE( "It should have complained insufficient available memory",
				  false );
	}
      */
    }
    catch( SpkException& e )
      {
	CPPUNIT_ASSERT_MESSAGE( "Good it detected insufficient memory.", true );
	return;
      }
    BlockFree(p);
    CPPUNIT_ASSERT_MESSAGE( "It should have complained insufficient available memory", false );

}
void BlockAllocTest::testBlockMemoryAlloc()
{     
    bool ok = true;

    // get initial number of blocks in use
    int NumberInUser = BlockFree(NULL);

    // run the test
    if( ok )
        ok = BlockAlloc_Ok();

    // check for a memroy leak
    if( ok )
        ok = BlockFree(NULL) == NumberInUser;

    // return memory to the operating system
    BlockReturn();

    //
    // [ Sachiko, 09/26/2002 ]
    //
    // Replaced assert(...) with CppUnit::assertImplementation( bool_exp, message, __LINE__, __FILE__ ).
    // ANSII assert() does not get executed when the executable is built release.

    // report result
    CPPUNIT_ASSERT_MESSAGE( "BlockAllocTest::testBlockMemoryAlloc() failed", ok );
}
