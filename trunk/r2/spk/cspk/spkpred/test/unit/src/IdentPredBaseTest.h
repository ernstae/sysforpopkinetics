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
 * File: IdentPredBaseTest.h
 *
 *
 * Unit test for the class IdentPredBase.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef IDENTPREDBASE_TEST_H
#define IDENTPREDBASE_TEST_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// CppUnit framework header files.
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>


/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class IdentPredBaseTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void PaperTwoCompExample_notIdentifiable_Test();
  void PaperTwoCompExample_globallyIdentifiable_Test();
};

#endif
