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
 * File: PopPredModelTest.h
 *
 *
 * Unit test for the class PopPredModel.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef POPPREDMODEL_TEST_H
#define POPPREDMODEL_TEST_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/SpkValarray.h>

// CppUnit framework header files.
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

// Standard library header files.
#include <string>


/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class PopPredModelTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void NoEta_OneExpF_ModelBasedExpY_Test();
  void NoEta_OneExpF_AdditivePlusThetaDepY_Test();

  void OneExpF_ModelBasedExpY_Test();
  void OneExpF_AdditivePlusThetaDepY_Test();
  void OneExpF_AdditivePlusThetaDepY_FullSigma_Test();

  void OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test();

  void isCachingProperlyTest();
};

#endif
