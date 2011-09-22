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
 * File: broadCastEndOfSpkTest.cpp
 *
 *
 * Test cases for broadCastEndOfSpk
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include "Test.h"
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "DoubleMatrix.h"
#include "broadCastEndOfSpk.h"
#include "broadCastEndOfSpkTest.h"
#include "System.h"
#include "File.h"
#include "PARALLEL_FILE_CONSTS.h"

void broadCastEndOfSpkTest::setUp()
{
    // initializations
}
void broadCastEndOfSpkTest::tearDown()
{
    // clean up
}

Test* broadCastEndOfSpkTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite;

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<broadCastEndOfSpkTest>("testBroadcastEndOfSpk", testBroadcastEndOfSpk));


    return suiteOfTests;
}

void broadCastEndOfSpkTest::testBroadcastEndOfSpk()
{
    using namespace parallel_const;
    File cwd(".\\", "");
	File endFile(cwd.getPath(), SPK_ESPK+"."+SPK_MASTER_SUFFIX);

	broadCastEndOfSpk(cwd);

	assertImplementation(System::exist(endFile));
    System::del(endFile);
}
