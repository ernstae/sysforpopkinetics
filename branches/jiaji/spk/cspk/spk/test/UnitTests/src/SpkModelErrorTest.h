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
 * File: SpkModelErrorTest.h
 *
 *
 * Exception throwing SpkModel objects tests
 *
 * Author: Sachiko
 *
 *************************************************************************/

#ifndef SPKMODELERR_TEST_H
#define SPKMODELERR_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>
#include "../../../spk/SpkModel.h"
#include "../../../spk/SpkValarray.h"

namespace spkmodelerrortest
{
  typedef void (SpkModel<double>::*selectIndividual_proto)(int);
  typedef void (SpkModel<double>::*setXXXPar_proto)(const SPK_VA::valarray<double>&);
  typedef void (SpkModel<double>::*model_proto)( SPK_VA::valarray<double>& ) const;
  typedef bool (SpkModel<double>::*deriv_proto)( SPK_VA::valarray<double>& ) const;
}

class SpkModelErrorTest : public CppUnit::TestFixture
{

public: 
  static CppUnit::Test* suite();

  void setUp();
  void tearDown();

  void throwIntModelCase();
  void throwStdExceptionModelCase();
  void throwSpkExceptionModelCase();
};

#endif
