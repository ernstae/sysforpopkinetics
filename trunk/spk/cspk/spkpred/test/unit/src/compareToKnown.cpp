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
 * File: compareToKnown.cpp
 *
 *
 * Compares an array of calculated values to an array of known values.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: compareToKnown  (bool version)
 *
 *
 * Compares an array of calculated values xCalc to an array of known
 * xKnown values.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "compareToKnown.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// CppUnit framework header files.
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <string>

using namespace CppUnit;
using std::string;
using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void compareToKnown(
  const valarray<bool>&  xCalc,
  const valarray<bool>&  xKnown,
  const string&          name )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Compare the arrays.
  //------------------------------------------------------------

  assert( xCalc.size() == xKnown.size() );

  int i;
  bool ok = true;

  for ( i = 0; i < xCalc.size(); i++ )
  {
    if ( xKnown[i] != xCalc[i] )
    {
      ok &= false;
    }
  }

  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for " + name + " do not agree.",
    ok );

}

/*************************************************************************
 *
 * Function: compareToKnown  (double version)
 *
 *
 * Compares an array of calculated values xCalc to an array of known
 * xKnown values.
 *
 * If an element of xKnown is nonzero, then this function checks to 
 * see the corresponding element in xCalc is within relative tolerance 
 * tol of the known value.
 *
 * If an element of xKnown zero, then this function checks to see the
 * corresponding element in xCalc is within absolute tolerance tol of
 * the origin.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void compareToKnown(
  const valarray<double>&  xCalc,
  const valarray<double>&  xKnown,
  const string&            name,
  double                   tol )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Compare the arrays.
  //------------------------------------------------------------

  assert( xCalc.size() == xKnown.size() );

  int i;
  bool ok = true;

  for ( i = 0; i < xCalc.size(); i++ )
  {
    if ( xKnown[i] != 0.0 )
    {
      // If the known value is not zero, see if the values
      // are equal up to relative tolerance.  
      ok &= fabs( ( xCalc[i] - xKnown[i] ) / xKnown[i] ) < tol;
    }
    else
    {
      // If the known value is zero, see if the calculated
      // value is within absolute tolerance of the origin.
      ok &= fabs( xCalc[i] ) < tol;
    }
  }

  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for " + name + " do not agree.",
    ok );

}

