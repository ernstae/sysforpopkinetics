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
 * File: isEqual.cpp
 *
 *
 * This function returns true if two arrays are equal.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: isEqual
 *
 *
 * Returns true if the arrays x and y have the same number of elements
 * and if every element in x is equal to the corresponding element in y.
 * 
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/SpkValarray.h>

using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

bool isEqual(
  const valarray<double>&  x,
  const valarray<double>&  y )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int n = x.size();

  // If the dimensions don't agree, then they're not equal.
  if ( n != y.size() )
  {
    return false;
  }


  //------------------------------------------------------------
  // Compare the elements in the two arrays.
  //------------------------------------------------------------

  int i;

  // If any element is different, then return false.
  for ( int i = 0; i < n; i++ )
  {
    if ( x[i] != y[i] )
    {
      return false;
    }
  }

  // If all of the elements are the same, then return true.
  return true;
}

