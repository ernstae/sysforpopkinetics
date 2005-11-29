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
// This source code is stored in the file DiffEqnModelS.cpp
# include "../../../../spk/SpkValarray.h"

# include "DiffEqnModel.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
    bool isNotEqual(const valarray<double>& x, const valarray<double>& y );
} // [End: unnamed namespace]

void DiffEqnModel::doSelectIndividual(int i /* 0 base */)
{    
    assert( 0 <= i && i < _N.size());
    if ( i != _who )
    { 
        // If the individual has changed, then any cached 
        // values are no longer valid.
        invalidateCachedValues();

        _who = i;
        _ti = 0;
        for(int j = 0; j <= i; j++)
        {    
            _Ni = _N[j];
            if( j < _who  )
                _ti += _Ni;
        }
    }
}
void DiffEqnModel::doSetPopPar( const valarray<double>& alp ) 
{
  if ( isNotEqual( alp, _alp ) )
    { 
        // If the fixed population parameter vector has changed,
        // then any cached values are no longer valid.
        invalidateCachedValues();
	_alp.resize( alp.size() );
        _alp = alp; 
    }
}
void DiffEqnModel::doSetIndPar( const valarray<double>& b ) 
{ 
    if ( isNotEqual( b, _b ) )
    { 
        // If the random population parameter vector has changed,
        // then any cached values are no longer valid.
        invalidateCachedValues();
	_b.resize( b.size() );
        _b = b; 
    }
}

/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: isNotEqual
 *
 *
 * Returns true if any of the elements in the vector x is 
 * not equal to the corresponding element in the vector y,
 * or if the vectors have different dimensions.
 * 
 *************************************************************************/

bool isNotEqual( const valarray<double>& x, const valarray<double>& y )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int n = x.size();

  // If the dimensions don't agree, then they're not equal.
  if ( y.size() != n )
  {
    return true;
  }


  //------------------------------------------------------------
  // Compare the elements in the two matrices.
  //------------------------------------------------------------

  // Find the first element that's different and then return true.
  for ( int i = 0; i < n; i++ )
  {
    if ( x[i] != y[i] )
    {
      return true;
    }
  }

  return false;
}


} // [End: unnamed namespace]
