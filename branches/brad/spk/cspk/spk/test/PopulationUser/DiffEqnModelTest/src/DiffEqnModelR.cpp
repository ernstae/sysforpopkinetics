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
// This source code is stored in the file DiffEqnModelR.cpp
#pragma warning( disable : 4786 )

# include "../../../../spk/SpkValarray.h"
# include "../../../../spk/allZero.h"
# include "../../../../spk/inverse.h"

# include "DiffEqnModel.h"

using namespace std;

# include <cmath>
using SPK_VA::valarray;
using SPK_VA::slice;

void DiffEqnModel::doDataVariance( valarray<double>& RiOut ) const 
{    
  RiOut.resize( _Ni * _Ni );
  if( isCachedRiValid )
    {
      RiOut = cachedRi;
      return;
    }

  const double sigsq = _exp? exp(_alp[3]):_alp[3];
  cachedRi.resize(_Ni*_Ni, 0.0); 

  if( _er == ER_ADDITIVE )
    {
      cachedRi[ slice(0, _Ni, _Ni+1) ] = sigsq;
    }
  
  if( _er == ER_MODEL_BASED )
    {
      if(!isCachedFiValid)
	{
	  doDataMean(cachedFi);
	  isCachedFiValid = true;
	}
      cachedRi[ slice(0, _Ni, _Ni+1) ] = cachedFi * cachedFi * sigsq;
    }
  
  if( _er == ER_DATA_BASED )
    {
      cachedRi[ slice(0, _Ni, _Ni+1) ] = _y[ slice(_ti, _Ni, 1) ] *
	_y[ slice(_ti, _Ni, 1) ] * sigsq;
    }
  
  isCachedRiValid = true;
  RiOut = cachedRi;
}
bool DiffEqnModel::doDataVariance_popPar( valarray<double>& Ri_alpOut ) const 
{    
  Ri_alpOut.resize( _Ni * _Ni * _alp.size() );
  if(isCachedRiaValid)
    {
      Ri_alpOut = cachedRia;
      return !allZero(Ri_alpOut);
    }
  
  const double sigsq = _exp? exp(_alp[3]):_alp[3];
  cachedRia.resize(_Ni*_Ni*_P, 0.0);
  
  if( _er == ER_ADDITIVE )
    {
      if( _exp )
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = sigsq;
      else
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = 1.0;
    }

  if( _er == ER_MODEL_BASED )
    {
      if(!isCachedFiValid)
	{
	  doDataMean(cachedFi);
	  isCachedFiValid = true;
	}
      if(!isCachedFiaValid)
	{
	  doDataMean_popPar(cachedFia);
	  isCachedFiaValid = true;
	}
      
      valarray<double> Fia1 = cachedFia[ slice(0, _Ni, 1) ];
      cachedRia[ slice(0, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fia1 * sigsq;
      
      valarray<double> Fia2 = cachedFia[ slice(_Ni, _Ni, 1) ];	                                        
      cachedRia[ slice(_Ni*_Ni, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fia2 * sigsq;
      
      valarray<double> Fia3 = cachedFia[ slice(2*_Ni, _Ni, 1) ];	                                        
      cachedRia[ slice(2*_Ni*_Ni, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fia3 * sigsq;
      
      if( _exp )
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = cachedFi * cachedFi * sigsq;
      else
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = cachedFi * cachedFi;
    }
  
  if( _er == ER_DATA_BASED )
    {
      if( _exp )
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = _y[ slice(_ti, _Ni, 1) ]  
	  * _y[ slice(_ti, _Ni, 1) ] 
	  * sigsq;
      else
	cachedRia[ slice(3*_Ni*_Ni, _Ni, _Ni+1) ] = _y[ slice(_ti, _Ni, 1) ]  
	  * _y[ slice(_ti, _Ni, 1) ];
    }
  
  isCachedRiaValid = true;
  Ri_alpOut = cachedRia;
  return !allZero(Ri_alpOut);
}
bool DiffEqnModel::doDataVariance_indPar( valarray<double>& Ri_bOut ) const 
{    
  Ri_bOut.resize( _Ni * _Ni * _b.size() );
  if(isCachedRibValid)
    {
      Ri_bOut = cachedRib;
      return !allZero(Ri_bOut);
    }
  
  const double sigsq = _exp? exp(_alp[3]):_alp[3];
  cachedRib.resize(_Ni*_Ni*_Q, 0.0); 
  
  if( _er == ER_MODEL_BASED )
    {
      if(!isCachedFiValid)
	{
	  doDataMean(cachedFi);
	  isCachedFiValid = true;
	}
      if(!isCachedFibValid)
	{
	  doDataMean_indPar(cachedFib);
	  isCachedFibValid = true;
	}
      
      valarray<double> Fib1 = cachedFib[ slice(0, _Ni, 1) ];
      cachedRib[ slice(0, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fib1 * sigsq;
      
      valarray<double> Fib2 = cachedFib[ slice(_Ni, _Ni, 1) ];
      cachedRib[ slice(_Ni*_Ni, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fib2 * sigsq;
      
      valarray<double> Fib3 = cachedFib[ slice(_Ni+_Ni, _Ni, 1) ];
      cachedRib[ slice(2*_Ni*_Ni, _Ni, _Ni+1) ] = 2.0 * cachedFi * Fib3 * sigsq;
    }
  
  isCachedRibValid = true;
  Ri_bOut = cachedRib;
  return false;
}
void DiffEqnModel::doDataVarianceInv( valarray<double>& RiInvOut ) const
{
  RiInvOut.resize( _Ni * _Ni );
  if( isCachedRiInvValid )
    {
      RiInvOut = cachedRiInv;
      return;
    }
  valarray<double> RiOut( _Ni * _Ni );
  doDataVariance(RiOut);

  cachedRiInv.resize( _Ni * _Ni );
  cachedRiInv = inverse(RiOut, _Ni);
  isCachedRiInvValid = true;
  
  RiInvOut = cachedRiInv;
  return;
}

