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
// This source code is stored in the file DiffEqnModelD.cpp
#pragma warning( disable : 4786 )

# include "../../../../spk/SpkValarray.h"
# include "../../../../spk/AkronBtimesC.h"
# include "../../../../spk/allZero.h"
# include "../../../../spk/multiply.h"
# include "../../../../spk/transpose.h"
# include "../../../../spk/inverse.h"
# include "../../../../spk/transposeDerivative.h"

# include "DiffEqnModel.h"

using SPK_VA::valarray;
using SPK_VA::slice;

const valarray<double> DiffEqnModel::Dval( int Q, const valarray<double> &alp, const enum OMEGA_TYPE &omega )
{    
    int m = 4;
    valarray<double> C(0.0, Q*Q); 

    if(omega == BLOCK)
      {
	for( int j = 0, i = m; j < Q; j++, i += j )
	  {
            C[ slice(j, j + 1, Q) ] =  alp[ slice(i, j + 1, 1) ];
	  }
        C = multiply(C, Q, transpose(C, Q), Q);
      }
    
    if(omega == DIAGONAL)
      {
        for( int j = 0; j < Q; j++ )
	  {
	    C[ j + j * Q ] = alp[ m++ ];
	  }
      }
    return C;
}

void DiffEqnModel::doIndParVariance( valarray<double> & DOut ) const
{   
    DOut.resize( _b.size() * _b.size() );

    if(isCachedDValid)
    {
      DOut = cachedD;
      return;
    }
    cachedD.resize( _b.size() * _b.size() );
    cachedD = Dval(_Q, _alp, _omega);
    isCachedDValid = true;

    DOut = cachedD;
}
bool DiffEqnModel::doIndParVariance_popPar( valarray<double>& D_alpOut ) const
{
    D_alpOut.resize( _b.size() * _b.size() * _alp.size() );
    
    if ( isCachedDaValid )
    {
      D_alpOut = cachedDa;
      return !allZero(D_alpOut);
    }
        
    valarray<double> C(0.0, _Q * _Q);
    valarray<double> C_a(0.0, _Q * _Q * _P);
    cachedDa.resize( _b.size() * _b.size() * _alp.size() );
 
    if(_omega == BLOCK)
      {
        int Cindex[]  = {0, 3, 4, 6, 7, 8};
        valarray<double> I(0.0, _Q * _Q);
        int j, m;
        I[ slice(0, _Q, _Q+1) ] = 1.0;
        for( j=0, m=4; j<_Q; j++, m+=j )
	  {
            C[ slice(j, j+1, _Q) ] =  _alp[ slice(m, j+1, 1) ];
	  }
        for( j=0, m=4; j < _Q; j++ )
	  {    
            for(int k = 0; k <= j; k++, m++)
	      {   
                C_a[ Cindex[m-4] + m * _Q * _Q] = 1.;
	      }
	  }
        if ( !isCachedDValid )
	  {
	    cachedD.resize( _b.size() * _b.size() );
            cachedD = multiply(C, _Q, transpose(C, _Q), _Q);
	  }
        valarray<double> Ct_a = transposeDerivative(C_a, _Q, _Q, _P);;
        cachedDa = AkronItimesC(C, _Q, I, _Q, Ct_a, _P) +IkronBtimesC(I,_Q, C, _Q, C_a, _P);
      }
    
    if(_omega == DIAGONAL)
      {
	int m = 4;
	C_a[36] = 1.0;
	C_a[49] = 1.0;
	C_a[62] = 1.0;
	if ( !isCachedDValid )
	  {
	    for(int j = 0; j < _Q; j++)
	      C[j * (_Q + 1)] = _alp[m++];
	    cachedD = C;
	  }
	cachedDa = C_a;
      }
    
    isCachedDValid = true;
    isCachedDaValid = true;
    D_alpOut = cachedDa;
    return !allZero(D_alpOut);
}
void DiffEqnModel::doIndParVarianceInv( valarray<double>& DInvOut ) const
{
  DInvOut.resize( _b.size() * _b.size() );
  if( isCachedDInvValid )
    {
      DInvOut = cachedDInv;
      return;
    }
  
  valarray<double> DOut( _b.size() * _b.size() );
  doIndParVariance(DOut); 

  cachedDInv.resize( _b.size() * _b.size() );
  try{
    cachedDInv = inverse(DOut, _Q);
  }
  catch(...)
    {
      std::cerr << "Failed to invert a matrix in doIndParVarianceInv()" << std:: endl;
      assert(false);
      
    }
  isCachedDInvValid = true;
  DInvOut = cachedDInv;
}
