#include <iostream>
#include <spk/SpkValarray.h>
#include <spk/SpkModel.h>
#include <spk/SpkException.h>
#include "FitPopulationTestModel.h"

using SPK_VA::valarray;
using namespace std;
/*************************************************************************
 *
 * Class: FitPopulationTestModel
 *
 *************************************************************************/
FitPopulationTestModel::FitPopulationTestModel()
{
}
FitPopulationTestModel::FitPopulationTestModel
( const FitPopulationTestModel& right )
{
}
FitPopulationTestModel& FitPopulationTestModel::operator=
( const FitPopulationTestModel& right )
{
} 
FitPopulationTestModel::FitPopulationTestModel
( int nAlpIn, int nBIn, valarray<int>& NIn )
{
  alp.resize( nAlpIn );
  b  .resize( nBIn   );
  N  .resize( NIn.size() );
  N = NIn;
}   
FitPopulationTestModel::~FitPopulationTestModel()
{
}

void FitPopulationTestModel::doSelectIndividual
( int whoIn )
{
  who = whoIn;
  assert( N[who] > 0 );
}
void FitPopulationTestModel::doSetPopPar
( const valarray<double>& alpIn )
{
  alp = alpIn;
}
void FitPopulationTestModel::doSetIndPar
( const valarray<double>& bIn )
{
  b = bIn;
}
void FitPopulationTestModel::doIndParVariance
( valarray<double>& DOut ) const
{
  DOut.resize( b.size()*b.size() );
  //
  // D = [ alp[1] ]
  //
  DOut[0] = alp[1];
}
bool FitPopulationTestModel::doIndParVariance_popPar
( valarray<double>& D_alpOut ) const
{
  D_alpOut.resize( b.size()*b.size()*alp.size(), 0.0 );
  //
  // D_alp = [ 0  1 ]
  //
  D_alpOut[0] = 0.0;
  D_alpOut[1] = 1.0;
  return true;
}
void FitPopulationTestModel::doIndParVarianceInv
( valarray<double>& DInvOut ) const
{
  DInvOut.resize( b.size()*b.size(), 0.0 );
  //
  // DInv = [ 1.0 / alp[1] ]
  //
  assert(alp[1] != 0.0);
  DInvOut[0] = ( 1.0 / alp[1] );
}
bool FitPopulationTestModel::doIndParVarianceInv_popPar
( valarray<double>& DInv_alpOut ) const
{
  DInv_alpOut.resize( b.size()*b.size()*alp.size(), 0.0 );
  //
  // DInv_alp = [ 0    -alp[1]^(-2) ]
  //
  DInv_alpOut[0] = 0.0;
  DInv_alpOut[1] = -1.0 / (alp[1]*alp[1]);
  return true;
}
void FitPopulationTestModel::doDataMean
( valarray<double>& fiOut ) const
{
  fiOut.resize( N[who], 0.0 );
  //
  // fi = [ alp[0]+b[0] ]
  //
  fiOut[0] = ( alp[0] + b[0] );
}
bool FitPopulationTestModel::doDataMean_popPar
( valarray<double>& fi_alpOut ) const
{
  fi_alpOut.resize( N[who] * alp.size(), 0.0 );
  //
  // f_alp = [ 1   0 ]
  //
  fi_alpOut[0] = 1.0;
  fi_alpOut[1] = 0.0;
  return true;
}
bool FitPopulationTestModel::doDataMean_indPar
( valarray<double>& fi_bOut ) const
{
  fi_bOut.resize( N[who] * b.size(), 0.0 );
  //
  // f_b = [ 1 ]
  //
  fi_bOut[0] = 1.0;
  return true;
}
void FitPopulationTestModel::doDataVariance
( valarray<double>& RiOut ) const
{
  RiOut.resize( N[who] * N[who], 0.0 );
  //
  // R = [ 1 ]
  //
  RiOut[0] = 1.0;
}
bool FitPopulationTestModel::doDataVariance_popPar
( valarray<double>& Ri_alpOut ) const
{
  Ri_alpOut.resize( N[who] * N[who] * alp.size(), 0.0 );
  //
  // R_alp = [ 0   0 ]
  //
  Ri_alpOut[0] = 0.0;
  Ri_alpOut[1] = 0.0;
  return false;
}
bool FitPopulationTestModel::doDataVariance_indPar
( valarray<double>& Ri_bOut ) const
{
  Ri_bOut.resize( N[who] * N[who] * b.size(), 0.0 );
  //
  // R_b = [ 0 ]
  //
  Ri_bOut[0] = 0.0;
  return false;
}
void FitPopulationTestModel::doDataVarianceInv
( valarray<double>& RiInvOut ) const
{
  RiInvOut.resize( N[who] * N[who], 0.0 );
  //
  // Rinv = [ 1 ]
  //
  RiInvOut[0] = 1.0;
}
bool FitPopulationTestModel::doDataVarianceInv_popPar
( valarray<double>& RiInv_alpOut ) const
{
  RiInv_alpOut.resize( N[who] * N[who] * alp.size(), 0.0 );
  //
  // Rinv_alp = [ 0  0 ]
  //
  RiInv_alpOut[0] = 0.0;
  RiInv_alpOut[1] = 0.0;
  return false;
}
bool FitPopulationTestModel::doDataVarianceInv_indPar
( valarray<double>& RiInv_bOut ) const
{
  RiInv_bOut.resize( N[who] * N[who] * b.size(), 0.0 );
  //
  // Rinv_b = [ 0 ]
  //
  RiInv_bOut[0] = 0.0;
  return false;
}   
