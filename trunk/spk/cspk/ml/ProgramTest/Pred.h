// Linear Model: Estimation
#ifndef PRED_H
#define PRED_H

#include <vector>
#include <string>
#include <spkpred/PredBase.h>
#include <CppAD/CppAD.h>
#include "DataSet.h"

template <class spk_ValueType>
class Pred : public PredBase<spk_ValueType>
{
public:
   Pred( const DataSet<spk_ValueType>* dataIn );
   ~Pred();
   int getNObservs( int ) const;
   bool eval( int spk_thetaOffset, int spk_thetaLen,
              int spk_etaOffset,   int spk_etaLen,
              int spk_epsOffset,   int spk_epsLen,
              int spk_fOffset,     int spk_fLen,
              int spk_yOffset,     int spk_yLen,
              int spk_i,
              int spk_j,
              const std::vector<spk_ValueType>& spk_indepVar,
              std::vector<spk_ValueType>& spk_depVar );

protected:
   Pred();
   Pred( const Pred& );
   Pred & operator=( const Pred& );
private:
   const int nIndividuals;
   const DataSet<spk_ValueType> *perm;
   DataSet<spk_ValueType> temp;
   mutable bool isIterationCompleted;

mutable std::string ID;
mutable spk_ValueType TIME;
mutable spk_ValueType DV;
mutable spk_ValueType MDV;
mutable spk_ValueType F;
mutable spk_ValueType PRED;
mutable spk_ValueType RES;
mutable spk_ValueType WRES;
mutable spk_ValueType Y;
};

template <class spk_ValueType>
Pred<spk_ValueType>::Pred( const DataSet<spk_ValueType>* dataIn )
: perm( dataIn ),
  nIndividuals( 10 ),
  isIterationCompleted( true )
{
}

template <class spk_ValueType>
Pred<spk_ValueType>::~Pred()
{
}

template <class spk_ValueType>
int Pred<spk_ValueType>::getNObservs( int spk_i ) const
{
  return perm->data[spk_i]->ID.size();
}

template <class spk_ValueType>
bool Pred<spk_ValueType>::eval( int spk_thetaOffset, int spk_thetaLen,
                        int spk_etaOffset,   int spk_etaLen,
                        int spk_epsOffset,   int spk_epsLen,
                        int spk_fOffset,     int spk_fLen,
                        int spk_yOffset,     int spk_yLen,
                        int spk_i,
                        int spk_j,
                        const std::vector<spk_ValueType>& spk_indepVar,
                        std::vector<spk_ValueType>& spk_depVar )
{
  assert( spk_thetaLen == 1 );
  assert( spk_etaLen   == 1 );
  assert( spk_epsLen   == 1 );

ID = perm->data[spk_i]->ID[spk_j];
TIME = perm->data[spk_i]->TIME[spk_j];
DV = perm->data[spk_i]->DV[spk_j];
MDV = perm->data[spk_i]->MDV[spk_j];
typename std::vector<spk_ValueType>::const_iterator THETA1 = spk_indepVar.begin() + spk_thetaOffset + 0;
typename std::vector<spk_ValueType>::const_iterator ETA1 = spk_indepVar.begin() + spk_etaOffset + 0;
typename std::vector<spk_ValueType>::const_iterator EPS1 = spk_indepVar.begin() + spk_epsOffset + 0;
typename std::vector<spk_ValueType>::const_iterator THETA = spk_indepVar.begin() + spk_thetaOffset;
typename std::vector<spk_ValueType>::const_iterator ETA = spk_indepVar.begin() + spk_etaOffset;
typename std::vector<spk_ValueType>::const_iterator EPS = spk_indepVar.begin() + spk_epsOffset;
spk_ValueType F = 0.0;
spk_ValueType Y = 0.0;
//=========================================
// Begin User Code                         
//-----------------------------------------

F = ( THETA[ ( 1 ) - 1 ] + ETA[ ( 1 ) - 1 ] ) * TIME;
Y = F + EPS[ ( 1 ) - 1 ];
//-----------------------------------------
// End User Code                           
//=========================================
PRED = F;
copy( EPS, EPS+spk_epsLen, temp.data[ spk_i ]->EPS[ spk_j ].begin() ); 
copy( ETA, ETA+spk_etaLen, temp.data[ spk_i ]->ETA[ spk_j ].begin() ); 
temp.data[ spk_i ]->F[ spk_j ] = F;
temp.data[ spk_i ]->PRED[ spk_j ] = PRED;
copy( THETA, THETA+spk_thetaLen, temp.data[ spk_i ]->THETA[ spk_j ].begin() ); 
temp.data[ spk_i ]->Y[ spk_j ] = Y;

if( spk_i == 10-1 && spk_j == perm->data[spk_i]->ID.size()-1 )
{
  // This means, SPK advanced in iteration.
  // Move temporary storage to permanent storage.
  isIterationCompleted = true;
  for( int i=0; i < nIndividuals; i++ )
  {
    perm->data[ i ]->EPS = temp.data[ i ]->EPS;
    perm->data[ i ]->ETA = temp.data[ i ]->ETA;
    perm->data[ i ]->F = temp.data[ i ]->F;
    perm->data[ i ]->PRED = temp.data[ i ]->PRED;
    perm->data[ i ]->THETA = temp.data[ i ]->THETA;
    perm->data[ i ]->Y = temp.data[ i ]->Y;
  }
}
else
{
  isIterationCompleted = false;
}

spk_depVar[ spk_fOffset+spk_j ] = F;
spk_depVar[ spk_yOffset+spk_j ] = Y;
if( perm->data[ spk_i ]->MDV[ spk_j ] == 0 )
   return true;
else return false;
}
template <class spk_ValueType>
Pred<spk_ValueType>::Pred()
{
}
template <class spk_ValueType>
Pred<spk_ValueType>::Pred( const Pred<spk_ValueType>& )
{
}
template <class spk_ValueType>
Pred<spk_ValueType> & Pred<spk_ValueType>::operator=( const Pred<spk_ValueType>& )
{
}
#endif
