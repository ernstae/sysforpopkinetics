// Population model of line with slope and intercept random effects.
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
   int getNRecords( int ) const;
   int getMeasurementIndex( int ) const;
   int getMeasurementIndex( int, int ) const;
   int getRecordIndex( int ) const;
   int getRecordIndex( int, int ) const;
   bool eval( int  spk_thetaOffset, int spk_thetaLen,
              int  spk_etaOffset,   int spk_etaLen,
              int  spk_epsOffset,   int spk_epsLen,
              int  spk_fOffset,     int spk_fLen,
              int  spk_yOffset,     int spk_yLen,
              int  spk_i,
              int  spk_j,
              int &spk_m,
              const std::vector<spk_ValueType>& spk_indepVar,
              std::vector<spk_ValueType>& spk_depVar );

   bool eval( int  spk_thetaOffset, int spk_thetaLen,
              int  spk_etaOffset,   int spk_etaLen,
              int  spk_epsOffset,   int spk_epsLen,
              int  spk_fOffset,     int spk_fLen,
              int  spk_yOffset,     int spk_yLen,
              int  spk_i,
              int  spk_j,
              const std::vector<spk_ValueType>& spk_indepVar,
              std::vector<spk_ValueType>& spk_depVar );

protected:
   Pred();
   Pred( const Pred& );
   Pred & operator=( const Pred& );
private:
   const int                     spk_nIndividuals;
   const DataSet<spk_ValueType> *spk_perm;
   DataSet<spk_ValueType>        spk_temp;
   bool                          spk_isIterCompleted;

std::string ID;
spk_ValueType TIME;
spk_ValueType DV;
spk_ValueType AMT;
spk_ValueType MDV;
spk_ValueType EVID;
spk_ValueType B;
spk_ValueType CPRED;
spk_ValueType CRES;
spk_ValueType CWRES;
typename std::vector<spk_ValueType>::const_iterator EPS;
typename std::vector<spk_ValueType>::const_iterator ETA;
spk_ValueType F;
spk_ValueType IPRED;
spk_ValueType IRES;
spk_ValueType IWRES;
spk_ValueType ORGDV;
spk_ValueType PPRED;
spk_ValueType PRED;
spk_ValueType PRES;
spk_ValueType PWRES;
spk_ValueType RES;
spk_ValueType S;
typename std::vector<spk_ValueType>::const_iterator THETA;
spk_ValueType WRES;
spk_ValueType Y;
};

template <class spk_ValueType>
Pred<spk_ValueType>::Pred( const DataSet<spk_ValueType>* dataIn )
: spk_perm( dataIn ),
  spk_nIndividuals( 10 ),
  spk_isIterCompleted( true )
{
}

template <class spk_ValueType>
Pred<spk_ValueType>::~Pred()
{
}

template <class spk_ValueType>
int Pred<spk_ValueType>::getNObservs( int spk_i ) const
{
  return spk_perm->getNObservs( spk_i );
}

template <class spk_ValueType>
int Pred<spk_ValueType>::getNRecords( int spk_i ) const
{
  return spk_perm->getNRecords( spk_i );
}

template <class spk_ValueType>
int Pred<spk_ValueType>::getMeasurementIndex( int who, int recordIndex ) const
{
   return spk_perm->getMeasurementIndex( who, recordIndex );
}
template <class spk_ValueType>
int Pred<spk_ValueType>::getMeasurementIndex( int recordIndex ) const
{
   return spk_perm->getMeasurementIndex( recordIndex );
}
template <class spk_ValueType>
int Pred<spk_ValueType>::getRecordIndex( int who, int measurementIndex ) const
{
   return spk_perm->getRecordIndex( who, measurementIndex );
}
template <class spk_ValueType>
int Pred<spk_ValueType>::getRecordIndex( int measurementIndex ) const
{
   return spk_perm->getRecordIndex( measurementIndex );
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
   int spk_m;
   return eval( spk_thetaOffset, spk_thetaLen,
                spk_etaOffset,   spk_etaLen,
                spk_epsOffset,   spk_epsLen,
                spk_fOffset,     spk_fLen,
                spk_yOffset,     spk_yLen,
                spk_i,
                spk_j,
                spk_m,
                spk_indepVar,
                spk_depVar );
}

template <class spk_ValueType>
bool Pred<spk_ValueType>::eval( int  spk_thetaOffset, int spk_thetaLen,
                                int  spk_etaOffset,   int spk_etaLen,
                                int  spk_epsOffset,   int spk_epsLen,
                                int  spk_fOffset,     int spk_fLen,
                                int  spk_yOffset,     int spk_yLen,
                                int  spk_i,
                                int  spk_j,
                                int &spk_m,
                                const std::vector<spk_ValueType>& spk_indepVar,
                                std::vector<spk_ValueType>& spk_depVar )
{
  assert( spk_thetaLen == 2 );
  assert( spk_etaLen   == 2 );
  assert( spk_epsLen   == 1 );

   ID = spk_perm->data[spk_i]->ID[spk_j];
   TIME = spk_perm->data[spk_i]->TIME[spk_j];
   DV = spk_perm->data[spk_i]->DV[spk_j];
   AMT = spk_perm->data[spk_i]->AMT[spk_j];
   MDV = spk_perm->data[spk_i]->MDV[spk_j];
   EVID = spk_perm->data[spk_i]->EVID[spk_j];
   THETA = spk_indepVar.begin() + spk_thetaOffset;
   typename std::vector<spk_ValueType>::const_iterator THETA1 = spk_indepVar.begin() + spk_thetaOffset + 0;
   typename std::vector<spk_ValueType>::const_iterator THETA2 = spk_indepVar.begin() + spk_thetaOffset + 1;
   ETA = spk_indepVar.begin() + spk_etaOffset;
   typename std::vector<spk_ValueType>::const_iterator ETA1 = spk_indepVar.begin() + spk_etaOffset + 0;
   typename std::vector<spk_ValueType>::const_iterator ETA2 = spk_indepVar.begin() + spk_etaOffset + 1;
   EPS = spk_indepVar.begin() + spk_epsOffset;
   typename std::vector<spk_ValueType>::const_iterator EPS1 = spk_indepVar.begin() + spk_epsOffset + 0;
//=========================================
// Begin User Code                         
//-----------------------------------------

S = THETA[ ( 1 ) - 1 ] + ETA[ ( 1 ) - 1 ];
B = THETA[ ( 2 ) - 1 ] + ETA[ ( 2 ) - 1 ];
F = S * TIME + B;
Y = F + EPS[ ( 1 ) - 1 ];
//-----------------------------------------
// End User Code                           
//=========================================
PRED = F;
RES = (MDV==0? DV-PRED : 0 );
   spk_temp.data[ spk_i ]->B[ spk_j ] = B;
   copy( EPS, EPS+spk_epsLen,    spk_temp.data[ spk_i ]->EPS[ spk_j ].begin() ); 
   copy( ETA, ETA+spk_etaLen,    spk_temp.data[ spk_i ]->ETA[ spk_j ].begin() ); 
   spk_temp.data[ spk_i ]->F[ spk_j ] = F;
   spk_temp.data[ spk_i ]->PRED[ spk_j ] = PRED;
   spk_temp.data[ spk_i ]->RES[ spk_j ] = RES;
   spk_temp.data[ spk_i ]->S[ spk_j ] = S;
   copy( THETA, THETA+spk_thetaLen,    spk_temp.data[ spk_i ]->THETA[ spk_j ].begin() ); 
   spk_temp.data[ spk_i ]->Y[ spk_j ] = Y;

   if( spk_i == 10-1 && spk_j == spk_perm->data[spk_i]->ID.size()-1 )
   {
     // This means, SPK advanced in iteration.
     // Move temporary storage to spk_permanent storage.
     spk_isIterCompleted = true;
     for( int i=0; i < spk_nIndividuals; i++ )
     {
       spk_perm->data[ i ]->B = spk_temp.data[ i ]->B;
       spk_perm->data[ i ]->EPS = spk_temp.data[ i ]->EPS;
       spk_perm->data[ i ]->ETA = spk_temp.data[ i ]->ETA;
       spk_perm->data[ i ]->F = spk_temp.data[ i ]->F;
       spk_perm->data[ i ]->PRED = spk_temp.data[ i ]->PRED;
       spk_perm->data[ i ]->RES = spk_temp.data[ i ]->RES;
       spk_perm->data[ i ]->S = spk_temp.data[ i ]->S;
       spk_perm->data[ i ]->THETA = spk_temp.data[ i ]->THETA;
       spk_perm->data[ i ]->Y = spk_temp.data[ i ]->Y;
     }
   }
   else
   {
     spk_isIterCompleted = false;
   }

   if( spk_perm->data[ spk_i ]->MDV[ spk_j ] == 0 )
   {
      spk_m = getMeasurementIndex( spk_i, spk_j );
      spk_depVar[ spk_fOffset+spk_m ] = F;
      spk_depVar[ spk_yOffset+spk_m ] = Y;
      return true;
   }
   else
      return false;
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
