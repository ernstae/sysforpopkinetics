/* BRAD_LINEAR_MODEL*/
#ifndef INDDATA_H
#define INDDATA_H
#include <vector>
#include <map>
#include <spk/SpkValarray.h>
#include <CppAD/CppAD.h>

template <class ValueType>
class IndData{
public:
IndData( int nIn,
	const std::vector<char*> & IDIn,
	const std::vector<ValueType> & TIMEIn,
	const std::vector<ValueType> & DVIn,
	const std::vector<ValueType> & MDVIn);

const std::vector<ValueType> DV;
std::vector< std::vector<ValueType> > EPS;
std::vector< std::vector<ValueType> > ETA;
std::vector<ValueType> F;
const std::vector<char *> ID;
const std::vector<ValueType> MDV;
std::vector<ValueType> PRED;
std::vector<ValueType> RES;
std::vector< std::vector<ValueType> > THETA;
const std::vector<ValueType> TIME;
std::vector<ValueType> WRES;
std::vector<ValueType> Y;

~IndData();
const SPK_VA::valarray<double> getMeasurements() const;

protected:
IndData();
IndData( const IndData& );
IndData& operator=( const IndData& );

int nY; // #of measurements.
SPK_VA::valarray<double> measurements;
private:
const int n; // #of measurements.
void assignToDbl( double&, const CppAD::AD<double>& ) const;
void assignToDbl( double&, double ) const;
};
template <class ValueType>
IndData<ValueType>::IndData( int nIn,
const std::vector<char*> & IDIn,
const std::vector<ValueType> & TIMEIn,
const std::vector<ValueType> & DVIn,
const std::vector<ValueType> & MDVIn)
: n( nIn ), 
  nY( 0 ) 
,
ID( IDIn ),
TIME( TIMEIn ),
DV( DVIn ),
MDV( MDVIn ),
EPS( nIn ),
ETA( nIn ),
F( nIn ),
PRED( nIn ),
RES( nIn ),
THETA( nIn ),
WRES( nIn ),
Y( nIn )
{
   for( int i=0; i<n; i++ )
   {
      if( MDV[i] != 1 )
          ++nY;
   }

   measurements.resize( nY ); 
   for( int i=0, j=0; i<n; i++ )
   {
      THETA[i].resize( 1 );
      ETA[i].resize( 1 );
      EPS[i].resize( 1 );
        if( MDV[i] != 1 )
        {
           assignToDbl( measurements[j], DV[i] );
           j++;
        }
   }
}

template <class ValueType>
IndData<ValueType>::~IndData(){}
template <class ValueType>
IndData<ValueType>::IndData(){}
template <class ValueType>
IndData<ValueType>::IndData( const IndData<ValueType>& ){}
template <class ValueType>
IndData<ValueType>& IndData<ValueType>::operator=( const IndData<ValueType>& ){}
template <class ValueType>
const SPK_VA::valarray<double> IndData<ValueType>::getMeasurements() const
{
   return measurements;
}
template <class ValueType>
void IndData<ValueType>::assignToDbl( double & d, const CppAD::AD<double>& ad ) const
{
   d = CppAD::Value( ad );
   return;
}
template <class ValueType>
void IndData<ValueType>::assignToDbl( double & left, double right  ) const
{
   left = right;
   return;
}
#endif
