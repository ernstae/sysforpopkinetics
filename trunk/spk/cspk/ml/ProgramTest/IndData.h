/* Linear Model: Estimation*/
#ifndef INDDATA_H
#define INDDATA_H

#include <vector>
#include <map>
#include <spk/SpkValarray.h>
#include <spk/cholesky.h>
#include <spk/multiply.h>
#include <CppAD/CppAD.h>

template <class spk_ValueType>
class IndData{
public:
IndData( int nIn,
	const std::vector<char*> & IDIn,
	const std::vector<spk_ValueType> & TIMEIn,
	const std::vector<spk_ValueType> & DVIn,
	const std::vector<spk_ValueType> & MDVIn);

const std::vector<spk_ValueType> DV;
std::vector< std::vector<spk_ValueType> > EPS;
std::vector< std::vector<spk_ValueType> > ETA;
std::vector<spk_ValueType> F;
std::vector<char *> ID;
std::vector<spk_ValueType> MDV;
std::vector<spk_ValueType> PRED;
std::vector<spk_ValueType> RES;
std::vector< std::vector<spk_ValueType> > THETA;
std::vector<spk_ValueType> TIME;
std::vector<spk_ValueType> WRES;
std::vector<spk_ValueType> Y;

~IndData();
const SPK_VA::valarray<double> getMeasurements() const;
void replaceMeasurements( const SPK_VA::valarray<double>& yyi );
void compResiduals();
void compWeightedResiduals( const SPK_VA::valarray<double>& Ri );

protected:
IndData();
IndData( const IndData& );
IndData& operator=( const IndData& );

int nY; // #of measurements (DVs where MDV=0).
SPK_VA::valarray<double> measurements;
private:
const int n; // the number of data records.
void assignToDbl( double&, const CppAD::AD<double>& ) const;
void assignToDbl( double&, double ) const;
};
template <class spk_ValueType>
IndData<spk_ValueType>::IndData( int nIn,
const std::vector<char*> & IDIn,
const std::vector<spk_ValueType> & TIMEIn,
const std::vector<spk_ValueType> & DVIn,
const std::vector<spk_ValueType> & MDVIn)
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

template <class spk_ValueType>
IndData<spk_ValueType>::~IndData(){}
template <class spk_ValueType>
IndData<spk_ValueType>::IndData(){}
template <class spk_ValueType>
IndData<spk_ValueType>::IndData( const IndData<spk_ValueType>& ){}
template <class spk_ValueType>
IndData<spk_ValueType>& IndData<spk_ValueType>::operator=( const IndData<spk_ValueType>& ){}
// Return SPK's y
template <class spk_ValueType>
const SPK_VA::valarray<double> IndData<spk_ValueType>::getMeasurements() const
{
   return measurements;
}

// Replace y with the given y'.
template <class spk_ValueType>
void IndData<spk_ValueType>::replaceMeasurements( const SPK_VA::valarray<double>& yyi )
{
   for( int i=0, k=0; i<n; i++ )
   {
      if( MDV[i] != 1 )
      {
         ORGDV[i] = DV[i];
         DV[i] = yyi[k];
         k++;
      }
   }
}
template <class spk_ValueType>
void IndData<spk_ValueType>::assignToDbl( double & d, const CppAD::AD<double>& ad ) const
{
   d = CppAD::Value( ad );
   return;
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assignToDbl( double & left, double right ) const
{
   left = right;
   return;
}

// Compute residuals.
template <class spk_ValueType>
void IndData<spk_ValueType>::compResiduals()
{
   for( int i=0; i<n; i++ )
   {
      RES[i] =DV[i] - PRED[i];
   }
}

// Compute weighted residual such that:
// r = DV-PRED
// WRES = C * r, where C is an matrix such that Ri * C * C^t.
//
// The type of template argument must have Value() operator 
// that returns a corresponding double-precision value.
// It is (unfortunately) essentially requiring that the argument is of CppAD. 
template <class spk_ValueType>
void IndData<spk_ValueType>::compWeightedResiduals( const SPK_VA::valarray<double>& Ri )
{
   using SPK_VA::valarray;
   using std::vector;
   assert( Ri.size() == n * n );
   compResiduals();
   valarray<double> r( n );
   for( int i=0; i<n; i++ )
   {
      r[i] = CppAD::Value( RES[i] );
   }
   valarray<double> C( 0.0, n * n );
   C = cholesky( Ri, n );
   valarray<double> w = multiply( C, n, r, 1 );
   vector< CppAD::AD<double> > Cr(n);
   for( int i=0; i<n; i++ )
      WRES[i] = w[i];
   return;
}

#endif
