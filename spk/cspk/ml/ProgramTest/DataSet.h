// BRAD_LINEAR_MODEL
#ifndef DATASET_H
#define DATASET_H
#include <vector>
#include <spk/SpkValarray.h>
#include "IndData.h"

template <class ValueType>
class DataSet
{
public:
DataSet();
~DataSet();

std::vector<IndData<ValueType>*> data;
const int popSize;
const SPK_VA::valarray<double> getAllMeasurements() const;
void compRES();
void compWRES( const SPK_VA::valarray<double>& R );

protected:
DataSet( const DataSet& );
DataSet& operator=( const DataSet& );

private:
SPK_VA::valarray<double> measurements; // a long vector containg all measurements
SPK_VA::valarray<int> N; // a vector containing the # of measurements for each individual.
};
template <class ValueType>
DataSet<ValueType>::DataSet()
: popSize( 2 ),
  data( 2 ),
  N( 2 )
{
//------------------------------------
// Subject <1> 
// # of sampling points = 3
//------------------------------------
   N[0] = 3;
char* ID_0_c[] = { "1", "1", "1" };
std::vector<char*> ID_0( 3 );
copy( ID_0_c, ID_0_c+3, ID_0.begin() );
ValueType TIME_0_c[] = { 0, 1, 2 };
std::vector<ValueType> TIME_0( 3 );
copy( TIME_0_c, TIME_0_c+3, TIME_0.begin() );
ValueType DV_0_c[] = { 0, 1, 2 };
std::vector<ValueType> DV_0( 3 );
copy( DV_0_c, DV_0_c+3, DV_0.begin() );
ValueType MDV_0_c[] = { 0, 0, 0 };
std::vector<ValueType> MDV_0( 3 );
copy( MDV_0_c, MDV_0_c+3, MDV_0.begin() );
data[0] = new IndData<ValueType>( 3, ID_0, TIME_0, DV_0, MDV_0 );

//------------------------------------
// Subject <2> 
// # of sampling points = 3
//------------------------------------
   N[1] = 3;
char* ID_1_c[] = { "2", "2", "2" };
std::vector<char*> ID_1( 3 );
copy( ID_1_c, ID_1_c+3, ID_1.begin() );
ValueType TIME_1_c[] = { 0, 1, 2 };
std::vector<ValueType> TIME_1( 3 );
copy( TIME_1_c, TIME_1_c+3, TIME_1.begin() );
ValueType DV_1_c[] = { 0, 1, 2 };
std::vector<ValueType> DV_1( 3 );
copy( DV_1_c, DV_1_c+3, DV_1.begin() );
ValueType MDV_1_c[] = { 0, 0, 0 };
std::vector<ValueType> MDV_1( 3 );
copy( MDV_1_c, MDV_1_c+3, MDV_1.begin() );
data[1] = new IndData<ValueType>( 3, ID_1, TIME_1, DV_1, MDV_1 );

   int nY = N.sum();
   measurements.resize( nY ); 
   for( int i=0, m=0; i<popSize; i++ )
   {
      int nYi = data[i]->getMeasurements().size();
      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();
      m+=nYi;
   }
}
template <class ValueType>
DataSet<ValueType>::~DataSet()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      delete data[i];
   }
}
template <class ValueType>
DataSet<ValueType>::DataSet( const DataSet<ValueType>& ){}
template <class ValueType>
DataSet<ValueType>& DataSet<ValueType>::operator=( const DataSet<ValueType>& ){}
template <class ValueType>
const SPK_VA::valarray<double> DataSet<ValueType>::getAllMeasurements() const
{
   return measurements;
}

template <class ValueType>
void DataSet<ValueType>::compRES()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      data[0]->RES[i] = y[i] - data[0]->PRED[i];
   }
}
template <class ValueType>
void DataSet<ValueType>::compWRES( const SPK_VA::valarray<double>& R )
{
   using SPK_VA::valarray;
   using std::vector;
   const int n = data.size();
   assert( R.size() == n * n );
   compRES();
   valarray<double> r( n );
   for( int i=0; i<n; i++ )
      r[i] = CppAD::Value( data[0]->RES[i] );
   valarray<double> C( 0.0, n * n );
   C = cholesky( Ri, n );
   valarray<double> w = multiply( C, n, r, 1 );
   vector< CppAD::AD<double> > Cr(n);
   for( int i=0; i<n; i++ )
      data[0]->WRES[i] = w[i];
   return;
}
#endif
