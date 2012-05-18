// THIS FILE IS GENERATED BY THE ASPK COMPILER <NonmemTranslator.cpp>
#ifndef DATASET_H
#define DATASET_H
#include <vector>
#include <spk/SpkValarray.h>
#include "IndData.h"

template <class T>
class DataSet
{
public:
DataSet();
~DataSet();

std::vector<IndData<T>*> data;
const int popSize;

protected:
DataSet( const DataSet& );
DataSet& operator=( const DataSet& );

private:
};
template <class T>
DataSet<T>::DataSet()
: data( 2 ),
  popSize( 2 )
{
//------------------------------------
// Subject <1> 
// # of sampling points = 3
//------------------------------------
char* ID_0_c[] = { "1", "1", "1" };
std::vector<char*> ID_0( 3 );
copy( ID_0_c, ID_0_c+3, ID_0.begin() );
T TIME_0_c[] = { 0, 1, 2 };
std::vector<T> TIME_0( 3 );
copy( TIME_0_c, TIME_0_c+3, TIME_0.begin() );
T DV_0_c[] = { 0, 1, 2 };
std::vector<T> DV_0( 3 );
copy( DV_0_c, DV_0_c+3, DV_0.begin() );
T MDV_0_c[] = { 0, 0, 0 };
std::vector<T> MDV_0( 3 );
copy( MDV_0_c, MDV_0_c+3, MDV_0.begin() );
data[0] = new IndData<T>( 3, ID_0, TIME_0, DV_0, MDV_0 );

//------------------------------------
// Subject <2> 
// # of sampling points = 3
//------------------------------------
char* ID_1_c[] = { "2", "2", "2" };
std::vector<char*> ID_1( 3 );
copy( ID_1_c, ID_1_c+3, ID_1.begin() );
T TIME_1_c[] = { 0, 1, 2 };
std::vector<T> TIME_1( 3 );
copy( TIME_1_c, TIME_1_c+3, TIME_1.begin() );
T DV_1_c[] = { 0, 1, 2 };
std::vector<T> DV_1( 3 );
copy( DV_1_c, DV_1_c+3, DV_1.begin() );
T MDV_1_c[] = { 0, 0, 0 };
std::vector<T> MDV_1( 3 );
copy( MDV_1_c, MDV_1_c+3, MDV_1.begin() );
data[1] = new IndData<T>( 3, ID_1, TIME_1, DV_1, MDV_1 );

}
template <class T>
DataSet<T>::~DataSet()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      delete data[i];
   }
}
template <class T>
DataSet<T>::DataSet( const DataSet<T>& ){}
template <class T>
DataSet<T>& DataSet<T>::operator=( const DataSet<T>& ){}
#endif