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
int getPopSize() const;
const SPK_VA::valarray<int> getN() const;
void compAllResiduals();
void compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R );
std::ostream& xmlOut( std::ostream& o, const char* title ) const;

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
int DataSet<ValueType>::getPopSize() const
{
   return popSize;
}

template <class ValueType>
const SPK_VA::valarray<int> DataSet<ValueType>::getN() const
{
   return N;
}

template <class ValueType>
void DataSet<ValueType>::compAllResiduals()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      data[i]->compResiduals();
   }
}

template <class ValueType>
void DataSet<ValueType>::compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R )
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      data[i]->compWeightedResiduals( R[i] );
   }
}

template <class ValueType>
std::ostream& DataSet<ValueType>::xmlOut( std::ostream& o, const char * title ) const
{
o << "<" << title << " rows=\"" << N.sum() << "\" ";
o << "columns=\"12\">" << endl;
o << "<data_labels>" << endl;
o << "<DEFANGED_label name=\"ID\"/>" << endl;
///////////////////////////////////////////////////////////////////
//   DATA SET Specific
o << "<DEFANGED_label name=\"DV\"/>" << endl;
o << "<DEFANGED_label name=\"EPS(1)\"/>" << endl;
o << "<DEFANGED_label name=\"ETA(1)\"/>" << endl;
o << "<DEFANGED_label name=\"F\"/>" << endl;
o << "<DEFANGED_label name=\"MDV\"/>" << endl;
o << "<DEFANGED_label name=\"PRED\"/>" << endl;
o << "<DEFANGED_label name=\"RES\"/>" << endl;
o << "<DEFANGED_label name=\"THETA(1)\"/>" << endl;
o << "<DEFANGED_label name=\"TIME\"/>" << endl;
o << "<DEFANGED_label name=\"WRES\"/>" << endl;
o << "<DEFANGED_label name=\"Y\"/>" << endl;
//
///////////////////////////////////////////////////////////////////
o << "</data_labels>" << endl;

for( int i=0, position=1; i<popSize; i++ )
{
   for( int j=0; j<N[i]; j++, position++ )
   {
      o << "<row position=\"" << position << "\">" << endl;
      ///////////////////////////////////////////////////////////////////
      //   DATA SET Specific
   o << "<value ref=\"ID\">" << data[i]->ID[j] << "</value>" << endl;
   o << "<value ref=\"DV\">" << data[i]->DV[j] << "</value>" << endl;
   o << "<value ref=\"EPS(1)\">" << data[i]->EPS[j][0] << "</value>" << endl;
   o << "<value ref=\"ETA(1)\">" << data[i]->ETA[j][0] << "</value>" << endl;
   o << "<value ref=\"F\">" << data[i]->F[j] << "</value>" << endl;
   o << "<value ref=\"MDV\">" << data[i]->MDV[j] << "</value>" << endl;
   o << "<value ref=\"PRED\">" << data[i]->PRED[j] << "</value>" << endl;
   o << "<value ref=\"RES\">" << data[i]->RES[j] << "</value>" << endl;
   o << "<value ref=\"THETA(1)\">" << data[i]->THETA[j][0] << "</value>" << endl;
   o << "<value ref=\"TIME\">" << data[i]->TIME[j] << "</value>" << endl;
   o << "<value ref=\"WRES\">" << data[i]->WRES[j] << "</value>" << endl;
   o << "<value ref=\"Y\">" << data[i]->Y[j] << "</value>" << endl;
      //
      ///////////////////////////////////////////////////////////////////
      o << "</row>" << endl;
   }
}
o << "</" << title << ">" << endl;
}
#endif
