// Linear Model: Estimation
#ifndef DATASET_H
#define DATASET_H
#include <vector>
#include <spk/SpkValarray.h>
#include "IndData.h"

template <class spk_ValueType>
class DataSet
{
public:
   DataSet();
   ~DataSet();

   std::vector<IndData<spk_ValueType>*> data;
   const SPK_VA::valarray<double> getAllMeasurements() const;
   int getPopSize() const;
   const SPK_VA::valarray<int> getN() const;
   void replaceAllMeasurements( const SPK_VA::valarray<double> & yy );
   void compAllResiduals();
   void compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R );
   friend std::ostream& operator<< <spk_ValueType>( std::ostream& o, const DataSet<spk_ValueType>& A );

protected:
   DataSet( const DataSet& );
   DataSet& operator=( const DataSet& );

private:
   SPK_VA::valarray<double> measurements; // a long vector containg all measurements
   SPK_VA::valarray<int> N; // a vector containing the # of measurements for each individual.
   const int popSize;
};

template <class spk_ValueType>
DataSet<spk_ValueType>::DataSet()
: popSize( 10 ),
  data( 10 ),
  N( 10 )
{
   //------------------------------------
   // Subject <1> 
   // # of sampling points = 2
   //------------------------------------
   N[0] = 2;
char* ID_0_c[] = { "1", "1" };
   std::vector<char*> ID_0( 2 );
   copy( ID_0_c, ID_0_c+2, ID_0.begin() );
spk_ValueType TIME_0_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_0( 2 );
   copy( TIME_0_c, TIME_0_c+2, TIME_0.begin() );
spk_ValueType DV_0_c[] = { -0.0566087, 2.15878 };
   std::vector<spk_ValueType> DV_0( 2 );
   copy( DV_0_c, DV_0_c+2, DV_0.begin() );
spk_ValueType MDV_0_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_0( 2 );
   copy( MDV_0_c, MDV_0_c+2, MDV_0.begin() );
   data[0] = new IndData<spk_ValueType>( 2, ID_0, TIME_0, DV_0, MDV_0 );

   //------------------------------------
   // Subject <2> 
   // # of sampling points = 2
   //------------------------------------
   N[1] = 2;
char* ID_1_c[] = { "2", "2" };
   std::vector<char*> ID_1( 2 );
   copy( ID_1_c, ID_1_c+2, ID_1.begin() );
spk_ValueType TIME_1_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_1( 2 );
   copy( TIME_1_c, TIME_1_c+2, TIME_1.begin() );
spk_ValueType DV_1_c[] = { -0.00733425, 2.88191 };
   std::vector<spk_ValueType> DV_1( 2 );
   copy( DV_1_c, DV_1_c+2, DV_1.begin() );
spk_ValueType MDV_1_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_1( 2 );
   copy( MDV_1_c, MDV_1_c+2, MDV_1.begin() );
   data[1] = new IndData<spk_ValueType>( 2, ID_1, TIME_1, DV_1, MDV_1 );

   //------------------------------------
   // Subject <3> 
   // # of sampling points = 2
   //------------------------------------
   N[2] = 2;
char* ID_2_c[] = { "3", "3" };
   std::vector<char*> ID_2( 2 );
   copy( ID_2_c, ID_2_c+2, ID_2.begin() );
spk_ValueType TIME_2_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_2( 2 );
   copy( TIME_2_c, TIME_2_c+2, TIME_2.begin() );
spk_ValueType DV_2_c[] = { 0.075537, 0.831321 };
   std::vector<spk_ValueType> DV_2( 2 );
   copy( DV_2_c, DV_2_c+2, DV_2.begin() );
spk_ValueType MDV_2_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_2( 2 );
   copy( MDV_2_c, MDV_2_c+2, MDV_2.begin() );
   data[2] = new IndData<spk_ValueType>( 2, ID_2, TIME_2, DV_2, MDV_2 );

   //------------------------------------
   // Subject <4> 
   // # of sampling points = 2
   //------------------------------------
   N[3] = 2;
char* ID_3_c[] = { "4", "4" };
   std::vector<char*> ID_3( 2 );
   copy( ID_3_c, ID_3_c+2, ID_3.begin() );
spk_ValueType TIME_3_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_3( 2 );
   copy( TIME_3_c, TIME_3_c+2, TIME_3.begin() );
spk_ValueType DV_3_c[] = { 0.120011, -0.588709 };
   std::vector<spk_ValueType> DV_3( 2 );
   copy( DV_3_c, DV_3_c+2, DV_3.begin() );
spk_ValueType MDV_3_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_3( 2 );
   copy( MDV_3_c, MDV_3_c+2, MDV_3.begin() );
   data[3] = new IndData<spk_ValueType>( 2, ID_3, TIME_3, DV_3, MDV_3 );

   //------------------------------------
   // Subject <5> 
   // # of sampling points = 2
   //------------------------------------
   N[4] = 2;
char* ID_4_c[] = { "5", "5" };
   std::vector<char*> ID_4( 2 );
   copy( ID_4_c, ID_4_c+2, ID_4.begin() );
spk_ValueType TIME_4_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_4( 2 );
   copy( TIME_4_c, TIME_4_c+2, TIME_4.begin() );
spk_ValueType DV_4_c[] = { 0.0157215, -0.407271 };
   std::vector<spk_ValueType> DV_4( 2 );
   copy( DV_4_c, DV_4_c+2, DV_4.begin() );
spk_ValueType MDV_4_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_4( 2 );
   copy( MDV_4_c, MDV_4_c+2, MDV_4.begin() );
   data[4] = new IndData<spk_ValueType>( 2, ID_4, TIME_4, DV_4, MDV_4 );

   //------------------------------------
   // Subject <6> 
   // # of sampling points = 2
   //------------------------------------
   N[5] = 2;
char* ID_5_c[] = { "6", "6" };
   std::vector<char*> ID_5( 2 );
   copy( ID_5_c, ID_5_c+2, ID_5.begin() );
spk_ValueType TIME_5_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_5( 2 );
   copy( TIME_5_c, TIME_5_c+2, TIME_5.begin() );
spk_ValueType DV_5_c[] = { -0.02444, 1.93778 };
   std::vector<spk_ValueType> DV_5( 2 );
   copy( DV_5_c, DV_5_c+2, DV_5.begin() );
spk_ValueType MDV_5_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_5( 2 );
   copy( MDV_5_c, MDV_5_c+2, MDV_5.begin() );
   data[5] = new IndData<spk_ValueType>( 2, ID_5, TIME_5, DV_5, MDV_5 );

   //------------------------------------
   // Subject <7> 
   // # of sampling points = 2
   //------------------------------------
   N[6] = 2;
char* ID_6_c[] = { "7", "7" };
   std::vector<char*> ID_6( 2 );
   copy( ID_6_c, ID_6_c+2, ID_6.begin() );
spk_ValueType TIME_6_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_6( 2 );
   copy( TIME_6_c, TIME_6_c+2, TIME_6.begin() );
spk_ValueType DV_6_c[] = { -0.141565, -1.48958 };
   std::vector<spk_ValueType> DV_6( 2 );
   copy( DV_6_c, DV_6_c+2, DV_6.begin() );
spk_ValueType MDV_6_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_6( 2 );
   copy( MDV_6_c, MDV_6_c+2, MDV_6.begin() );
   data[6] = new IndData<spk_ValueType>( 2, ID_6, TIME_6, DV_6, MDV_6 );

   //------------------------------------
   // Subject <8> 
   // # of sampling points = 2
   //------------------------------------
   N[7] = 2;
char* ID_7_c[] = { "8", "8" };
   std::vector<char*> ID_7( 2 );
   copy( ID_7_c, ID_7_c+2, ID_7.begin() );
spk_ValueType TIME_7_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_7( 2 );
   copy( TIME_7_c, TIME_7_c+2, TIME_7.begin() );
spk_ValueType DV_7_c[] = { -0.159169, 1.88597 };
   std::vector<spk_ValueType> DV_7( 2 );
   copy( DV_7_c, DV_7_c+2, DV_7.begin() );
spk_ValueType MDV_7_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_7( 2 );
   copy( MDV_7_c, MDV_7_c+2, MDV_7.begin() );
   data[7] = new IndData<spk_ValueType>( 2, ID_7, TIME_7, DV_7, MDV_7 );

   //------------------------------------
   // Subject <9> 
   // # of sampling points = 2
   //------------------------------------
   N[8] = 2;
char* ID_8_c[] = { "9", "9" };
   std::vector<char*> ID_8( 2 );
   copy( ID_8_c, ID_8_c+2, ID_8.begin() );
spk_ValueType TIME_8_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_8( 2 );
   copy( TIME_8_c, TIME_8_c+2, TIME_8.begin() );
spk_ValueType DV_8_c[] = { -0.133682, 2.06994 };
   std::vector<spk_ValueType> DV_8( 2 );
   copy( DV_8_c, DV_8_c+2, DV_8.begin() );
spk_ValueType MDV_8_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_8( 2 );
   copy( MDV_8_c, MDV_8_c+2, MDV_8.begin() );
   data[8] = new IndData<spk_ValueType>( 2, ID_8, TIME_8, DV_8, MDV_8 );

   //------------------------------------
   // Subject <10> 
   // # of sampling points = 2
   //------------------------------------
   N[9] = 2;
char* ID_9_c[] = { "10", "10" };
   std::vector<char*> ID_9( 2 );
   copy( ID_9_c, ID_9_c+2, ID_9.begin() );
spk_ValueType TIME_9_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_9( 2 );
   copy( TIME_9_c, TIME_9_c+2, TIME_9.begin() );
spk_ValueType DV_9_c[] = { 0.0132823, 2.25377 };
   std::vector<spk_ValueType> DV_9( 2 );
   copy( DV_9_c, DV_9_c+2, DV_9.begin() );
spk_ValueType MDV_9_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_9( 2 );
   copy( MDV_9_c, MDV_9_c+2, MDV_9.begin() );
   data[9] = new IndData<spk_ValueType>( 2, ID_9, TIME_9, DV_9, MDV_9 );


   int nY = N.sum();
   measurements.resize( nY ); 
   for( int i=0, m=0; i<popSize; i++ )
   {
      int nYi = data[i]->getMeasurements().size();
      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();
      m+=nYi;
   }
}

template <class spk_ValueType>
DataSet<spk_ValueType>::~DataSet()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      delete data[i];
   }
}

template <class spk_ValueType>
DataSet<spk_ValueType>::DataSet( const DataSet<spk_ValueType>& )
{
}

template <class spk_ValueType>
DataSet<spk_ValueType>& DataSet<spk_ValueType>::operator=( const DataSet<spk_ValueType>& )
{
}

// Returns SPK's y.
template <class spk_ValueType>
const SPK_VA::valarray<double> DataSet<spk_ValueType>::getAllMeasurements() const
{
   return measurements;
}

// Returns the population size.
template <class spk_ValueType>
int DataSet<spk_ValueType>::getPopSize() const
{
   return popSize;
}

// Return SPK's N (ie. N[i] is the number of measurements of the i-th individual.
template <class spk_ValueType>
const SPK_VA::valarray<int> DataSet<spk_ValueType>::getN() const
{
   return N;
}

// Replace the currently kept y with the given y'.
template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceAllMeasurements( const SPK_VA::valarray<double> & yy )
{
   const int n= data.size();
   for( int i=0, k=0; i<n; k+=N[i++] )
   {
      data[i]->replaceMeasurements( yy[ SPK_VA::slice(k, N[i], 1) ] );
   }
   measurements = yy;
}

// Compute the residuals for all individuals.
template <class spk_ValueType>
void DataSet<spk_ValueType>::compAllResiduals()
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      data[i]->compResiduals();
   }
}

// Compute the weighted residuals for all individuals.
template <class spk_ValueType>
void DataSet<spk_ValueType>::compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R )
{
   const int n = data.size();
   for( int i=0; i<n; i++ )
   {
      data[i]->compWeightedResiduals( R[i] );
   }
}

// Extracts the contents of this class object in the SpkResultML::presentation_data form.
template <class spk_ValueType>
std::ostream& operator<<( std::ostream& o, const DataSet<spk_ValueType>& A )
{
   o << "<" << "presentation_data" << " rows=\"" << A.N.sum() << "\" ";
   o << "columns=\"12\">" << endl;
   o << "<data_labels>" << endl;
   o << "<label name=\"ID\"/>" << endl;
   o << "<label name=\"DV\"/>" << endl;
   o << "<label name=\"EPS(1)\"/>" << endl;
   o << "<label name=\"ETA(1)\"/>" << endl;
   o << "<label name=\"F\"/>" << endl;
   o << "<label name=\"MDV\"/>" << endl;
   o << "<label name=\"PRED\"/>" << endl;
   o << "<label name=\"RES\"/>" << endl;
   o << "<label name=\"THETA(1)\"/>" << endl;
   o << "<label name=\"TIME\"/>" << endl;
   o << "<label name=\"WRES\"/>" << endl;
   o << "<label name=\"Y\"/>" << endl;
   o << "</data_labels>" << endl;

   for( int i=0, position=1; i<A.getPopSize(); i++ )
   {
      for( int j=0; j<A.N[i]; j++, position++ )
      {
         o << "<row position=\"" << position << "\">" << endl;
         o << "<value ref=\"ID\">" << A.data[i]->ID[j] << "</value>" << endl;
         o << "<value ref=\"DV\">" << A.data[i]->DV[j] << "</value>" << endl;
         o << "<value ref=\"EPS(1)\">" << A.data[i]->EPS[j][0] << "</value>" << endl;
         o << "<value ref=\"ETA(1)\">" << A.data[i]->ETA[j][0] << "</value>" << endl;
         o << "<value ref=\"F\">" << A.data[i]->F[j] << "</value>" << endl;
         o << "<value ref=\"MDV\">" << A.data[i]->MDV[j] << "</value>" << endl;
         o << "<value ref=\"PRED\">" << A.data[i]->PRED[j] << "</value>" << endl;
         o << "<value ref=\"RES\">" << A.data[i]->RES[j] << "</value>" << endl;
         o << "<value ref=\"THETA(1)\">" << A.data[i]->THETA[j][0] << "</value>" << endl;
         o << "<value ref=\"TIME\">" << A.data[i]->TIME[j] << "</value>" << endl;
         o << "<value ref=\"WRES\">" << A.data[i]->WRES[j] << "</value>" << endl;
         o << "<value ref=\"Y\">" << A.data[i]->Y[j] << "</value>" << endl;
         o << "</row>" << endl;
      }
   }
   o << "</" << "presentation_data" << ">";
}
#endif
