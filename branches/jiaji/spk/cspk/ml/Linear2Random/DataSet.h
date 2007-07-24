// Population model of line where slope and intercept are random effects.
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
   void expand( const SPK_VA::valarray<double>& measurements, SPK_VA::valarray<double>& records ) const;
   int getMeasurementIndex( int recordIndex ) const;
   int getMeasurementIndex( int who, int recordIndex ) const;
   int getRecordIndex( int measurementIndex ) const;
   int getRecordIndex( int who, int measurementIndex ) const;
   int getPopSize() const;
   const SPK_VA::valarray<int> getN() const;
   const SPK_VA::valarray<int> getNObservs() const;
   int getNObservs( int i ) const;
   const SPK_VA::valarray<int> getNRecords() const;
   int getNRecords( int i ) const;
   const SPK_VA::valarray<double> getAllMeasurements() const;
   void replaceAllMeasurements( const SPK_VA::valarray<double> & yy );
   void replaceEta     ( const SPK_VA::valarray<double>& EtaIn );
   void replacePred    ( const SPK_VA::valarray<double>& PredIn );
   void replaceRes     ( const SPK_VA::valarray<double>& ResIn );
   void replaceWRes    ( const SPK_VA::valarray<double>& WResIn );
   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaResIn );
   void replaceWEtaRes ( const SPK_VA::valarray<double>& WEtaResIn );
   void replaceIPred   ( const SPK_VA::valarray<double>& iPredIn );
   void replaceIRes    ( const SPK_VA::valarray<double>& iResIn );
   void replaceIWRes   ( const SPK_VA::valarray<double>& iWResIn );
   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaResIn );
   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn );
   void replacePPred   ( const SPK_VA::valarray<double>& pPredIn );
   void replacePRes    ( const SPK_VA::valarray<double>& pResAllIn );
   void replacePWRes   ( const SPK_VA::valarray<double>& pWResAllIn );
   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResAllIn );
   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResAllIn );
   void replaceCPred   ( const SPK_VA::valarray<double>& cPredIn );
   void replaceCRes    ( const SPK_VA::valarray<double>& cResAllIn );
   void replaceCWRes   ( const SPK_VA::valarray<double>& cWResAllIn );
   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResAllIn );
   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResAllIn );

   friend std::ostream& operator<< <spk_ValueType>( std::ostream& o, const DataSet<spk_ValueType>& A );

protected:
   DataSet( const DataSet& );
   DataSet& operator=( const DataSet& );

private:
   SPK_VA::valarray<double> measurements; // a long vector containg all measurements
   SPK_VA::valarray<int> NRecords;  // # of records for each individual.
   SPK_VA::valarray<int> NObservs; // # of measurements for each individual.
   const int popSize;

   /////////////////////////////////////////////////////////
   //      original                     y
   //  -------------------      -------------------
   //   j    i   MDV   DV         j'  j   i   DV
   //  -------------------      -------------------
   //   0    0    0    0.1        0   0   0   0.1
   //   1    0    1               1   2   0   0.2
   //   2    0    0    0.2        2   4   0   0.3
   //   3    0    1               3   5   1   0.01
   //   4    0    0    0.3        4   7   1   0.02
   //   5    1    0    0.1        5   9   1   0.03
   //   6    1    1
   //   7    1    0    0.2
   //   8    1    1
   //   9    1    0    0.3
   //
   //
   //   jTojPrime            jPrimeToj
   //  -----------          -----------
   //    j    j'              j'   j
   //  -----------          -----------
   //    0    0               0    0
   //    1   -1*              1    2
   //    2    1               2    4
   //    3   -1*              3    5
   //    4    2               4    7
   //    5    3               5    9
   //    6   -1*
   //    7    4
   //    8   -1*
   //    9    5
   //
   //  * (-1) points to no j', i.e. MDV=1
   /////////////////////////////////////////////////////////
   std::vector<int> jTojPrime;
   std::vector<int> jPrimeToj;
};

template <class spk_ValueType>
DataSet<spk_ValueType>::DataSet()
: popSize( 10 ),
  data( 10 ),
  NRecords( 10 ), // #of records for each individual
  NObservs( 10 )  // #of DVs for each individuals
{
   //------------------------------------
   // Subject <1> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[0] = 5;
char* ID_0_c[] = { "1", "1", "1", "1", "1" };
   std::vector<char*> ID_0( 5 );
   copy( ID_0_c, ID_0_c+5, ID_0.begin() );
spk_ValueType TIME_0_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_0( 5 );
   copy( TIME_0_c, TIME_0_c+5, TIME_0.begin() );
spk_ValueType DV_0_c[] = { 2.81556, 4.25946, 2.88029, 4.8092, 3.76698 };
   std::vector<spk_ValueType> DV_0( 5 );
   copy( DV_0_c, DV_0_c+5, DV_0.begin() );
spk_ValueType AMT_0_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_0( 5 );
   copy( AMT_0_c, AMT_0_c+5, AMT_0.begin() );
spk_ValueType MDV_0_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_0( 5 );
   copy( MDV_0_c, MDV_0_c+5, MDV_0.begin() );
int EVID_0_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_0( 5 );
   copy( EVID_0_c, EVID_0_c+5, EVID_0.begin() );
   data[0] = new IndData<spk_ValueType>( 5, ID_0, TIME_0, DV_0, AMT_0, MDV_0, EVID_0 );

   //------------------------------------
   // Subject <2> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[1] = 5;
char* ID_1_c[] = { "2", "2", "2", "2", "2" };
   std::vector<char*> ID_1( 5 );
   copy( ID_1_c, ID_1_c+5, ID_1.begin() );
spk_ValueType TIME_1_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_1( 5 );
   copy( TIME_1_c, TIME_1_c+5, TIME_1.begin() );
spk_ValueType DV_1_c[] = { 0.256901, 2.81346, 0.568923, -0.231429, 1.3445 };
   std::vector<spk_ValueType> DV_1( 5 );
   copy( DV_1_c, DV_1_c+5, DV_1.begin() );
spk_ValueType AMT_1_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_1( 5 );
   copy( AMT_1_c, AMT_1_c+5, AMT_1.begin() );
spk_ValueType MDV_1_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_1( 5 );
   copy( MDV_1_c, MDV_1_c+5, MDV_1.begin() );
int EVID_1_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_1( 5 );
   copy( EVID_1_c, EVID_1_c+5, EVID_1.begin() );
   data[1] = new IndData<spk_ValueType>( 5, ID_1, TIME_1, DV_1, AMT_1, MDV_1, EVID_1 );

   //------------------------------------
   // Subject <3> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[2] = 5;
char* ID_2_c[] = { "3", "3", "3", "3", "3" };
   std::vector<char*> ID_2( 5 );
   copy( ID_2_c, ID_2_c+5, ID_2.begin() );
spk_ValueType TIME_2_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_2( 5 );
   copy( TIME_2_c, TIME_2_c+5, TIME_2.begin() );
spk_ValueType DV_2_c[] = { 2.29028, 6.10964, 3.55954, 6.38663, 9.53105 };
   std::vector<spk_ValueType> DV_2( 5 );
   copy( DV_2_c, DV_2_c+5, DV_2.begin() );
spk_ValueType AMT_2_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_2( 5 );
   copy( AMT_2_c, AMT_2_c+5, AMT_2.begin() );
spk_ValueType MDV_2_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_2( 5 );
   copy( MDV_2_c, MDV_2_c+5, MDV_2.begin() );
int EVID_2_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_2( 5 );
   copy( EVID_2_c, EVID_2_c+5, EVID_2.begin() );
   data[2] = new IndData<spk_ValueType>( 5, ID_2, TIME_2, DV_2, AMT_2, MDV_2, EVID_2 );

   //------------------------------------
   // Subject <4> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[3] = 5;
char* ID_3_c[] = { "4", "4", "4", "4", "4" };
   std::vector<char*> ID_3( 5 );
   copy( ID_3_c, ID_3_c+5, ID_3.begin() );
spk_ValueType TIME_3_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_3( 5 );
   copy( TIME_3_c, TIME_3_c+5, TIME_3.begin() );
spk_ValueType DV_3_c[] = { 1.34402, 2.49768, -0.57402, -0.561433, 0.876495 };
   std::vector<spk_ValueType> DV_3( 5 );
   copy( DV_3_c, DV_3_c+5, DV_3.begin() );
spk_ValueType AMT_3_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_3( 5 );
   copy( AMT_3_c, AMT_3_c+5, AMT_3.begin() );
spk_ValueType MDV_3_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_3( 5 );
   copy( MDV_3_c, MDV_3_c+5, MDV_3.begin() );
int EVID_3_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_3( 5 );
   copy( EVID_3_c, EVID_3_c+5, EVID_3.begin() );
   data[3] = new IndData<spk_ValueType>( 5, ID_3, TIME_3, DV_3, AMT_3, MDV_3, EVID_3 );

   //------------------------------------
   // Subject <5> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[4] = 5;
char* ID_4_c[] = { "5", "5", "5", "5", "5" };
   std::vector<char*> ID_4( 5 );
   copy( ID_4_c, ID_4_c+5, ID_4.begin() );
spk_ValueType TIME_4_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_4( 5 );
   copy( TIME_4_c, TIME_4_c+5, TIME_4.begin() );
spk_ValueType DV_4_c[] = { -0.112967, 0.486252, 1.49783, 3.51853, 4.94744 };
   std::vector<spk_ValueType> DV_4( 5 );
   copy( DV_4_c, DV_4_c+5, DV_4.begin() );
spk_ValueType AMT_4_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_4( 5 );
   copy( AMT_4_c, AMT_4_c+5, AMT_4.begin() );
spk_ValueType MDV_4_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_4( 5 );
   copy( MDV_4_c, MDV_4_c+5, MDV_4.begin() );
int EVID_4_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_4( 5 );
   copy( EVID_4_c, EVID_4_c+5, EVID_4.begin() );
   data[4] = new IndData<spk_ValueType>( 5, ID_4, TIME_4, DV_4, AMT_4, MDV_4, EVID_4 );

   //------------------------------------
   // Subject <6> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[5] = 5;
char* ID_5_c[] = { "6", "6", "6", "6", "6" };
   std::vector<char*> ID_5( 5 );
   copy( ID_5_c, ID_5_c+5, ID_5.begin() );
spk_ValueType TIME_5_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_5( 5 );
   copy( TIME_5_c, TIME_5_c+5, TIME_5.begin() );
spk_ValueType DV_5_c[] = { -1.92357, 3.42401, 3.00741, 4.3451, 7.11909 };
   std::vector<spk_ValueType> DV_5( 5 );
   copy( DV_5_c, DV_5_c+5, DV_5.begin() );
spk_ValueType AMT_5_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_5( 5 );
   copy( AMT_5_c, AMT_5_c+5, AMT_5.begin() );
spk_ValueType MDV_5_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_5( 5 );
   copy( MDV_5_c, MDV_5_c+5, MDV_5.begin() );
int EVID_5_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_5( 5 );
   copy( EVID_5_c, EVID_5_c+5, EVID_5.begin() );
   data[5] = new IndData<spk_ValueType>( 5, ID_5, TIME_5, DV_5, AMT_5, MDV_5, EVID_5 );

   //------------------------------------
   // Subject <7> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[6] = 5;
char* ID_6_c[] = { "7", "7", "7", "7", "7" };
   std::vector<char*> ID_6( 5 );
   copy( ID_6_c, ID_6_c+5, ID_6.begin() );
spk_ValueType TIME_6_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_6( 5 );
   copy( TIME_6_c, TIME_6_c+5, TIME_6.begin() );
spk_ValueType DV_6_c[] = { -2.18017, 2.00627, 0.60578, 1.19295, -0.767411 };
   std::vector<spk_ValueType> DV_6( 5 );
   copy( DV_6_c, DV_6_c+5, DV_6.begin() );
spk_ValueType AMT_6_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_6( 5 );
   copy( AMT_6_c, AMT_6_c+5, AMT_6.begin() );
spk_ValueType MDV_6_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_6( 5 );
   copy( MDV_6_c, MDV_6_c+5, MDV_6.begin() );
int EVID_6_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_6( 5 );
   copy( EVID_6_c, EVID_6_c+5, EVID_6.begin() );
   data[6] = new IndData<spk_ValueType>( 5, ID_6, TIME_6, DV_6, AMT_6, MDV_6, EVID_6 );

   //------------------------------------
   // Subject <8> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[7] = 5;
char* ID_7_c[] = { "8", "8", "8", "8", "8" };
   std::vector<char*> ID_7( 5 );
   copy( ID_7_c, ID_7_c+5, ID_7.begin() );
spk_ValueType TIME_7_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_7( 5 );
   copy( TIME_7_c, TIME_7_c+5, TIME_7.begin() );
spk_ValueType DV_7_c[] = { 0.482672, -0.489369, -1.69607, -4.39203, -2.61387 };
   std::vector<spk_ValueType> DV_7( 5 );
   copy( DV_7_c, DV_7_c+5, DV_7.begin() );
spk_ValueType AMT_7_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_7( 5 );
   copy( AMT_7_c, AMT_7_c+5, AMT_7.begin() );
spk_ValueType MDV_7_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_7( 5 );
   copy( MDV_7_c, MDV_7_c+5, MDV_7.begin() );
int EVID_7_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_7( 5 );
   copy( EVID_7_c, EVID_7_c+5, EVID_7.begin() );
   data[7] = new IndData<spk_ValueType>( 5, ID_7, TIME_7, DV_7, AMT_7, MDV_7, EVID_7 );

   //------------------------------------
   // Subject <9> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[8] = 5;
char* ID_8_c[] = { "9", "9", "9", "9", "9" };
   std::vector<char*> ID_8( 5 );
   copy( ID_8_c, ID_8_c+5, ID_8.begin() );
spk_ValueType TIME_8_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_8( 5 );
   copy( TIME_8_c, TIME_8_c+5, TIME_8.begin() );
spk_ValueType DV_8_c[] = { 2.51285, 1.42056, 1.09081, 1.32213, -0.189584 };
   std::vector<spk_ValueType> DV_8( 5 );
   copy( DV_8_c, DV_8_c+5, DV_8.begin() );
spk_ValueType AMT_8_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_8( 5 );
   copy( AMT_8_c, AMT_8_c+5, AMT_8.begin() );
spk_ValueType MDV_8_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_8( 5 );
   copy( MDV_8_c, MDV_8_c+5, MDV_8.begin() );
int EVID_8_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_8( 5 );
   copy( EVID_8_c, EVID_8_c+5, EVID_8.begin() );
   data[8] = new IndData<spk_ValueType>( 5, ID_8, TIME_8, DV_8, AMT_8, MDV_8, EVID_8 );

   //------------------------------------
   // Subject <10> 
   // # of sampling points = 5
   //------------------------------------
   NRecords[9] = 5;
char* ID_9_c[] = { "10", "10", "10", "10", "10" };
   std::vector<char*> ID_9( 5 );
   copy( ID_9_c, ID_9_c+5, ID_9.begin() );
spk_ValueType TIME_9_c[] = { 0, 1, 2, 3, 4 };
   std::vector<spk_ValueType> TIME_9( 5 );
   copy( TIME_9_c, TIME_9_c+5, TIME_9.begin() );
spk_ValueType DV_9_c[] = { 1.02133, 1.2694, 0.154778, -1.88743, -0.260074 };
   std::vector<spk_ValueType> DV_9( 5 );
   copy( DV_9_c, DV_9_c+5, DV_9.begin() );
spk_ValueType AMT_9_c[] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_9( 5 );
   copy( AMT_9_c, AMT_9_c+5, AMT_9.begin() );
spk_ValueType MDV_9_c[] = { 0, 0, 0, 0, 0 };
   std::vector<spk_ValueType> MDV_9( 5 );
   copy( MDV_9_c, MDV_9_c+5, MDV_9.begin() );
int EVID_9_c[] = { 0, 0, 0, 0, 0 };
   std::vector<int> EVID_9( 5 );
   copy( EVID_9_c, EVID_9_c+5, EVID_9.begin() );
   data[9] = new IndData<spk_ValueType>( 5, ID_9, TIME_9, DV_9, AMT_9, MDV_9, EVID_9 );


   int nRecords = 0;
   for( int i=0; i<popSize; i++ )
      nRecords += data[i]->getNRecords();
   
   int nY = 0;  // # of DVs
   for( int i=0; i<popSize; i++ )
   {
      NObservs[i] = data[i]->getMeasurements().size();
      nY += NObservs[i];
   }
   measurements.resize( nY ); 
   jPrimeToj.resize( nY );
   jTojPrime.resize( nRecords );
   for( int i=0, m=0, j=0, jPrime=0; i<popSize; i++ )
   {
      int nYi = data[i]->getMeasurements().size();
      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();
      m+=nYi;
      int n = data[i]->getNRecords();
      for( int k=0; k<n; k++, j++ )
      {
         if( data[i]->MDV[k] == 0 )
         {
            jPrimeToj[jPrime] = j;
            jTojPrime[j] = jPrime;
            jPrime++;
         }
         else
            jTojPrime[j] = -1;
      }
   }
}

template <class spk_ValueType>
DataSet<spk_ValueType>::~DataSet()
{
   const int nPop = data.size();
   for( int i=0; i<nPop; i++ )
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

template <class spk_ValueType>
int DataSet<spk_ValueType>::getMeasurementIndex( int recordIndex ) const
{
   return jTojPrime[ recordIndex ];
}
template <class spk_ValueType>
int DataSet<spk_ValueType>::getMeasurementIndex( int who, int recordIndex ) const
{
   return data[who]->getMeasurementIndex(recordIndex);
}
template <class spk_ValueType>
int DataSet<spk_ValueType>::getRecordIndex( int measurementIndex ) const
{
   return jPrimeToj[ measurementIndex ];
}
template <class spk_ValueType>
int DataSet<spk_ValueType>::getRecordIndex( int who, int measurementIndex ) const
{
   return data[who]->getRecordIndex(measurementIndex);
}
// Returns SPK's y.
template <class spk_ValueType>
const SPK_VA::valarray<double> DataSet<spk_ValueType>::getAllMeasurements() const
{
   return measurements;
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::expand( 
       const SPK_VA::valarray<double> & measurements,
       SPK_VA::valarray<double> & records ) const
{
   const int n = NObservs.sum();
   int m = 0;
   for( int i=0; i<popSize; i++ )
      m += getNRecords(i);
   records.resize( m );
   records = 0.0;
   for( int i=0; i<n; i++ )
   {
      records[ getRecordIndex( i ) ] = measurements[i];
   }
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
   return NObservs;
}

// Return the number of measurements (DVs) for the entire population.
template <class spk_ValueType>
const SPK_VA::valarray<int> DataSet<spk_ValueType>::getNObservs() const
{
   return NObservs;
}
// Return the number of measurements (DVs) of the i-th individual.
template <class spk_ValueType>
int DataSet<spk_ValueType>::getNObservs( int i ) const
{
   return data[i]->getNObservs();
}
// Return the number of data records (including MDV=1) for the entire population.
template <class spk_ValueType>
const SPK_VA::valarray<int> DataSet<spk_ValueType>::getNRecords() const
{
   return NRecords;
}
// Return the number of data records (including MDV=1) of the i-th individual.
template <class spk_ValueType>
int DataSet<spk_ValueType>::getNRecords( int i ) const
{
   return data[i]->getNRecords();
}
// Replace the currently kept y with the given y'.
template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceAllMeasurements( const SPK_VA::valarray<double> & yy )
{
   const int n= data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceMeasurements( yy[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
   measurements = yy;
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& PredIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replacePred( PredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceRes( ResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceWRes( WResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceIPred( iPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceIRes( iResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceIWRes( iWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replacePPred( pPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replacePRes( pResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replacePWRes( pWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceCPred( cPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceCRes( cResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )
{
   const int n = data.size();
   for( int i=0, k=0; i<n; k+=NRecords[i++] )
   {
      data[i]->replaceCWRes( cWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( etaIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceEta( etaIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( EtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceEtaRes( EtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( WEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceWEtaRes( WEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( iEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceIEtaRes( iEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( iWEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceIWEtaRes( iWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( pEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replacePEtaRes( pEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( pWEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replacePWEtaRes( pWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( cEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceCEtaRes( cEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

template <class spk_ValueType>
void DataSet<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )
{
   const int n = data.size();
   const int nEta = 2; // the length of eta
   assert( cWEtaResIn.size() == n * nEta );
   for( int i=0; i<n; i++ )
   {
      data[i]->replaceCWEtaRes( cWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );
   }
}

// Extracts the contents of this class object in the SpkResultML::presentation_data form.
template <class spk_ValueType>
std::ostream& operator<<( std::ostream& o, const DataSet<spk_ValueType>& A )
{
   using std::endl;
   o << "<" << "presentation_data" << " rows=\"" << A.NRecords.sum() << "\" ";
   o << "columns=\"44\">" << endl;
   o << "<data_labels>" << endl;
   o << "<label name=\"ID\"/>" << endl;
   o << "<label name=\"AMT\"/>" << endl;
   o << "<label name=\"B\"/>" << endl;
   o << "<label name=\"CETARES(1)\"/>" << endl;
   o << "<label name=\"CETARES(2)\"/>" << endl;
   o << "<label name=\"CPRED\"/>" << endl;
   o << "<label name=\"CRES\"/>" << endl;
   o << "<label name=\"CWETARES(1)\"/>" << endl;
   o << "<label name=\"CWETARES(2)\"/>" << endl;
   o << "<label name=\"CWRES\"/>" << endl;
   o << "<label name=\"DV\"/>" << endl;
   o << "<label name=\"EPS(1)\"/>" << endl;
   o << "<label name=\"ETA(1)\"/>" << endl;
   o << "<label name=\"ETA(2)\"/>" << endl;
   o << "<label name=\"ETARES(1)\"/>" << endl;
   o << "<label name=\"ETARES(2)\"/>" << endl;
   o << "<label name=\"EVID\"/>" << endl;
   o << "<label name=\"F\"/>" << endl;
   o << "<label name=\"IETARES(1)\"/>" << endl;
   o << "<label name=\"IETARES(2)\"/>" << endl;
   o << "<label name=\"IPRED\"/>" << endl;
   o << "<label name=\"IRES\"/>" << endl;
   o << "<label name=\"IWETARES(1)\"/>" << endl;
   o << "<label name=\"IWETARES(2)\"/>" << endl;
   o << "<label name=\"IWRES\"/>" << endl;
   o << "<label name=\"MDV\"/>" << endl;
   o << "<label name=\"ORGDV\"/>" << endl;
   o << "<label name=\"PETARES(1)\"/>" << endl;
   o << "<label name=\"PETARES(2)\"/>" << endl;
   o << "<label name=\"PPRED\"/>" << endl;
   o << "<label name=\"PRED\"/>" << endl;
   o << "<label name=\"PRES\"/>" << endl;
   o << "<label name=\"PWETARES(1)\"/>" << endl;
   o << "<label name=\"PWETARES(2)\"/>" << endl;
   o << "<label name=\"PWRES\"/>" << endl;
   o << "<label name=\"RES\"/>" << endl;
   o << "<label name=\"S\"/>" << endl;
   o << "<label name=\"THETA(1)\"/>" << endl;
   o << "<label name=\"THETA(2)\"/>" << endl;
   o << "<label name=\"TIME\"/>" << endl;
   o << "<label name=\"WETARES(1)\"/>" << endl;
   o << "<label name=\"WETARES(2)\"/>" << endl;
   o << "<label name=\"WRES\"/>" << endl;
   o << "<label name=\"Y\"/>" << endl;
   o << "</data_labels>" << endl;

   for( int i=0, position=1; i<A.getPopSize(); i++ )
   {
      for( int j=0; j<A.NRecords[i]; j++, position++ )
      {
         o << "<row position=\"" << position << "\">" << endl;
         o << "<value ref=\"ID\">" << A.data[i]->ID[j] << "</value>" << endl;
         o << "<value ref=\"AMT\">" << A.data[i]->AMT[j] << "</value>" << endl;
         o << "<value ref=\"B\">" << A.data[i]->B[j] << "</value>" << endl;
         o << "<value ref=\"CETARES(1)\">" << A.data[i]->CETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"CETARES(2)\">" << A.data[i]->CETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"CPRED\">" << A.data[i]->CPRED[j] << "</value>" << endl;
         o << "<value ref=\"CRES\">" << A.data[i]->CRES[j] << "</value>" << endl;
         o << "<value ref=\"CWETARES(1)\">" << A.data[i]->CWETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"CWETARES(2)\">" << A.data[i]->CWETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"CWRES\">" << A.data[i]->CWRES[j] << "</value>" << endl;
         o << "<value ref=\"DV\">" << A.data[i]->DV[j] << "</value>" << endl;
         o << "<value ref=\"EPS(1)\">" << A.data[i]->EPS[j][0] << "</value>" << endl;
         o << "<value ref=\"ETA(1)\">" << A.data[i]->ETA[j][0] << "</value>" << endl;
         o << "<value ref=\"ETA(2)\">" << A.data[i]->ETA[j][1] << "</value>" << endl;
         o << "<value ref=\"ETARES(1)\">" << A.data[i]->ETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"ETARES(2)\">" << A.data[i]->ETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"EVID\">" << A.data[i]->EVID[j] << "</value>" << endl;
         o << "<value ref=\"F\">" << A.data[i]->F[j] << "</value>" << endl;
         o << "<value ref=\"IETARES(1)\">" << A.data[i]->IETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"IETARES(2)\">" << A.data[i]->IETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"IPRED\">" << A.data[i]->IPRED[j] << "</value>" << endl;
         o << "<value ref=\"IRES\">" << A.data[i]->IRES[j] << "</value>" << endl;
         o << "<value ref=\"IWETARES(1)\">" << A.data[i]->IWETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"IWETARES(2)\">" << A.data[i]->IWETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"IWRES\">" << A.data[i]->IWRES[j] << "</value>" << endl;
         o << "<value ref=\"MDV\">" << A.data[i]->MDV[j] << "</value>" << endl;
         o << "<value ref=\"ORGDV\">" << A.data[i]->ORGDV[j] << "</value>" << endl;
         o << "<value ref=\"PETARES(1)\">" << A.data[i]->PETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"PETARES(2)\">" << A.data[i]->PETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"PPRED\">" << A.data[i]->PPRED[j] << "</value>" << endl;
         o << "<value ref=\"PRED\">" << A.data[i]->PRED[j] << "</value>" << endl;
         o << "<value ref=\"PRES\">" << A.data[i]->PRES[j] << "</value>" << endl;
         o << "<value ref=\"PWETARES(1)\">" << A.data[i]->PWETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"PWETARES(2)\">" << A.data[i]->PWETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"PWRES\">" << A.data[i]->PWRES[j] << "</value>" << endl;
         o << "<value ref=\"RES\">" << A.data[i]->RES[j] << "</value>" << endl;
         o << "<value ref=\"S\">" << A.data[i]->S[j] << "</value>" << endl;
         o << "<value ref=\"THETA(1)\">" << A.data[i]->THETA[j][0] << "</value>" << endl;
         o << "<value ref=\"THETA(2)\">" << A.data[i]->THETA[j][1] << "</value>" << endl;
         o << "<value ref=\"TIME\">" << A.data[i]->TIME[j] << "</value>" << endl;
         o << "<value ref=\"WETARES(1)\">" << A.data[i]->WETARES[j][0] << "</value>" << endl;
         o << "<value ref=\"WETARES(2)\">" << A.data[i]->WETARES[j][1] << "</value>" << endl;
         o << "<value ref=\"WRES\">" << A.data[i]->WRES[j] << "</value>" << endl;
         o << "<value ref=\"Y\">" << A.data[i]->Y[j] << "</value>" << endl;
         o << "</row>" << endl;
      }
   }
   o << "</" << "presentation_data" << ">";
};
template <class spk_ValueType>
std::ostream& operator<< ( std::ostream& o, const DataSet<spk_ValueType>& A );
#endif
