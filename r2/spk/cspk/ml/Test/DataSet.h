// Linear Model: FO 100
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
   void expand( int who, const SPK_VA::valarray<double>& measurements_who, SPK_VA::valarray<double>& records_who ) const;
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

    // Commented out by Brad on 2006-06-27
   // friend std::ostream& operator<< <spk_ValueType>( std::ostream& o, const DataSet<spk_ValueType>& A );

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
: popSize( 100 ),
  data( 100 ),
  NRecords( 100 ), // #of records for each individual
  NObservs( 100 )  // #of DVs for each individuals
{
   //------------------------------------
   // Subject <1> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[0] = 2;
char* ID_0_c[] = { "1", "1" };
   std::vector<char*> ID_0( 2 );
   copy( ID_0_c, ID_0_c+2, ID_0.begin() );
spk_ValueType TIME_0_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_0( 2 );
   copy( TIME_0_c, TIME_0_c+2, TIME_0.begin() );
spk_ValueType DV_0_c[] = { -0.0566087, 2.15878 };
   std::vector<spk_ValueType> DV_0( 2 );
   copy( DV_0_c, DV_0_c+2, DV_0.begin() );
spk_ValueType AMT_0_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_0( 2 );
   copy( AMT_0_c, AMT_0_c+2, AMT_0.begin() );
spk_ValueType MDV_0_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_0( 2 );
   copy( MDV_0_c, MDV_0_c+2, MDV_0.begin() );
int EVID_0_c[] = { 0, 0 };
   std::vector<int> EVID_0( 2 );
   copy( EVID_0_c, EVID_0_c+2, EVID_0.begin() );
   data[0] = new IndData<spk_ValueType>( 2, ID_0, TIME_0, DV_0, AMT_0, MDV_0, EVID_0 );

   //------------------------------------
   // Subject <2> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[1] = 2;
char* ID_1_c[] = { "2", "2" };
   std::vector<char*> ID_1( 2 );
   copy( ID_1_c, ID_1_c+2, ID_1.begin() );
spk_ValueType TIME_1_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_1( 2 );
   copy( TIME_1_c, TIME_1_c+2, TIME_1.begin() );
spk_ValueType DV_1_c[] = { -0.00733425, 2.88191 };
   std::vector<spk_ValueType> DV_1( 2 );
   copy( DV_1_c, DV_1_c+2, DV_1.begin() );
spk_ValueType AMT_1_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_1( 2 );
   copy( AMT_1_c, AMT_1_c+2, AMT_1.begin() );
spk_ValueType MDV_1_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_1( 2 );
   copy( MDV_1_c, MDV_1_c+2, MDV_1.begin() );
int EVID_1_c[] = { 0, 0 };
   std::vector<int> EVID_1( 2 );
   copy( EVID_1_c, EVID_1_c+2, EVID_1.begin() );
   data[1] = new IndData<spk_ValueType>( 2, ID_1, TIME_1, DV_1, AMT_1, MDV_1, EVID_1 );

   //------------------------------------
   // Subject <3> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[2] = 2;
char* ID_2_c[] = { "3", "3" };
   std::vector<char*> ID_2( 2 );
   copy( ID_2_c, ID_2_c+2, ID_2.begin() );
spk_ValueType TIME_2_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_2( 2 );
   copy( TIME_2_c, TIME_2_c+2, TIME_2.begin() );
spk_ValueType DV_2_c[] = { 0.075537, 0.831321 };
   std::vector<spk_ValueType> DV_2( 2 );
   copy( DV_2_c, DV_2_c+2, DV_2.begin() );
spk_ValueType AMT_2_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_2( 2 );
   copy( AMT_2_c, AMT_2_c+2, AMT_2.begin() );
spk_ValueType MDV_2_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_2( 2 );
   copy( MDV_2_c, MDV_2_c+2, MDV_2.begin() );
int EVID_2_c[] = { 0, 0 };
   std::vector<int> EVID_2( 2 );
   copy( EVID_2_c, EVID_2_c+2, EVID_2.begin() );
   data[2] = new IndData<spk_ValueType>( 2, ID_2, TIME_2, DV_2, AMT_2, MDV_2, EVID_2 );

   //------------------------------------
   // Subject <4> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[3] = 2;
char* ID_3_c[] = { "4", "4" };
   std::vector<char*> ID_3( 2 );
   copy( ID_3_c, ID_3_c+2, ID_3.begin() );
spk_ValueType TIME_3_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_3( 2 );
   copy( TIME_3_c, TIME_3_c+2, TIME_3.begin() );
spk_ValueType DV_3_c[] = { 0.120011, -0.588709 };
   std::vector<spk_ValueType> DV_3( 2 );
   copy( DV_3_c, DV_3_c+2, DV_3.begin() );
spk_ValueType AMT_3_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_3( 2 );
   copy( AMT_3_c, AMT_3_c+2, AMT_3.begin() );
spk_ValueType MDV_3_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_3( 2 );
   copy( MDV_3_c, MDV_3_c+2, MDV_3.begin() );
int EVID_3_c[] = { 0, 0 };
   std::vector<int> EVID_3( 2 );
   copy( EVID_3_c, EVID_3_c+2, EVID_3.begin() );
   data[3] = new IndData<spk_ValueType>( 2, ID_3, TIME_3, DV_3, AMT_3, MDV_3, EVID_3 );

   //------------------------------------
   // Subject <5> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[4] = 2;
char* ID_4_c[] = { "5", "5" };
   std::vector<char*> ID_4( 2 );
   copy( ID_4_c, ID_4_c+2, ID_4.begin() );
spk_ValueType TIME_4_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_4( 2 );
   copy( TIME_4_c, TIME_4_c+2, TIME_4.begin() );
spk_ValueType DV_4_c[] = { 0.0157215, -0.407271 };
   std::vector<spk_ValueType> DV_4( 2 );
   copy( DV_4_c, DV_4_c+2, DV_4.begin() );
spk_ValueType AMT_4_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_4( 2 );
   copy( AMT_4_c, AMT_4_c+2, AMT_4.begin() );
spk_ValueType MDV_4_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_4( 2 );
   copy( MDV_4_c, MDV_4_c+2, MDV_4.begin() );
int EVID_4_c[] = { 0, 0 };
   std::vector<int> EVID_4( 2 );
   copy( EVID_4_c, EVID_4_c+2, EVID_4.begin() );
   data[4] = new IndData<spk_ValueType>( 2, ID_4, TIME_4, DV_4, AMT_4, MDV_4, EVID_4 );

   //------------------------------------
   // Subject <6> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[5] = 2;
char* ID_5_c[] = { "6", "6" };
   std::vector<char*> ID_5( 2 );
   copy( ID_5_c, ID_5_c+2, ID_5.begin() );
spk_ValueType TIME_5_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_5( 2 );
   copy( TIME_5_c, TIME_5_c+2, TIME_5.begin() );
spk_ValueType DV_5_c[] = { -0.02444, 1.93778 };
   std::vector<spk_ValueType> DV_5( 2 );
   copy( DV_5_c, DV_5_c+2, DV_5.begin() );
spk_ValueType AMT_5_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_5( 2 );
   copy( AMT_5_c, AMT_5_c+2, AMT_5.begin() );
spk_ValueType MDV_5_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_5( 2 );
   copy( MDV_5_c, MDV_5_c+2, MDV_5.begin() );
int EVID_5_c[] = { 0, 0 };
   std::vector<int> EVID_5( 2 );
   copy( EVID_5_c, EVID_5_c+2, EVID_5.begin() );
   data[5] = new IndData<spk_ValueType>( 2, ID_5, TIME_5, DV_5, AMT_5, MDV_5, EVID_5 );

   //------------------------------------
   // Subject <7> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[6] = 2;
char* ID_6_c[] = { "7", "7" };
   std::vector<char*> ID_6( 2 );
   copy( ID_6_c, ID_6_c+2, ID_6.begin() );
spk_ValueType TIME_6_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_6( 2 );
   copy( TIME_6_c, TIME_6_c+2, TIME_6.begin() );
spk_ValueType DV_6_c[] = { -0.141565, -1.48958 };
   std::vector<spk_ValueType> DV_6( 2 );
   copy( DV_6_c, DV_6_c+2, DV_6.begin() );
spk_ValueType AMT_6_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_6( 2 );
   copy( AMT_6_c, AMT_6_c+2, AMT_6.begin() );
spk_ValueType MDV_6_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_6( 2 );
   copy( MDV_6_c, MDV_6_c+2, MDV_6.begin() );
int EVID_6_c[] = { 0, 0 };
   std::vector<int> EVID_6( 2 );
   copy( EVID_6_c, EVID_6_c+2, EVID_6.begin() );
   data[6] = new IndData<spk_ValueType>( 2, ID_6, TIME_6, DV_6, AMT_6, MDV_6, EVID_6 );

   //------------------------------------
   // Subject <8> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[7] = 2;
char* ID_7_c[] = { "8", "8" };
   std::vector<char*> ID_7( 2 );
   copy( ID_7_c, ID_7_c+2, ID_7.begin() );
spk_ValueType TIME_7_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_7( 2 );
   copy( TIME_7_c, TIME_7_c+2, TIME_7.begin() );
spk_ValueType DV_7_c[] = { -0.159169, 1.88597 };
   std::vector<spk_ValueType> DV_7( 2 );
   copy( DV_7_c, DV_7_c+2, DV_7.begin() );
spk_ValueType AMT_7_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_7( 2 );
   copy( AMT_7_c, AMT_7_c+2, AMT_7.begin() );
spk_ValueType MDV_7_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_7( 2 );
   copy( MDV_7_c, MDV_7_c+2, MDV_7.begin() );
int EVID_7_c[] = { 0, 0 };
   std::vector<int> EVID_7( 2 );
   copy( EVID_7_c, EVID_7_c+2, EVID_7.begin() );
   data[7] = new IndData<spk_ValueType>( 2, ID_7, TIME_7, DV_7, AMT_7, MDV_7, EVID_7 );

   //------------------------------------
   // Subject <9> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[8] = 2;
char* ID_8_c[] = { "9", "9" };
   std::vector<char*> ID_8( 2 );
   copy( ID_8_c, ID_8_c+2, ID_8.begin() );
spk_ValueType TIME_8_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_8( 2 );
   copy( TIME_8_c, TIME_8_c+2, TIME_8.begin() );
spk_ValueType DV_8_c[] = { -0.133682, 2.06994 };
   std::vector<spk_ValueType> DV_8( 2 );
   copy( DV_8_c, DV_8_c+2, DV_8.begin() );
spk_ValueType AMT_8_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_8( 2 );
   copy( AMT_8_c, AMT_8_c+2, AMT_8.begin() );
spk_ValueType MDV_8_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_8( 2 );
   copy( MDV_8_c, MDV_8_c+2, MDV_8.begin() );
int EVID_8_c[] = { 0, 0 };
   std::vector<int> EVID_8( 2 );
   copy( EVID_8_c, EVID_8_c+2, EVID_8.begin() );
   data[8] = new IndData<spk_ValueType>( 2, ID_8, TIME_8, DV_8, AMT_8, MDV_8, EVID_8 );

   //------------------------------------
   // Subject <10> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[9] = 2;
char* ID_9_c[] = { "10", "10" };
   std::vector<char*> ID_9( 2 );
   copy( ID_9_c, ID_9_c+2, ID_9.begin() );
spk_ValueType TIME_9_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_9( 2 );
   copy( TIME_9_c, TIME_9_c+2, TIME_9.begin() );
spk_ValueType DV_9_c[] = { 0.0132823, 2.25377 };
   std::vector<spk_ValueType> DV_9( 2 );
   copy( DV_9_c, DV_9_c+2, DV_9.begin() );
spk_ValueType AMT_9_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_9( 2 );
   copy( AMT_9_c, AMT_9_c+2, AMT_9.begin() );
spk_ValueType MDV_9_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_9( 2 );
   copy( MDV_9_c, MDV_9_c+2, MDV_9.begin() );
int EVID_9_c[] = { 0, 0 };
   std::vector<int> EVID_9( 2 );
   copy( EVID_9_c, EVID_9_c+2, EVID_9.begin() );
   data[9] = new IndData<spk_ValueType>( 2, ID_9, TIME_9, DV_9, AMT_9, MDV_9, EVID_9 );

   //------------------------------------
   // Subject <11> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[10] = 2;
char* ID_10_c[] = { "11", "11" };
   std::vector<char*> ID_10( 2 );
   copy( ID_10_c, ID_10_c+2, ID_10.begin() );
spk_ValueType TIME_10_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_10( 2 );
   copy( TIME_10_c, TIME_10_c+2, TIME_10.begin() );
spk_ValueType DV_10_c[] = { -0.121345, -0.322666 };
   std::vector<spk_ValueType> DV_10( 2 );
   copy( DV_10_c, DV_10_c+2, DV_10.begin() );
spk_ValueType AMT_10_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_10( 2 );
   copy( AMT_10_c, AMT_10_c+2, AMT_10.begin() );
spk_ValueType MDV_10_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_10( 2 );
   copy( MDV_10_c, MDV_10_c+2, MDV_10.begin() );
int EVID_10_c[] = { 0, 0 };
   std::vector<int> EVID_10( 2 );
   copy( EVID_10_c, EVID_10_c+2, EVID_10.begin() );
   data[10] = new IndData<spk_ValueType>( 2, ID_10, TIME_10, DV_10, AMT_10, MDV_10, EVID_10 );

   //------------------------------------
   // Subject <12> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[11] = 2;
char* ID_11_c[] = { "12", "12" };
   std::vector<char*> ID_11( 2 );
   copy( ID_11_c, ID_11_c+2, ID_11.begin() );
spk_ValueType TIME_11_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_11( 2 );
   copy( TIME_11_c, TIME_11_c+2, TIME_11.begin() );
spk_ValueType DV_11_c[] = { 0.154709, 1.71706 };
   std::vector<spk_ValueType> DV_11( 2 );
   copy( DV_11_c, DV_11_c+2, DV_11.begin() );
spk_ValueType AMT_11_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_11( 2 );
   copy( AMT_11_c, AMT_11_c+2, AMT_11.begin() );
spk_ValueType MDV_11_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_11( 2 );
   copy( MDV_11_c, MDV_11_c+2, MDV_11.begin() );
int EVID_11_c[] = { 0, 0 };
   std::vector<int> EVID_11( 2 );
   copy( EVID_11_c, EVID_11_c+2, EVID_11.begin() );
   data[11] = new IndData<spk_ValueType>( 2, ID_11, TIME_11, DV_11, AMT_11, MDV_11, EVID_11 );

   //------------------------------------
   // Subject <13> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[12] = 2;
char* ID_12_c[] = { "13", "13" };
   std::vector<char*> ID_12( 2 );
   copy( ID_12_c, ID_12_c+2, ID_12.begin() );
spk_ValueType TIME_12_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_12( 2 );
   copy( TIME_12_c, TIME_12_c+2, TIME_12.begin() );
spk_ValueType DV_12_c[] = { -0.324364, 1.12677 };
   std::vector<spk_ValueType> DV_12( 2 );
   copy( DV_12_c, DV_12_c+2, DV_12.begin() );
spk_ValueType AMT_12_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_12( 2 );
   copy( AMT_12_c, AMT_12_c+2, AMT_12.begin() );
spk_ValueType MDV_12_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_12( 2 );
   copy( MDV_12_c, MDV_12_c+2, MDV_12.begin() );
int EVID_12_c[] = { 0, 0 };
   std::vector<int> EVID_12( 2 );
   copy( EVID_12_c, EVID_12_c+2, EVID_12.begin() );
   data[12] = new IndData<spk_ValueType>( 2, ID_12, TIME_12, DV_12, AMT_12, MDV_12, EVID_12 );

   //------------------------------------
   // Subject <14> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[13] = 2;
char* ID_13_c[] = { "14", "14" };
   std::vector<char*> ID_13( 2 );
   copy( ID_13_c, ID_13_c+2, ID_13.begin() );
spk_ValueType TIME_13_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_13( 2 );
   copy( TIME_13_c, TIME_13_c+2, TIME_13.begin() );
spk_ValueType DV_13_c[] = { -0.00295516, 0.823584 };
   std::vector<spk_ValueType> DV_13( 2 );
   copy( DV_13_c, DV_13_c+2, DV_13.begin() );
spk_ValueType AMT_13_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_13( 2 );
   copy( AMT_13_c, AMT_13_c+2, AMT_13.begin() );
spk_ValueType MDV_13_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_13( 2 );
   copy( MDV_13_c, MDV_13_c+2, MDV_13.begin() );
int EVID_13_c[] = { 0, 0 };
   std::vector<int> EVID_13( 2 );
   copy( EVID_13_c, EVID_13_c+2, EVID_13.begin() );
   data[13] = new IndData<spk_ValueType>( 2, ID_13, TIME_13, DV_13, AMT_13, MDV_13, EVID_13 );

   //------------------------------------
   // Subject <15> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[14] = 2;
char* ID_14_c[] = { "15", "15" };
   std::vector<char*> ID_14( 2 );
   copy( ID_14_c, ID_14_c+2, ID_14.begin() );
spk_ValueType TIME_14_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_14( 2 );
   copy( TIME_14_c, TIME_14_c+2, TIME_14.begin() );
spk_ValueType DV_14_c[] = { -0.093999, 0.266634 };
   std::vector<spk_ValueType> DV_14( 2 );
   copy( DV_14_c, DV_14_c+2, DV_14.begin() );
spk_ValueType AMT_14_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_14( 2 );
   copy( AMT_14_c, AMT_14_c+2, AMT_14.begin() );
spk_ValueType MDV_14_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_14( 2 );
   copy( MDV_14_c, MDV_14_c+2, MDV_14.begin() );
int EVID_14_c[] = { 0, 0 };
   std::vector<int> EVID_14( 2 );
   copy( EVID_14_c, EVID_14_c+2, EVID_14.begin() );
   data[14] = new IndData<spk_ValueType>( 2, ID_14, TIME_14, DV_14, AMT_14, MDV_14, EVID_14 );

   //------------------------------------
   // Subject <16> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[15] = 2;
char* ID_15_c[] = { "16", "16" };
   std::vector<char*> ID_15( 2 );
   copy( ID_15_c, ID_15_c+2, ID_15.begin() );
spk_ValueType TIME_15_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_15( 2 );
   copy( TIME_15_c, TIME_15_c+2, TIME_15.begin() );
spk_ValueType DV_15_c[] = { -0.0052742, 2.45519 };
   std::vector<spk_ValueType> DV_15( 2 );
   copy( DV_15_c, DV_15_c+2, DV_15.begin() );
spk_ValueType AMT_15_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_15( 2 );
   copy( AMT_15_c, AMT_15_c+2, AMT_15.begin() );
spk_ValueType MDV_15_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_15( 2 );
   copy( MDV_15_c, MDV_15_c+2, MDV_15.begin() );
int EVID_15_c[] = { 0, 0 };
   std::vector<int> EVID_15( 2 );
   copy( EVID_15_c, EVID_15_c+2, EVID_15.begin() );
   data[15] = new IndData<spk_ValueType>( 2, ID_15, TIME_15, DV_15, AMT_15, MDV_15, EVID_15 );

   //------------------------------------
   // Subject <17> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[16] = 2;
char* ID_16_c[] = { "17", "17" };
   std::vector<char*> ID_16( 2 );
   copy( ID_16_c, ID_16_c+2, ID_16.begin() );
spk_ValueType TIME_16_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_16( 2 );
   copy( TIME_16_c, TIME_16_c+2, TIME_16.begin() );
spk_ValueType DV_16_c[] = { -0.223906, -0.650243 };
   std::vector<spk_ValueType> DV_16( 2 );
   copy( DV_16_c, DV_16_c+2, DV_16.begin() );
spk_ValueType AMT_16_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_16( 2 );
   copy( AMT_16_c, AMT_16_c+2, AMT_16.begin() );
spk_ValueType MDV_16_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_16( 2 );
   copy( MDV_16_c, MDV_16_c+2, MDV_16.begin() );
int EVID_16_c[] = { 0, 0 };
   std::vector<int> EVID_16( 2 );
   copy( EVID_16_c, EVID_16_c+2, EVID_16.begin() );
   data[16] = new IndData<spk_ValueType>( 2, ID_16, TIME_16, DV_16, AMT_16, MDV_16, EVID_16 );

   //------------------------------------
   // Subject <18> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[17] = 2;
char* ID_17_c[] = { "18", "18" };
   std::vector<char*> ID_17( 2 );
   copy( ID_17_c, ID_17_c+2, ID_17.begin() );
spk_ValueType TIME_17_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_17( 2 );
   copy( TIME_17_c, TIME_17_c+2, TIME_17.begin() );
spk_ValueType DV_17_c[] = { 0.0792584, 2.68461 };
   std::vector<spk_ValueType> DV_17( 2 );
   copy( DV_17_c, DV_17_c+2, DV_17.begin() );
spk_ValueType AMT_17_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_17( 2 );
   copy( AMT_17_c, AMT_17_c+2, AMT_17.begin() );
spk_ValueType MDV_17_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_17( 2 );
   copy( MDV_17_c, MDV_17_c+2, MDV_17.begin() );
int EVID_17_c[] = { 0, 0 };
   std::vector<int> EVID_17( 2 );
   copy( EVID_17_c, EVID_17_c+2, EVID_17.begin() );
   data[17] = new IndData<spk_ValueType>( 2, ID_17, TIME_17, DV_17, AMT_17, MDV_17, EVID_17 );

   //------------------------------------
   // Subject <19> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[18] = 2;
char* ID_18_c[] = { "19", "19" };
   std::vector<char*> ID_18( 2 );
   copy( ID_18_c, ID_18_c+2, ID_18.begin() );
spk_ValueType TIME_18_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_18( 2 );
   copy( TIME_18_c, TIME_18_c+2, TIME_18.begin() );
spk_ValueType DV_18_c[] = { -0.120322, -1.90754 };
   std::vector<spk_ValueType> DV_18( 2 );
   copy( DV_18_c, DV_18_c+2, DV_18.begin() );
spk_ValueType AMT_18_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_18( 2 );
   copy( AMT_18_c, AMT_18_c+2, AMT_18.begin() );
spk_ValueType MDV_18_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_18( 2 );
   copy( MDV_18_c, MDV_18_c+2, MDV_18.begin() );
int EVID_18_c[] = { 0, 0 };
   std::vector<int> EVID_18( 2 );
   copy( EVID_18_c, EVID_18_c+2, EVID_18.begin() );
   data[18] = new IndData<spk_ValueType>( 2, ID_18, TIME_18, DV_18, AMT_18, MDV_18, EVID_18 );

   //------------------------------------
   // Subject <20> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[19] = 2;
char* ID_19_c[] = { "20", "20" };
   std::vector<char*> ID_19( 2 );
   copy( ID_19_c, ID_19_c+2, ID_19.begin() );
spk_ValueType TIME_19_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_19( 2 );
   copy( TIME_19_c, TIME_19_c+2, TIME_19.begin() );
spk_ValueType DV_19_c[] = { 0.0345411, -0.448381 };
   std::vector<spk_ValueType> DV_19( 2 );
   copy( DV_19_c, DV_19_c+2, DV_19.begin() );
spk_ValueType AMT_19_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_19( 2 );
   copy( AMT_19_c, AMT_19_c+2, AMT_19.begin() );
spk_ValueType MDV_19_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_19( 2 );
   copy( MDV_19_c, MDV_19_c+2, MDV_19.begin() );
int EVID_19_c[] = { 0, 0 };
   std::vector<int> EVID_19( 2 );
   copy( EVID_19_c, EVID_19_c+2, EVID_19.begin() );
   data[19] = new IndData<spk_ValueType>( 2, ID_19, TIME_19, DV_19, AMT_19, MDV_19, EVID_19 );

   //------------------------------------
   // Subject <21> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[20] = 2;
char* ID_20_c[] = { "21", "21" };
   std::vector<char*> ID_20( 2 );
   copy( ID_20_c, ID_20_c+2, ID_20.begin() );
spk_ValueType TIME_20_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_20( 2 );
   copy( TIME_20_c, TIME_20_c+2, TIME_20.begin() );
spk_ValueType DV_20_c[] = { 0.0400333, 0.68418 };
   std::vector<spk_ValueType> DV_20( 2 );
   copy( DV_20_c, DV_20_c+2, DV_20.begin() );
spk_ValueType AMT_20_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_20( 2 );
   copy( AMT_20_c, AMT_20_c+2, AMT_20.begin() );
spk_ValueType MDV_20_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_20( 2 );
   copy( MDV_20_c, MDV_20_c+2, MDV_20.begin() );
int EVID_20_c[] = { 0, 0 };
   std::vector<int> EVID_20( 2 );
   copy( EVID_20_c, EVID_20_c+2, EVID_20.begin() );
   data[20] = new IndData<spk_ValueType>( 2, ID_20, TIME_20, DV_20, AMT_20, MDV_20, EVID_20 );

   //------------------------------------
   // Subject <22> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[21] = 2;
char* ID_21_c[] = { "22", "22" };
   std::vector<char*> ID_21( 2 );
   copy( ID_21_c, ID_21_c+2, ID_21.begin() );
spk_ValueType TIME_21_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_21( 2 );
   copy( TIME_21_c, TIME_21_c+2, TIME_21.begin() );
spk_ValueType DV_21_c[] = { -0.140115, 0.978679 };
   std::vector<spk_ValueType> DV_21( 2 );
   copy( DV_21_c, DV_21_c+2, DV_21.begin() );
spk_ValueType AMT_21_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_21( 2 );
   copy( AMT_21_c, AMT_21_c+2, AMT_21.begin() );
spk_ValueType MDV_21_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_21( 2 );
   copy( MDV_21_c, MDV_21_c+2, MDV_21.begin() );
int EVID_21_c[] = { 0, 0 };
   std::vector<int> EVID_21( 2 );
   copy( EVID_21_c, EVID_21_c+2, EVID_21.begin() );
   data[21] = new IndData<spk_ValueType>( 2, ID_21, TIME_21, DV_21, AMT_21, MDV_21, EVID_21 );

   //------------------------------------
   // Subject <23> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[22] = 2;
char* ID_22_c[] = { "23", "23" };
   std::vector<char*> ID_22( 2 );
   copy( ID_22_c, ID_22_c+2, ID_22.begin() );
spk_ValueType TIME_22_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_22( 2 );
   copy( TIME_22_c, TIME_22_c+2, TIME_22.begin() );
spk_ValueType DV_22_c[] = { -0.00168632, 1.53082 };
   std::vector<spk_ValueType> DV_22( 2 );
   copy( DV_22_c, DV_22_c+2, DV_22.begin() );
spk_ValueType AMT_22_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_22( 2 );
   copy( AMT_22_c, AMT_22_c+2, AMT_22.begin() );
spk_ValueType MDV_22_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_22( 2 );
   copy( MDV_22_c, MDV_22_c+2, MDV_22.begin() );
int EVID_22_c[] = { 0, 0 };
   std::vector<int> EVID_22( 2 );
   copy( EVID_22_c, EVID_22_c+2, EVID_22.begin() );
   data[22] = new IndData<spk_ValueType>( 2, ID_22, TIME_22, DV_22, AMT_22, MDV_22, EVID_22 );

   //------------------------------------
   // Subject <24> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[23] = 2;
char* ID_23_c[] = { "24", "24" };
   std::vector<char*> ID_23( 2 );
   copy( ID_23_c, ID_23_c+2, ID_23.begin() );
spk_ValueType TIME_23_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_23( 2 );
   copy( TIME_23_c, TIME_23_c+2, TIME_23.begin() );
spk_ValueType DV_23_c[] = { -0.0495205, 1.41544 };
   std::vector<spk_ValueType> DV_23( 2 );
   copy( DV_23_c, DV_23_c+2, DV_23.begin() );
spk_ValueType AMT_23_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_23( 2 );
   copy( AMT_23_c, AMT_23_c+2, AMT_23.begin() );
spk_ValueType MDV_23_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_23( 2 );
   copy( MDV_23_c, MDV_23_c+2, MDV_23.begin() );
int EVID_23_c[] = { 0, 0 };
   std::vector<int> EVID_23( 2 );
   copy( EVID_23_c, EVID_23_c+2, EVID_23.begin() );
   data[23] = new IndData<spk_ValueType>( 2, ID_23, TIME_23, DV_23, AMT_23, MDV_23, EVID_23 );

   //------------------------------------
   // Subject <25> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[24] = 2;
char* ID_24_c[] = { "25", "25" };
   std::vector<char*> ID_24( 2 );
   copy( ID_24_c, ID_24_c+2, ID_24.begin() );
spk_ValueType TIME_24_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_24( 2 );
   copy( TIME_24_c, TIME_24_c+2, TIME_24.begin() );
spk_ValueType DV_24_c[] = { 0.0129166, 0.403814 };
   std::vector<spk_ValueType> DV_24( 2 );
   copy( DV_24_c, DV_24_c+2, DV_24.begin() );
spk_ValueType AMT_24_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_24( 2 );
   copy( AMT_24_c, AMT_24_c+2, AMT_24.begin() );
spk_ValueType MDV_24_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_24( 2 );
   copy( MDV_24_c, MDV_24_c+2, MDV_24.begin() );
int EVID_24_c[] = { 0, 0 };
   std::vector<int> EVID_24( 2 );
   copy( EVID_24_c, EVID_24_c+2, EVID_24.begin() );
   data[24] = new IndData<spk_ValueType>( 2, ID_24, TIME_24, DV_24, AMT_24, MDV_24, EVID_24 );

   //------------------------------------
   // Subject <26> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[25] = 2;
char* ID_25_c[] = { "26", "26" };
   std::vector<char*> ID_25( 2 );
   copy( ID_25_c, ID_25_c+2, ID_25.begin() );
spk_ValueType TIME_25_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_25( 2 );
   copy( TIME_25_c, TIME_25_c+2, TIME_25.begin() );
spk_ValueType DV_25_c[] = { -0.158593, -0.414689 };
   std::vector<spk_ValueType> DV_25( 2 );
   copy( DV_25_c, DV_25_c+2, DV_25.begin() );
spk_ValueType AMT_25_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_25( 2 );
   copy( AMT_25_c, AMT_25_c+2, AMT_25.begin() );
spk_ValueType MDV_25_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_25( 2 );
   copy( MDV_25_c, MDV_25_c+2, MDV_25.begin() );
int EVID_25_c[] = { 0, 0 };
   std::vector<int> EVID_25( 2 );
   copy( EVID_25_c, EVID_25_c+2, EVID_25.begin() );
   data[25] = new IndData<spk_ValueType>( 2, ID_25, TIME_25, DV_25, AMT_25, MDV_25, EVID_25 );

   //------------------------------------
   // Subject <27> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[26] = 2;
char* ID_26_c[] = { "27", "27" };
   std::vector<char*> ID_26( 2 );
   copy( ID_26_c, ID_26_c+2, ID_26.begin() );
spk_ValueType TIME_26_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_26( 2 );
   copy( TIME_26_c, TIME_26_c+2, TIME_26.begin() );
spk_ValueType DV_26_c[] = { -0.0924305, -0.113211 };
   std::vector<spk_ValueType> DV_26( 2 );
   copy( DV_26_c, DV_26_c+2, DV_26.begin() );
spk_ValueType AMT_26_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_26( 2 );
   copy( AMT_26_c, AMT_26_c+2, AMT_26.begin() );
spk_ValueType MDV_26_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_26( 2 );
   copy( MDV_26_c, MDV_26_c+2, MDV_26.begin() );
int EVID_26_c[] = { 0, 0 };
   std::vector<int> EVID_26( 2 );
   copy( EVID_26_c, EVID_26_c+2, EVID_26.begin() );
   data[26] = new IndData<spk_ValueType>( 2, ID_26, TIME_26, DV_26, AMT_26, MDV_26, EVID_26 );

   //------------------------------------
   // Subject <28> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[27] = 2;
char* ID_27_c[] = { "28", "28" };
   std::vector<char*> ID_27( 2 );
   copy( ID_27_c, ID_27_c+2, ID_27.begin() );
spk_ValueType TIME_27_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_27( 2 );
   copy( TIME_27_c, TIME_27_c+2, TIME_27.begin() );
spk_ValueType DV_27_c[] = { 0.132604, 1.4415 };
   std::vector<spk_ValueType> DV_27( 2 );
   copy( DV_27_c, DV_27_c+2, DV_27.begin() );
spk_ValueType AMT_27_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_27( 2 );
   copy( AMT_27_c, AMT_27_c+2, AMT_27.begin() );
spk_ValueType MDV_27_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_27( 2 );
   copy( MDV_27_c, MDV_27_c+2, MDV_27.begin() );
int EVID_27_c[] = { 0, 0 };
   std::vector<int> EVID_27( 2 );
   copy( EVID_27_c, EVID_27_c+2, EVID_27.begin() );
   data[27] = new IndData<spk_ValueType>( 2, ID_27, TIME_27, DV_27, AMT_27, MDV_27, EVID_27 );

   //------------------------------------
   // Subject <29> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[28] = 2;
char* ID_28_c[] = { "29", "29" };
   std::vector<char*> ID_28( 2 );
   copy( ID_28_c, ID_28_c+2, ID_28.begin() );
spk_ValueType TIME_28_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_28( 2 );
   copy( TIME_28_c, TIME_28_c+2, TIME_28.begin() );
spk_ValueType DV_28_c[] = { -0.0466769, 1.87893 };
   std::vector<spk_ValueType> DV_28( 2 );
   copy( DV_28_c, DV_28_c+2, DV_28.begin() );
spk_ValueType AMT_28_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_28( 2 );
   copy( AMT_28_c, AMT_28_c+2, AMT_28.begin() );
spk_ValueType MDV_28_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_28( 2 );
   copy( MDV_28_c, MDV_28_c+2, MDV_28.begin() );
int EVID_28_c[] = { 0, 0 };
   std::vector<int> EVID_28( 2 );
   copy( EVID_28_c, EVID_28_c+2, EVID_28.begin() );
   data[28] = new IndData<spk_ValueType>( 2, ID_28, TIME_28, DV_28, AMT_28, MDV_28, EVID_28 );

   //------------------------------------
   // Subject <30> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[29] = 2;
char* ID_29_c[] = { "30", "30" };
   std::vector<char*> ID_29( 2 );
   copy( ID_29_c, ID_29_c+2, ID_29.begin() );
spk_ValueType TIME_29_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_29( 2 );
   copy( TIME_29_c, TIME_29_c+2, TIME_29.begin() );
spk_ValueType DV_29_c[] = { 0.0846816, 0.282284 };
   std::vector<spk_ValueType> DV_29( 2 );
   copy( DV_29_c, DV_29_c+2, DV_29.begin() );
spk_ValueType AMT_29_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_29( 2 );
   copy( AMT_29_c, AMT_29_c+2, AMT_29.begin() );
spk_ValueType MDV_29_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_29( 2 );
   copy( MDV_29_c, MDV_29_c+2, MDV_29.begin() );
int EVID_29_c[] = { 0, 0 };
   std::vector<int> EVID_29( 2 );
   copy( EVID_29_c, EVID_29_c+2, EVID_29.begin() );
   data[29] = new IndData<spk_ValueType>( 2, ID_29, TIME_29, DV_29, AMT_29, MDV_29, EVID_29 );

   //------------------------------------
   // Subject <31> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[30] = 2;
char* ID_30_c[] = { "31", "31" };
   std::vector<char*> ID_30( 2 );
   copy( ID_30_c, ID_30_c+2, ID_30.begin() );
spk_ValueType TIME_30_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_30( 2 );
   copy( TIME_30_c, TIME_30_c+2, TIME_30.begin() );
spk_ValueType DV_30_c[] = { -0.0264248, 1.5679 };
   std::vector<spk_ValueType> DV_30( 2 );
   copy( DV_30_c, DV_30_c+2, DV_30.begin() );
spk_ValueType AMT_30_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_30( 2 );
   copy( AMT_30_c, AMT_30_c+2, AMT_30.begin() );
spk_ValueType MDV_30_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_30( 2 );
   copy( MDV_30_c, MDV_30_c+2, MDV_30.begin() );
int EVID_30_c[] = { 0, 0 };
   std::vector<int> EVID_30( 2 );
   copy( EVID_30_c, EVID_30_c+2, EVID_30.begin() );
   data[30] = new IndData<spk_ValueType>( 2, ID_30, TIME_30, DV_30, AMT_30, MDV_30, EVID_30 );

   //------------------------------------
   // Subject <32> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[31] = 2;
char* ID_31_c[] = { "32", "32" };
   std::vector<char*> ID_31( 2 );
   copy( ID_31_c, ID_31_c+2, ID_31.begin() );
spk_ValueType TIME_31_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_31( 2 );
   copy( TIME_31_c, TIME_31_c+2, TIME_31.begin() );
spk_ValueType DV_31_c[] = { -0.0474415, 0.0601454 };
   std::vector<spk_ValueType> DV_31( 2 );
   copy( DV_31_c, DV_31_c+2, DV_31.begin() );
spk_ValueType AMT_31_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_31( 2 );
   copy( AMT_31_c, AMT_31_c+2, AMT_31.begin() );
spk_ValueType MDV_31_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_31( 2 );
   copy( MDV_31_c, MDV_31_c+2, MDV_31.begin() );
int EVID_31_c[] = { 0, 0 };
   std::vector<int> EVID_31( 2 );
   copy( EVID_31_c, EVID_31_c+2, EVID_31.begin() );
   data[31] = new IndData<spk_ValueType>( 2, ID_31, TIME_31, DV_31, AMT_31, MDV_31, EVID_31 );

   //------------------------------------
   // Subject <33> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[32] = 2;
char* ID_32_c[] = { "33", "33" };
   std::vector<char*> ID_32( 2 );
   copy( ID_32_c, ID_32_c+2, ID_32.begin() );
spk_ValueType TIME_32_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_32( 2 );
   copy( TIME_32_c, TIME_32_c+2, TIME_32.begin() );
spk_ValueType DV_32_c[] = { 0.155549, 1.28909 };
   std::vector<spk_ValueType> DV_32( 2 );
   copy( DV_32_c, DV_32_c+2, DV_32.begin() );
spk_ValueType AMT_32_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_32( 2 );
   copy( AMT_32_c, AMT_32_c+2, AMT_32.begin() );
spk_ValueType MDV_32_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_32( 2 );
   copy( MDV_32_c, MDV_32_c+2, MDV_32.begin() );
int EVID_32_c[] = { 0, 0 };
   std::vector<int> EVID_32( 2 );
   copy( EVID_32_c, EVID_32_c+2, EVID_32.begin() );
   data[32] = new IndData<spk_ValueType>( 2, ID_32, TIME_32, DV_32, AMT_32, MDV_32, EVID_32 );

   //------------------------------------
   // Subject <34> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[33] = 2;
char* ID_33_c[] = { "34", "34" };
   std::vector<char*> ID_33( 2 );
   copy( ID_33_c, ID_33_c+2, ID_33.begin() );
spk_ValueType TIME_33_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_33( 2 );
   copy( TIME_33_c, TIME_33_c+2, TIME_33.begin() );
spk_ValueType DV_33_c[] = { -0.0301632, 1.86845 };
   std::vector<spk_ValueType> DV_33( 2 );
   copy( DV_33_c, DV_33_c+2, DV_33.begin() );
spk_ValueType AMT_33_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_33( 2 );
   copy( AMT_33_c, AMT_33_c+2, AMT_33.begin() );
spk_ValueType MDV_33_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_33( 2 );
   copy( MDV_33_c, MDV_33_c+2, MDV_33.begin() );
int EVID_33_c[] = { 0, 0 };
   std::vector<int> EVID_33( 2 );
   copy( EVID_33_c, EVID_33_c+2, EVID_33.begin() );
   data[33] = new IndData<spk_ValueType>( 2, ID_33, TIME_33, DV_33, AMT_33, MDV_33, EVID_33 );

   //------------------------------------
   // Subject <35> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[34] = 2;
char* ID_34_c[] = { "35", "35" };
   std::vector<char*> ID_34( 2 );
   copy( ID_34_c, ID_34_c+2, ID_34.begin() );
spk_ValueType TIME_34_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_34( 2 );
   copy( TIME_34_c, TIME_34_c+2, TIME_34.begin() );
spk_ValueType DV_34_c[] = { 0.174929, 1.91197 };
   std::vector<spk_ValueType> DV_34( 2 );
   copy( DV_34_c, DV_34_c+2, DV_34.begin() );
spk_ValueType AMT_34_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_34( 2 );
   copy( AMT_34_c, AMT_34_c+2, AMT_34.begin() );
spk_ValueType MDV_34_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_34( 2 );
   copy( MDV_34_c, MDV_34_c+2, MDV_34.begin() );
int EVID_34_c[] = { 0, 0 };
   std::vector<int> EVID_34( 2 );
   copy( EVID_34_c, EVID_34_c+2, EVID_34.begin() );
   data[34] = new IndData<spk_ValueType>( 2, ID_34, TIME_34, DV_34, AMT_34, MDV_34, EVID_34 );

   //------------------------------------
   // Subject <36> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[35] = 2;
char* ID_35_c[] = { "36", "36" };
   std::vector<char*> ID_35( 2 );
   copy( ID_35_c, ID_35_c+2, ID_35.begin() );
spk_ValueType TIME_35_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_35( 2 );
   copy( TIME_35_c, TIME_35_c+2, TIME_35.begin() );
spk_ValueType DV_35_c[] = { 0.0202055, 0.690458 };
   std::vector<spk_ValueType> DV_35( 2 );
   copy( DV_35_c, DV_35_c+2, DV_35.begin() );
spk_ValueType AMT_35_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_35( 2 );
   copy( AMT_35_c, AMT_35_c+2, AMT_35.begin() );
spk_ValueType MDV_35_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_35( 2 );
   copy( MDV_35_c, MDV_35_c+2, MDV_35.begin() );
int EVID_35_c[] = { 0, 0 };
   std::vector<int> EVID_35( 2 );
   copy( EVID_35_c, EVID_35_c+2, EVID_35.begin() );
   data[35] = new IndData<spk_ValueType>( 2, ID_35, TIME_35, DV_35, AMT_35, MDV_35, EVID_35 );

   //------------------------------------
   // Subject <37> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[36] = 2;
char* ID_36_c[] = { "37", "37" };
   std::vector<char*> ID_36( 2 );
   copy( ID_36_c, ID_36_c+2, ID_36.begin() );
spk_ValueType TIME_36_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_36( 2 );
   copy( TIME_36_c, TIME_36_c+2, TIME_36.begin() );
spk_ValueType DV_36_c[] = { -0.0291555, -0.807475 };
   std::vector<spk_ValueType> DV_36( 2 );
   copy( DV_36_c, DV_36_c+2, DV_36.begin() );
spk_ValueType AMT_36_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_36( 2 );
   copy( AMT_36_c, AMT_36_c+2, AMT_36.begin() );
spk_ValueType MDV_36_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_36( 2 );
   copy( MDV_36_c, MDV_36_c+2, MDV_36.begin() );
int EVID_36_c[] = { 0, 0 };
   std::vector<int> EVID_36( 2 );
   copy( EVID_36_c, EVID_36_c+2, EVID_36.begin() );
   data[36] = new IndData<spk_ValueType>( 2, ID_36, TIME_36, DV_36, AMT_36, MDV_36, EVID_36 );

   //------------------------------------
   // Subject <38> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[37] = 2;
char* ID_37_c[] = { "38", "38" };
   std::vector<char*> ID_37( 2 );
   copy( ID_37_c, ID_37_c+2, ID_37.begin() );
spk_ValueType TIME_37_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_37( 2 );
   copy( TIME_37_c, TIME_37_c+2, TIME_37.begin() );
spk_ValueType DV_37_c[] = { 0.0749429, 1.82014 };
   std::vector<spk_ValueType> DV_37( 2 );
   copy( DV_37_c, DV_37_c+2, DV_37.begin() );
spk_ValueType AMT_37_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_37( 2 );
   copy( AMT_37_c, AMT_37_c+2, AMT_37.begin() );
spk_ValueType MDV_37_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_37( 2 );
   copy( MDV_37_c, MDV_37_c+2, MDV_37.begin() );
int EVID_37_c[] = { 0, 0 };
   std::vector<int> EVID_37( 2 );
   copy( EVID_37_c, EVID_37_c+2, EVID_37.begin() );
   data[37] = new IndData<spk_ValueType>( 2, ID_37, TIME_37, DV_37, AMT_37, MDV_37, EVID_37 );

   //------------------------------------
   // Subject <39> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[38] = 2;
char* ID_38_c[] = { "39", "39" };
   std::vector<char*> ID_38( 2 );
   copy( ID_38_c, ID_38_c+2, ID_38.begin() );
spk_ValueType TIME_38_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_38( 2 );
   copy( TIME_38_c, TIME_38_c+2, TIME_38.begin() );
spk_ValueType DV_38_c[] = { 0.0613985, 2.5951 };
   std::vector<spk_ValueType> DV_38( 2 );
   copy( DV_38_c, DV_38_c+2, DV_38.begin() );
spk_ValueType AMT_38_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_38( 2 );
   copy( AMT_38_c, AMT_38_c+2, AMT_38.begin() );
spk_ValueType MDV_38_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_38( 2 );
   copy( MDV_38_c, MDV_38_c+2, MDV_38.begin() );
int EVID_38_c[] = { 0, 0 };
   std::vector<int> EVID_38( 2 );
   copy( EVID_38_c, EVID_38_c+2, EVID_38.begin() );
   data[38] = new IndData<spk_ValueType>( 2, ID_38, TIME_38, DV_38, AMT_38, MDV_38, EVID_38 );

   //------------------------------------
   // Subject <40> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[39] = 2;
char* ID_39_c[] = { "40", "40" };
   std::vector<char*> ID_39( 2 );
   copy( ID_39_c, ID_39_c+2, ID_39.begin() );
spk_ValueType TIME_39_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_39( 2 );
   copy( TIME_39_c, TIME_39_c+2, TIME_39.begin() );
spk_ValueType DV_39_c[] = { -0.0368306, 1.12708 };
   std::vector<spk_ValueType> DV_39( 2 );
   copy( DV_39_c, DV_39_c+2, DV_39.begin() );
spk_ValueType AMT_39_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_39( 2 );
   copy( AMT_39_c, AMT_39_c+2, AMT_39.begin() );
spk_ValueType MDV_39_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_39( 2 );
   copy( MDV_39_c, MDV_39_c+2, MDV_39.begin() );
int EVID_39_c[] = { 0, 0 };
   std::vector<int> EVID_39( 2 );
   copy( EVID_39_c, EVID_39_c+2, EVID_39.begin() );
   data[39] = new IndData<spk_ValueType>( 2, ID_39, TIME_39, DV_39, AMT_39, MDV_39, EVID_39 );

   //------------------------------------
   // Subject <41> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[40] = 2;
char* ID_40_c[] = { "41", "41" };
   std::vector<char*> ID_40( 2 );
   copy( ID_40_c, ID_40_c+2, ID_40.begin() );
spk_ValueType TIME_40_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_40( 2 );
   copy( TIME_40_c, TIME_40_c+2, TIME_40.begin() );
spk_ValueType DV_40_c[] = { 0.0861209, 1.12036 };
   std::vector<spk_ValueType> DV_40( 2 );
   copy( DV_40_c, DV_40_c+2, DV_40.begin() );
spk_ValueType AMT_40_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_40( 2 );
   copy( AMT_40_c, AMT_40_c+2, AMT_40.begin() );
spk_ValueType MDV_40_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_40( 2 );
   copy( MDV_40_c, MDV_40_c+2, MDV_40.begin() );
int EVID_40_c[] = { 0, 0 };
   std::vector<int> EVID_40( 2 );
   copy( EVID_40_c, EVID_40_c+2, EVID_40.begin() );
   data[40] = new IndData<spk_ValueType>( 2, ID_40, TIME_40, DV_40, AMT_40, MDV_40, EVID_40 );

   //------------------------------------
   // Subject <42> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[41] = 2;
char* ID_41_c[] = { "42", "42" };
   std::vector<char*> ID_41( 2 );
   copy( ID_41_c, ID_41_c+2, ID_41.begin() );
spk_ValueType TIME_41_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_41( 2 );
   copy( TIME_41_c, TIME_41_c+2, TIME_41.begin() );
spk_ValueType DV_41_c[] = { -0.00333146, 1.83132 };
   std::vector<spk_ValueType> DV_41( 2 );
   copy( DV_41_c, DV_41_c+2, DV_41.begin() );
spk_ValueType AMT_41_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_41( 2 );
   copy( AMT_41_c, AMT_41_c+2, AMT_41.begin() );
spk_ValueType MDV_41_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_41( 2 );
   copy( MDV_41_c, MDV_41_c+2, MDV_41.begin() );
int EVID_41_c[] = { 0, 0 };
   std::vector<int> EVID_41( 2 );
   copy( EVID_41_c, EVID_41_c+2, EVID_41.begin() );
   data[41] = new IndData<spk_ValueType>( 2, ID_41, TIME_41, DV_41, AMT_41, MDV_41, EVID_41 );

   //------------------------------------
   // Subject <43> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[42] = 2;
char* ID_42_c[] = { "43", "43" };
   std::vector<char*> ID_42( 2 );
   copy( ID_42_c, ID_42_c+2, ID_42.begin() );
spk_ValueType TIME_42_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_42( 2 );
   copy( TIME_42_c, TIME_42_c+2, TIME_42.begin() );
spk_ValueType DV_42_c[] = { -0.0168309, 0.261003 };
   std::vector<spk_ValueType> DV_42( 2 );
   copy( DV_42_c, DV_42_c+2, DV_42.begin() );
spk_ValueType AMT_42_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_42( 2 );
   copy( AMT_42_c, AMT_42_c+2, AMT_42.begin() );
spk_ValueType MDV_42_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_42( 2 );
   copy( MDV_42_c, MDV_42_c+2, MDV_42.begin() );
int EVID_42_c[] = { 0, 0 };
   std::vector<int> EVID_42( 2 );
   copy( EVID_42_c, EVID_42_c+2, EVID_42.begin() );
   data[42] = new IndData<spk_ValueType>( 2, ID_42, TIME_42, DV_42, AMT_42, MDV_42, EVID_42 );

   //------------------------------------
   // Subject <44> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[43] = 2;
char* ID_43_c[] = { "44", "44" };
   std::vector<char*> ID_43( 2 );
   copy( ID_43_c, ID_43_c+2, ID_43.begin() );
spk_ValueType TIME_43_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_43( 2 );
   copy( TIME_43_c, TIME_43_c+2, TIME_43.begin() );
spk_ValueType DV_43_c[] = { -0.101137, 0.85609 };
   std::vector<spk_ValueType> DV_43( 2 );
   copy( DV_43_c, DV_43_c+2, DV_43.begin() );
spk_ValueType AMT_43_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_43( 2 );
   copy( AMT_43_c, AMT_43_c+2, AMT_43.begin() );
spk_ValueType MDV_43_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_43( 2 );
   copy( MDV_43_c, MDV_43_c+2, MDV_43.begin() );
int EVID_43_c[] = { 0, 0 };
   std::vector<int> EVID_43( 2 );
   copy( EVID_43_c, EVID_43_c+2, EVID_43.begin() );
   data[43] = new IndData<spk_ValueType>( 2, ID_43, TIME_43, DV_43, AMT_43, MDV_43, EVID_43 );

   //------------------------------------
   // Subject <45> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[44] = 2;
char* ID_44_c[] = { "45", "45" };
   std::vector<char*> ID_44( 2 );
   copy( ID_44_c, ID_44_c+2, ID_44.begin() );
spk_ValueType TIME_44_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_44( 2 );
   copy( TIME_44_c, TIME_44_c+2, TIME_44.begin() );
spk_ValueType DV_44_c[] = { 0.0623178, 1.51998 };
   std::vector<spk_ValueType> DV_44( 2 );
   copy( DV_44_c, DV_44_c+2, DV_44.begin() );
spk_ValueType AMT_44_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_44( 2 );
   copy( AMT_44_c, AMT_44_c+2, AMT_44.begin() );
spk_ValueType MDV_44_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_44( 2 );
   copy( MDV_44_c, MDV_44_c+2, MDV_44.begin() );
int EVID_44_c[] = { 0, 0 };
   std::vector<int> EVID_44( 2 );
   copy( EVID_44_c, EVID_44_c+2, EVID_44.begin() );
   data[44] = new IndData<spk_ValueType>( 2, ID_44, TIME_44, DV_44, AMT_44, MDV_44, EVID_44 );

   //------------------------------------
   // Subject <46> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[45] = 2;
char* ID_45_c[] = { "46", "46" };
   std::vector<char*> ID_45( 2 );
   copy( ID_45_c, ID_45_c+2, ID_45.begin() );
spk_ValueType TIME_45_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_45( 2 );
   copy( TIME_45_c, TIME_45_c+2, TIME_45.begin() );
spk_ValueType DV_45_c[] = { -0.00967314, 0.00733689 };
   std::vector<spk_ValueType> DV_45( 2 );
   copy( DV_45_c, DV_45_c+2, DV_45.begin() );
spk_ValueType AMT_45_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_45( 2 );
   copy( AMT_45_c, AMT_45_c+2, AMT_45.begin() );
spk_ValueType MDV_45_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_45( 2 );
   copy( MDV_45_c, MDV_45_c+2, MDV_45.begin() );
int EVID_45_c[] = { 0, 0 };
   std::vector<int> EVID_45( 2 );
   copy( EVID_45_c, EVID_45_c+2, EVID_45.begin() );
   data[45] = new IndData<spk_ValueType>( 2, ID_45, TIME_45, DV_45, AMT_45, MDV_45, EVID_45 );

   //------------------------------------
   // Subject <47> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[46] = 2;
char* ID_46_c[] = { "47", "47" };
   std::vector<char*> ID_46( 2 );
   copy( ID_46_c, ID_46_c+2, ID_46.begin() );
spk_ValueType TIME_46_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_46( 2 );
   copy( TIME_46_c, TIME_46_c+2, TIME_46.begin() );
spk_ValueType DV_46_c[] = { -0.0803036, 0.876871 };
   std::vector<spk_ValueType> DV_46( 2 );
   copy( DV_46_c, DV_46_c+2, DV_46.begin() );
spk_ValueType AMT_46_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_46( 2 );
   copy( AMT_46_c, AMT_46_c+2, AMT_46.begin() );
spk_ValueType MDV_46_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_46( 2 );
   copy( MDV_46_c, MDV_46_c+2, MDV_46.begin() );
int EVID_46_c[] = { 0, 0 };
   std::vector<int> EVID_46( 2 );
   copy( EVID_46_c, EVID_46_c+2, EVID_46.begin() );
   data[46] = new IndData<spk_ValueType>( 2, ID_46, TIME_46, DV_46, AMT_46, MDV_46, EVID_46 );

   //------------------------------------
   // Subject <48> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[47] = 2;
char* ID_47_c[] = { "48", "48" };
   std::vector<char*> ID_47( 2 );
   copy( ID_47_c, ID_47_c+2, ID_47.begin() );
spk_ValueType TIME_47_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_47( 2 );
   copy( TIME_47_c, TIME_47_c+2, TIME_47.begin() );
spk_ValueType DV_47_c[] = { 0.0550749, 0.236306 };
   std::vector<spk_ValueType> DV_47( 2 );
   copy( DV_47_c, DV_47_c+2, DV_47.begin() );
spk_ValueType AMT_47_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_47( 2 );
   copy( AMT_47_c, AMT_47_c+2, AMT_47.begin() );
spk_ValueType MDV_47_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_47( 2 );
   copy( MDV_47_c, MDV_47_c+2, MDV_47.begin() );
int EVID_47_c[] = { 0, 0 };
   std::vector<int> EVID_47( 2 );
   copy( EVID_47_c, EVID_47_c+2, EVID_47.begin() );
   data[47] = new IndData<spk_ValueType>( 2, ID_47, TIME_47, DV_47, AMT_47, MDV_47, EVID_47 );

   //------------------------------------
   // Subject <49> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[48] = 2;
char* ID_48_c[] = { "49", "49" };
   std::vector<char*> ID_48( 2 );
   copy( ID_48_c, ID_48_c+2, ID_48.begin() );
spk_ValueType TIME_48_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_48( 2 );
   copy( TIME_48_c, TIME_48_c+2, TIME_48.begin() );
spk_ValueType DV_48_c[] = { -0.00968583, 2.61409 };
   std::vector<spk_ValueType> DV_48( 2 );
   copy( DV_48_c, DV_48_c+2, DV_48.begin() );
spk_ValueType AMT_48_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_48( 2 );
   copy( AMT_48_c, AMT_48_c+2, AMT_48.begin() );
spk_ValueType MDV_48_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_48( 2 );
   copy( MDV_48_c, MDV_48_c+2, MDV_48.begin() );
int EVID_48_c[] = { 0, 0 };
   std::vector<int> EVID_48( 2 );
   copy( EVID_48_c, EVID_48_c+2, EVID_48.begin() );
   data[48] = new IndData<spk_ValueType>( 2, ID_48, TIME_48, DV_48, AMT_48, MDV_48, EVID_48 );

   //------------------------------------
   // Subject <50> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[49] = 2;
char* ID_49_c[] = { "50", "50" };
   std::vector<char*> ID_49( 2 );
   copy( ID_49_c, ID_49_c+2, ID_49.begin() );
spk_ValueType TIME_49_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_49( 2 );
   copy( TIME_49_c, TIME_49_c+2, TIME_49.begin() );
spk_ValueType DV_49_c[] = { -0.184949, 0.311032 };
   std::vector<spk_ValueType> DV_49( 2 );
   copy( DV_49_c, DV_49_c+2, DV_49.begin() );
spk_ValueType AMT_49_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_49( 2 );
   copy( AMT_49_c, AMT_49_c+2, AMT_49.begin() );
spk_ValueType MDV_49_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_49( 2 );
   copy( MDV_49_c, MDV_49_c+2, MDV_49.begin() );
int EVID_49_c[] = { 0, 0 };
   std::vector<int> EVID_49( 2 );
   copy( EVID_49_c, EVID_49_c+2, EVID_49.begin() );
   data[49] = new IndData<spk_ValueType>( 2, ID_49, TIME_49, DV_49, AMT_49, MDV_49, EVID_49 );

   //------------------------------------
   // Subject <51> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[50] = 2;
char* ID_50_c[] = { "51", "51" };
   std::vector<char*> ID_50( 2 );
   copy( ID_50_c, ID_50_c+2, ID_50.begin() );
spk_ValueType TIME_50_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_50( 2 );
   copy( TIME_50_c, TIME_50_c+2, TIME_50.begin() );
spk_ValueType DV_50_c[] = { -0.034423, 1.27541 };
   std::vector<spk_ValueType> DV_50( 2 );
   copy( DV_50_c, DV_50_c+2, DV_50.begin() );
spk_ValueType AMT_50_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_50( 2 );
   copy( AMT_50_c, AMT_50_c+2, AMT_50.begin() );
spk_ValueType MDV_50_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_50( 2 );
   copy( MDV_50_c, MDV_50_c+2, MDV_50.begin() );
int EVID_50_c[] = { 0, 0 };
   std::vector<int> EVID_50( 2 );
   copy( EVID_50_c, EVID_50_c+2, EVID_50.begin() );
   data[50] = new IndData<spk_ValueType>( 2, ID_50, TIME_50, DV_50, AMT_50, MDV_50, EVID_50 );

   //------------------------------------
   // Subject <52> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[51] = 2;
char* ID_51_c[] = { "52", "52" };
   std::vector<char*> ID_51( 2 );
   copy( ID_51_c, ID_51_c+2, ID_51.begin() );
spk_ValueType TIME_51_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_51( 2 );
   copy( TIME_51_c, TIME_51_c+2, TIME_51.begin() );
spk_ValueType DV_51_c[] = { 0.0422192, 0.895246 };
   std::vector<spk_ValueType> DV_51( 2 );
   copy( DV_51_c, DV_51_c+2, DV_51.begin() );
spk_ValueType AMT_51_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_51( 2 );
   copy( AMT_51_c, AMT_51_c+2, AMT_51.begin() );
spk_ValueType MDV_51_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_51( 2 );
   copy( MDV_51_c, MDV_51_c+2, MDV_51.begin() );
int EVID_51_c[] = { 0, 0 };
   std::vector<int> EVID_51( 2 );
   copy( EVID_51_c, EVID_51_c+2, EVID_51.begin() );
   data[51] = new IndData<spk_ValueType>( 2, ID_51, TIME_51, DV_51, AMT_51, MDV_51, EVID_51 );

   //------------------------------------
   // Subject <53> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[52] = 2;
char* ID_52_c[] = { "53", "53" };
   std::vector<char*> ID_52( 2 );
   copy( ID_52_c, ID_52_c+2, ID_52.begin() );
spk_ValueType TIME_52_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_52( 2 );
   copy( TIME_52_c, TIME_52_c+2, TIME_52.begin() );
spk_ValueType DV_52_c[] = { -0.190935, 2.04056 };
   std::vector<spk_ValueType> DV_52( 2 );
   copy( DV_52_c, DV_52_c+2, DV_52.begin() );
spk_ValueType AMT_52_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_52( 2 );
   copy( AMT_52_c, AMT_52_c+2, AMT_52.begin() );
spk_ValueType MDV_52_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_52( 2 );
   copy( MDV_52_c, MDV_52_c+2, MDV_52.begin() );
int EVID_52_c[] = { 0, 0 };
   std::vector<int> EVID_52( 2 );
   copy( EVID_52_c, EVID_52_c+2, EVID_52.begin() );
   data[52] = new IndData<spk_ValueType>( 2, ID_52, TIME_52, DV_52, AMT_52, MDV_52, EVID_52 );

   //------------------------------------
   // Subject <54> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[53] = 2;
char* ID_53_c[] = { "54", "54" };
   std::vector<char*> ID_53( 2 );
   copy( ID_53_c, ID_53_c+2, ID_53.begin() );
spk_ValueType TIME_53_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_53( 2 );
   copy( TIME_53_c, TIME_53_c+2, TIME_53.begin() );
spk_ValueType DV_53_c[] = { -0.0480628, 1.06088 };
   std::vector<spk_ValueType> DV_53( 2 );
   copy( DV_53_c, DV_53_c+2, DV_53.begin() );
spk_ValueType AMT_53_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_53( 2 );
   copy( AMT_53_c, AMT_53_c+2, AMT_53.begin() );
spk_ValueType MDV_53_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_53( 2 );
   copy( MDV_53_c, MDV_53_c+2, MDV_53.begin() );
int EVID_53_c[] = { 0, 0 };
   std::vector<int> EVID_53( 2 );
   copy( EVID_53_c, EVID_53_c+2, EVID_53.begin() );
   data[53] = new IndData<spk_ValueType>( 2, ID_53, TIME_53, DV_53, AMT_53, MDV_53, EVID_53 );

   //------------------------------------
   // Subject <55> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[54] = 2;
char* ID_54_c[] = { "55", "55" };
   std::vector<char*> ID_54( 2 );
   copy( ID_54_c, ID_54_c+2, ID_54.begin() );
spk_ValueType TIME_54_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_54( 2 );
   copy( TIME_54_c, TIME_54_c+2, TIME_54.begin() );
spk_ValueType DV_54_c[] = { -0.168261, 1.44156 };
   std::vector<spk_ValueType> DV_54( 2 );
   copy( DV_54_c, DV_54_c+2, DV_54.begin() );
spk_ValueType AMT_54_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_54( 2 );
   copy( AMT_54_c, AMT_54_c+2, AMT_54.begin() );
spk_ValueType MDV_54_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_54( 2 );
   copy( MDV_54_c, MDV_54_c+2, MDV_54.begin() );
int EVID_54_c[] = { 0, 0 };
   std::vector<int> EVID_54( 2 );
   copy( EVID_54_c, EVID_54_c+2, EVID_54.begin() );
   data[54] = new IndData<spk_ValueType>( 2, ID_54, TIME_54, DV_54, AMT_54, MDV_54, EVID_54 );

   //------------------------------------
   // Subject <56> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[55] = 2;
char* ID_55_c[] = { "56", "56" };
   std::vector<char*> ID_55( 2 );
   copy( ID_55_c, ID_55_c+2, ID_55.begin() );
spk_ValueType TIME_55_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_55( 2 );
   copy( TIME_55_c, TIME_55_c+2, TIME_55.begin() );
spk_ValueType DV_55_c[] = { -0.066374, -0.75986 };
   std::vector<spk_ValueType> DV_55( 2 );
   copy( DV_55_c, DV_55_c+2, DV_55.begin() );
spk_ValueType AMT_55_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_55( 2 );
   copy( AMT_55_c, AMT_55_c+2, AMT_55.begin() );
spk_ValueType MDV_55_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_55( 2 );
   copy( MDV_55_c, MDV_55_c+2, MDV_55.begin() );
int EVID_55_c[] = { 0, 0 };
   std::vector<int> EVID_55( 2 );
   copy( EVID_55_c, EVID_55_c+2, EVID_55.begin() );
   data[55] = new IndData<spk_ValueType>( 2, ID_55, TIME_55, DV_55, AMT_55, MDV_55, EVID_55 );

   //------------------------------------
   // Subject <57> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[56] = 2;
char* ID_56_c[] = { "57", "57" };
   std::vector<char*> ID_56( 2 );
   copy( ID_56_c, ID_56_c+2, ID_56.begin() );
spk_ValueType TIME_56_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_56( 2 );
   copy( TIME_56_c, TIME_56_c+2, TIME_56.begin() );
spk_ValueType DV_56_c[] = { 0.0966028, 2.51321 };
   std::vector<spk_ValueType> DV_56( 2 );
   copy( DV_56_c, DV_56_c+2, DV_56.begin() );
spk_ValueType AMT_56_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_56( 2 );
   copy( AMT_56_c, AMT_56_c+2, AMT_56.begin() );
spk_ValueType MDV_56_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_56( 2 );
   copy( MDV_56_c, MDV_56_c+2, MDV_56.begin() );
int EVID_56_c[] = { 0, 0 };
   std::vector<int> EVID_56( 2 );
   copy( EVID_56_c, EVID_56_c+2, EVID_56.begin() );
   data[56] = new IndData<spk_ValueType>( 2, ID_56, TIME_56, DV_56, AMT_56, MDV_56, EVID_56 );

   //------------------------------------
   // Subject <58> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[57] = 2;
char* ID_57_c[] = { "58", "58" };
   std::vector<char*> ID_57( 2 );
   copy( ID_57_c, ID_57_c+2, ID_57.begin() );
spk_ValueType TIME_57_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_57( 2 );
   copy( TIME_57_c, TIME_57_c+2, TIME_57.begin() );
spk_ValueType DV_57_c[] = { 0.00312126, 0.396155 };
   std::vector<spk_ValueType> DV_57( 2 );
   copy( DV_57_c, DV_57_c+2, DV_57.begin() );
spk_ValueType AMT_57_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_57( 2 );
   copy( AMT_57_c, AMT_57_c+2, AMT_57.begin() );
spk_ValueType MDV_57_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_57( 2 );
   copy( MDV_57_c, MDV_57_c+2, MDV_57.begin() );
int EVID_57_c[] = { 0, 0 };
   std::vector<int> EVID_57( 2 );
   copy( EVID_57_c, EVID_57_c+2, EVID_57.begin() );
   data[57] = new IndData<spk_ValueType>( 2, ID_57, TIME_57, DV_57, AMT_57, MDV_57, EVID_57 );

   //------------------------------------
   // Subject <59> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[58] = 2;
char* ID_58_c[] = { "59", "59" };
   std::vector<char*> ID_58( 2 );
   copy( ID_58_c, ID_58_c+2, ID_58.begin() );
spk_ValueType TIME_58_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_58( 2 );
   copy( TIME_58_c, TIME_58_c+2, TIME_58.begin() );
spk_ValueType DV_58_c[] = { -0.0876859, 2.97883 };
   std::vector<spk_ValueType> DV_58( 2 );
   copy( DV_58_c, DV_58_c+2, DV_58.begin() );
spk_ValueType AMT_58_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_58( 2 );
   copy( AMT_58_c, AMT_58_c+2, AMT_58.begin() );
spk_ValueType MDV_58_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_58( 2 );
   copy( MDV_58_c, MDV_58_c+2, MDV_58.begin() );
int EVID_58_c[] = { 0, 0 };
   std::vector<int> EVID_58( 2 );
   copy( EVID_58_c, EVID_58_c+2, EVID_58.begin() );
   data[58] = new IndData<spk_ValueType>( 2, ID_58, TIME_58, DV_58, AMT_58, MDV_58, EVID_58 );

   //------------------------------------
   // Subject <60> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[59] = 2;
char* ID_59_c[] = { "60", "60" };
   std::vector<char*> ID_59( 2 );
   copy( ID_59_c, ID_59_c+2, ID_59.begin() );
spk_ValueType TIME_59_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_59( 2 );
   copy( TIME_59_c, TIME_59_c+2, TIME_59.begin() );
spk_ValueType DV_59_c[] = { 0.0783031, 1.03521 };
   std::vector<spk_ValueType> DV_59( 2 );
   copy( DV_59_c, DV_59_c+2, DV_59.begin() );
spk_ValueType AMT_59_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_59( 2 );
   copy( AMT_59_c, AMT_59_c+2, AMT_59.begin() );
spk_ValueType MDV_59_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_59( 2 );
   copy( MDV_59_c, MDV_59_c+2, MDV_59.begin() );
int EVID_59_c[] = { 0, 0 };
   std::vector<int> EVID_59( 2 );
   copy( EVID_59_c, EVID_59_c+2, EVID_59.begin() );
   data[59] = new IndData<spk_ValueType>( 2, ID_59, TIME_59, DV_59, AMT_59, MDV_59, EVID_59 );

   //------------------------------------
   // Subject <61> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[60] = 2;
char* ID_60_c[] = { "61", "61" };
   std::vector<char*> ID_60( 2 );
   copy( ID_60_c, ID_60_c+2, ID_60.begin() );
spk_ValueType TIME_60_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_60( 2 );
   copy( TIME_60_c, TIME_60_c+2, TIME_60.begin() );
spk_ValueType DV_60_c[] = { -0.0130572, 0.874889 };
   std::vector<spk_ValueType> DV_60( 2 );
   copy( DV_60_c, DV_60_c+2, DV_60.begin() );
spk_ValueType AMT_60_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_60( 2 );
   copy( AMT_60_c, AMT_60_c+2, AMT_60.begin() );
spk_ValueType MDV_60_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_60( 2 );
   copy( MDV_60_c, MDV_60_c+2, MDV_60.begin() );
int EVID_60_c[] = { 0, 0 };
   std::vector<int> EVID_60( 2 );
   copy( EVID_60_c, EVID_60_c+2, EVID_60.begin() );
   data[60] = new IndData<spk_ValueType>( 2, ID_60, TIME_60, DV_60, AMT_60, MDV_60, EVID_60 );

   //------------------------------------
   // Subject <62> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[61] = 2;
char* ID_61_c[] = { "62", "62" };
   std::vector<char*> ID_61( 2 );
   copy( ID_61_c, ID_61_c+2, ID_61.begin() );
spk_ValueType TIME_61_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_61( 2 );
   copy( TIME_61_c, TIME_61_c+2, TIME_61.begin() );
spk_ValueType DV_61_c[] = { -0.186266, -0.862529 };
   std::vector<spk_ValueType> DV_61( 2 );
   copy( DV_61_c, DV_61_c+2, DV_61.begin() );
spk_ValueType AMT_61_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_61( 2 );
   copy( AMT_61_c, AMT_61_c+2, AMT_61.begin() );
spk_ValueType MDV_61_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_61( 2 );
   copy( MDV_61_c, MDV_61_c+2, MDV_61.begin() );
int EVID_61_c[] = { 0, 0 };
   std::vector<int> EVID_61( 2 );
   copy( EVID_61_c, EVID_61_c+2, EVID_61.begin() );
   data[61] = new IndData<spk_ValueType>( 2, ID_61, TIME_61, DV_61, AMT_61, MDV_61, EVID_61 );

   //------------------------------------
   // Subject <63> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[62] = 2;
char* ID_62_c[] = { "63", "63" };
   std::vector<char*> ID_62( 2 );
   copy( ID_62_c, ID_62_c+2, ID_62.begin() );
spk_ValueType TIME_62_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_62( 2 );
   copy( TIME_62_c, TIME_62_c+2, TIME_62.begin() );
spk_ValueType DV_62_c[] = { 0.139324, 0.435562 };
   std::vector<spk_ValueType> DV_62( 2 );
   copy( DV_62_c, DV_62_c+2, DV_62.begin() );
spk_ValueType AMT_62_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_62( 2 );
   copy( AMT_62_c, AMT_62_c+2, AMT_62.begin() );
spk_ValueType MDV_62_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_62( 2 );
   copy( MDV_62_c, MDV_62_c+2, MDV_62.begin() );
int EVID_62_c[] = { 0, 0 };
   std::vector<int> EVID_62( 2 );
   copy( EVID_62_c, EVID_62_c+2, EVID_62.begin() );
   data[62] = new IndData<spk_ValueType>( 2, ID_62, TIME_62, DV_62, AMT_62, MDV_62, EVID_62 );

   //------------------------------------
   // Subject <64> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[63] = 2;
char* ID_63_c[] = { "64", "64" };
   std::vector<char*> ID_63( 2 );
   copy( ID_63_c, ID_63_c+2, ID_63.begin() );
spk_ValueType TIME_63_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_63( 2 );
   copy( TIME_63_c, TIME_63_c+2, TIME_63.begin() );
spk_ValueType DV_63_c[] = { 0.046929, 2.14963 };
   std::vector<spk_ValueType> DV_63( 2 );
   copy( DV_63_c, DV_63_c+2, DV_63.begin() );
spk_ValueType AMT_63_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_63( 2 );
   copy( AMT_63_c, AMT_63_c+2, AMT_63.begin() );
spk_ValueType MDV_63_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_63( 2 );
   copy( MDV_63_c, MDV_63_c+2, MDV_63.begin() );
int EVID_63_c[] = { 0, 0 };
   std::vector<int> EVID_63( 2 );
   copy( EVID_63_c, EVID_63_c+2, EVID_63.begin() );
   data[63] = new IndData<spk_ValueType>( 2, ID_63, TIME_63, DV_63, AMT_63, MDV_63, EVID_63 );

   //------------------------------------
   // Subject <65> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[64] = 2;
char* ID_64_c[] = { "65", "65" };
   std::vector<char*> ID_64( 2 );
   copy( ID_64_c, ID_64_c+2, ID_64.begin() );
spk_ValueType TIME_64_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_64( 2 );
   copy( TIME_64_c, TIME_64_c+2, TIME_64.begin() );
spk_ValueType DV_64_c[] = { -0.000231484, 0.00820708 };
   std::vector<spk_ValueType> DV_64( 2 );
   copy( DV_64_c, DV_64_c+2, DV_64.begin() );
spk_ValueType AMT_64_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_64( 2 );
   copy( AMT_64_c, AMT_64_c+2, AMT_64.begin() );
spk_ValueType MDV_64_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_64( 2 );
   copy( MDV_64_c, MDV_64_c+2, MDV_64.begin() );
int EVID_64_c[] = { 0, 0 };
   std::vector<int> EVID_64( 2 );
   copy( EVID_64_c, EVID_64_c+2, EVID_64.begin() );
   data[64] = new IndData<spk_ValueType>( 2, ID_64, TIME_64, DV_64, AMT_64, MDV_64, EVID_64 );

   //------------------------------------
   // Subject <66> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[65] = 2;
char* ID_65_c[] = { "66", "66" };
   std::vector<char*> ID_65( 2 );
   copy( ID_65_c, ID_65_c+2, ID_65.begin() );
spk_ValueType TIME_65_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_65( 2 );
   copy( TIME_65_c, TIME_65_c+2, TIME_65.begin() );
spk_ValueType DV_65_c[] = { 0.00595989, 1.26938 };
   std::vector<spk_ValueType> DV_65( 2 );
   copy( DV_65_c, DV_65_c+2, DV_65.begin() );
spk_ValueType AMT_65_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_65( 2 );
   copy( AMT_65_c, AMT_65_c+2, AMT_65.begin() );
spk_ValueType MDV_65_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_65( 2 );
   copy( MDV_65_c, MDV_65_c+2, MDV_65.begin() );
int EVID_65_c[] = { 0, 0 };
   std::vector<int> EVID_65( 2 );
   copy( EVID_65_c, EVID_65_c+2, EVID_65.begin() );
   data[65] = new IndData<spk_ValueType>( 2, ID_65, TIME_65, DV_65, AMT_65, MDV_65, EVID_65 );

   //------------------------------------
   // Subject <67> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[66] = 2;
char* ID_66_c[] = { "67", "67" };
   std::vector<char*> ID_66( 2 );
   copy( ID_66_c, ID_66_c+2, ID_66.begin() );
spk_ValueType TIME_66_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_66( 2 );
   copy( TIME_66_c, TIME_66_c+2, TIME_66.begin() );
spk_ValueType DV_66_c[] = { -0.389094, -0.84412 };
   std::vector<spk_ValueType> DV_66( 2 );
   copy( DV_66_c, DV_66_c+2, DV_66.begin() );
spk_ValueType AMT_66_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_66( 2 );
   copy( AMT_66_c, AMT_66_c+2, AMT_66.begin() );
spk_ValueType MDV_66_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_66( 2 );
   copy( MDV_66_c, MDV_66_c+2, MDV_66.begin() );
int EVID_66_c[] = { 0, 0 };
   std::vector<int> EVID_66( 2 );
   copy( EVID_66_c, EVID_66_c+2, EVID_66.begin() );
   data[66] = new IndData<spk_ValueType>( 2, ID_66, TIME_66, DV_66, AMT_66, MDV_66, EVID_66 );

   //------------------------------------
   // Subject <68> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[67] = 2;
char* ID_67_c[] = { "68", "68" };
   std::vector<char*> ID_67( 2 );
   copy( ID_67_c, ID_67_c+2, ID_67.begin() );
spk_ValueType TIME_67_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_67( 2 );
   copy( TIME_67_c, TIME_67_c+2, TIME_67.begin() );
spk_ValueType DV_67_c[] = { -0.0118859, 0.452508 };
   std::vector<spk_ValueType> DV_67( 2 );
   copy( DV_67_c, DV_67_c+2, DV_67.begin() );
spk_ValueType AMT_67_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_67( 2 );
   copy( AMT_67_c, AMT_67_c+2, AMT_67.begin() );
spk_ValueType MDV_67_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_67( 2 );
   copy( MDV_67_c, MDV_67_c+2, MDV_67.begin() );
int EVID_67_c[] = { 0, 0 };
   std::vector<int> EVID_67( 2 );
   copy( EVID_67_c, EVID_67_c+2, EVID_67.begin() );
   data[67] = new IndData<spk_ValueType>( 2, ID_67, TIME_67, DV_67, AMT_67, MDV_67, EVID_67 );

   //------------------------------------
   // Subject <69> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[68] = 2;
char* ID_68_c[] = { "69", "69" };
   std::vector<char*> ID_68( 2 );
   copy( ID_68_c, ID_68_c+2, ID_68.begin() );
spk_ValueType TIME_68_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_68( 2 );
   copy( TIME_68_c, TIME_68_c+2, TIME_68.begin() );
spk_ValueType DV_68_c[] = { -0.0123666, 0.691749 };
   std::vector<spk_ValueType> DV_68( 2 );
   copy( DV_68_c, DV_68_c+2, DV_68.begin() );
spk_ValueType AMT_68_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_68( 2 );
   copy( AMT_68_c, AMT_68_c+2, AMT_68.begin() );
spk_ValueType MDV_68_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_68( 2 );
   copy( MDV_68_c, MDV_68_c+2, MDV_68.begin() );
int EVID_68_c[] = { 0, 0 };
   std::vector<int> EVID_68( 2 );
   copy( EVID_68_c, EVID_68_c+2, EVID_68.begin() );
   data[68] = new IndData<spk_ValueType>( 2, ID_68, TIME_68, DV_68, AMT_68, MDV_68, EVID_68 );

   //------------------------------------
   // Subject <70> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[69] = 2;
char* ID_69_c[] = { "70", "70" };
   std::vector<char*> ID_69( 2 );
   copy( ID_69_c, ID_69_c+2, ID_69.begin() );
spk_ValueType TIME_69_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_69( 2 );
   copy( TIME_69_c, TIME_69_c+2, TIME_69.begin() );
spk_ValueType DV_69_c[] = { 0.0891382, 2.92451 };
   std::vector<spk_ValueType> DV_69( 2 );
   copy( DV_69_c, DV_69_c+2, DV_69.begin() );
spk_ValueType AMT_69_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_69( 2 );
   copy( AMT_69_c, AMT_69_c+2, AMT_69.begin() );
spk_ValueType MDV_69_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_69( 2 );
   copy( MDV_69_c, MDV_69_c+2, MDV_69.begin() );
int EVID_69_c[] = { 0, 0 };
   std::vector<int> EVID_69( 2 );
   copy( EVID_69_c, EVID_69_c+2, EVID_69.begin() );
   data[69] = new IndData<spk_ValueType>( 2, ID_69, TIME_69, DV_69, AMT_69, MDV_69, EVID_69 );

   //------------------------------------
   // Subject <71> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[70] = 2;
char* ID_70_c[] = { "71", "71" };
   std::vector<char*> ID_70( 2 );
   copy( ID_70_c, ID_70_c+2, ID_70.begin() );
spk_ValueType TIME_70_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_70( 2 );
   copy( TIME_70_c, TIME_70_c+2, TIME_70.begin() );
spk_ValueType DV_70_c[] = { -0.0680821, 1.11872 };
   std::vector<spk_ValueType> DV_70( 2 );
   copy( DV_70_c, DV_70_c+2, DV_70.begin() );
spk_ValueType AMT_70_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_70( 2 );
   copy( AMT_70_c, AMT_70_c+2, AMT_70.begin() );
spk_ValueType MDV_70_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_70( 2 );
   copy( MDV_70_c, MDV_70_c+2, MDV_70.begin() );
int EVID_70_c[] = { 0, 0 };
   std::vector<int> EVID_70( 2 );
   copy( EVID_70_c, EVID_70_c+2, EVID_70.begin() );
   data[70] = new IndData<spk_ValueType>( 2, ID_70, TIME_70, DV_70, AMT_70, MDV_70, EVID_70 );

   //------------------------------------
   // Subject <72> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[71] = 2;
char* ID_71_c[] = { "72", "72" };
   std::vector<char*> ID_71( 2 );
   copy( ID_71_c, ID_71_c+2, ID_71.begin() );
spk_ValueType TIME_71_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_71( 2 );
   copy( TIME_71_c, TIME_71_c+2, TIME_71.begin() );
spk_ValueType DV_71_c[] = { -0.128997, 0.971013 };
   std::vector<spk_ValueType> DV_71( 2 );
   copy( DV_71_c, DV_71_c+2, DV_71.begin() );
spk_ValueType AMT_71_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_71( 2 );
   copy( AMT_71_c, AMT_71_c+2, AMT_71.begin() );
spk_ValueType MDV_71_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_71( 2 );
   copy( MDV_71_c, MDV_71_c+2, MDV_71.begin() );
int EVID_71_c[] = { 0, 0 };
   std::vector<int> EVID_71( 2 );
   copy( EVID_71_c, EVID_71_c+2, EVID_71.begin() );
   data[71] = new IndData<spk_ValueType>( 2, ID_71, TIME_71, DV_71, AMT_71, MDV_71, EVID_71 );

   //------------------------------------
   // Subject <73> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[72] = 2;
char* ID_72_c[] = { "73", "73" };
   std::vector<char*> ID_72( 2 );
   copy( ID_72_c, ID_72_c+2, ID_72.begin() );
spk_ValueType TIME_72_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_72( 2 );
   copy( TIME_72_c, TIME_72_c+2, TIME_72.begin() );
spk_ValueType DV_72_c[] = { -0.0666756, 1.63008 };
   std::vector<spk_ValueType> DV_72( 2 );
   copy( DV_72_c, DV_72_c+2, DV_72.begin() );
spk_ValueType AMT_72_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_72( 2 );
   copy( AMT_72_c, AMT_72_c+2, AMT_72.begin() );
spk_ValueType MDV_72_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_72( 2 );
   copy( MDV_72_c, MDV_72_c+2, MDV_72.begin() );
int EVID_72_c[] = { 0, 0 };
   std::vector<int> EVID_72( 2 );
   copy( EVID_72_c, EVID_72_c+2, EVID_72.begin() );
   data[72] = new IndData<spk_ValueType>( 2, ID_72, TIME_72, DV_72, AMT_72, MDV_72, EVID_72 );

   //------------------------------------
   // Subject <74> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[73] = 2;
char* ID_73_c[] = { "74", "74" };
   std::vector<char*> ID_73( 2 );
   copy( ID_73_c, ID_73_c+2, ID_73.begin() );
spk_ValueType TIME_73_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_73( 2 );
   copy( TIME_73_c, TIME_73_c+2, TIME_73.begin() );
spk_ValueType DV_73_c[] = { 0.0883766, 0.0464441 };
   std::vector<spk_ValueType> DV_73( 2 );
   copy( DV_73_c, DV_73_c+2, DV_73.begin() );
spk_ValueType AMT_73_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_73( 2 );
   copy( AMT_73_c, AMT_73_c+2, AMT_73.begin() );
spk_ValueType MDV_73_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_73( 2 );
   copy( MDV_73_c, MDV_73_c+2, MDV_73.begin() );
int EVID_73_c[] = { 0, 0 };
   std::vector<int> EVID_73( 2 );
   copy( EVID_73_c, EVID_73_c+2, EVID_73.begin() );
   data[73] = new IndData<spk_ValueType>( 2, ID_73, TIME_73, DV_73, AMT_73, MDV_73, EVID_73 );

   //------------------------------------
   // Subject <75> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[74] = 2;
char* ID_74_c[] = { "75", "75" };
   std::vector<char*> ID_74( 2 );
   copy( ID_74_c, ID_74_c+2, ID_74.begin() );
spk_ValueType TIME_74_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_74( 2 );
   copy( TIME_74_c, TIME_74_c+2, TIME_74.begin() );
spk_ValueType DV_74_c[] = { -0.239096, 2.34413 };
   std::vector<spk_ValueType> DV_74( 2 );
   copy( DV_74_c, DV_74_c+2, DV_74.begin() );
spk_ValueType AMT_74_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_74( 2 );
   copy( AMT_74_c, AMT_74_c+2, AMT_74.begin() );
spk_ValueType MDV_74_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_74( 2 );
   copy( MDV_74_c, MDV_74_c+2, MDV_74.begin() );
int EVID_74_c[] = { 0, 0 };
   std::vector<int> EVID_74( 2 );
   copy( EVID_74_c, EVID_74_c+2, EVID_74.begin() );
   data[74] = new IndData<spk_ValueType>( 2, ID_74, TIME_74, DV_74, AMT_74, MDV_74, EVID_74 );

   //------------------------------------
   // Subject <76> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[75] = 2;
char* ID_75_c[] = { "76", "76" };
   std::vector<char*> ID_75( 2 );
   copy( ID_75_c, ID_75_c+2, ID_75.begin() );
spk_ValueType TIME_75_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_75( 2 );
   copy( TIME_75_c, TIME_75_c+2, TIME_75.begin() );
spk_ValueType DV_75_c[] = { -0.148912, 1.44285 };
   std::vector<spk_ValueType> DV_75( 2 );
   copy( DV_75_c, DV_75_c+2, DV_75.begin() );
spk_ValueType AMT_75_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_75( 2 );
   copy( AMT_75_c, AMT_75_c+2, AMT_75.begin() );
spk_ValueType MDV_75_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_75( 2 );
   copy( MDV_75_c, MDV_75_c+2, MDV_75.begin() );
int EVID_75_c[] = { 0, 0 };
   std::vector<int> EVID_75( 2 );
   copy( EVID_75_c, EVID_75_c+2, EVID_75.begin() );
   data[75] = new IndData<spk_ValueType>( 2, ID_75, TIME_75, DV_75, AMT_75, MDV_75, EVID_75 );

   //------------------------------------
   // Subject <77> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[76] = 2;
char* ID_76_c[] = { "77", "77" };
   std::vector<char*> ID_76( 2 );
   copy( ID_76_c, ID_76_c+2, ID_76.begin() );
spk_ValueType TIME_76_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_76( 2 );
   copy( TIME_76_c, TIME_76_c+2, TIME_76.begin() );
spk_ValueType DV_76_c[] = { -0.0800006, 1.76299 };
   std::vector<spk_ValueType> DV_76( 2 );
   copy( DV_76_c, DV_76_c+2, DV_76.begin() );
spk_ValueType AMT_76_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_76( 2 );
   copy( AMT_76_c, AMT_76_c+2, AMT_76.begin() );
spk_ValueType MDV_76_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_76( 2 );
   copy( MDV_76_c, MDV_76_c+2, MDV_76.begin() );
int EVID_76_c[] = { 0, 0 };
   std::vector<int> EVID_76( 2 );
   copy( EVID_76_c, EVID_76_c+2, EVID_76.begin() );
   data[76] = new IndData<spk_ValueType>( 2, ID_76, TIME_76, DV_76, AMT_76, MDV_76, EVID_76 );

   //------------------------------------
   // Subject <78> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[77] = 2;
char* ID_77_c[] = { "78", "78" };
   std::vector<char*> ID_77( 2 );
   copy( ID_77_c, ID_77_c+2, ID_77.begin() );
spk_ValueType TIME_77_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_77( 2 );
   copy( TIME_77_c, TIME_77_c+2, TIME_77.begin() );
spk_ValueType DV_77_c[] = { 0.127269, 0.187613 };
   std::vector<spk_ValueType> DV_77( 2 );
   copy( DV_77_c, DV_77_c+2, DV_77.begin() );
spk_ValueType AMT_77_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_77( 2 );
   copy( AMT_77_c, AMT_77_c+2, AMT_77.begin() );
spk_ValueType MDV_77_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_77( 2 );
   copy( MDV_77_c, MDV_77_c+2, MDV_77.begin() );
int EVID_77_c[] = { 0, 0 };
   std::vector<int> EVID_77( 2 );
   copy( EVID_77_c, EVID_77_c+2, EVID_77.begin() );
   data[77] = new IndData<spk_ValueType>( 2, ID_77, TIME_77, DV_77, AMT_77, MDV_77, EVID_77 );

   //------------------------------------
   // Subject <79> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[78] = 2;
char* ID_78_c[] = { "79", "79" };
   std::vector<char*> ID_78( 2 );
   copy( ID_78_c, ID_78_c+2, ID_78.begin() );
spk_ValueType TIME_78_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_78( 2 );
   copy( TIME_78_c, TIME_78_c+2, TIME_78.begin() );
spk_ValueType DV_78_c[] = { 0.0176967, -1.01734 };
   std::vector<spk_ValueType> DV_78( 2 );
   copy( DV_78_c, DV_78_c+2, DV_78.begin() );
spk_ValueType AMT_78_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_78( 2 );
   copy( AMT_78_c, AMT_78_c+2, AMT_78.begin() );
spk_ValueType MDV_78_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_78( 2 );
   copy( MDV_78_c, MDV_78_c+2, MDV_78.begin() );
int EVID_78_c[] = { 0, 0 };
   std::vector<int> EVID_78( 2 );
   copy( EVID_78_c, EVID_78_c+2, EVID_78.begin() );
   data[78] = new IndData<spk_ValueType>( 2, ID_78, TIME_78, DV_78, AMT_78, MDV_78, EVID_78 );

   //------------------------------------
   // Subject <80> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[79] = 2;
char* ID_79_c[] = { "80", "80" };
   std::vector<char*> ID_79( 2 );
   copy( ID_79_c, ID_79_c+2, ID_79.begin() );
spk_ValueType TIME_79_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_79( 2 );
   copy( TIME_79_c, TIME_79_c+2, TIME_79.begin() );
spk_ValueType DV_79_c[] = { -0.130503, -0.00338884 };
   std::vector<spk_ValueType> DV_79( 2 );
   copy( DV_79_c, DV_79_c+2, DV_79.begin() );
spk_ValueType AMT_79_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_79( 2 );
   copy( AMT_79_c, AMT_79_c+2, AMT_79.begin() );
spk_ValueType MDV_79_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_79( 2 );
   copy( MDV_79_c, MDV_79_c+2, MDV_79.begin() );
int EVID_79_c[] = { 0, 0 };
   std::vector<int> EVID_79( 2 );
   copy( EVID_79_c, EVID_79_c+2, EVID_79.begin() );
   data[79] = new IndData<spk_ValueType>( 2, ID_79, TIME_79, DV_79, AMT_79, MDV_79, EVID_79 );

   //------------------------------------
   // Subject <81> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[80] = 2;
char* ID_80_c[] = { "81", "81" };
   std::vector<char*> ID_80( 2 );
   copy( ID_80_c, ID_80_c+2, ID_80.begin() );
spk_ValueType TIME_80_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_80( 2 );
   copy( TIME_80_c, TIME_80_c+2, TIME_80.begin() );
spk_ValueType DV_80_c[] = { 0.0405343, 0.832713 };
   std::vector<spk_ValueType> DV_80( 2 );
   copy( DV_80_c, DV_80_c+2, DV_80.begin() );
spk_ValueType AMT_80_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_80( 2 );
   copy( AMT_80_c, AMT_80_c+2, AMT_80.begin() );
spk_ValueType MDV_80_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_80( 2 );
   copy( MDV_80_c, MDV_80_c+2, MDV_80.begin() );
int EVID_80_c[] = { 0, 0 };
   std::vector<int> EVID_80( 2 );
   copy( EVID_80_c, EVID_80_c+2, EVID_80.begin() );
   data[80] = new IndData<spk_ValueType>( 2, ID_80, TIME_80, DV_80, AMT_80, MDV_80, EVID_80 );

   //------------------------------------
   // Subject <82> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[81] = 2;
char* ID_81_c[] = { "82", "82" };
   std::vector<char*> ID_81( 2 );
   copy( ID_81_c, ID_81_c+2, ID_81.begin() );
spk_ValueType TIME_81_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_81( 2 );
   copy( TIME_81_c, TIME_81_c+2, TIME_81.begin() );
spk_ValueType DV_81_c[] = { -0.0936649, 1.34716 };
   std::vector<spk_ValueType> DV_81( 2 );
   copy( DV_81_c, DV_81_c+2, DV_81.begin() );
spk_ValueType AMT_81_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_81( 2 );
   copy( AMT_81_c, AMT_81_c+2, AMT_81.begin() );
spk_ValueType MDV_81_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_81( 2 );
   copy( MDV_81_c, MDV_81_c+2, MDV_81.begin() );
int EVID_81_c[] = { 0, 0 };
   std::vector<int> EVID_81( 2 );
   copy( EVID_81_c, EVID_81_c+2, EVID_81.begin() );
   data[81] = new IndData<spk_ValueType>( 2, ID_81, TIME_81, DV_81, AMT_81, MDV_81, EVID_81 );

   //------------------------------------
   // Subject <83> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[82] = 2;
char* ID_82_c[] = { "83", "83" };
   std::vector<char*> ID_82( 2 );
   copy( ID_82_c, ID_82_c+2, ID_82.begin() );
spk_ValueType TIME_82_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_82( 2 );
   copy( TIME_82_c, TIME_82_c+2, TIME_82.begin() );
spk_ValueType DV_82_c[] = { -0.059333, 1.40311 };
   std::vector<spk_ValueType> DV_82( 2 );
   copy( DV_82_c, DV_82_c+2, DV_82.begin() );
spk_ValueType AMT_82_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_82( 2 );
   copy( AMT_82_c, AMT_82_c+2, AMT_82.begin() );
spk_ValueType MDV_82_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_82( 2 );
   copy( MDV_82_c, MDV_82_c+2, MDV_82.begin() );
int EVID_82_c[] = { 0, 0 };
   std::vector<int> EVID_82( 2 );
   copy( EVID_82_c, EVID_82_c+2, EVID_82.begin() );
   data[82] = new IndData<spk_ValueType>( 2, ID_82, TIME_82, DV_82, AMT_82, MDV_82, EVID_82 );

   //------------------------------------
   // Subject <84> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[83] = 2;
char* ID_83_c[] = { "84", "84" };
   std::vector<char*> ID_83( 2 );
   copy( ID_83_c, ID_83_c+2, ID_83.begin() );
spk_ValueType TIME_83_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_83( 2 );
   copy( TIME_83_c, TIME_83_c+2, TIME_83.begin() );
spk_ValueType DV_83_c[] = { -0.155986, 0.858712 };
   std::vector<spk_ValueType> DV_83( 2 );
   copy( DV_83_c, DV_83_c+2, DV_83.begin() );
spk_ValueType AMT_83_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_83( 2 );
   copy( AMT_83_c, AMT_83_c+2, AMT_83.begin() );
spk_ValueType MDV_83_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_83( 2 );
   copy( MDV_83_c, MDV_83_c+2, MDV_83.begin() );
int EVID_83_c[] = { 0, 0 };
   std::vector<int> EVID_83( 2 );
   copy( EVID_83_c, EVID_83_c+2, EVID_83.begin() );
   data[83] = new IndData<spk_ValueType>( 2, ID_83, TIME_83, DV_83, AMT_83, MDV_83, EVID_83 );

   //------------------------------------
   // Subject <85> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[84] = 2;
char* ID_84_c[] = { "85", "85" };
   std::vector<char*> ID_84( 2 );
   copy( ID_84_c, ID_84_c+2, ID_84.begin() );
spk_ValueType TIME_84_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_84( 2 );
   copy( TIME_84_c, TIME_84_c+2, TIME_84.begin() );
spk_ValueType DV_84_c[] = { 0.00216943, 0.570571 };
   std::vector<spk_ValueType> DV_84( 2 );
   copy( DV_84_c, DV_84_c+2, DV_84.begin() );
spk_ValueType AMT_84_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_84( 2 );
   copy( AMT_84_c, AMT_84_c+2, AMT_84.begin() );
spk_ValueType MDV_84_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_84( 2 );
   copy( MDV_84_c, MDV_84_c+2, MDV_84.begin() );
int EVID_84_c[] = { 0, 0 };
   std::vector<int> EVID_84( 2 );
   copy( EVID_84_c, EVID_84_c+2, EVID_84.begin() );
   data[84] = new IndData<spk_ValueType>( 2, ID_84, TIME_84, DV_84, AMT_84, MDV_84, EVID_84 );

   //------------------------------------
   // Subject <86> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[85] = 2;
char* ID_85_c[] = { "86", "86" };
   std::vector<char*> ID_85( 2 );
   copy( ID_85_c, ID_85_c+2, ID_85.begin() );
spk_ValueType TIME_85_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_85( 2 );
   copy( TIME_85_c, TIME_85_c+2, TIME_85.begin() );
spk_ValueType DV_85_c[] = { 0.034708, 0.146395 };
   std::vector<spk_ValueType> DV_85( 2 );
   copy( DV_85_c, DV_85_c+2, DV_85.begin() );
spk_ValueType AMT_85_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_85( 2 );
   copy( AMT_85_c, AMT_85_c+2, AMT_85.begin() );
spk_ValueType MDV_85_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_85( 2 );
   copy( MDV_85_c, MDV_85_c+2, MDV_85.begin() );
int EVID_85_c[] = { 0, 0 };
   std::vector<int> EVID_85( 2 );
   copy( EVID_85_c, EVID_85_c+2, EVID_85.begin() );
   data[85] = new IndData<spk_ValueType>( 2, ID_85, TIME_85, DV_85, AMT_85, MDV_85, EVID_85 );

   //------------------------------------
   // Subject <87> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[86] = 2;
char* ID_86_c[] = { "87", "87" };
   std::vector<char*> ID_86( 2 );
   copy( ID_86_c, ID_86_c+2, ID_86.begin() );
spk_ValueType TIME_86_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_86( 2 );
   copy( TIME_86_c, TIME_86_c+2, TIME_86.begin() );
spk_ValueType DV_86_c[] = { 0.19641, 2.6197 };
   std::vector<spk_ValueType> DV_86( 2 );
   copy( DV_86_c, DV_86_c+2, DV_86.begin() );
spk_ValueType AMT_86_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_86( 2 );
   copy( AMT_86_c, AMT_86_c+2, AMT_86.begin() );
spk_ValueType MDV_86_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_86( 2 );
   copy( MDV_86_c, MDV_86_c+2, MDV_86.begin() );
int EVID_86_c[] = { 0, 0 };
   std::vector<int> EVID_86( 2 );
   copy( EVID_86_c, EVID_86_c+2, EVID_86.begin() );
   data[86] = new IndData<spk_ValueType>( 2, ID_86, TIME_86, DV_86, AMT_86, MDV_86, EVID_86 );

   //------------------------------------
   // Subject <88> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[87] = 2;
char* ID_87_c[] = { "88", "88" };
   std::vector<char*> ID_87( 2 );
   copy( ID_87_c, ID_87_c+2, ID_87.begin() );
spk_ValueType TIME_87_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_87( 2 );
   copy( TIME_87_c, TIME_87_c+2, TIME_87.begin() );
spk_ValueType DV_87_c[] = { -0.0462768, -0.113167 };
   std::vector<spk_ValueType> DV_87( 2 );
   copy( DV_87_c, DV_87_c+2, DV_87.begin() );
spk_ValueType AMT_87_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_87( 2 );
   copy( AMT_87_c, AMT_87_c+2, AMT_87.begin() );
spk_ValueType MDV_87_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_87( 2 );
   copy( MDV_87_c, MDV_87_c+2, MDV_87.begin() );
int EVID_87_c[] = { 0, 0 };
   std::vector<int> EVID_87( 2 );
   copy( EVID_87_c, EVID_87_c+2, EVID_87.begin() );
   data[87] = new IndData<spk_ValueType>( 2, ID_87, TIME_87, DV_87, AMT_87, MDV_87, EVID_87 );

   //------------------------------------
   // Subject <89> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[88] = 2;
char* ID_88_c[] = { "89", "89" };
   std::vector<char*> ID_88( 2 );
   copy( ID_88_c, ID_88_c+2, ID_88.begin() );
spk_ValueType TIME_88_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_88( 2 );
   copy( TIME_88_c, TIME_88_c+2, TIME_88.begin() );
spk_ValueType DV_88_c[] = { -0.00150226, 0.290611 };
   std::vector<spk_ValueType> DV_88( 2 );
   copy( DV_88_c, DV_88_c+2, DV_88.begin() );
spk_ValueType AMT_88_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_88( 2 );
   copy( AMT_88_c, AMT_88_c+2, AMT_88.begin() );
spk_ValueType MDV_88_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_88( 2 );
   copy( MDV_88_c, MDV_88_c+2, MDV_88.begin() );
int EVID_88_c[] = { 0, 0 };
   std::vector<int> EVID_88( 2 );
   copy( EVID_88_c, EVID_88_c+2, EVID_88.begin() );
   data[88] = new IndData<spk_ValueType>( 2, ID_88, TIME_88, DV_88, AMT_88, MDV_88, EVID_88 );

   //------------------------------------
   // Subject <90> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[89] = 2;
char* ID_89_c[] = { "90", "90" };
   std::vector<char*> ID_89( 2 );
   copy( ID_89_c, ID_89_c+2, ID_89.begin() );
spk_ValueType TIME_89_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_89( 2 );
   copy( TIME_89_c, TIME_89_c+2, TIME_89.begin() );
spk_ValueType DV_89_c[] = { 0.0998495, 3.33228 };
   std::vector<spk_ValueType> DV_89( 2 );
   copy( DV_89_c, DV_89_c+2, DV_89.begin() );
spk_ValueType AMT_89_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_89( 2 );
   copy( AMT_89_c, AMT_89_c+2, AMT_89.begin() );
spk_ValueType MDV_89_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_89( 2 );
   copy( MDV_89_c, MDV_89_c+2, MDV_89.begin() );
int EVID_89_c[] = { 0, 0 };
   std::vector<int> EVID_89( 2 );
   copy( EVID_89_c, EVID_89_c+2, EVID_89.begin() );
   data[89] = new IndData<spk_ValueType>( 2, ID_89, TIME_89, DV_89, AMT_89, MDV_89, EVID_89 );

   //------------------------------------
   // Subject <91> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[90] = 2;
char* ID_90_c[] = { "91", "91" };
   std::vector<char*> ID_90( 2 );
   copy( ID_90_c, ID_90_c+2, ID_90.begin() );
spk_ValueType TIME_90_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_90( 2 );
   copy( TIME_90_c, TIME_90_c+2, TIME_90.begin() );
spk_ValueType DV_90_c[] = { 0.0959897, 1.16682 };
   std::vector<spk_ValueType> DV_90( 2 );
   copy( DV_90_c, DV_90_c+2, DV_90.begin() );
spk_ValueType AMT_90_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_90( 2 );
   copy( AMT_90_c, AMT_90_c+2, AMT_90.begin() );
spk_ValueType MDV_90_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_90( 2 );
   copy( MDV_90_c, MDV_90_c+2, MDV_90.begin() );
int EVID_90_c[] = { 0, 0 };
   std::vector<int> EVID_90( 2 );
   copy( EVID_90_c, EVID_90_c+2, EVID_90.begin() );
   data[90] = new IndData<spk_ValueType>( 2, ID_90, TIME_90, DV_90, AMT_90, MDV_90, EVID_90 );

   //------------------------------------
   // Subject <92> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[91] = 2;
char* ID_91_c[] = { "92", "92" };
   std::vector<char*> ID_91( 2 );
   copy( ID_91_c, ID_91_c+2, ID_91.begin() );
spk_ValueType TIME_91_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_91( 2 );
   copy( TIME_91_c, TIME_91_c+2, TIME_91.begin() );
spk_ValueType DV_91_c[] = { 0.0149749, 0.975927 };
   std::vector<spk_ValueType> DV_91( 2 );
   copy( DV_91_c, DV_91_c+2, DV_91.begin() );
spk_ValueType AMT_91_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_91( 2 );
   copy( AMT_91_c, AMT_91_c+2, AMT_91.begin() );
spk_ValueType MDV_91_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_91( 2 );
   copy( MDV_91_c, MDV_91_c+2, MDV_91.begin() );
int EVID_91_c[] = { 0, 0 };
   std::vector<int> EVID_91( 2 );
   copy( EVID_91_c, EVID_91_c+2, EVID_91.begin() );
   data[91] = new IndData<spk_ValueType>( 2, ID_91, TIME_91, DV_91, AMT_91, MDV_91, EVID_91 );

   //------------------------------------
   // Subject <93> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[92] = 2;
char* ID_92_c[] = { "93", "93" };
   std::vector<char*> ID_92( 2 );
   copy( ID_92_c, ID_92_c+2, ID_92.begin() );
spk_ValueType TIME_92_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_92( 2 );
   copy( TIME_92_c, TIME_92_c+2, TIME_92.begin() );
spk_ValueType DV_92_c[] = { 0.0427235, 2.24433 };
   std::vector<spk_ValueType> DV_92( 2 );
   copy( DV_92_c, DV_92_c+2, DV_92.begin() );
spk_ValueType AMT_92_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_92( 2 );
   copy( AMT_92_c, AMT_92_c+2, AMT_92.begin() );
spk_ValueType MDV_92_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_92( 2 );
   copy( MDV_92_c, MDV_92_c+2, MDV_92.begin() );
int EVID_92_c[] = { 0, 0 };
   std::vector<int> EVID_92( 2 );
   copy( EVID_92_c, EVID_92_c+2, EVID_92.begin() );
   data[92] = new IndData<spk_ValueType>( 2, ID_92, TIME_92, DV_92, AMT_92, MDV_92, EVID_92 );

   //------------------------------------
   // Subject <94> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[93] = 2;
char* ID_93_c[] = { "94", "94" };
   std::vector<char*> ID_93( 2 );
   copy( ID_93_c, ID_93_c+2, ID_93.begin() );
spk_ValueType TIME_93_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_93( 2 );
   copy( TIME_93_c, TIME_93_c+2, TIME_93.begin() );
spk_ValueType DV_93_c[] = { -0.147815, 1.62233 };
   std::vector<spk_ValueType> DV_93( 2 );
   copy( DV_93_c, DV_93_c+2, DV_93.begin() );
spk_ValueType AMT_93_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_93( 2 );
   copy( AMT_93_c, AMT_93_c+2, AMT_93.begin() );
spk_ValueType MDV_93_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_93( 2 );
   copy( MDV_93_c, MDV_93_c+2, MDV_93.begin() );
int EVID_93_c[] = { 0, 0 };
   std::vector<int> EVID_93( 2 );
   copy( EVID_93_c, EVID_93_c+2, EVID_93.begin() );
   data[93] = new IndData<spk_ValueType>( 2, ID_93, TIME_93, DV_93, AMT_93, MDV_93, EVID_93 );

   //------------------------------------
   // Subject <95> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[94] = 2;
char* ID_94_c[] = { "95", "95" };
   std::vector<char*> ID_94( 2 );
   copy( ID_94_c, ID_94_c+2, ID_94.begin() );
spk_ValueType TIME_94_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_94( 2 );
   copy( TIME_94_c, TIME_94_c+2, TIME_94.begin() );
spk_ValueType DV_94_c[] = { -0.0398534, -0.272861 };
   std::vector<spk_ValueType> DV_94( 2 );
   copy( DV_94_c, DV_94_c+2, DV_94.begin() );
spk_ValueType AMT_94_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_94( 2 );
   copy( AMT_94_c, AMT_94_c+2, AMT_94.begin() );
spk_ValueType MDV_94_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_94( 2 );
   copy( MDV_94_c, MDV_94_c+2, MDV_94.begin() );
int EVID_94_c[] = { 0, 0 };
   std::vector<int> EVID_94( 2 );
   copy( EVID_94_c, EVID_94_c+2, EVID_94.begin() );
   data[94] = new IndData<spk_ValueType>( 2, ID_94, TIME_94, DV_94, AMT_94, MDV_94, EVID_94 );

   //------------------------------------
   // Subject <96> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[95] = 2;
char* ID_95_c[] = { "96", "96" };
   std::vector<char*> ID_95( 2 );
   copy( ID_95_c, ID_95_c+2, ID_95.begin() );
spk_ValueType TIME_95_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_95( 2 );
   copy( TIME_95_c, TIME_95_c+2, TIME_95.begin() );
spk_ValueType DV_95_c[] = { -0.132389, 1.33837 };
   std::vector<spk_ValueType> DV_95( 2 );
   copy( DV_95_c, DV_95_c+2, DV_95.begin() );
spk_ValueType AMT_95_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_95( 2 );
   copy( AMT_95_c, AMT_95_c+2, AMT_95.begin() );
spk_ValueType MDV_95_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_95( 2 );
   copy( MDV_95_c, MDV_95_c+2, MDV_95.begin() );
int EVID_95_c[] = { 0, 0 };
   std::vector<int> EVID_95( 2 );
   copy( EVID_95_c, EVID_95_c+2, EVID_95.begin() );
   data[95] = new IndData<spk_ValueType>( 2, ID_95, TIME_95, DV_95, AMT_95, MDV_95, EVID_95 );

   //------------------------------------
   // Subject <97> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[96] = 2;
char* ID_96_c[] = { "97", "97" };
   std::vector<char*> ID_96( 2 );
   copy( ID_96_c, ID_96_c+2, ID_96.begin() );
spk_ValueType TIME_96_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_96( 2 );
   copy( TIME_96_c, TIME_96_c+2, TIME_96.begin() );
spk_ValueType DV_96_c[] = { -0.0665698, 0.743039 };
   std::vector<spk_ValueType> DV_96( 2 );
   copy( DV_96_c, DV_96_c+2, DV_96.begin() );
spk_ValueType AMT_96_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_96( 2 );
   copy( AMT_96_c, AMT_96_c+2, AMT_96.begin() );
spk_ValueType MDV_96_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_96( 2 );
   copy( MDV_96_c, MDV_96_c+2, MDV_96.begin() );
int EVID_96_c[] = { 0, 0 };
   std::vector<int> EVID_96( 2 );
   copy( EVID_96_c, EVID_96_c+2, EVID_96.begin() );
   data[96] = new IndData<spk_ValueType>( 2, ID_96, TIME_96, DV_96, AMT_96, MDV_96, EVID_96 );

   //------------------------------------
   // Subject <98> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[97] = 2;
char* ID_97_c[] = { "98", "98" };
   std::vector<char*> ID_97( 2 );
   copy( ID_97_c, ID_97_c+2, ID_97.begin() );
spk_ValueType TIME_97_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_97( 2 );
   copy( TIME_97_c, TIME_97_c+2, TIME_97.begin() );
spk_ValueType DV_97_c[] = { 0.029398, 0.200666 };
   std::vector<spk_ValueType> DV_97( 2 );
   copy( DV_97_c, DV_97_c+2, DV_97.begin() );
spk_ValueType AMT_97_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_97( 2 );
   copy( AMT_97_c, AMT_97_c+2, AMT_97.begin() );
spk_ValueType MDV_97_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_97( 2 );
   copy( MDV_97_c, MDV_97_c+2, MDV_97.begin() );
int EVID_97_c[] = { 0, 0 };
   std::vector<int> EVID_97( 2 );
   copy( EVID_97_c, EVID_97_c+2, EVID_97.begin() );
   data[97] = new IndData<spk_ValueType>( 2, ID_97, TIME_97, DV_97, AMT_97, MDV_97, EVID_97 );

   //------------------------------------
   // Subject <99> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[98] = 2;
char* ID_98_c[] = { "99", "99" };
   std::vector<char*> ID_98( 2 );
   copy( ID_98_c, ID_98_c+2, ID_98.begin() );
spk_ValueType TIME_98_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_98( 2 );
   copy( TIME_98_c, TIME_98_c+2, TIME_98.begin() );
spk_ValueType DV_98_c[] = { 0.000240563, 0.883807 };
   std::vector<spk_ValueType> DV_98( 2 );
   copy( DV_98_c, DV_98_c+2, DV_98.begin() );
spk_ValueType AMT_98_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_98( 2 );
   copy( AMT_98_c, AMT_98_c+2, AMT_98.begin() );
spk_ValueType MDV_98_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_98( 2 );
   copy( MDV_98_c, MDV_98_c+2, MDV_98.begin() );
int EVID_98_c[] = { 0, 0 };
   std::vector<int> EVID_98( 2 );
   copy( EVID_98_c, EVID_98_c+2, EVID_98.begin() );
   data[98] = new IndData<spk_ValueType>( 2, ID_98, TIME_98, DV_98, AMT_98, MDV_98, EVID_98 );

   //------------------------------------
   // Subject <100> 
   // # of sampling points = 2
   //------------------------------------
   NRecords[99] = 2;
char* ID_99_c[] = { "100", "100" };
   std::vector<char*> ID_99( 2 );
   copy( ID_99_c, ID_99_c+2, ID_99.begin() );
spk_ValueType TIME_99_c[] = { 0, 1 };
   std::vector<spk_ValueType> TIME_99( 2 );
   copy( TIME_99_c, TIME_99_c+2, TIME_99.begin() );
spk_ValueType DV_99_c[] = { 0.00286124, 1.92676 };
   std::vector<spk_ValueType> DV_99( 2 );
   copy( DV_99_c, DV_99_c+2, DV_99.begin() );
spk_ValueType AMT_99_c[] = { 0.0, 0.0 };
   std::vector<spk_ValueType> AMT_99( 2 );
   copy( AMT_99_c, AMT_99_c+2, AMT_99.begin() );
spk_ValueType MDV_99_c[] = { 0, 0 };
   std::vector<spk_ValueType> MDV_99( 2 );
   copy( MDV_99_c, MDV_99_c+2, MDV_99.begin() );
int EVID_99_c[] = { 0, 0 };
   std::vector<int> EVID_99( 2 );
   copy( EVID_99_c, EVID_99_c+2, EVID_99.begin() );
   data[99] = new IndData<spk_ValueType>( 2, ID_99, TIME_99, DV_99, AMT_99, MDV_99, EVID_99 );


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

// Sets the record values that have measurement values for all of the individuals.
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
// Sets the record values that have measurement values for the who-th individual.
template <class spk_ValueType>
void DataSet<spk_ValueType>::expand( 
       int who,
       const SPK_VA::valarray<double>& measurements_who,
       SPK_VA::valarray<double>& records_who ) const
{
   const int n = measurements_who.size();
   assert( n == NObservs[who] );
   int m = getNRecords(who);
   assert( m >= n );
   records_who.resize( m );
   records_who = 0.0;
   for( int i=0; i<n; i++ )
   {
      records_who[ getRecordIndex( who, i ) ] = measurements_who[i];
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   const int nEta = 1; // the length of eta
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
   o << "columns=\"32\">" << endl;
   o << "ID,AMT,CETARES(1),CPRED,CRES,CWETARES(1),CWRES,DV,EPS(1),ETA(1),ETARES(1),EVID,F,IETARES(1),IPRED,IRES,IWETARES(1),IWRES,MDV,ORGDV,PETARES(1),PPRED,PRED,PRES,PWETARES(1),PWRES,RES,THETA(1),TIME,WETARES(1),WRES,Y" << endl;
   for( int i=0, position=1; i<A.getPopSize(); i++ )
   {
      for( int j=0; j<A.NRecords[i]; j++, position++ )
      {
         o << A.data[i]->ID[j] << "," << A.data[i]->AMT[j] << "," << A.data[i]->CETARES[j][0] << "," << A.data[i]->CPRED[j] << "," << A.data[i]->CRES[j] << "," << A.data[i]->CWETARES[j][0] << "," << A.data[i]->CWRES[j] << "," << A.data[i]->DV[j] << "," << A.data[i]->EPS[j][0] << "," << A.data[i]->ETA[j][0] << "," << A.data[i]->ETARES[j][0] << "," << A.data[i]->EVID[j] << "," << A.data[i]->F[j] << "," << A.data[i]->IETARES[j][0] << "," << A.data[i]->IPRED[j] << "," << A.data[i]->IRES[j] << "," << A.data[i]->IWETARES[j][0] << "," << A.data[i]->IWRES[j] << "," << A.data[i]->MDV[j] << "," << A.data[i]->ORGDV[j] << "," << A.data[i]->PETARES[j][0] << "," << A.data[i]->PPRED[j] << "," << A.data[i]->PRED[j] << "," << A.data[i]->PRES[j] << "," << A.data[i]->PWETARES[j][0] << "," << A.data[i]->PWRES[j] << "," << A.data[i]->RES[j] << "," << A.data[i]->THETA[j][0] << "," << A.data[i]->TIME[j] << "," << A.data[i]->WETARES[j][0] << "," << A.data[i]->WRES[j] << "," << A.data[i]->Y[j] << endl;
      }
   }
   o << "</" << "presentation_data" << ">";
};
template <class spk_ValueType>
std::ostream& operator<< ( std::ostream& o, const DataSet<spk_ValueType>& A );
#endif
