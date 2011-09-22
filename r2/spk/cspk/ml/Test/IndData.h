/* Linear Model: FO 100*/
#ifndef INDDATA_H
#define INDDATA_H

#include <vector>
#include <map>
#include <spk/SpkValarray.h>
#include <spk/cholesky.h>
#include <spk/multiply.h>
#include <CppAD/CppAD.h>
#include <ginac/ginac.h>

template<class Scalar>
Scalar undefinedValue()
{
  // The default undefined value is a NaN (Not a Number).
  double zero = 0.0;
  return zero / zero;
}

template<>
GiNaC::ex undefinedValue< GiNaC::ex >()
{
  // The undefined value for double values used in GiNaC expressions
  // is -99999.0 because GiNaC does not accept NaN's.
  return -99999.0;
}

template <class spk_ValueType>
class IndData{
public:
   IndData( int nRecordsIn,
	   const std::vector<char*> & IDIn,
	   const std::vector<spk_ValueType> & TIMEIn,
	   const std::vector<spk_ValueType> & DVIn,
	   const std::vector<spk_ValueType> & AMTIn,
	   const std::vector<spk_ValueType> & MDVIn,
	   const std::vector<int> & EVIDIn );

   std::vector<spk_ValueType> AMT;
   std::vector< std::vector<spk_ValueType> > CETARES;
   std::vector<spk_ValueType> CPRED;
   std::vector<spk_ValueType> CRES;
   std::vector< std::vector<spk_ValueType> > CWETARES;
   std::vector<spk_ValueType> CWRES;
   std::vector<spk_ValueType> DV;
   std::vector< std::vector<spk_ValueType> > EPS;
   std::vector< std::vector<spk_ValueType> > ETA;
   std::vector< std::vector<spk_ValueType> > ETARES;
   std::vector<int> EVID;
   std::vector<spk_ValueType> F;
   std::vector<char*> ID;
   std::vector< std::vector<spk_ValueType> > IETARES;
   std::vector<spk_ValueType> IPRED;
   std::vector<spk_ValueType> IRES;
   std::vector< std::vector<spk_ValueType> > IWETARES;
   std::vector<spk_ValueType> IWRES;
   std::vector<spk_ValueType> MDV;
   std::vector<spk_ValueType> ORGDV;
   std::vector< std::vector<spk_ValueType> > PETARES;
   std::vector<spk_ValueType> PPRED;
   std::vector<spk_ValueType> PRED;
   std::vector<spk_ValueType> PRES;
   std::vector< std::vector<spk_ValueType> > PWETARES;
   std::vector<spk_ValueType> PWRES;
   std::vector<spk_ValueType> RES;
   std::vector< std::vector<spk_ValueType> > THETA;
   std::vector<spk_ValueType> TIME;
   std::vector< std::vector<spk_ValueType> > WETARES;
   std::vector<spk_ValueType> WRES;
   std::vector<spk_ValueType> Y;

   ~IndData();
   int getNRecords() const;
   int getNObservs() const;
   const SPK_VA::valarray<double> getMeasurements() const;
   int getRecordIndex( int measurementIndex ) const;
   int getMeasurementIndex( int recordIndex ) const;
   void replaceMeasurements( const SPK_VA::valarray<double>& yyi );
   void replacePred   ( const SPK_VA::valarray<double>& predIn );
   void replaceRes    ( const SPK_VA::valarray<double>& ResIn );
   void replaceWRes   ( const SPK_VA::valarray<double>& WresIn );
   void replacePPred  ( const SPK_VA::valarray<double>& pPredIn );
   void replacePRes   ( const SPK_VA::valarray<double>& pResIn );
   void replacePWRes  ( const SPK_VA::valarray<double>& pWResIn );
   void replaceIPred  ( const SPK_VA::valarray<double>& iPredIn );
   void replaceIRes   ( const SPK_VA::valarray<double>& iResIn );
   void replaceIWRes  ( const SPK_VA::valarray<double>& iWresIn );
   void replaceCPred  ( const SPK_VA::valarray<double>& cPredIn );
   void replaceCRes   ( const SPK_VA::valarray<double>& cResIn );
   void replaceCWRes  ( const SPK_VA::valarray<double>& cWresIn );
   void replaceEta     ( const SPK_VA::valarray<double>& etaIn );
   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaresIn );
   void replaceWEtaRes ( const SPK_VA::valarray<double>& WetaresIn );
   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaresIn );
   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWetaresIn );
   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResIn );
   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn );
   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResIn );
   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn );

protected:
   IndData();
   IndData( const IndData& );
   IndData& operator=( const IndData& );

   int nY; // #of measurements (DVs where MDV=0).
   SPK_VA::valarray<double> measurements;
private:
   const int nRecords; // the number of data records.
   void assign( double&, const CppAD::AD< CppAD::AD< CppAD::AD<double> > >& ) const;
   void assign( double&, const CppAD::AD< CppAD::AD<double> >& ) const;
   void assign( double&, const CppAD::AD<double>& ) const;
   void assign( double&, const GiNaC::ex& ) const;
   void assign( double&, double ) const;
   /////////////////////////////////////////////////////////
   //      original                     y
   //  -------------------      -------------------
   //   j    i   MDV   DV         j'  j   i   DV
   //  -------------------      -------------------
   //   0    0    0    0.1        0   0   0   0.1
   //   1    0    1               1   2   0   0.2
   //   2    0    0    0.2        2   4   0   0.3
   //   3    0    1
   //   4    0    0    0.3
   //
   //
   //   jTojPrime            jPrimeToj
   //  -----------          -----------
   //    j    j'              j'   j
   //  -----------          -----------
   //    0    0               0    0
   //    1   -1*              1    2
   //    2    1               2    4
   //    3   -1*
   //    4    2
   //
   //  * (-1) points to no j', i.e. MDV=1
   /////////////////////////////////////////////////////////
   std::vector<int> jTojPrime;
   std::vector<int> jPrimeToj;
};
template <class spk_ValueType>
IndData<spk_ValueType>::IndData( int nRecordsIn,
const std::vector<char*> & IDIn,
const std::vector<spk_ValueType> & TIMEIn,
const std::vector<spk_ValueType> & DVIn,
const std::vector<spk_ValueType> & AMTIn,
const std::vector<spk_ValueType> & MDVIn,
const std::vector<int> & EVIDIn)
: nRecords( nRecordsIn ), // # of records (including MDV=0)
  nY( 0 )   // # of measurements
,
ID( IDIn ),
TIME( TIMEIn ),
DV( DVIn ),
AMT( AMTIn ),
MDV( MDVIn ),
EVID( EVIDIn ),
CETARES( nRecordsIn ),
CPRED( nRecordsIn ),
CRES( nRecordsIn ),
CWETARES( nRecordsIn ),
CWRES( nRecordsIn ),
EPS( nRecordsIn ),
ETA( nRecordsIn ),
ETARES( nRecordsIn ),
F( nRecordsIn ),
IETARES( nRecordsIn ),
IPRED( nRecordsIn ),
IRES( nRecordsIn ),
IWETARES( nRecordsIn ),
IWRES( nRecordsIn ),
ORGDV( nRecordsIn ),
PETARES( nRecordsIn ),
PPRED( nRecordsIn ),
PRED( nRecordsIn ),
PRES( nRecordsIn ),
PWETARES( nRecordsIn ),
PWRES( nRecordsIn ),
RES( nRecordsIn ),
THETA( nRecordsIn ),
WETARES( nRecordsIn ),
WRES( nRecordsIn ),
Y( nRecordsIn )
{
   for( int j=0; j<nRecords; j++ )
   {
      if( MDV[j] == 0 )
      {
         nY++;
      }
   }
   measurements.resize( nY );
   //
   // Initialize scalar variables with a value that indicates
   // undefined values. 
   //
   spk_ValueType undefVal = undefinedValue<spk_ValueType>();
   fill( CPRED.begin(), CPRED.end(), undefVal );
   fill( CRES.begin(), CRES.end(), undefVal );
   fill( CWRES.begin(), CWRES.end(), undefVal );
   fill( F.begin(), F.end(), undefVal );
   fill( IPRED.begin(), IPRED.end(), undefVal );
   fill( IRES.begin(), IRES.end(), undefVal );
   fill( IWRES.begin(), IWRES.end(), undefVal );
   fill( ORGDV.begin(), ORGDV.end(), undefVal );
   fill( PPRED.begin(), PPRED.end(), undefVal );
   fill( PRED.begin(), PRED.end(), undefVal );
   fill( PRES.begin(), PRES.end(), undefVal );
   fill( PWRES.begin(), PWRES.end(), undefVal );
   fill( RES.begin(), RES.end(), undefVal );
   fill( WRES.begin(), WRES.end(), undefVal );
   fill( Y.begin(), Y.end(), undefVal );

copy( DV.begin(), DV.end(), ORGDV.begin() );

   //
   // Resize and initialize vector variables
   //
   jTojPrime.resize( nRecords );
   jPrimeToj.resize( nY );
   for( int j=0, jPrime=0; j<nRecords; j++ )
   {
      THETA[j].resize( 1 );
      fill( THETA[j].begin(), THETA[j].end(), undefVal );
      ETA[j].resize( 1 );
      fill( ETA[j].begin(), ETA[j].end(), undefVal );
      ETARES[j].resize( 1 );
      WETARES[j].resize( 1 );
      IETARES[j].resize( 1 );
      IWETARES[j].resize( 1 );
      PETARES[j].resize( 1 );
      PWETARES[j].resize( 1 );
      CETARES[j].resize( 1 );
      CWETARES[j].resize( 1 );
      fill( ETARES[j].begin(), ETARES[j].end(), undefVal );
      fill( WETARES[j].begin(), WETARES[j].end(), undefVal );
      fill( IETARES[j].begin(), IETARES[j].end(), undefVal );
      fill( IWETARES[j].begin(), IWETARES[j].end(), undefVal );
      fill( PETARES[j].begin(), PETARES[j].end(), undefVal );
      fill( PWETARES[j].begin(), PWETARES[j].end(), undefVal );
      fill( CETARES[j].begin(), CETARES[j].end(), undefVal );
      fill( CWETARES[j].begin(), CWETARES[j].end(), undefVal );
      EPS[j].resize( 1 );
      fill( EPS[j].begin(), EPS[j].end(), undefVal );
        if( MDV[j] == 0 )
        {
           assign( measurements[jPrime], DV[j] );
           jPrimeToj[jPrime] = j;
           jTojPrime[j] = jPrime;
           jPrime++;
        }
        else
        {
           jTojPrime[j] = -1;
        }
   } // end of FOR loop over vector variables
}

template <class spk_ValueType>
IndData<spk_ValueType>::~IndData(){}
template <class spk_ValueType>
IndData<spk_ValueType>::IndData(){}
template <class spk_ValueType>
IndData<spk_ValueType>::IndData( const IndData<spk_ValueType>& ){}
template <class spk_ValueType>
IndData<spk_ValueType>& IndData<spk_ValueType>::operator=( const IndData<spk_ValueType>& ){}
// return the number of data records (include MDV=1)
template <class spk_ValueType>
int IndData<spk_ValueType>::getNRecords() const 
{
   return nRecords;
}
// return the number of measurements (only ones with MDV=0)
template <class spk_ValueType>
int IndData<spk_ValueType>::getNObservs() const 
{
   return nY;
}
// Return an index to y (measurements/DVs) vector, j, such that 
// the value of j-th element in y corresponds to the DV value of 
// the j'-th record in the data set .  
// If the j'-th record does not have a DV value, the returned value is -1.
template <class spk_ValueType>
int IndData<spk_ValueType>::getMeasurementIndex( int recordIndex ) const
{
   return jTojPrime[recordIndex];
}
// Return the index, j', to a record in the dataset to which the value of 
// the j-th element of y (measurements/DVs) vector belongs.
template <class spk_ValueType>
int IndData<spk_ValueType>::getRecordIndex( int measurementIndex ) const
{
   return jPrimeToj[measurementIndex];
}
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
   for( int i=0, k=0; i<nRecords; i++ )
   {
      if( MDV[i] == 0 )
      {
         ORGDV[i] = DV[i];
         DV[i] = yyi[k];
         k++;
      }
   }
}
template <class spk_ValueType>
void IndData<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& predIn )
{
   assert( predIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->PRED.begin();
   for( int i=0; itr != this->PRED.end(); itr++, i++ )
   {
      *itr = predIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )
{
   assert( ResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = RES.begin();
   for( int i=0; itr != RES.end(); itr++, i++ )
   {
      *itr = ResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )
{
   assert( WResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = WRES.begin();
   for( int i=0; itr != WRES.end(); itr++, i++ )
   {
      *itr = WResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )
{
   assert( iPredIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = IPRED.begin();
   for( int i=0; itr != IPRED.end(); itr++, i++ )
   {
      *itr = iPredIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )
{
   assert( iResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = IRES.begin();
   for( int i=0; itr != IRES.end(); itr++, i++ )
   {
      *itr = iResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )
{
   assert( iWResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = IWRES.begin();
   for( int i=0; itr != IWRES.end(); itr++, i++ )
   {
      *itr = iWResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )
{
   assert( pPredIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->PPRED.begin();
   for( int i=0; itr != this->PPRED.end(); itr++, i++ )
   {
      *itr = pPredIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )
{
   assert( pResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->PRES.begin();
   for( int i=0; itr != this->PRES.end(); itr++, i++ )
   {
      *itr = pResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )
{
   assert( pWResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->PWRES.begin();
   for( int i=0; itr != this->PWRES.end(); itr++, i++ )
   {
      *itr = pWResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )
{
   assert( cPredIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->CPRED.begin();
   for( int i=0; itr != this->CPRED.end(); itr++, i++ )
   {
      *itr = cPredIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )
{
   assert( cResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->CRES.begin();
   for( int i=0; itr != this->CRES.end(); itr++, i++ )
   {
      *itr = cResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )
{
   assert( cWResIn.size() == nRecords );
   typename std::vector<spk_ValueType>::iterator itr = this->CWRES.begin();
   for( int i=0; itr != this->CWRES.end(); itr++, i++ )
   {
      *itr = cWResIn[i];
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )
{
   const int nEta = 1;
   assert( etaIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         ETA[i][j] = etaIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )
{
   const int nEta = 1;
   assert( EtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         ETARES[i][j] = EtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )
{
   const int nEta = 1;
   assert( WEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         WETARES[i][j] = WEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )
{
   const int nEta = 1;
   assert( iEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         IETARES[i][j] = iEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )
{
   const int nEta = 1;
   assert( iWEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         IWETARES[i][j] = iWEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )
{
   const int nEta = 1;
   assert( pEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         PETARES[i][j] = pEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )
{
   const int nEta = 1;
   assert( pWEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         PWETARES[i][j] = pWEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )
{
   const int nEta = 1;
   assert( cEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         CETARES[i][j] = cEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )
{
   const int nEta = 1;
   assert( cWEtaResIn.size() == nEta );
   for( int i=0; i<nRecords; i++ )
   {
      for( int j=0; j<nEta; j++ )
      {
         CWETARES[i][j] = cWEtaResIn[j];
      }
   }
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assign( double & d, const CppAD::AD< CppAD::AD< CppAD::AD<double> > >& addd ) const
{
   d = CppAD::Value( CppAD::Value( CppAD::Value( addd ) ) );
   return;
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assign( double & d, const CppAD::AD< CppAD::AD<double> >& add ) const
{
   d = CppAD::Value( CppAD::Value( add ) );
   return;
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assign( double & d, const CppAD::AD<double>& ad ) const
{
   d = CppAD::Value( ad );
   return;
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assign( double & d, const GiNaC::ex& ex ) const
{
   using namespace GiNaC;
   d = ex_to<numeric>( ex ).to_double();
   return;
}

template <class spk_ValueType>
void IndData<spk_ValueType>::assign( double & left, double right ) const
{
   left = right;
   return;
}

#endif
