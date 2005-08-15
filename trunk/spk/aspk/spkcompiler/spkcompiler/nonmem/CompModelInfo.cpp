#include "CompModelInfo.h"
#include "CompartmentInfo.h"

#include <vector>
#include <string>

CompModelInfo::CompModelInfo()
: nCompartments( 0 ),
  nParameters  ( 0 ),
  nEquilibrims ( 0 ),
  compartments ( 1 ),
  isPkFunctionOfT( true )
{
}
CompModelInfo::CompModelInfo( const CompModelInfo& right )
: nCompartments( right.nCompartments ),
  nParameters  ( right.nParameters ),
  nEquilibrims ( right.nEquilibrims ),
  compartments ( right.nCompartments ),
  isPkFunctionOfT( right.isPkFunctionOfT )
{
  for( int i=0; i<nCompartments; i++ )
  {
    compartments[i] = right.compartments[i];
  } 
}
CompModelInfo::~CompModelInfo()
{
}
CompModelInfo& CompModelInfo::operator=( const CompModelInfo& right )
{
  nCompartments = right.nCompartments;
  nParameters   = right.nParameters;
  nEquilibrims  = right.nEquilibrims;
  isPkFunctionOfT = right.isPkFunctionOfT;
  compartments.resize( right.nCompartments );

  for( int i=0; i<nCompartments; i++ )
  {
    compartments[i] = right.compartments[i];
  }
}

CompModelInfo::CompModelInfo( int nCompartmentsIn,
                      int nParametersIn,
                      const std::vector<CompartmentInfo>& compartmentsIn )
: nCompartments( nCompartmentsIn ),
  nParameters  ( nParametersIn ),
  nEquilibrims ( 0 ),
  compartments ( compartmentsIn ),
  isPkFunctionOfT( true )
{
  for( int i=0; i<nCompartments; i++ )
  {
     if( compartments[i].getName() == "CENTRAL" )
        compartments[i].set_default_observation( true );
     if( compartments[i].getName() == "DOSE" )
        compartments[i].set_default_dose( true );
  }
}
CompModelInfo::CompModelInfo( int nCompartmentsIn,
                      int nParametersIn )
: nCompartments( nCompartmentsIn ),
  nParameters  ( nParametersIn ),
  nEquilibrims ( 0 ),
  compartments ( nCompartmentsIn ),
  isPkFunctionOfT( true )
{
  for( int i=0; i<nCompartments; i++ )
  {
     char *name = new char[ 10 ];
     sprintf( name, "COMP%d", i+1 );
     compartments[i].setName( name );
     if( i==0 )
     {
        compartments[i].set_default_observation( true );
        compartments[i].set_default_dose( true );
     }
  }
}
CompModelInfo::CompModelInfo( int nCompartmentsIn,
                      int nParametersIn,
                      int nEquilibrimsIn,
                      const std::vector<CompartmentInfo>& compartmentsIn )
: nCompartments( nCompartmentsIn ),
  nParameters  ( nParametersIn ),
  nEquilibrims ( nEquilibrimsIn ),
  compartments ( compartmentsIn ),
  isPkFunctionOfT( true )
{
  for( int i=0; i<nCompartments; i++ )
  {
     if( compartments[i].getName() == "CENTRAL" )
        compartments[i].set_default_observation( true );
     if( compartments[i].getName() == "DOSE" )
        compartments[i].set_default_dose( true );
  }
}
CompModelInfo::CompModelInfo( int nCompartmentsIn,
                      int nParametersIn,
                      int nEquilibrimsIn )
: nCompartments( nCompartmentsIn ),
  nParameters  ( nParametersIn ),
  nEquilibrims ( nEquilibrimsIn ),
  compartments ( nCompartmentsIn ),
  isPkFunctionOfT( true )
{
  for( int i=0; i<nCompartments; i++ )
  {
     char *name = new char[ 10 ];
     sprintf( name, "COMP%d", i+1 );
     compartments[i].setName( name );
      if( i==0 )
     {
        compartments[i].set_default_observation( true );
        compartments[i].set_default_dose( true );
     }
  }
}
int CompModelInfo::getNCompartments() const
{
  return nCompartments;
}
int CompModelInfo::getNParameters() const
{
  return nParameters;
}
int CompModelInfo::getNEquilibrims() const
{
  return nEquilibrims;
}
bool CompModelInfo::is_pkFunctionOfT() const
{
  return isPkFunctionOfT;
}
void CompModelInfo::setPkFunctionOfT( bool isPkFunctionOfTIn )
{
  isPkFunctionOfT = isPkFunctionOfTIn;
}
  
int CompModelInfo::getDefaultDose() const
{
  for( int i=0; i<nCompartments; i++ )
    {
      if( compartments[i].is_default_dose() )
	return i;
    }
}
int CompModelInfo::getDefaultObservation() const
{
  for( int i=0; i<nCompartments; i++ )
    {
      if( compartments[i].is_default_observation() )
	return i;
    }
}
#include <iostream>
using namespace std;
void CompModelInfo::getInitialOff( std::vector<bool>& initial_off ) const
{
  initial_off.resize( nCompartments );
  for( int i=0; i<nCompartments; i++ )
    {
      initial_off[i] = compartments[i].is_initial_off();
    }
}
void CompModelInfo::getNoOff( std::vector<bool>& no_off ) const
{
  no_off.resize( nCompartments );
  for( int i=0; i<nCompartments; i++ )
    {
      no_off[i] = compartments[i].is_no_off();
    }
}
void CompModelInfo::getNoDose( std::vector<bool>& no_dose ) const
{
  no_dose.resize( nCompartments );
  for( int i=0; i<nCompartments; i++ )
    {
      no_dose[i] = compartments[i].is_no_dose();
    }
}
const CompartmentInfo& CompModelInfo::operator[]( int i ) const
{
  return compartments[i];
}
CompartmentInfo& CompModelInfo::operator[]( int i ) 
{
  return compartments[i];
}
