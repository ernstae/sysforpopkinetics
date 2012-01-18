/**
 * @file CompModelInfo.cpp
 * Define the CompModelInfo class.
 */
#include "CompModelInfo.h"
#include "CompartmentInfo.h"

#include <vector>
#include <string>
#include <stdio.h>

using namespace std;

CompModelInfo::CompModelInfo()
: nCompartments( 0 ),
  nParameters  ( 0 ),
  nEquilibrims ( 0 ),
  compartments ( 1 ),
  is_pkFunctionOfT( true ),
  relTol       ( 0 )
{
}
CompModelInfo::CompModelInfo( const CompModelInfo& right )
: nCompartments   ( right.nCompartments ),
  nParameters     ( right.nParameters ),
  nEquilibrims    ( right.nEquilibrims ),
  compartments    ( right.nCompartments ),
  is_pkFunctionOfT( right.is_pkFunctionOfT ),
  relTol          ( right.relTol )
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
  nCompartments    = right.nCompartments;
  nParameters      = right.nParameters;
  nEquilibrims     = right.nEquilibrims;
  is_pkFunctionOfT = right.is_pkFunctionOfT;
  relTol           = right.relTol;
  compartments.resize( right.nCompartments );

  for( int i=0; i<nCompartments; i++ )
  {
    compartments[i] = right.compartments[i];
  }
}
#include <iostream>
CompModelInfo::CompModelInfo( int nCompartmentsIn,
			      int nParametersIn,
			      int nEquilibrimsIn,
			      double relTolIn )
: nCompartments   ( nCompartmentsIn ),
  nParameters     ( nParametersIn ),
  nEquilibrims    ( nEquilibrimsIn ),
  compartments    ( nCompartmentsIn ),
  is_pkFunctionOfT( true ),
  relTol          ( relTolIn )
{
  for( int i=0; i<nCompartments; i++ )
    {
      char c_name[56];
      snprintf( c_name, 56, "COMP%d", i+1 );
      std::string name = c_name;
      compartments[i].setName( name );
      if( i==0 )
	{
	  compartments[i].set_default_observation( true );
	  compartments[i].set_default_dose       ( true );
	}
      else
	{
	  compartments[i].set_default_observation( false );
	  compartments[i].set_default_dose       ( false );
	}
      compartments[i].set_no_off                 ( false );
      compartments[i].set_no_dose                ( false );
      compartments[i].set_equilibrim             ( false );
      compartments[i].set_exclude                ( false );

      if( i==nCompartments-1 ) // last (output) compartment
	{
	  compartments[i].set_initial_off        ( true );
	}
      else
	{
	  compartments[i].set_initial_off        ( false );
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
bool CompModelInfo::isPkFunctionOfT() const
{
  return is_pkFunctionOfT;
}
void CompModelInfo::setPkFunctionOfT( bool is_pkFunctionOfTIn )
{
  is_pkFunctionOfT = is_pkFunctionOfTIn;
}
double CompModelInfo::getRelTol() const
{
  return relTol;
}
void CompModelInfo::setRelTol( double relTolIn ) 
{
  relTol = relTolIn;
}
int CompModelInfo::getDefaultDose() const
{
  for( int i=0; i<nCompartments; i++ )
    {
      if( compartments[i].is_default_dose() )
	return i+1;
    }
}
int CompModelInfo::getDefaultObservation() const
{
  for( int i=0; i<nCompartments; i++ )
    {
      if( compartments[i].is_default_observation() )
	return i+1;
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
const CompartmentInfo& CompModelInfo::getCompartment( int i ) const
{
  return compartments[i];
}
CompartmentInfo& CompModelInfo::operator[]( int i ) 
{
  return compartments[i];
}
CompartmentInfo& CompModelInfo::getCompartment( int i ) 
{
  return compartments[i];
}

