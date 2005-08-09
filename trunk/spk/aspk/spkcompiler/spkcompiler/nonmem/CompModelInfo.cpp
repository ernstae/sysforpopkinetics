#include "CompModelInfo.h"
#include "CompartmentInfo.h"

#include <vector>
#include <string>

CompModelInfo::CompModelInfo()
: nCompartments( 0 ),
  nParameters  ( 0 ),
  nEquilibrims ( 0 ),
  compartments ( 1 )
{
}
CompModelInfo::CompModelInfo( const CompModelInfo& right )
: nCompartments( right.nCompartments ),
  nParameters  ( right.nParameters ),
  nEquilibrims ( right.nEquilibrims ),
  compartments ( right.nCompartments )
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
  compartments ( compartmentsIn )
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
  compartments ( nCompartmentsIn )
{
  for( int i=0; i<nCompartments; i++ )
  {
     /*
     names[i] = new char[ 10 ];
     sprintf( names[i], "COMP%d", i+1 );
     compartments[i].setName( names[i] );
     */
     compartments[i].setName( "honda" );
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
  compartments ( compartmentsIn )
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
  compartments ( nCompartmentsIn )
{
  for( int i=0; i<nCompartments; i++ )
  {
     /*
     names[i] = new char[ 10 ];
     sprintf( names[i], "COMP%d", i+1 );
     compartments[i].setName( names[i] );
     */
     compartments[i].setName( "sachiko" );
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
const CompartmentInfo CompModelInfo::operator[]( int i ) const
{
  return compartments[i];
}
CompartmentInfo CompModelInfo::operator[]( int i ) 
{
  return compartments[i];
}
