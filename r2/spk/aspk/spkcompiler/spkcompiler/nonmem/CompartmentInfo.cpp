/**
 * @file CompartmentInfo.cpp
 * Define the CompartmentInfo class.
 */
#include "CompartmentInfo.h"
#include <iostream>
#include <string>
using namespace std;

CompartmentInfo& CompartmentInfo::operator=( const CompartmentInfo& right )
{
  name = right.name;
  initial_off         = right.initial_off;
  no_off              = right.no_off;
  no_dose             = right.no_dose;
  equilibrim          = right.equilibrim;
  exclude             = right.exclude;
  default_observation = right.default_observation;
  default_dose        = right.default_dose;
}
CompartmentInfo::~CompartmentInfo()
{
}

CompartmentInfo::CompartmentInfo()
: name               ( "dummy" ),
  initial_off        ( false ),
  no_off             ( false ),
  no_dose            ( false ),
  equilibrim         ( false ),
  exclude            ( false ),
  default_observation( false ),
  default_dose       ( false )
{
}

CompartmentInfo::CompartmentInfo( const std::string& nameIn )
: name               ( nameIn ),
  initial_off        ( false ),
  no_off             ( false ),
  no_dose            ( false ),
  equilibrim         ( false ),
  exclude            ( false ),
  default_observation( false ),
  default_dose       ( false )
{
}
CompartmentInfo::CompartmentInfo( const std::string& nameIn,
                          bool initial_offIn,
                          bool no_offIn,
                          bool no_doseIn,
                          bool equilibrimIn,
                          bool excludeIn,
                          bool default_observationIn,
                          bool default_doseIn )
: name               ( nameIn ),
  initial_off        ( initial_offIn ),
  no_off             ( no_offIn ),
  no_dose            ( no_doseIn ),
  equilibrim         ( equilibrimIn ),
  exclude            ( excludeIn ),
  default_observation( default_observationIn ),
  default_dose       ( default_doseIn )
{
}
CompartmentInfo::CompartmentInfo( const CompartmentInfo& right )
: name               ( right.name ),
  initial_off        ( right.initial_off ),
  no_off             ( right.no_off ),
  no_dose            ( right.no_dose ),
  equilibrim         ( right.equilibrim ),
  exclude            ( right.exclude ),
  default_observation( right.default_observation ),
  default_dose       ( right.default_dose )
{
}
const std::string CompartmentInfo::getName() const
{
  return name;
}
bool CompartmentInfo::is_initial_off() const
{
  return initial_off;
}
bool CompartmentInfo::is_no_off() const
{
  return no_off;
}
bool CompartmentInfo::is_no_dose() const
{
  return no_dose;
}
bool CompartmentInfo::is_equilibrim() const
{
  return equilibrim;
}
bool CompartmentInfo::is_exclude() const
{
  return exclude;
}
bool CompartmentInfo::is_default_observation() const
{
  return default_observation;
}
bool CompartmentInfo::is_default_dose() const
{
  return default_dose;
}
void CompartmentInfo::setName( const std::string& nameIn )
{
  name = nameIn;
  copy( nameIn.begin(), nameIn.end(), name.begin() );
}
void CompartmentInfo::set_initial_off( bool initial_offIn )
{
  initial_off = initial_offIn;
}
void CompartmentInfo::set_no_off( bool no_offIn )
{
  no_off = no_offIn;
}
void CompartmentInfo::set_no_dose( bool no_doseIn )
{
  no_dose = no_doseIn;
}
void CompartmentInfo::set_equilibrim( bool equilibrimIn )
{
  equilibrim = equilibrimIn;
}
void CompartmentInfo::set_exclude( bool excludeIn )
{
  exclude = excludeIn;
}
void CompartmentInfo::set_default_observation( bool default_observationIn )
{
  default_observation = default_observationIn;
}
void CompartmentInfo::set_default_dose( bool default_doseIn )
{
  default_dose = default_doseIn;
}
