#include <xercesc/util/XMLString.hpp>
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

SpkCompilerUtil::SpkCompilerUtil()
{
}
SpkCompilerUtil::~SpkCompilerUtil()
{
  releaseXmlStrings();
  releaseCStrings();
}
int SpkCompilerUtil::releaseXmlStrings()
{
  int i;
  for( i=0; i<xml_strings.size(); i++ )
  {
    XMLString::release( &xml_strings[i] );
  }
  xml_strings.erase( xml_strings.begin(), xml_strings.end() );
  return i;
}
int SpkCompilerUtil::releaseCStrings()
{
  int i;
  for( i=0; i<c_strings.size(); i++ )
    {
      XMLString::release( &c_strings[i] );
    }
  c_strings.erase( c_strings.begin(), c_strings.end() );
  return i;
}
const XMLCh * const SpkCompilerUtil::createXmlString( const char * c_str )
{
  XMLCh * x_str = XMLString::transcode( c_str );
  assert( x_str != NULL );
  xml_strings.push_back( x_str );
  return x_str;
}
const char * const SpkCompilerUtil::createCString( const XMLCh * x_str )
{
  char * c_str = XMLString::transcode( x_str );
  assert( c_str != NULL );
  c_strings.push_back( c_str );
  return c_str;
}

#ifndef NDEBUG
const std::vector< XMLCh* > & SpkCompilerUtil::debug_xml_strings()
{
  return xml_strings;
}
const std::vector< char*  > & SpkCompilerUtil::debug_c_strings()
{
  return c_strings;
}
int SpkCompilerUtil::debug_call_releaseXmlStrings()
{
  return releaseXmlStrings();
}
int SpkCompilerUtil::debug_call_releaseCStrings()
{
  return releaseCStrings();
}
#endif
