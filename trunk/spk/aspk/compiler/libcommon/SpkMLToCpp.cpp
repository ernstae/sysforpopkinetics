#include "SpkMLToCpp.h"
#include "SpkCompilerUtil.h"
#include "../libnonmem/NonmemSpkMLToCpp.h"

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <fstream>
#include <vector>

using namespace std;
using namespace xercesc;

static const char* trim( const XMLCh* source )
{
  XMLCh* target = XMLString::replicate( source );
  XMLString::trim( target );
  return C( target );
}

SpkMLToCpp::SpkMLToCpp( const char* inputSpkMLIn )
  : inputSpkML( inputSpkMLIn ), who( client::NOT_SUPPORTED ), tree( NULL ), client_translator( NULL )
{
  initializeDOM();
  tree = buildTreeFromSpkML( inputSpkML );
  who  = discoverClient( tree );
  client_translator = SpkMLToCpp::createChild( who );
  assert( client_translator != NULL );
}

SpkMLToCpp::~SpkMLToCpp()
{
  //  tree->release();
  delete parser;
  //delete client_translator;
  terminateDOM();
}

void SpkMLToCpp::translate()
{
  assemble( tree );
  emit( tree );
}
const SpkMLToCpp * SpkMLToCpp::getInstance() const
{
  return client_translator;
}
const struct FitParameters * SpkMLToCpp::getSpkParameters() const
{
  return client_translator->getSpkParameters();
}
const void* SpkMLToCpp::getClientParameters() const
{
  return client_translator->getClientParameters();
}

void SpkMLToCpp::assemble( xercesc::DOMDocument * tree )
{
  client_translator->assemble( tree );
}
void SpkMLToCpp::emit( xercesc::DOMDocument * tree )
{
  client_translator->emit( tree );
}
enum client::type SpkMLToCpp::getClient() const
{
  return who;
}
const char* SpkMLToCpp::getInputFilename() const
{
  return inputSpkML;
}

const char * SpkMLToCpp::getDriverFilename() const
{
  return driver_file;
}

const std::vector< const char * > SpkMLToCpp::getModelFilenameList() const
{
  return model_files;
}

SpkMLToCpp::SpkMLToCpp()
  : inputSpkML( "EMPTY" ), who( client::NOT_SUPPORTED )
{
}

SpkMLToCpp::SpkMLToCpp( const SpkMLToCpp& right )
{
}

const SpkMLToCpp& SpkMLToCpp::operator=( const SpkMLToCpp& right )
{
  return *this;
}

void SpkMLToCpp::setDriverFilename( const char * filename )
{
  strcpy( driver_file, filename );
}
void SpkMLToCpp::addModelFilename( const char * filename )
{
  model_files.push_back( filename );
}

void SpkMLToCpp::initializeDOM() const
{
  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException & toCatch )
    {
      char buf[256];
      sprintf( buf, "Error during Xerces-c Initialization.\nException message: %s.\n",
	       C(toCatch.getMessage() ) );
      fprintf( stderr, buf );
      throw;
    }
}
void SpkMLToCpp::terminateDOM() const
{
  XMLPlatformUtils::Terminate();
}
DOMDocument* SpkMLToCpp::buildTreeFromSpkML( const char * input )
{
  parser = new xercesc::XercesDOMParser;
  parser->setValidationScheme( XercesDOMParser::Val_Auto );
  parser->setDoNamespaces(true );
  parser->setDoSchema(true );
  parser->setValidationSchemaFullChecking(true);
  parser->setCreateEntityReferenceNodes(true);

  try
  {
    ifstream ifs( input );
    if( !ifs.good() )
      {
	terminateDOM();
	char buf[256];
	sprintf( buf, "Failed to open %s!\n", input);
	fprintf( stderr, buf );
	exit(-1);
      }
    else
      ifs.close();
    parser->parse(input);
  }
  catch( const XMLException& e )
  {
    terminateDOM();
    char buf[256];
    sprintf( buf, "An error occurred during parsing\n   Message: %s\n", C(e.getMessage() ) );
    fprintf( stderr, buf );
    exit( -1 );
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      char buf[256];
      sprintf( buf, "DOM Error during parsing \"%s\"\nDOMException code is: %d\n", input, e.code );
      fprintf( stderr, buf );
      exit(-1);

      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
	terminateDOM();
	char buf[256];
	sprintf( buf, "Message is: %s", C(errText) );
	fprintf( stderr, buf );
	exit( -1 );
      }
  }
  catch( ... )
  {
    terminateDOM();
    char buf[128];
    sprintf( buf, "An unknown error occurred during parsing\n " );
    fprintf( stderr, buf );
    exit( -1 );
  }
  
  return parser->getDocument();
}
enum client::type SpkMLToCpp::discoverClient( const xercesc::DOMDocument* tree ) const
{
  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
  assert( content_node != NULL );

  const char * c_client = trim( content_node->getAttribute( X("client") ) );
  if( strcmp( c_client, "nonmem" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Anything besides \"nonmem\" does not make sense in this context!  \
                  You gave me %s (case sensitive).\n", c_client );
    fprintf( stderr, buf );
    exit( -1 );
  }

  return client::toEnum( c_client );
}
SpkMLToCpp * SpkMLToCpp::createChild( enum client::type who ) const
{
  if( who == client::NONMEM )
    {
      return new NonmemSpkMLToCpp( tree );
    }
  else
    {
      char buf[256];
      sprintf( buf, "Not supported (%s)! (%d, %s)\n", client::toString( who ), __LINE__, __FILE__ );
      fprintf( stderr, buf );
      return NULL;
    }
  return NULL;
}
ClientTranslator * SpkMLToCpp::createTranslator( enum client::type who ) const
{
  if( who == client::NONMEM )
    {
      return new NonmemTranslator( tree );
    }
  else
    {
      char buf[256];
      sprintf( buf, "Not supported (%s)! (%d, %s)\n", client::toString( who ), __LINE__, __FILE__ );
      fprintf( stderr, buf );
      return NULL;
    }
  return NULL;
}
