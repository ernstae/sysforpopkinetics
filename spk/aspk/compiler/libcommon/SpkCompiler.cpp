#include <iostream>

#include <xercesc/util/PlatformUtils.hpp>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationLS.hpp>
#include <xercesc/dom/DOMWriter.hpp>

#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <string.h>
#include <stdlib.h>
#include <fstream>

#include "SpkCompiler.h"
#include "ParseTree.h"
#include "SymbolTable.h"
#include "client.h"


using namespace xercesc;
using namespace std;

////////////////////////////////////////////////////////////////////////////////////
// REVISIT: 06/03/03
// This routine should be replaced by something more rigorous.
////////////////////////////////////////////////////////////////////////////////////
static int error( const char * message )
{
  fprintf( stderr, "!!! ERROR !!! %s (%d: %s)\n", message, __LINE__, __FILE__);
  return 1;
}

SpkCompiler::SpkCompiler()
{
}
SpkCompiler::SpkCompiler( const SpkCompiler& )
{
}
SpkCompiler& SpkCompiler::operator=( const SpkCompiler& )
{
}
SpkCompiler::~SpkCompiler() throw()
{
  delete parser;
  delete table;
  XMLPlatformUtils::Terminate();
}
SpkCompiler::SpkCompiler( client::type c, const string inputSpkML ) throw( XMLException, bad_exception )
  : input( inputSpkML ), 
    initialized( false ), 
    parsed( false ), 
    table( NULL ), 
    parser( NULL ), 
    doc( NULL ),
    who( c )
{
  // Initialize the XML4C2 system
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch(const XMLException &toCatch)
  {
    char buf[256];
    sprintf( buf, "Error during Xerces-c Initialization.\nException message: %s", C(toCatch.getMessage()) );
    error( buf );
    throw;
  }
  table = new SymbolTable( who );
  assert( table != NULL );

  initialized = true;
}
void SpkCompiler::parse() throw( XMLException, DOMException, bad_exception )
{
  assert( initialized );
  parser = new XercesDOMParser();
  parser->setValidationScheme(XercesDOMParser::Val_Auto);
  parser->setDoNamespaces(true);
  parser->setDoSchema(true);
  parser->setValidationSchemaFullChecking(true);
  parser->setCreateEntityReferenceNodes(true);

  try
  {
    ifstream ifs( input.c_str());
    if( !ifs.good() )
      {
	cleanup();
	char buf[256];
	sprintf( buf, "Failed to open %s!\n", input.c_str());
	exit( error( buf ) );
      }
    else
      ifs.close();
    parser->parse(input.c_str());
  }
  catch( const XMLException& e )
  {
    cleanup();
    char buf[256];
    sprintf( buf, "An error occurred during parsing\n   Message: %s\n", C(e.getMessage() ) );
    exit( error( buf ) );
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      char buf[256];
      sprintf( buf, "DOM Error during parsing \"%s\"\nDOMException code is: %d\n", input.c_str(), e.code );
      exit( error( buf ) );

      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
	cleanup();
	char buf[256];
	sprintf( buf, "Message is: %s", C(errText) );
	exit( error( buf ) );
      }
  }
  catch( ... )
  {
    cleanup();
    char buf[128];
    sprintf( buf, "An unknown error occurred during parsing\n " );
    exit( error( buf ) );
  }

  //
  // Set "doc" point to the DOM document tree root.
  //
  doc = parser->getDocument();
  assert( doc != NULL );

  //
  // Verify at least the presense of these top level elements:
  //  <content>
  //  <data>
  //  <model>
  //  <driver>
  //
  // REVISIT - Sachiko 06/16/03
  //    If any of them were missing, exit with a non-zero value.
  //    A more sophisticated error handling should replace
  //    this current brutal behavior.
  // 
  if( doc->getElementsByTagName( X( "content" ) ) == NULL )
    {
      cleanup();
      char buf[128];
      sprintf( buf, "Missing \"content\" tag!\n" );
      exit( error( buf ) );
    }
  if( doc->getElementsByTagName( X( "data" ) ) == NULL )
    {
      cleanup();
      char buf[128];
      sprintf( buf, "Missing \"data\" tag!\n" );
      exit( error( buf ) );
    }
  if( doc->getElementsByTagName( X( "driver" ) ) == NULL )
    {
      cleanup();
      char buf[128];
      sprintf( buf, "Missing \"driver\" tag!\n" );
      exit( error( buf ) );
    }
  if( doc->getElementsByTagName( X( "model" ) ) == NULL )
    {
      cleanup();
      char buf[128];
      sprintf( buf, "Missing \"model\" tag!\n" );
      exit( error( buf ) );
    }

  parsed = true;
  return;
}

void SpkCompiler::printTree( ) const throw( XMLException, bad_exception ) 
{
  printTree( doc );
}
/**
 * Output a tree to STDOUT (by default) or a file.
 * @param root is a pointer to the root of the (sub)tree to be printed out.
 * @gOutputFile is a pointer to a filename (default=NULL).
 */
void SpkCompiler::printTree( DOMNode * root, const char * gOutputFile ) 
  const throw( XMLException, bad_exception )
{
  try
  {
      // get a serializer, an instance of DOMWriter
      XMLCh tempStr[100];
      XMLString::transcode("LS", tempStr, 99);
      DOMImplementation *impl          = DOMImplementationRegistry::getDOMImplementation(tempStr);
      DOMWriter         *theSerializer = ((DOMImplementationLS*)impl)->createDOMWriter();

      // set user specified end of line sequence and output encoding
      const XMLCh * gMyEOLSequence  = 0;
      XMLCh * gOutputEncoding = 0;
      theSerializer->setNewLine(gMyEOLSequence);
      theSerializer->setEncoding(gOutputEncoding);

      //
      // Plug in a format target to receive the resultant
      // XML stream from the serializer.
      //
      // StdOutFormatTarget prints the resultant XML stream
      // to stdout once it receives any thing from the serializer.
      //
      XMLFormatTarget *myFormTarget;
      if (gOutputFile)
          myFormTarget = new LocalFileFormatTarget(gOutputFile);
      else
          myFormTarget = new StdOutFormatTarget();

      //
      // do the serialization through DOMWriter::writeNode();
      //
      theSerializer->writeNode(myFormTarget, *root);

      delete theSerializer;

      //
      // Filter, formatTarget and error handler
      // are NOT owned by the serializer.
      //
      delete myFormTarget;
  }
  catch( const XMLException& e )
  {
    char buf[256];
    sprintf( buf, "An error occurred during creation of output transcoder. Msg is:\n\t%s\n", C( e.getMessage() ) );
    exit( error( buf ) );
  }

}
enum client::type SpkCompiler::getClient() const
{
  return who;
}
DOMDocument* SpkCompiler::getDOMDoc() const
{
  return doc;
}
XercesDOMParser* SpkCompiler::getDOMParser() const
{
  return parser;
}
SymbolTable * SpkCompiler::getTable() const
{
  return table;
}
void SpkCompiler::cleanup()
{
  delete parser;
  delete table;
}
