/** 
 * @file compiler.cpp
 *
 * This is supposed to be the ASPK Compliler driver that
 * makes a pair of SpkSourceML and SpkDataML documents
 * translated into C++ source code and the build information.
 */

#include <iostream>
#include <fstream>
#include <stdlib.h>


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/time.h>

#include <spkcompiler/client.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

using namespace std;
using namespace xercesc;

/**
 * client::type getClientName( xercesc::DOMDocument* source )
 *
 * Extract information about the origin of the source document.
 *
 * @return The enumulator indicating the type of client.
 * @param source A pointer to the DOMDocument tree containing the client information.
 */
static client::type getClientName( xercesc::DOMDocument* source )
{
   DOMElement * root = source->getDocumentElement();
   DOMTreeWalker * walker = source->createTreeWalker( root, 
						      DOMNodeFilter::SHOW_ELEMENT, 
						      NULL, 
						      false );
   const XMLCh * c = walker->firstChild()->getNodeName();
   delete walker;

   if( XMLString::equals( c, XMLString::transcode("nonmem") ) )
      return client::NONMEM;
   return client::NOT_SUPPORTED;
}

/**
 * Usage: <code>compiler <em>SOURCE</em> <em>DATA</em> [-print]</code>
 *
 * <dl>
 *   <dt><em>SOURCE</em></dt><dd>An SpkSourceML document file path</dd>
 *   <dt><em>DATA</i></em><dd>An SpkDataML document file path</dd>
 *   <dt>-print</dt><dd>request for displaying the progress in the standard output.</dd>
 * </dl>
 */
static void usage()
{
  cout << "Usage: compiler SOURCE DATA [-print]" << endl;
  cout << endl;
  cout << "   SOURCE    --- An SpkSourceML document file path" << endl;
  cout << "   DATA      --- An SpkDataML document file path" << endl;
  cout << "   -print    --- request for displaying the progress in the standard output " << endl;
  return;
}
/**
 * The ASPK Compiler driver
 * The driver takes two required arguments and an optional argument.
 * Within it, a parse tree is generated from each XML document: SpkSourceML and SpkDataML.
 * The SpkSourceML document contains the information about the origin of the
 * document.  Once the client is determined, a corresponding XML->C++ translator takes
 * care of the rest of the interpretation work.
 *
 * Usage: <code>compiler <em>SOURCE</em> <em>DATA</em> [-print]</code>
 *
 * <dl>
 *   <dt><em>SOURCE</em></dt><dd>An SpkSourceML document file path</dd>
 *   <dt><em>DATA</i></em><dd>An SpkDataML document file path</dd>
 *   <dt>-print</dt><dd>request for displaying the progress in the standard output.</dd>
 * </dl>
 *
 */
int main( int argc, char * argv[] )
{
  if (argc < 3)
  {
    usage();
    return -1;
  }  
  const char * gSource = argv[1];
  const char * gData   = argv[2];
  bool isPrint = false;
  if( argc == 4 && strcmp( argv[3], "-print") == 0 )
    {
      isPrint = true;
    }
  
  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException & toCatch )
    {
      fprintf( stderr, "Error during Xerces-c Initialization.\nException message: %s.\n",
               XMLString::transcode( toCatch.getMessage() ) );

      return -1;
    }

  //
  // A pointer to the SpkSourceML document.
  //
  xercesc::DOMDocument * source;

  //
  // A pointer to the SpkDataML document.
  //
  xercesc::DOMDocument * data;

  //
  // Initializes the XML DOM parser.
  //
  xercesc::XercesDOMParser *parser = new xercesc::XercesDOMParser;
  parser->setValidationScheme( XercesDOMParser::Val_Auto );
  parser->setDoNamespaces( true );
  parser->setDoSchema( true );
  parser->setValidationSchemaFullChecking( true );
  parser->setCreateEntityReferenceNodes( true );

  //
  // Parsing the SpkSourceML document.
  //
  try
  {
    ifstream iSource( gSource );
    if( !iSource.good() )
      {
        XMLPlatformUtils::Terminate();
        fprintf( stderr, "Failed to open %s!\n", gSource );
        return -1;
      }
    iSource.close();
    parser->parse( gSource );
    source = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    fprintf( stderr, "An error occurred during parsing\n   Message: %s\n", 
	     XMLString::transcode(e.getMessage() ) );
    return -1;
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      fprintf( stderr, "DOM Error during parsing \"%s\".\nDOMException code is: %d\n",
               gSource, e.code );
      
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
        XMLPlatformUtils::Terminate();
        fprintf( stderr, "Message is: %s.\n %d, %s\n", 
		 XMLString::transcode(errText), __LINE__, __FILE__ );
        return -1;
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    fprintf( stderr, "An unknown error occurred during parsing.\n %d, %s\n", 
	     __LINE__, __FILE__ );
    return -1;
  }

  //
  // Parsing the SpkDataML document.
  //
  try{
    ifstream iData( gData );
    if( !iData.good() )
    {
       XMLPlatformUtils::Terminate();
       fprintf( stderr, "Failed to open %s!\n", gData );
       return -1;
    }
    parser->parse( gData );
    data = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    fprintf( stderr, "An error occurred during parsing\n   Message: %s\n", 
	     XMLString::transcode(e.getMessage() ) );
    return -1;
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      fprintf( stderr, "DOM Error during parsing \"%s\".\nDOMException code is: %d\n",
               gData, e.code );
      
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
        XMLPlatformUtils::Terminate();
        fprintf( stderr, "Message is: %s.\n %d, %s\n", 
		 XMLString::transcode(errText), __LINE__, __FILE__ );
        return -1;
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    fprintf( stderr, "An unknown error occurred during parsing.\n %d, %s\n", 
	     __LINE__, __FILE__ );
    return -1;
  }
                                                                                
  client::type cl = getClientName( source );
  cout << "client = " << (cl==client::NONMEM? "nonmem" : "not supported") << endl;

  // Call/construct an appropriate translator.
  // Have it translate the data XML.
  // Have it translate the source XML.
  // Zip archive the generated files and store it in the current directory.

  return 0;
}
