/** 
 * @file compiler.cpp
 *
 * This is the ASPK Compliler that compiles C++ source code from
 * a pair of SpkSourceML and SpkDataML documents.
 *
 * Usage: <code>compiler <em>SOURCE</em> <em>DATA</em> [-print]</code>
 * @htmlonly
 * <dl>
 *   <dt><em>SOURCE</em></dt><dd>file path to an SpkSourceML document</dd>
 *   <dt><em>DATA</i></em><dd>file path to an SpkDataML document</dd>
 *   <dt>-print</dt><dd>request for displaying the progress in the standard output.</dd>
 * </dl>
 * @endhtmlonly
 *
 */

#include <iostream>
#include <fstream>
#include <stdlib.h>


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/time.h>

#include <spkcompiler/client.h>
#include <spkcompiler/SpkCompilerException.h>
#include <spkcompiler/ClientTranslator.h>
#include <spkcompiler/nonmem/NonmemTranslator.h>

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
 
   if( XMLString::equals( c, XMLString::transcode("nonmem") ) )
      return client::NONMEM;
   return client::NOT_SUPPORTED;
}

/**
 * Usage: <code>compiler <em>SOURCE</em> <em>DATA</em> [-print]</code>
 *
 * <dl>
 *   <dt><em>SOURCE</em></dt><dd>file path to an SpkSourceML document</dd>
 *   <dt><em>DATA</i></em><dd>file path to an SpkDataML document</dd>
 *   <dt>-print</dt><dd>request for displaying the progress in the standard output.</dd>
 * </dl>
 */
static void usage()
{
  cout << "Usage: compiler SOURCE DATA [-print]" << endl;
  cout << endl;
  cout << "   SOURCE    --- file path to an SpkSourceML document" << endl;
  cout << "   DATA      --- file path to an SpkDataML document" << endl;
  cout << "   -print    --- request for displaying the progress in the standard output " << endl;
  return;
}

class SpkCompilerErrorXML{

public:
  SpkCompilerErrorXML()
  {}

  int push( enum SpkCompilerError::ErrorCode code, const char* message, int line, const char* file )
  {
    SpkCompilerError e ( code, message, line, file );
    myException.push( e );

    return myException.size();
  }
  int push( const SpkCompilerError& e )
  {
    myException.push( e );
    return myException.size();
  }

  friend ostream& operator<<( ostream& o, const SpkCompilerErrorXML& obj )
  {
    o << "<spkresultML>" << endl;
    o << "<error_message>" << endl;
    o << obj.myException << endl;
    o << "</error_message>" << endl;
    o << "</spkresultML>" << endl;
    return o;
  }

private:
  SpkCompilerException myException;  
};

enum RETURN_CODE { SUCCESS=0, FAILURE };

/**
 * <code>compiler</code> compiles C++ source code from 
 * a pair of an SpkSourceML and an SpkDataML documents.
 * <em>When 0 is returned</em>, there will be a set of following files in the current directory:
 * <dl>
 *   <dt><code>driver.cpp</code></dt><dd>The CSPK driver for the given pair of SpkSourceML
 *       and SpkDataML documents.</dd>
 *   <dt><code>Pred.h</code></dt><dd>Declare and define a template class, Pred.</dd>
 *   <dt><code>DataSet.h</code></dt><dd>Declare and define a template class, DataSet.</dd>
 *   <dt><code>IndData.h</code></dt><dd>Declare and define a template class, IndData.</dd>
 *   <dt><code>generatedMakefile</code></dt><dd>Makefile file to build an executable from 
 *       the above files.</dd>
 * </dl>
 * <em>When non-zero is returned</em>, a file named:
 * <dl>
 * <dt><code>compilation_error.xml</code></dt><dd>an SpkResultML document containing error messages.</dd>
 * </dl>
 * will be placed in the current directory.
 *
 */
int main( int argc, char * argv[] )
{
  SpkCompilerException myError;
  char compilation_error_xml[] = "compilation_error.xml";
  char error_message[ SpkCompilerError::maxMessageLen() ];
  ofstream oError( compilation_error_xml );
  if( !oError.good() )
    {
      cerr << "Failed to create a file, " << compilation_error_xml << "!!!" << endl;
      return FAILURE;
    }

  if (argc < 3)
  {
    usage();
    sprintf( error_message, "Usage: compiler SOURCE DATA [-print]" );
    myError.push( SpkCompilerError::SPK_COMPILER_USER_INPUT_ERR, error_message, __LINE__, __FILE__ );
    oError << myError << endl;
    oError.close();
    
    return FAILURE;
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
      sprintf( error_message, "Error during Xerces-c Initialization.\nException message: %s.\n",
               XMLString::transcode( toCatch.getMessage() ) );
      myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
      oError << myError << endl;
      oError.close();
      return FAILURE;
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
        sprintf( error_message, "Failed to open %s!\n", gSource );
	myError.push( SpkCompilerError::SPK_COMPILER_STD_ERR, error_message, __LINE__, __FILE__ );
	oError << myError << endl;
	oError.close();
        return FAILURE;
      }
    iSource.close();
    parser->parse( gSource );
    source = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    sprintf( error_message, "An error occurred during parsing\n   Message: %s\n", 
	     XMLString::transcode(e.getMessage() ) );
    myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
    oError << myError << endl;
    oError.close();
    return FAILURE;
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
	
        sprintf( error_message, "Message is: %s.\n", 
		 XMLString::transcode(errText) );
	myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
	oError << myError << endl;
	oError.close();
        return FAILURE;
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    sprintf( error_message, "An unknown error occurred during parsing.\n %d, %s\n" );
    myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
    return FAILURE;
  }

  //
  // Parsing the SpkDataML document.
  //
  try{
    ifstream iData( gData );
    if( !iData.good() )
    {
       XMLPlatformUtils::Terminate();
       sprintf( error_message, "Failed to open %s!\n", gData );
       myError.push( SpkCompilerError::SPK_COMPILER_STD_ERR, error_message, __LINE__, __FILE__ );
       oError << myError << endl;
       oError.close();
       return FAILURE;
    }
    parser->parse( gData );
    data = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    sprintf( error_message, "An error occurred during parsing\n   Message: %s\n", 
	     XMLString::transcode(e.getMessage() ) );
    myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
    oError << myError << endl;
    oError.close();
    return FAILURE;
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
        sprintf( error_message, "Message is: %s.\n", XMLString::transcode(errText) );
	myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
	oError << myError << endl;
	oError.close();
        return FAILURE;
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    fprintf( stderr, "An unknown error occurred during parsing %s.\n", gData );
    myError.push( SpkCompilerError::SPK_COMPILER_XMLDOM_ERR, error_message, __LINE__, __FILE__ );
    oError << myError << endl;
    oError.close();
    return FAILURE;
  }
                                                                                
  client::type cl = getClientName( source );
  cout << "client = " << (cl==client::NONMEM? "nonmem" : "not supported") << endl;

  // Call/construct an appropriate translator.

  ClientTranslator *xlator = NULL;

  if( cl == client::NONMEM )
    xlator = new NonmemTranslator( source, data );

  xlator->translate();
  delete xlator;

  return SUCCESS;
}
