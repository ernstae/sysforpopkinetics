/**
 * @file main.cpp
 * The main body of SPK Compiler.
 */
/**
 * @mainpage SPK Compiler API Documentation 
 *
 * @section overall Overall 
 * For the top down conceptual documentation, <a href="../spkcompiler.html">click here.</a>
 *
 * The code are organized in the following directory structure:
 * <code><pre>
 *    spkcompiler -+- autogen.sh
 *                 +  configure.ac
 *                 +  Makefile.am
 *                 +  README
 *                 +  AUTHORS
 *                 +  NEWS
 *                 +  INSTALL
 *                 +  ChangeLog
 *                 |
 *                 +- spkcompiler    -+-  (common components)
 *                 |                  +-  Makefile.am
 *                 |                  +-  nonmem               -+- (NONMEM specific components)
 *                 |                                            +- Makefile.am
 *                 |
 *                 +- tests          -+-  common               -+- (unit tests for common comp)
 *                 |                  |                         +- Makefile.am
 *                 |                  |
 *                 |                  +-  nonmem               -+- (unit tests for NONMEM comp)
 *                 |                                            +- Makefile.am
 *                 |
 *                 +- doc            -+-  spkcompiler.xml
 *                                    +-  spkcompiler.cfg
 *                                    +-  Makefile.am
 *                                    +-  html                 -+- index.html
 *                                                              +- ...
 * </pre></code>
 *
 * @section install_sec Build & Installation
 * @subsection step1 Step 1: Configure
 * <code><pre>
 *    [r2/src/apps/spk/aspk/spkcompiler] $ ./autogen.sh
 * </pre></code>
 * <code>autogen</code> is a bootstrap script that ultimately
 * executes <code>configure</code> and generates Makefile's.
 * This script is needed to be executed only for the first
 * time after you checkout the module or only after
 * when <code>configure.ac</code> was modified.
 *
 * <code>configure.ac</code> has an extra option as well as
 * standard options provided by the autoconf system.
 * <code><pre>
 *    [r2/src/apps/spk/aspk/spkcompiler] $ ./configure --enable-release-build
 * </pre></code>
 * <code>--enable-release-build</code> option is for building SPK Compiler
 * with full code-optimization and with no debug information.
 * By default, it builds with no code-optimization and with debug info.
 *
 * @subsection step2 Step 2: Make
 * <code><pre>
 *    [r2/src/apps/spk/aspk/spkcompiler] $ make 
 * </pre></code>
 * Running "make" at this level builds all the compiler, unit tests and documentations.
 * If you are interested in only building, for example, the compiler,
 * change directory to "r2/src/apps/spk/aspk/spkcompiler/spkcompiler" and run make.
 *
 * @subsection step3 Step 3: Install
 * <code><pre>
 *    [r2/src/apps/spk/aspk/spkcompiler] $ su
 *    [r2/src/apps/spk/aspk/spkcompiler] $ make install
 * </pre></code>
 * The compiler, <code>spkcompiler</code>, is intalled into <code>/usr/local/bin/spktest/</code>.
 *
 * @section usage Usage
 * <code><pre>
 *    Usage: spkcompiler SOURCE_XML DATA_XML
 *
 *    SOURCE_XML --- an SpkSourceML document
 *    DATA_XML   --- an SpkDataML document
 * </pre></code> 
 *
 * @bug The search facility on this page is not working.
 * The webserver (i.e. "webserver" in RFPK) on which this documentation
 * is installed must be equipped with 
 * <a href="http://us2.php.net/manual/en/install.php">PHP</a>
 * version 4.1.0 or higher.
 *
 * @author Sachiko Honda
 * @date December 23, 2005
 */
/** 
 * The <code>main</code> function defined in this file
 * compiles C++ source code from a pair of an SpkSourceML and an SpkDataML documents.
 *
 * The executable should have a name, <code>spkcompiler</code> and installed into
 * <code>/usr/local/bin/</code> in which the ASPK daemon expects to find it.
 * 
 * - Dependencies:
 *   - xerces-c version 2.5.0 library
 *   - spkcompiler version 0.1 library
 *
 * - Built executable name and install directory requirements:
 *   - <code>spkcompiler</code>
 *   - <code>/usr/local/bin/</code>
 *
 * - Usage: <code>spkcompiler SOURCE DATA</code>
 *   - <em>SOURCE</em>    --- file path to an SpkSourceML document
 *   - <em>DATA</em>      --- file path to an SpkDataML document
 *
 * - @ref EXIT_CODE "Normal Exit Code":
 *   - <code>SUCCESS</code> (0)       --- The compilation completed successfully
 *                         A makefile, <code>Makefile.SPK</code>, along with the source
 *                         code files are generated in the current directory.
 *   - <code>XML_PARSE_ERR</code> (1) --- Syntax errors are found in either/both SOURCE or/and DATA.
 *                         <code>compilation_error.xml</code> will be generated in the current directory.
 *   - <code>ACCESS_ERR</code> (2)    --- File/directory access denied.
 *   - &gt;2              --- Any other error is reported with an unspecified exit code 
 *                        greater than 2. <code>compilation_error.xml</code> may be generated in 
 *                        the current directory.
 *
 * - Generated files (upon <em>successful</em> completion):
 *   - <code>spkDriver.cpp</code> --- The CSPK driver for the given pair of SpkSourceML
 *       and SpkDataML documents.
 *   - <code>Pred.h</code>    --- Declare and define a template class, Pred.
 *   - <code>DataSet.h</code> --- Declare and define a template class, DataSet.
 *   - <code>IndData.h</code> --- Declare and define a template class, IndData.
 *   - <code>Makefile.SPK</code> --- Makefile file to build an executable from 
 *       the above files.
 * 
 * - Generated files (upon <em>abnormal</em> completion):
 *   - <code>compilation_error.xml</code> --- An SpkResult instance containing the list of
 *                                        error messages.  
 *                                        This file may no be generated
 *                                        if the return code is <code>ACCESS_ERR</code>
 *                                        or the program is terminated abruptly 
 *                                        (ex. by asynchronous signals such as 
 *                                        <code>SIG_ABRT</code>, <code>SIG_TERM</code> ...).
 * 
 */

#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/time.h>

#include "client.h"
#include "SpkCompilerException.h"
#include "ClientTranslator.h"
#include "nonmem/NonmemTranslator.h"
#include "DOMPrint.h"

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

using namespace std;
using namespace xercesc;


namespace{ 

  const char compilation_error_xml[] = "compilation_error.xml";

  /**
   * @enum EXIT_CODE
   *
   * Exit codes.
   */
  enum EXIT_CODE { SUCCESS=0,       /**< The compilation completed successfully. */
                   XML_PARSE_ERR=1, /**< Syntax errors detected in either/both SpkSourceML or SpkDataML documents. */
		   ACCESS_ERR=2,    /**< File/directory access denied. */
		   OTHER_ERR        /**< Unclassified error. */
                 };
};
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
 * Generate an SpkReport document.
 */
static void genCompilationErrorML( const SpkCompilerException& e )
{
  ofstream o( compilation_error_xml );
  if( !o.good() )
    {
      cerr << "Failed to create " << compilation_error_xml << "." << endl;
      cerr << "Abnormal termination..." << endl;
      abort();
    }
  o << "<?xml version=\"1.0\"?>" << endl;
  o << "<spkreport>" << endl;
  o << e.getXml() << endl;
  o << "</spkreport>" << endl;
  o.close();
  return;
}
/**
 * Displays the usage of "spkcompiler".
 *
 * @param o A string buffer in which a generated message is placed.
 * 
 */
static void usage( char* o )
{
  strcpy( o, "\n\n" );
  strcat( o, "Usage: spkcompiler SOURCE DATA\n" );
  strcat( o, "\n" );
  strcat( o, "   SOURCE    --- file path to an SpkSourceML document\n" );
  strcat( o ,"   DATA      --- file path to an SpkDataML document\n" );
  strcat( o, "\n" );
  strcat( o, "<Exit Code>\n" );
  strcat( o, "   0: The compilation completed successfully.\n" );
  strcat( o, "      A makefile, \"Makefile.SPK\", along with the source\n" );
  strcat( o, "      code files are generated in the current directory.\n" );
  strcat( o, "   1: Syntax errors are found in either/both SOURCE or/and DATA.\n" );
  strcat( o, "      compilation_error.xml will be generated in the current directory.\n" );
  strcat( o, "   2: File/directory access denied.\n" );
  strcat( o, "   x: Any other error is reported with an unspecified exit code greater than 2.\n" );
  strcat( o, "      compilation_error.xml will generated in the current directory.\n" );
    
  return;
}

/* rename -- rename a file
int rename (const char * old, const char * new)

Renames a file from old to new.  If new already
exists, it is removed.

*/
static int rename (char* zfrom, char* zto)
{
  if (link (zfrom, zto) < 0)
    {
      if (errno != EEXIST)
	return -1;
      if (unlink (zto) < 0
	  || link (zfrom, zto) < 0)
	return -1;
    }
  return unlink (zfrom);
}

/**
 * This main() function is the SPK Compiler.  
 * It takes file paths to an SpkSourceML and an SpkDataML documents
 * and compiles C++ source code from these documents.
 *
 * @param argc The number of command line arguments.  This is given by the OS automatically.
 *             When argc = 1, the usage is printed out on the standard out.
 * 
 * @param argv The array of pointers to strings that represent command line arguments.
 *             argv[0] is the name of the executable given by the system automatically, 
 *             <code>spkcompiler</code> in this case.
 *             argv[1] shall be a file path to an SpkSourceML document and argv[2] shall be
 *             a file path to an SpkDataML document.
 *
 * @return An @ref EXIT_CODE "exit code" indicating the state of completion.
 *         See the Detailed Description section of this page for details.
 *
 */
int main( int argc, const char* argv[] )
{
  if (argc < 3)
  {
    char error_message[ SpkCompilerError::maxMessageLen() ];
    usage( error_message );
    cerr << error_message << endl;
    SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
                               "Missing command line arguments: spkcompiler SOURCE DATA", 
                               __LINE__, __FILE__ );
    genCompilationErrorML( e );
    
    return OTHER_ERR;
  }  
  const char * gSource = argv[1];
  const char * gData   = argv[2];

  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException & toCatch )
    {
      char error_message[ SpkCompilerError::maxMessageLen() ];
      snprintf( error_message, 
		SpkCompilerError::maxMessageLen(),
		"Error during Xerces-c Initialization.\nException message: %s.\n",
      XMLString::transcode( toCatch.getMessage() ) );
      SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                                 error_message, 
                                 __LINE__, __FILE__ );
      genCompilationErrorML( e );
      return OTHER_ERR;
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
        char error_message[ SpkCompilerError::maxMessageLen() ];
        snprintf( error_message, 
		  SpkCompilerError::maxMessageLen(),
		  "Failed to open %s!\n", gSource );
	SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, 
                                   error_message, 
                                   __LINE__, __FILE__ );
        genCompilationErrorML( e );
        return ACCESS_ERR;
      }
    iSource.close();
    parser->parse( gSource );
    source = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    char error_message[ SpkCompilerError::maxMessageLen() ];
    snprintf( error_message, 
	      SpkCompilerError::maxMessageLen(),
	      "An error occurred during parsing\n   Message: %s\n", 
	      XMLString::transcode(e.getMessage() ) );
    SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                               error_message, 
                               __LINE__, __FILE__ );
    genCompilationErrorML( e );
    return XML_PARSE_ERR;
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = SpkCompilerError::maxMessageLen()-20;
      XMLCh errText[maxChars + 1];
      fprintf( stderr, "DOM Error during parsing \"%s\".\nDOMException code is: %d\n",
               gSource, e.code );
      
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
        XMLPlatformUtils::Terminate();
        char error_message[ SpkCompilerError::maxMessageLen() ];	
        snprintf( error_message, 
		  SpkCompilerError::maxMessageLen(),
		  "Message is: %s.\n", 
		  XMLString::transcode(errText) );
	SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                                   error_message, 
                                   __LINE__, __FILE__ );
        genCompilationErrorML( e );
        return XML_PARSE_ERR;
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    char error_message[ SpkCompilerError::maxMessageLen() ];
    snprintf( error_message, 
	      SpkCompilerError::maxMessageLen(),
	      "An unknown error occurred during parsing.\n %d, %s\n" );
    SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                               error_message, 
                               __LINE__, __FILE__ );
    genCompilationErrorML( e );
    return XML_PARSE_ERR;
  }

  //
  // Parsing the SpkDataML document.
  //
  try{
    ifstream iData( gData );
    if( !iData.good() )
    {
       XMLPlatformUtils::Terminate();
       char error_message[ SpkCompilerError::maxMessageLen() ];
       snprintf( error_message, 
		 SpkCompilerError::maxMessageLen(),
		 "Failed to open %s!\n", gData );
       SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, 
                                  error_message, 
                                  __LINE__, __FILE__ );
       genCompilationErrorML( e );
       return ACCESS_ERR;
    }
    parser->parse( gData );
    data = parser->getDocument();
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    char error_message[ SpkCompilerError::maxMessageLen() ];
    snprintf( error_message, 
	      SpkCompilerError::maxMessageLen(),
	      "An error occurred during parsing\n   Message: %s\n", 
	      XMLString::transcode(e.getMessage() ) );
    SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                               error_message, 
                               __LINE__, __FILE__ );
    genCompilationErrorML( e );
    return XML_PARSE_ERR;
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      char error_message[ SpkCompilerError::maxMessageLen() ];
      
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
        XMLPlatformUtils::Terminate();
        snprintf( error_message,
		  SpkCompilerError::maxMessageLen(),
                 "DOM Error during parsing \"%s\".\nDOMException code is: %d\n \
                  Message is: %s.\n", XMLString::transcode(errText),
                 gData, e.code );
	SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                                   error_message, 
                                   __LINE__, __FILE__ );
        genCompilationErrorML( e );
      }
      else
      {
        snprintf( error_message, 
		  SpkCompilerError::maxMessageLen(),
		  "DOM Error during parsing \"%s\".\nDOMException code is: %d\n",
                 gData, e.code );
	SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                                   error_message, 
                                   __LINE__, __FILE__ );
      }
      return XML_PARSE_ERR;
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    char error_message[ SpkCompilerError::maxMessageLen() ];
    snprintf( error_message, 
	      SpkCompilerError::maxMessageLen(),
	      "An unknown error occurred during parsing %s.\n", gData );
    SpkCompilerException e( SpkCompilerError::ASPK_XMLDOM_ERR, 
                            error_message, __LINE__, __FILE__ );
    genCompilationErrorML( e ); 
    return XML_PARSE_ERR;
  }
                                                                                
  client::type cl = getClientName( source );
  if( cl == client::NOT_SUPPORTED )
    {
      XMLPlatformUtils::Terminate();
      char error_message[ SpkCompilerError::maxMessageLen() ];
      snprintf( error_message,
		SpkCompilerError::maxMessageLen(),
		"Not a supported client!" );
      cerr << error_message << endl;
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
                                 error_message, 
                                 __LINE__, __FILE__ );
      genCompilationErrorML( e );
      return OTHER_ERR;
    }

  // Call/construct an appropriate translator.

  ClientTranslator *xlator = NULL;

  if( cl == client::NONMEM )
    xlator = new NonmemTranslator( source, data );

  try{
    xlator->translate();
  }
  catch( const SpkCompilerException& e )
  {
    XMLPlatformUtils::Terminate();
    genCompilationErrorML( e );
    return XML_PARSE_ERR;
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    char error_message[ SpkCompilerError::maxMessageLen() ];
    snprintf( error_message, 
	      SpkCompilerError::maxMessageLen(),
	      "An unknown error occurred during compilation.\n %d, %s\n" );
    SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
                               error_message, __LINE__, __FILE__ );
    genCompilationErrorML( e );
    return XML_PARSE_ERR;
  }

  delete xlator;

  cout << "Completed succssfully." << endl;
  return SUCCESS;
}
