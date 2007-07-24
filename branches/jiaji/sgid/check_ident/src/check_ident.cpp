/*************************************************************************
 *//**
 * @file check_ident.cpp
 * 
 * 
 * Implements the main() function that is the driver for the
 * identifiability calculation.
 *
 * In order to execute the driver, use the following syntax:
\verbatim
     ./check_ident input_file.xml output_file.xml
\endverbatim
 *
 * For details on the input and output XML files, see the
 * specifications for the main function's arguments.
 *
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Identifiability header files.
#include <checkParamIdent.h>
#include <IdentException.h>

// Standard library header files.
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <sys/time.h>

// Xerces XML parser library header files.
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif

XERCES_CPP_NAMESPACE_USE 
  

/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  char* inputFileName;

  char* outputFileName;

  const char* stdoutFileName = "identDetails.txt";

  std::string errorMessage = "";

  // This will be the seed for the random parameter values.
  int paramSeed = 0;

  // This will be the number of elapsed seconds.
  int nElapsedSec = 0;

  // This will be the number of Groebner basis solutions that were
  // found during the identifiability calculation.
  int nGroebnerBasisSoln = 0;

  // This will be the status for the Groebner basis solutions.
  std::string identStatus = "Unknown";

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local function definition
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  /*************************************************************************
   *
   * Function: bugReportMessage
   *
   *//**
   * Returns a string that contains a bug report message.
   *
  /*************************************************************************/

  std::string bugReportMessage( const std::string& messageIn )
  {
    std::string messageOut = messageIn + "\n"
                          + "\n"
                          + "Please submit a bug report.\n";

    return messageOut;
  }


  /*************************************************************************
   *
   * Function: getElementText
   *
   *//**
   * Returns the text from an XML element like this
   *
   *     <element>text</element>
   *
   * The argument pDOMElementRoot points to the root element of the XML 
   * file that has currently been parsed.
   *
  /*************************************************************************/

  std::string getElementText( 
    const DOMElement* pDOMElementRoot,
    const std::string& elementName )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Find the element.
    //----------------------------------------------------------

    XMLCh tempXMLCh[100];

    // Set the element's name XML string.
    XMLString::transcode( elementName.c_str(), tempXMLCh, 99 );

    // Get a pointer to a list containing the elements that match the
    // element's name.
    DOMNodeList* pDOMNodeList = pDOMElementRoot->getElementsByTagName( tempXMLCh );

    // Check that there is at least one element.
    if ( pDOMNodeList->getLength() < 1 )
    {
      std::string message = "There was no " 
                         + elementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Check that there is no more than one element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + elementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }


    //----------------------------------------------------------
    // Get the element's text.
    //----------------------------------------------------------

    // Get a pointer to the element in the DOM document tree.
    DOMElement* pDOMELement = dynamic_cast<DOMElement*>( pDOMNodeList->item( 0 ) );

    // Get the element's text as an XML string.
    const XMLCh* nodeTextXMLCh = pDOMELement->getFirstChild()->getNodeValue();

    // Convert the element's text to be a C style string.
    char* nodeTextCStr =  XMLString::transcode( nodeTextXMLCh );

    // Set the element's text.
    std::string elementTextStrOut = nodeTextCStr;

    // Free the memory allocated for this C style string.
    XMLString::release( &nodeTextCStr );


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------

    return elementTextStrOut;  
  }


  /*************************************************************************
   *
   * Function: getAttributeText
   *
   *//**
   * Returns the attribute text from an XML element like this
   *
   *     <element attribute="attribute_text">element_text</element>
   *
   * The argument pDOMElementRoot points to the root element of the XML 
   * file that has currently been parsed.
   *
  /*************************************************************************/

  std::string getAttributeText( 
    const DOMElement* pDOMElementRoot,
    const std::string& elementName,
    const std::string& attributeName )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Find the element.
    //----------------------------------------------------------

    XMLCh tempXMLCh[100];

    // Set the element's name XML string.
    XMLString::transcode( elementName.c_str(), tempXMLCh, 99 );

    // Get a pointer to a list containing the elements that match the
    // element's name.
    DOMNodeList* pDOMNodeList = pDOMElementRoot->getElementsByTagName( tempXMLCh );

    // Check that there is at least one element.
    if ( pDOMNodeList->getLength() < 1 )
    {
      std::string message = "There was no " 
                         + elementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Check that there is no more than one element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + elementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Set this to point to the element in the DOM tree.
    DOMElement* pDOMElement = dynamic_cast<DOMElement*> ( pDOMNodeList->item( 0 ) );


    //----------------------------------------------------------
    // Get the attribute's text.
    //----------------------------------------------------------

    // Set the attribute's name XML string.
    XMLString::transcode( attributeName.c_str(), tempXMLCh, 99 );

    // Check that the element has the attribute.
    if ( !pDOMElement->hasAttribute( tempXMLCh ) )
    {
      std::string message = "There was no " 
                         + attributeName + " attribute for the "
                         + elementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Get the attribute's text as an XML string.
    const XMLCh* attributeTextXMLCh = pDOMElement->getAttribute( tempXMLCh );

    // Convert the attribute's text to be a C style string.
    char* attributeTextCStr =  XMLString::transcode( attributeTextXMLCh );

    // Set the attribute's text.
    std::string attributeTextStrOut = attributeTextCStr;

    // Free the memory allocated for this C style string.
    XMLString::release( &attributeTextCStr );


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------

    return attributeTextStrOut;  
  }


  /*************************************************************************
   *
   * Function: getElementTextVector
   *
   *//**
   * Gets the text from all of the inner elements for an XML block
   * that is organized like this
   *
   *     <outer_element>
   *       <inner_element>text_1</inner_element>
   *       <inner_element>text_2</inner_element>
   *       <inner_element>text_3</inner_element>
   *       <inner_element>text_4</inner_element>
   *     </outer_element>
   *
   * and puts each inner element's text into the corresponding element
   * of parameterInner_ElementStr.
   *
   * The argument pDOMElementRoot points to the root element of the XML 
   * file that has currently been parsed.
   *
  /*************************************************************************/

  void getElementTextVector( 
    const DOMElement*           pDOMElementRoot,
    const std::string&          outerElementName,
    const std::string&          innerElementName,
    std::vector< std::string >& innerElementVectorOut )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Find the outer element.
    //----------------------------------------------------------

    XMLCh tempXMLCh[100];

    // Set the outer element's name XML string.
    XMLString::transcode( outerElementName.c_str(), tempXMLCh, 99 );

    // Get a pointer to a list containing the elements that match the
    // outer element's name.
    DOMNodeList* pDOMNodeList = pDOMElementRoot->getElementsByTagName( tempXMLCh );

    // Check that there is at least one outer element.
    if ( pDOMNodeList->getLength() < 1 )
    {
      std::string message = "There was no " 
                         + outerElementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Check that there is no more than one outer element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + outerElementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Set this to point to the outer element in the DOM tree.
    DOMElement* pDOMElementOuter = dynamic_cast<DOMElement*> ( pDOMNodeList->item( 0 ) );


    //----------------------------------------------------------
    // Find the inner elements.
    //----------------------------------------------------------

    // Set the inner element's name XML string.
    XMLString::transcode( innerElementName.c_str(), tempXMLCh, 99 );

    // Get a pointer to a list containing the elements that match the
    // inner element's name.
    pDOMNodeList = pDOMElementOuter->getElementsByTagName( tempXMLCh );

    // Check that there is at least one inner element.
    if ( pDOMNodeList->getLength() < 1 )
    {
      std::string message = "There was no " 
                         + innerElementName
                         + " element in the identifiability input file.";

      throw IdentException( message );
    }

    // Set the number of inner elements.
    int nInner = pDOMNodeList->getLength();

    // Make room for each of the inner elements in this vector.
    innerElementVectorOut.resize( nInner );


    //----------------------------------------------------------
    // Set the vector of inner elements' texts.
    //----------------------------------------------------------

    int i;

    for ( i = 0; i < nInner; i++ )
    {
      // Get a pointer to the inner element in the DOM document tree.
      DOMElement* pDOMELementInner = dynamic_cast<DOMElement*>( pDOMNodeList->item( i ) );

      // Get the inner element's text as an XML string.
      const XMLCh* innerNodeTextXMLCh = pDOMELementInner->getFirstChild()->getNodeValue();

      // Convert the inner element's text to be a C style string.
      char* innerNodeTextCStr =  XMLString::transcode( innerNodeTextXMLCh );

      // Set this inner element's text in the vector.
      innerElementVectorOut[i] = innerNodeTextCStr;

      // Free the memory allocated for this C style string.
      XMLString::release( &innerNodeTextCStr );
    }

  }


  /*************************************************************************
   *
   * Function: printResultsFile
   *
   *//**
   * Prints the results from the identifiability calculation to a file.
   *
   * Note that this function uses variables defined in the unnamed
   * namespace.
   *
  /*************************************************************************/

  void printResultsFile()
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;

    // Close stdout, which was redirected to a temporary file.
    fclose( stdout );

    // Open the temporary file that was used to capture the details of
    // the identifiability calculation as they were printed to stdout.
    ifstream calcDetailsStream;
    calcDetailsStream.open( stdoutFileName );
    if ( !calcDetailsStream )
    {
      errorMessage = bugReportMessage( "The identifiability details file could not be opened." );
    }

    // Open the identifiability results file.
    ofstream identResultsStream;
    identResultsStream.open( outputFileName, ios::trunc );
    if ( !identResultsStream )
    {
      errorMessage = bugReportMessage( "The identifiability results file could not be opened." );
    }


    //----------------------------------------------------------
    // Print identifiability information results to the file.
    //----------------------------------------------------------

    identResultsStream << "<identifiability_output>" << endl;
    identResultsStream << endl;
    identResultsStream << "<elapsed_seconds>" << nElapsedSec << "</elapsed_seconds>" << endl;
    identResultsStream << endl;
    identResultsStream << "<parameter_seed>" << paramSeed << "</parameter_seed>" << endl;
    identResultsStream << endl;
    identResultsStream << "<error_messages>" << endl;
    if ( errorMessage.length() > 0 )
    {
      identResultsStream << errorMessage << endl;
    }
    identResultsStream << "</error_messages>" << endl;
    identResultsStream << endl;
    identResultsStream << "<number_of_solutions>" << nGroebnerBasisSoln << "</number_of_solutions>" << endl;
    identResultsStream << endl;
    identResultsStream << "<status_of_the_solutions>" << identStatus << "</status_of_the_solutions>" << endl;
    identResultsStream << endl;
    identResultsStream << "<calculation_details>" << endl;
    identResultsStream << calcDetailsStream.rdbuf();
    identResultsStream << "</calculation_details>" << endl;
    identResultsStream << endl;
    identResultsStream << "</identifiability_output>" << endl;


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------

    // Close the identifiability results file.
    identResultsStream.close();

    // Close the identifiability details file.
    calcDetailsStream.close();

    // Delete the identifiability details file.
    int removeReturnValue = remove( stdoutFileName );
  }

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: main
 *
 *//**
 * This main function is the driver for the identifiability
 * calculation.
 *
 * In order to execute the driver, use the following syntax:
\verbatim
     ./check_ident input_file.xml output_file.xml
\endverbatim
 *
 * For details on the input and output XML files, see the
 * specifications for this function's arguments.
 *
 * This function reads in the identifiability XML input file specified
 * in the array of arguments for this function, argCStr, parses the
 * file to get the identifiability information, and then calls the
 * function that performs the identifiability calculation.
 *
 * The results from the identifiability calculation are then put into
 * the identifiability XML results file, which is also specified in
 * the array of arguments, argCStr.
 *
 * See the specification for argCStr below for details on the XML
 * input and output files.
 *
 *
 * @param nArg
 * 
 * Number of elements in the array of C style string arguments,
 * argCStr.
 * 
 *
 * @param argCStr
 * 
 * This array of C style string contains the arguments for main.
 *
 * @param argCStr[0]
 * 
 * The operating system will set this argument equal to the path and
 * name for the identifiability executable when it is run.
 * 
 * 
 * @param argCStr[1]
 * 
 * This argument specifies the path and name for the identifiability
 * input XML file, which contains information needed for the
 * calculation.
 *
 * The following is an example of an input XML file that shows all of
 * the required elements:
\verbatim

<?xml version="1.0" encoding="UTF-8"?>

<identifiability_input>

  <parameters seed="34890">

    <name>k21</name>

    <name>k12</name>

    <name>CL</name>

    <name>V</name>

  </parameters>

  <system_experiment_model>

    <differential_equations number_of_inputs="1">   

                                             If number_of_inputs = 1,
                                             the input must be called U.
                                             If number_of_inputs > 1, 
                                             they must be U1, U2, etc.

      <equation>A1[T] = k12*V*A2+CL*U*A1*A1+U+k12*A2*A1</equation>

      <equation>A2[T] = k21*A1-k12*A2</equation>

    </differential_equations>

    <output_equations>

                                             If number = 1, the output
                                             must be called Y.
                                             If number > 1, they must
                                             be Y1, Y2, etc.

      <equation>Y = A1/V</equation>

    </output_equations>

  </system_experiment_model>

</identifiability_input>

\endverbatim
 * 
 *
 * @param argCStr[2]
 * 
 * This argument specifies the path and name for the identifiability
 * output XML file, which contains the results of the calculation.
 *
 * The following is an example of an input XML file that shows all of
 * the required elements:
\verbatim

<?xml version="1.0" encoding="UTF-8"?>

<identifiability_output>

<elapsed_seconds>4439</elapsed_seconds>

<error_messages>
The following error occurred during the identifiability calculation:

The left hand side of differential equation 3 was not A3[T].
</error_messages>

<number_of_solutions>1</number_of_solutions>

<status_of_the_solutions>Globally (Uniquely) Identifiable</status_of_the_solutions>

<calculation_details>

Begin identifiability calculation.

     .
     .
     .

End identifiability calculation.

</calculation_details>

</identifiability_output>

\endverbatim
 * 
 *
/*************************************************************************/

int main( int nArg, char* argCStr[] )
{
  //----------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------

  using namespace std;

  // Set the names for the input and output XML files.
  inputFileName  = argCStr[1];
  outputFileName = argCStr[2];

  // Redirect stdout to a temporary file in order to capture the
  // details of the identifiability calculation as they are printed.
  freopen( stdoutFileName, "w", stdout );


  //----------------------------------------------------------
  // Prepare the XML parser.
  //----------------------------------------------------------

  // Initialize the Xerces XML parser.
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( "The identifiability input file parser could not be initialized." );
    printResultsFile();
    return 1;
  }


  //----------------------------------------------------------
  // Read in the input file for the identifiability calculation.
  //----------------------------------------------------------

  // Instantiate the XML parser.
  XercesDOMParser* pXMLParser = new XercesDOMParser();

  // Set some options for the XML parser.
  pXMLParser->setValidationScheme            ( XercesDOMParser::Val_Always );
  pXMLParser->setDoNamespaces                ( true );
  pXMLParser->setDoNamespaces                ( true );
  pXMLParser->setDoSchema                    ( true );
  pXMLParser->setValidationSchemaFullChecking( true );
  pXMLParser->setCreateEntityReferenceNodes  ( true );

  // Parse the identifiability XML input file specified by the first C
  // style string argument for this function.
  try
  {
    pXMLParser->parse( inputFileName );
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( "The identifiability input file could not be parsed." );
    printResultsFile();
    return 1;
  }


  //----------------------------------------------------------
  // Prepare to get the inputs for the identifiability calculation.
  //----------------------------------------------------------

  DOMDocument* pDOMDocument;

  // Get the DOM document tree that contains all of the elements and
  // text nodes from the input identifiability file
  try
  {
    pDOMDocument = pXMLParser->getDocument();
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( "The identifiability input information could not be processed." );
    printResultsFile();
    return 1;
  }


  // Get the root (first) element of the DOM document tree.
  DOMElement* pDOMElementRoot = pDOMDocument->getDocumentElement();

  // Check that there are elements in the DOM document tree.
  if ( !pDOMElementRoot )
  {
    errorMessage = bugReportMessage( "The identifiability input file was empty." );
    printResultsFile();
    return 1;
  }

  XMLCh tempXMLCh[100];

  // Check the name of the root element.
  XMLString::transcode( "identifiability_input", tempXMLCh, 99 );
  if ( !XMLString::equals( pDOMElementRoot->getTagName(), tempXMLCh ) ) 
  {
    errorMessage = bugReportMessage(
      "The first element of the identifiability input file had the wrong name." );
    printResultsFile();
    return 1;
  }


  //----------------------------------------------------------
  // Handle
  //
  //     <parameters seed="34890">
  //       <name>k21</name>
  //       <name>k12</name>
  //       <name>CL</name>
  //       <name>V</name>
  //     </parameters>
  //
  //----------------------------------------------------------
 
  const string parametersNameErrorStr =
    "The parameter names could not be determined from the input identifiability file.";

  vector<string> parametersNameStr;

  try
  {
    getElementTextVector( pDOMElementRoot, "parameters", "name", parametersNameStr );
  }
  catch ( const IdentException& e )
  {
    errorMessage = bugReportMessage( parametersNameErrorStr + "\n\n" + e.what() );
    printResultsFile();
    return 1;
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( parametersNameErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    printResultsFile();
    return 1;
  }

  // Set the number of parameters.
  int nParam = parametersNameStr.size();

  const string seedErrorStr =
    "The seed could not be determined from the input identifiability file.";

  string seedStr;

  try
  {
    seedStr = getAttributeText( pDOMElementRoot, "parameters", "seed" );
  }
  catch ( const IdentException& e )
  {
    errorMessage = bugReportMessage( seedErrorStr + "\n\n" + e.what() );
    printResultsFile();
    return 1;
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( seedErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    printResultsFile();
    return 1;
  }

  // Convert the seed string to be a number.
  paramSeed = atoi( seedStr.c_str() );


  //----------------------------------------------------------
  // Handle
  //
  //     <differential_equations number_of_inputs="1">   
  //       <equation>A1[T] = k12*V*A2+CL*U*A1*A1+U+k12*A2*A1</equation>
  //       <equation>A2[T] = k21*A1-k12*A2</equation>
  //     </differential_equations>
  //
  // Note: If the number of inputs = 1, the input must be called U.
  // If the number of inputs > 1, they must be U1, U2, etc.
  //----------------------------------------------------------
 
  const string differentialEquationErrorStr =
    "The differential equations could not be determined from the input identifiability file.";

  vector<string> differentialEquationStr;

  try
  {
    getElementTextVector( pDOMElementRoot, "differential_equations", "equation", differentialEquationStr );
  }
  catch ( const IdentException& e )
  {
    errorMessage = bugReportMessage( differentialEquationErrorStr + "\n\n" + e.what() );
    printResultsFile();
    return 1;
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( differentialEquationErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    printResultsFile();
    return 1;
  }

  // Set the number of differential equations.
  int nDiffEqn = differentialEquationStr.size();

  const string number_of_inputsErrorStr =
    "The number of inputs could not be determined from the input identifiability file.";

  string number_of_inputsStr;

  try
  {
    number_of_inputsStr = getAttributeText( pDOMElementRoot, "differential_equations", "number_of_inputs" );
  }
  catch ( const IdentException& e )
  {
    errorMessage = bugReportMessage( number_of_inputsErrorStr + "\n\n" + e.what() );
    printResultsFile();
    return 1;
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( number_of_inputsErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    printResultsFile();
    return 1;
  }

  // Convert the number of inputs string to be a number.
  int nInputType = atoi( number_of_inputsStr.c_str() );


  //----------------------------------------------------------
  // Handle
  //
  //     <output_equations>
  //       <equation>Y = A1/V</equation>
  //     </output_equations>
  //
  // Note: If the number of outputs = 1, the output must be called Y.
  // If the number of outputs > 1, they must be Y1, Y2, etc.
  //----------------------------------------------------------
 
  const string outputEquationErrorStr =
    "The output equations could not be determined from the input identifiability file.";

  vector<string> outputEquationStr;

  try
  {
    getElementTextVector( pDOMElementRoot, "output_equations", "equation", outputEquationStr );
  }
  catch ( const IdentException& e )
  {
    errorMessage = bugReportMessage( outputEquationErrorStr + "\n\n" + e.what() );
    printResultsFile();
    return 1;
  }
  catch ( ... )
  {
    errorMessage = bugReportMessage( outputEquationErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    printResultsFile();
    return 1;
  }

  // Set the number of output equations.
  int nOutputEqn = outputEquationStr.size();


  //----------------------------------------------------------
  // Perform the identifiability calculation.
  //----------------------------------------------------------

  // Before performing the identifiability calculation, free the
  // memory allocated for the XML parser.
  delete pXMLParser;

  // Set the level so that the intermediate calculation details will
  // be printed.
  int level = 1;

  timeval calcBegin;
  timeval calcEnd;

  // Get the time the calculation began.
  gettimeofday( &calcBegin, 0 );

  try
  {
    // Calculate the number of Groebner basis solutions for the
    // exhaustive summary.
    nGroebnerBasisSoln = checkParamIdent(
      level,
      nParam,
      parametersNameStr,
      paramSeed,
      nDiffEqn,
      nOutputEqn,
      nInputType,
      differentialEquationStr,
      outputEquationStr,
      identStatus );
  }
  catch( const IdentException& e )
  {
    errorMessage  = "The following error occurred during the identifiability calculation:\n\n";
    errorMessage += e.what();
    errorMessage += "\n";
  }
  catch( ... )
  {
    errorMessage  = "The identifiability calculation failed for an unknown reason.\n\n";
    errorMessage += "Please submit a bug report.\n";
  }

  // Get the time the calculation ended.
  gettimeofday( &calcEnd, 0 );

  // Calculate the total number of elapsed seconds.
  nElapsedSec = static_cast<int>( difftime( calcEnd.tv_sec, calcBegin.tv_sec ) );


  //----------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------

  // Print the results of the identifiability calculation.
  printResultsFile();

  // Tell the Xerces XML parser to finish.
  XMLPlatformUtils::Terminate();

  return 0;
}
