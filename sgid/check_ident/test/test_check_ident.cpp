/*************************************************************************
 *//**
 * @file main.cpp
 * 
 * 
 * Implements the identifiability driver's unit test.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Identifiability header files.
#include <IdentException.h>

// Standard library header files.
#include <cstdio>
#include <fstream>
#include <string>
#include <vector>

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
 * Local function definition
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  /*************************************************************************
   *
   * Function: testFailedMessage
   *
   *//**
   * Returns a string that contains a message when the tests fail.
   *
  /*************************************************************************/

  std::string testFailedMessage( const std::string& messageIn )
  {
    std::string messageOut = "\n"
                          + messageIn + "\n"
                          + "\n"
                          + "This test failed.\n"
                          + "\n";

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
                         + " element in the identifiability output file.";

      throw IdentException( message );
    }

    // Check that there is no more than one element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + elementName
                         + " element in the identifiability output file.";

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
                         + " element in the identifiability output file.";

      throw IdentException( message );
    }

    // Check that there is no more than one element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + elementName
                         + " element in the identifiability output file.";

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
                         + " element in the identifiability output file.";

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
                         + " element in the identifiability output file.";

      throw IdentException( message );
    }

    // Check that there is no more than one outer element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + outerElementName
                         + " element in the identifiability output file.";

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
                         + " element in the identifiability output file.";

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

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: main
 *
 *//**
 * This main function is the identifiability driver's unit test.
 *
/*************************************************************************/

int main( int nArg, char* argCStr[] )
{
  //----------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Execute the identifiability driver.
  //----------------------------------------------------------

  // Set the output file name.
  const char* outputFileName = "check_ident_output.xml";

  // Use the system command to execute check_ident with the input and
  // output XML files as the arguments.
  system( "../src/check_ident check_ident_input.xml check_ident_output.xml" );


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
    cout << testFailedMessage( "The identifiability output file parser could not be initialized." );
    return 1;
  }


  //----------------------------------------------------------
  // Read in the output file for the identifiability calculation.
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

  // Parse the identifiability XML output file specified by the first C
  // style string argument for this function.
  try
  {
    pXMLParser->parse( outputFileName );
  }
  catch ( ... )
  {
    cout << testFailedMessage( "The identifiability output file could not be parsed." );
    return 1;
  }


  //----------------------------------------------------------
  // Prepare to get the outputs for the identifiability calculation.
  //----------------------------------------------------------

  DOMDocument* pDOMDocument;

  // Get the DOM document tree that contains all of the elements and
  // text nodes from the output identifiability file
  try
  {
    pDOMDocument = pXMLParser->getDocument();
  }
  catch ( ... )
  {
    cout << testFailedMessage( "The identifiability output information could not be processed." );
    return 1;
  }


  // Get the root (first) element of the DOM document tree.
  DOMElement* pDOMElementRoot = pDOMDocument->getDocumentElement();

  // Check that there are elements in the DOM document tree.
  if ( !pDOMElementRoot )
  {
    cout << testFailedMessage( "The identifiability output file was empty." );
    return 1;
  }

  XMLCh tempXMLCh[100];

  // Check the name of the root element.
  XMLString::transcode( "identifiability_output", tempXMLCh, 99 );
  if ( !XMLString::equals( pDOMElementRoot->getTagName(), tempXMLCh ) ) 
  {
    cout << testFailedMessage(
      "The first element of the identifiability output file had the wrong name." );
    return 1;
  }


  //---------------------------------------------------------
  // Check the number of solutions for the Groebner basis or bases.
  //----------------------------------------------------------

  string number_of_solutionsStr;

  const string number_of_solutionsErrorStr =
    "The number of solutions could not be determined from the output identifiability file.";

  try
  {
    number_of_solutionsStr = getElementText( pDOMElementRoot, "number_of_solutions" );
  }
  catch ( const IdentException& e )
  {
    cout << testFailedMessage( number_of_solutionsErrorStr + "\n\n" + e.what() );
    return 1;
  }
  catch ( ... )
  {
    cout << testFailedMessage( number_of_solutionsErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
    return 1;
  }

  // Convert the number of solutions to be a number.
  int nSolution = atoi( number_of_solutionsStr.c_str() );

  // Convert the number of solutions to be a number.
  int nSolutionKnown = 1;

  // Check the values.
  if ( nSolution != nSolutionKnown )
  {
    cout << testFailedMessage(
      "The calculated number of Groebner bases solutions was wrong." );
    return 1;
  }


  //----------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------

  // If the test gets to here, then print a message saying it was a
  // success.
  cout << endl;
  cout << "This test passed." << endl;
  cout << endl;

  // Delete the output file.
  int removeReturnValue = remove( outputFileName );

  return( 0 );
}
