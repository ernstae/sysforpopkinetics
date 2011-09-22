#include "DOMPrint.h"

#include <xercesc/util/PlatformUtils.hpp>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationLS.hpp>
#include <xercesc/dom/DOMWriter.hpp>

#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/XMLUni.hpp>

#include "DOMTreeErrorReporter.hpp"
#include "DOMPrintFilter.hpp"
#include "DOMPrintErrorHandler.hpp"

#include <string.h>
#include <stdlib.h>

// ---------------------------------------------------------------------------
//  Local data
//
//  gXmlFile
//      The path to the file to parser. Set via command line.
//
//  gDoNamespaces
//      Indicates whether namespace processing should be done.
//
//  gDoSchema
//      Indicates whether schema processing should be done.
//
//  gSchemaFullChecking
//      Indicates whether full schema constraint checking should be done.
//
//  gDoCreate
//      Indicates whether entity reference nodes needs to be created or not
//      Defaults to false
//
//  gOutputEncoding
//      The encoding we are to output in. If not set on the command line,
//      then it is defaults to the encoding of the input XML file.
//
//  gSplitCdataSections
//      Indicates whether split-cdata-sections is to be enabled or not.
//
//  gDiscardDefaultContent
//      Indicates whether default content is discarded or not.
//
//  gUseFilter
//      Indicates if user wants to plug in the DOMPrintFilter.
//
//  gValScheme
//      Indicates what validation scheme to use. It defaults to 'auto', but
//      can be set via the -v= command.
//
// ---------------------------------------------------------------------------
static char*                    gXmlFile               = 0;
static bool                     gDoNamespaces          = false;
static bool                     gDoSchema              = false;
static bool                     gSchemaFullChecking    = false;
static bool                     gDoCreate              = false;

static char*                    goutputfile            = 0;
// options for DOMWriter's features
static XMLCh*                   gOutputEncoding        = 0;

static bool                     gSplitCdataSections    = true;
static bool                     gDiscardDefaultContent = true;
static bool                     gUseFilter             = false;
static bool                     gFormatPrettyPrint     = false;
static bool                     gWriteBOM              = false;

static XercesDOMParser::ValSchemes    gValScheme       = XercesDOMParser::Val_Auto;


// ---------------------------------------------------------------------------
//
//  DOMPrint() 
//
// ---------------------------------------------------------------------------
void DOMPrint( xercesc::DOMDocument* doc )
{
  int retval = 0;

  // Initialize the XML4C2 system
  try
    {
      XMLPlatformUtils::Initialize();
    }

  catch(const XMLException &toCatch)
    {
      XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
				<< "  Exception message:"
				<< StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
      throw;
    }

  // If the parse was successful, output the document data from the DOM tree
  DOMPrintFilter   *myFilter = 0;

  try
    {
      // get a serializer, an instance of DOMWriter
      XMLCh tempStr[100];
      XMLString::transcode("LS", tempStr, 99);
      DOMImplementation *impl          = DOMImplementationRegistry::getDOMImplementation(tempStr);
      DOMWriter         *theSerializer = ((DOMImplementationLS*)impl)->createDOMWriter();

      // set user specified output encoding
      theSerializer->setEncoding(gOutputEncoding);

      // plug in user's own filter
      if (gUseFilter)
	{
	  // even we say to show attribute, but the DOMWriter
	  // will not show attribute nodes to the filter as
	  // the specs explicitly says that DOMWriter shall
	  // NOT show attributes to DOMWriterFilter.
	  //
	  // so DOMNodeFilter::SHOW_ATTRIBUTE has no effect.
	  // same DOMNodeFilter::SHOW_DOCUMENT_TYPE, no effect.
	  //
	  myFilter = new DOMPrintFilter(DOMNodeFilter::SHOW_ELEMENT   |
					DOMNodeFilter::SHOW_ATTRIBUTE |
					DOMNodeFilter::SHOW_DOCUMENT_TYPE);
	  theSerializer->setFilter(myFilter);
	}

      // plug in user's own error handler
      DOMErrorHandler *myErrorHandler = new DOMPrintErrorHandler();
      theSerializer->setErrorHandler(myErrorHandler);

      // set feature if the serializer supports the feature/mode
      if (theSerializer->canSetFeature(XMLUni::fgDOMWRTSplitCdataSections, gSplitCdataSections))
	theSerializer->setFeature(XMLUni::fgDOMWRTSplitCdataSections, gSplitCdataSections);

      if (theSerializer->canSetFeature(XMLUni::fgDOMWRTDiscardDefaultContent, gDiscardDefaultContent))
	theSerializer->setFeature(XMLUni::fgDOMWRTDiscardDefaultContent, gDiscardDefaultContent);

      if (theSerializer->canSetFeature(XMLUni::fgDOMWRTFormatPrettyPrint, gFormatPrettyPrint))
	theSerializer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint, gFormatPrettyPrint);

      if (theSerializer->canSetFeature(XMLUni::fgDOMWRTBOM, gWriteBOM))
	theSerializer->setFeature(XMLUni::fgDOMWRTBOM, gWriteBOM);

      //
      // Plug in a format target to receive the resultant
      // XML stream from the serializer.
      //
      // StdOutFormatTarget prints the resultant XML stream
      // to stdout once it receives any thing from the serializer.
      //
      XMLFormatTarget *myFormTarget;
      myFormTarget = new StdOutFormatTarget();

      //
      // do the serialization through DOMWriter::writeNode();
      //
      theSerializer->writeNode(myFormTarget, *doc);

      delete theSerializer;

      //
      // Filter, formatTarget and error handler
      // are NOT owned by the serializer.
      //
      delete myFormTarget;
      delete myErrorHandler;

      if (gUseFilter)
	delete myFilter;

    }
  catch (XMLException& e)
    {
      XERCES_STD_QUALIFIER cerr << "An error occurred during creation of output transcoder. Msg is:"
				<< XERCES_STD_QUALIFIER endl
				<< StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
      retval = 4;
    }

  // And call the termination method
  XMLPlatformUtils::Terminate();

  XMLString::release(&gOutputEncoding);

  return;
}

