#ifndef READ_CONTENT_H
#define READ_CONTENT_H

#include <string>
#include <xercesc/dom/DOM.hpp>
#include "client.h"
#include "SpkParameters.h"

/**
 * @file read_content.h
 *
 * Declares read_content() function.
 */
/**
 * @example read_contentTest.cpp
 */
/**
 * Analyze the <content> subtree in the input SpkInML document
 * and return the verion of the document,
 * the @ref client type
 * and the @ref SpkParameters::Analysis "analysis" type 
 * specified as attributes of the given DOMElement.
 *
 * DTD for <content> is as follows:
 *
 * @code
 * <!ELEMENT content (#PCDATA)>
 * <!ATTLIST content spkinml_ver CDATA #FIXED "1.0">
 * <!ATTLIST content client (nonmem) #REQUIRED>
 * <!ATTLIST content analysis (population|individual) #REQUIRED>
 *
 * @endcode
 *
 * @return false if any of "spkinml_ver", "client" or"analysis" attribute
 * were missing in the <content> element, true otherwise.
 *
 * @param content_node is a pointer to the DOMElement node that
 * represents the root of <content> subtree.
 *
 * @param spkml_verOut will contain a string extracted as
 * the value of a <content> attribute, "spkinml_ver".
 *
 * @param clientOut will contain an enum value extracted as
 * the value of a <content> attribute, "client".
 *
 * @param analysisOut will contain an enum value extracted as
 * the value of a <content> attribute, "analysis".
 *
 */
bool read_content( xercesc::DOMElement * content_node, 
		   std::string & spkml_verOut, 
		   enum client::type & clientOut, 
		   enum SpkParameters::Analysis& analysisOut  );

#endif
