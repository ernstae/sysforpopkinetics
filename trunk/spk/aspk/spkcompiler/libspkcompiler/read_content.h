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
 * Extract the verion of SpkInML document, the @ref client type
 * and the @ref SpkParameters::Analysis "analysis" type 
 * specified as attributes of the given DOMElement.
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
 * @return true if it finds "spkinml_ver", "client" and "analysis" attribute
 * values associated with the <content> tag.
 */
bool read_content( xercesc::DOMElement * content_node, 
		   std::string & spkml_verOut, 
		   enum client::type & clientOut, 
		   enum SpkParameters::Analysis& analysisOut  );

#endif
