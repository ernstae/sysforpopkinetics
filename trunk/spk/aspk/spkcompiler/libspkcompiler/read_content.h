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
 * Extract the verion of SpkInML document, the @ref client type
 * and the @ref SpkParameters::Analysis "analysis" type from the given DOMDocumentation.
 *
 * @arg tree is a pointer to the DOMDocumentation that represents
 * the input SkInML document.  It must contain <content> tag.
 *
 * @arg spkml_verOut will contain a string extracted as
 * the value of a <content> attribute, "spkinml_ver".
 *
 * @arg clientOut will contain an enum value extracted as
 * the value of a <content> attribute, "client".
 *
 * @arg analysisOut will contain an enum value extracted as
 * the value of a <content> attribute, "analysis".
 *
 * @return true if it finds "spkinml_ver", "client" and "analysis" attribute
 * values associated with the <content> tag.
 */
bool read_content( xercesc::DOMDocument* tree, 
		   std::string & spkml_verOut, 
		   enum client::type & clientOut, 
		   enum SpkParameters::Analysis& analysisOut  );

#endif
