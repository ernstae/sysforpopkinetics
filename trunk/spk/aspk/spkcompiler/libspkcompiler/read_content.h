#ifndef READ_CONTENT_H
#define READ_CONTENT_H

#include <string>
#include <map>
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
 * 
 * <!ATTLIST content spkinml_ver CDATA                  #FIXED "1.0">
 * <!ATTLIST content client     (nonmem)                #REQUIRED>
 * <!ATTLIST content analysis   (population|individual) #REQUIRED>
 * <!ATTLIST content estimation (yes|no)                #REQUIRED>
 * <!ATTLIST content simulation (yes|no)                #REQUIRED>
 *
 * @endcode
 *
 * @return a pair of boolean values.  The first element is true
 * if the value of @a estimation attribute is @a yes and false if 
 * @a no.  The second element is true if the value of @a simulation
 * is @a yes and false if @a no.
 *
 * @param content_node is a pointer to the DOMElement node that
 * represents the root of <content> subtree.
 *
 * @param spkml_verOut will be the value of @a spkinml_ver attribute.
 *
 * @param clientOut will be a @a client::type enum value equivalent
 * to the value of @a client attribute.  
 * @c nonmem yields in @c client::NONMEM.
 *
 * @param analysisOut will be a @a SpkParameters::Analysis enum value
 * equivalent to the value of @a analysis attribute.
 * @c population yields in @c SpkParameters::POPULATION.
 * @c individual yields in @c SpkParameters::INDIVIDUAL.
 *
 */
std::pair<bool, bool> read_content( xercesc::DOMElement * content_node, 
				    std::string & spkml_verOut, 
				    enum client::type & clientOut, 
				    enum SpkParameters::Analysis& analysisOut );

#endif
