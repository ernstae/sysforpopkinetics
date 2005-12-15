/**
 * @file upper.h
 * Declare the upper() function.
 */
#ifndef UPPER_H
#define UPPER_H

#include <string>
/**
 * Capitalize all letters.
 *
 * @param source The source string (length >= 0).
 * @return       A new string object equivalent to the source with all 
 *               lower case letters converted to the upper case.
 * @callgraph
 * @author Sachiko Honda
 */
const std::string upper( const std::string& source );

#endif
