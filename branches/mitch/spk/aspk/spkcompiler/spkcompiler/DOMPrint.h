/**
 * @file DOMPrint.h
 * Declare the DOMPrint() function.
 */

#ifndef DOMPRINT_H
#define DOMPRINT_H

#include <xercesc/dom/DOMDocument.hpp>

/**
 * Print a DOM document.
 */
void DOMPrint( xercesc::DOMDocument* doc );

#endif
