#ifndef READ_NONMEM_DRIVER_H
#define READ_NONMEM_DRIVER_H

#include <xercesc/dom/DOM.hpp>

#include "SpkParameters.h"
#include "nonmem/NonmemTranslator.h"

int read_nonmem_driver( 
   xercesc::DOMElement* driverTree, 
   SpkParameters & spkOut, 
   NonmemParameters& nonmemOut );

#endif
