#ifndef READ_NONMEM_MODEL_H
#define READ_NONMEM_MODEL_H

#include <map>
#include <xercesc/dom/DOM.hpp>
#include "../SymbolTable.h"
#include "NonmemTranslator.h"

/**
 * @file read_nonmem_model.h
 * Declares read_nonmem_model() function.
 *
 * @ingroup nonmem
 */
/**
 * @example read_nonmem_modelTest.cpp
 */
std::pair<enum nonmem::MODEL, enum nonmem::TRANS>
   read_nonmem_model( xercesc::DOMElement* modelNode, 
			int nIndividuals, 
			SymbolTable * gSpkExpSymbolTable );

#endif
