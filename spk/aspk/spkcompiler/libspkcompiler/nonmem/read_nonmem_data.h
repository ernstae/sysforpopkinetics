#ifndef READ_NONMEM_DATA_H
#define READ_NONMEM_DATA_H

#include <xercesc/dom/DOM.hpp>
#include <map>
#include <valarray>
#include "../SymbolTable.h"
#include "../SpkParameters.h"
#include "NonmemTranslator.h"
/**
 * @file read_nonmem_data.h
 * Declares the read_nonmem_data() function.
 */
/**
 * Process <data> section of SpkInML document to gather information
 * needed to genreate the source code for the following entities:
 *
 * - the definition of IndRecords class
 * - the initialization of IndRecords objects for all individuals
 * 
 * IndRecords class
 *   This class lists all observation records associated with an individual.
 *   It allows accessing a set of observation records (column) by both
 *   "label" and "alias" (if an alias is given).
 */
void read_nonmem_data( 
	xercesc::DOMElement* dataNode, 
        int nIndividuals,
	SymbolTable & table,
	std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> &label_alias_mappingOut,
	std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_forOut[],
	std::string order_id_pairOut[],
        struct SpkParameters & spk
	);
#endif
