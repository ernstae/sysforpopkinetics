#ifndef EMIT_INDDATACLASS_H
#define EMIT_INDDATACLASS_H

#include <string>
#include <map>
#include <string>
#include "SymbolTable.h"
#include "NonmemTranslator.h"

/**
 * @file emit_IndDataClass.h
 *
 * Declare emit_IndDataClass(), emit_initIndDataObjects() and emit_releaseIndDataObjects().
 * 
 */

/**
 * Generate C++ source code defining IndData class and emit the code
 * into a file of which FILE hander is given by the caller.
 * 
 * @arg nIndividuals indicates the number of individuals in the population, 
 * which is also the number of IndData objects defined.
 *
 * @arg out points to an open FILE hander.  The generated C++ code
 * will be appended after the current position.
 *  
 * @arg table is a pointer to the symbol table in which all identifiers
 * to be declared and defined in the class.
 *
 * @arg label_alias_mapping is a mapping between two identifiers
 * that point to a single data set if any.
 *
 * @arg data_for is an array of map objects such that i-th map in the array
 * corresponds to i-th (in the processing order) individual's data set.
 * Each map is a set of pairs of title and its corresponding observation records.
 *
 * @arg order_id_pair is an array of strings such that i-th string in the
 * array corresponds to the ID for the individual (or his/her data set) which
 * shall be processed i-th during the optimization effort.
 *
 */
void emit_IndDataClass(
        FILE * out,
	int nIndividuals,
	SymbolTable* table,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
	const std::string order_id_pair[]
	                            );
/**
 * Generate C++ source code instantiating and initializing IndData 
 * objects for corresponding individuals' data sets. and
 * emit the code into a file of which FILE hander is given by the caller.
 * 
 * @arg nIndividuals indicates the number of individuals in the population, 
 * which is also the number of IndData objects defined.
 *
 * @arg out points to an open FILE hander.  The generated C++ code
 * will be appended after the current position.
 *  
 * @arg table is a pointer to the symbol table in which all identifiers
 * to be declared and defined in the class.
 *
 * @arg label_alias_mapping is a mapping between two identifiers
 * that point to a single data set if any.
 *
 * @arg data_for is an array of map objects such that i-th map in the array
 * corresponds to i-th (in the processing order) individual's data set.
 * Each map is a set of pairs of title and its corresponding observation records.
 *
 * @arg order_id_pair is an array of strings such that i-th string in the
 * array corresponds to the ID for the individual (or his/her data set) which
 * shall be processed i-th during the optimization effort.
 *
 */
void emit_initIndDataObjects(
        FILE * out,
	int nIndividuals,
	SymbolTable* table,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
	const std::string order_id_pair[]
	                            );
/**
 * Generate C++ source code releaseing resources allocated for IndData 
 * objects and emit the code into a file of which FILE hander is given by the caller.
 * 
 * @arg nIndividuals indicates the number of individuals in the population, 
 * which is also the number of IndData objects defined.
 *
 * @arg out points to an open FILE hander.  The generated C++ code
 * will be appended after the current position.
 *  
 * @arg table is a pointer to the symbol table in which all identifiers
 * to be declared and defined in the class.
 *
 * @arg label_alias_mapping is a mapping between two identifiers
 * that point to a single data set if any.
 *
 * @arg data_for is an array of map objects such that i-th map in the array
 * corresponds to i-th (in the processing order) individual's data set.
 * Each map is a set of pairs of title and its corresponding observation records.
 *
 * @arg order_id_pair is an array of strings such that i-th string in the
 * array corresponds to the ID for the individual (or his/her data set) which
 * shall be processed i-th during the optimization effort.
 *
 */
void emit_releaseIndDataObjects(
        FILE * out,
	int nIndividuals,
	SymbolTable* table,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
	const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
	const std::string order_id_pair[]
	                            );
#endif
