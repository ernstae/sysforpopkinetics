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
 * 
 * @ingroup nonmem
 */
/**
 * @example read_nonmem_dataTest.cpp
 */
/**
 * Process <data> section of SpkInML document.
 *
 * DTD for <data> subtree:
 * @code
 * <!ELEMENT data         (individual)+>
 * <!ELEMENT individual   (item)+>
 * <!ELEMENT item         (value)+>
 *
 * <!ATTLIST individual    order CDATA  #IMPLIED>
 * <!ATTLIST individual    id CDATA     #REQUIRED>
 * <!ATTLIST individual    length CDATA #REQUIRED>
 *
 * <!ATTLIST item label   (CDATA|id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3
 *                         |drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt|
 *                         |call|cont)  #REQUIRED>
 * <!ATTLIST item synonym (CDATA|id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3
 *                         |drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt
 *                         |call|cont)  #IMPLIED>
 * @endcode
 *
 * @return void
 *
 * @param dataNode Pointer to an DOMElement object that
 * is the root of <data> subtree in the parsed SpkInML document.
 * The subtree is expected to obey the DTD grammer specification.
 *
 * @param tableOut Symbol table to which the values of
 * attributes, @a label and @a synonym, are inserted as @a keyword (predefined).
 *
 * @param label_aliasOut Table to which the values of attribute,
 * @a label, is inserted as keys and @a synonym as entries.
 * When a @a synonym is not given, an empty string will be inserted
 * as the entry of the corresponding key (@a label).
 *
 * @param data_forOut Ordered list of tables in which the i-th table
 * contains the i-th subject's data set.  Each table mapps @a labels
 * and their corresponding arrays of data.
 * 
 * @param IDsOut Ordered list of strings that identify subjects.
 * The i-th entry identifies the i-th subject in the population.
 *
 * @param spkInfoOut Upon the successful completion,
 * @ref SpkParameters::nMeasurements "spkInfoOut::nMeasurements"
 * shall contain the number of <item>s in each <individual>.
 * The i-th value of spkInfoOut::nMeasurements is the number of
 * <item>s under the <individual> whose @a order attribute value is
 * @a i or the order in which the <individual> appeared in the
 * <data> subtree if @a order were not specified.
 * @ref SpkParameters::measurementsAll "spkInfoOut::measurementsAll" 
 * will be filled with <value>s: 
 * @a nMeasurements elements from (nMeasurements * i) 
 * in the vector are <value>s, in the order of appearance, under 
 * the <item> whose
 * @a label or @synonym is marked as @a dv (dependent variable) 
 * in the <individual>
 * subsubtree whose @a order attribute value is @a i or 
 * the tag which appeared @a i -th in the <data> subtree if
 * @a order were not specified.
 *
 * @note @a nIndividuals is considered as @em the number of
 * individuals in the population.  Thus, only and at most #a nIndividuals
 * number of <individual>s in the <data> subtree will be processed.
 * More or Less <individual>s will cause unexpected behaviors.
 */
void read_nonmem_data( 
	xercesc::DOMElement* dataNode, 
        int nIndividuals,
	SymbolTable & tableOut,
	std::map<std::string, std::string> &label_synonymOut,
	std::vector< std::map<std::string, SPK_VA::valarray<double> > > & data_forOut,
	std::string IDsOut[],
        struct SpkParameters & spkInfoOut
	);
#endif
