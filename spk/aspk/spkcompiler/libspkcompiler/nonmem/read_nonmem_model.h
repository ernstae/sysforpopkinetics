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
/**
 * Discover new symbols used in the expressions
 * embedded in the parse tree
 * and register them in addition to those known
 * symbols specific to the base model (or none in the case of PRED)
 * to the given symbol table.
 *
 * @return A pair of enumulators.  The first entry
 * indicates the type of base model for which the expressions are
 * given or @ref nonmem::MODEL "nonmem::NONE" when no canned model is based.
 * The second entry is the type of parameterization.  
 * This value is @ref nonmem::TRANS "nonmem::DEFAULT" when
 * no TRANS is based.
 * 
 * @param modelNode A pointer to the root of <model> parse tree.
 * DVD for the model tree is:
 * @code
 * <!---------------------------------------------------
 *    <model> section contains information
 *    necessary to define the physiological and
 *    the statistical models.
 * ----------------------------------------------------->
 * <!ELEMENT model ( (comp_model, diffeqn)
 *                   | (comp_model?, (pk, error), diffeqn?) 
 *                   | (pred) )>
 *
 * <!--
 *    <comp_model> is required when a general 
 *    model (ADVAN5-9) is used as the "base" model.
 *    In this block, parameters that define a compartmental
 *    model are provided.
 *    <comp_model> would usually follow <pk> block.  
 *    <pk> is omitted if <link>s are specified to define
 *    the compartmental model.
 * -->
 * <!ELEMENT comp_model (compartment+, link*) >
 *
 * <!ATTLIST comp_model ncompartments CDATA #REQUIRED>
 * <!ATTLIST comp_model nequilibrium  #REQUIRED>
 * <!ATTLIST comp_model nparameters #IMPLIED>
 *
 * <!ELEMENT compartment EMPTY>
 * <!ATTLIST compartment name CDATA #REQUIRED>
 * <!ATTLIST compartment initial_off (yes|no) #REQUIRED>
 * <!ATTLIST compartment no_off (yes|no) #REQUIRED>
 * <!ATTLIST compartment no_dose (yes|no) #REQUIRED>
 * <!ATTLIST compartment equilibrium (yes|no) #REQUIRED>
 * <!ATTLIST compartment exclude (yes|no) #REQUIRED>
 *
 * <!--
 *    <link> is required only in the absence of <pk>
 *    abbrivated code.  <link> defines a flow
 *    rate constant, k,  from compartment-"from" to 
 *    compartment-"to".  If the flow is bidirectional,
 *    specify the flows with two <link> blocks.
 * -->
 * <!ELEMENT link EMPTY>
 * <!ATTLIST link from CDATA #REQUIRED>
 * <!ATTLIST link to   CDATA #REQUIRED>
 * <!ATTLIST link k    CDATA #REQUIRED>
 *
 * <!--
 *    <diffeqn> is required with ADVAN6, 8, 9.
 *    In this block, a system of differential equations
 *    is provided. (this corresponds to $DES in NONMEM)
 * -->
 * <!ELEMENT diffeqn (#PCDATA)>
 *
 * <!--
 *    <pk> is required when "base" attribute of <model>
 *    is present: that is, when a canned model is used.
 *    This is the user-portion of f(alp, b) definition.
 * -->
 * <!ELEMENT pk (#PCDATA)>
 * 
 * <!--
 *      <error> is required when "base" attribute of
 *      <model> is present.  This is the user-portion of
 *      R(alp, b) definition.  
 *      Case INsensitive.
 * -->
 * <!ELEMENT error (#PCDATA)>
 * 
 * <!--
 *      <pred> is required when "base" attribute of
 *      <model> is NOT present.
 *      This is the user definition of the physiological
 *      model (combining f(alp,b) and R(alp,b).
 *      Case INsensitive.
 * -->
 * <!ELEMENT pred (#PCDATA)>
 * 
 * <!--
 *      <model> may take "base" attribute when
 *      a NONMEM canned model is going to be used.
 *      When this attribute is given, <pk> and <error>
 *      blocks must be provided too.
 *      When this is not given, <pred> must appear.
 * -->
 * <!ATTLIST model base (advan1|advan2|advan3|advan4|advan5|advan6
 *                      |advan7|advan8|advan9|advan10) #IMPLIED>
 * 
 * 
 * <!--
 *      When one of ADVAN models is used, the user may choose
 *      a specific TRANS routine from the list.  
 *      The default is TRANS1.
 * -->
 * <!ATTLIST model trans (trans1|trans2|trans3|trans4|trans5) #FIXED="trans1">
 * 
 * 
 * <!-- 
 *      When one of ADVAN models is used, the user may set "tolerance"
 *      to an integer value that defines the required number of digits
 *      of accuracy.
 * -->
 * <!ATTLIST model tolerance (#PCDATA) #IMPLIED>
 *
 * @endcode
 * 
 * @param nIndividuals The number of <individual> incidents
 * in the model tree.  Only the first @a nIndividual number of
 * <individual> incidents will be processed when there
 * are more.  If there are less <individual> incidents,
 * the resulting behavior is undetermined.
 *
 * @param symbol_table A pointer to the symbol table in which
 * newly dicovered symbols and known predefined symbols will
 * be registered.
 *
 */
std::pair<enum nonmem::MODEL, enum nonmem::TRANS>
   read_nonmem_model( xercesc::DOMElement* modelNode, 
			int nIndividuals, 
			SymbolTable * symbol_table );

#endif
