/**
 * @file NonmemTranslator.h
 * Declare NonmemTranslator class.
 */
#ifndef NONMEMTRANSLATOR_H
#define NONMEMTRANSLATOR_H

#include "../ClientTranslator.h"
#include "../SymbolTable.h"

#include <iostream>

#include <xercesc/dom/DOMDocument.hpp>

/**
 * NonmemTranslator is an implementation of ClientTranslator abstract class.
 *
 * This class implements, in particular, the "parseSource()" virtual
 * member function.  The function analyzes the SpkSourceML parse tree
 * and generete the source code files for IndData class, DataSet class,
 * the driver and Pred class.
 */
class NonmemTranslator : public ClientTranslator
{
 public:

  /**
   * The only legal constructor.
   *
   * All strings of type XMLCh* or XMLString that will be used within the member
   * functions will be created (ie. the resources are allocated dynamically)
   * here.
   *
   * @param sourceIn A pointer to the SpkSourceML document.
   * @param dataIn   A pointer to the SpkDataML document.  This is passed to the super class.
   */
  NonmemTranslator( xercesc::DOMDocument* sourceIn, xercesc::DOMDocument* dataIn );

  /**
   * Destructor.
   *
   * All strings of type XMLCh* or XMLString that were created in the construction
   * (ie. the resources were allocated dynamically) are destroyed properly
   * here. 
   */
  ~NonmemTranslator();

  /**
   * The implementation of ClientTranslator::parseSource() 
   * particular to NONMEM-user-based inputs.
   */
  virtual void parseSource();

 protected:
  NonmemTranslator();
  NonmemTranslator( const NonmemTranslator & );
  NonmemTranslator& operator=( const NonmemTranslator& );
  
 private:

  //
  // Analyze the <pop_analysis> subtree and returns the size of population.
  //
  int  parsePopAnalysis ( xercesc::DOMElement* sourceML );

  //
  // Analyze the <ind_analysis> subtree.
  //
  void parseIndAnalysis ( xercesc::DOMElement* sourceML );

  //
  // Analyzie the <pred> subtree.
  //
  void parsePred( xercesc::DOMElement* sourceML, SymbolTable& table, char [] );

  // void parseTables( xercesc::DOMNodeList* tables, std::vector<int>&  );

  // void parseScatterplots( xercesc::DOMNodeList*, std::vector<int>& );

  //
  // Generate C++ source code for declaring and defining IndData class which
  // is a C++ representation of a single patient records.
  //
  // @param final_symbol_table The (const) reference to the symbol table which
  // shall contain all entries extracted from SpkSourceML and SpkDataML
  // documents.
  //
  void generateIndData( const SymbolTable& final_symbol_table ) const;

  //
  // Generate C++ source code for declaring and defining DataSet class which
  // is a C++ representation of the set of patient records.
  //
  // @param pop_size The number of different IDs in the data set, 
  // which is interpreted as the size of the population.
  //
  // @param final_symbol_table The (const) reference to the symbol table which
  // shall contain all entries extracted from SpkSourceML and SpkDataML
  // documents.
  //
  void generateDataSet( int pop_size, const SymbolTable& final_symbol_table ) const;

  // The header file name for IndData class.
  const char *fIndData_h;

  // The definition file name for IndData class.
  const char *fIndData_cpp;

  // The header file name for DataSet class.
  const char *fDataSet_h;

  // The definition file name for DataSet class.
  const char *fDataSet_cpp;

  // The string for the file burner.
  const char *BURNER;

  //========================================
  // Dynamically allocated string objects
  //----------------------------------------
  XMLCh* X_YES;
  XMLCh* X_NO;
  XMLCh* X_FIXED;
  XMLCh* X_IN;
  XMLCh* X_LOW;
  XMLCh* X_UP;
  XMLCh* X_DIAGONAL;
  XMLCh* X_BLOCK;
  XMLCh* X_VALUE;
  XMLCh* X_STRUCT;
  XMLCh* X_DIMENSION;
  XMLCh* X_LABEL;
  XMLCh* X_IS_ERR_OUT;
  XMLCh* X_IS_CORR_OUT;
  XMLCh* X_IS_COV_OUT;
  XMLCh* X_IS_INV_COV_OUT;
  XMLCh* X_IS_COEF_OUT;
  XMLCh* X_IS_CONF_OUT;
  
  XMLCh* X_NONMEM;
  XMLCh* X_POP_ANALYSIS;
  XMLCh* X_IND_ANALYSIS;
  XMLCh* X_CONSTRAINT;
  XMLCh* X_MODEL;
  XMLCh* X_PRED;
  XMLCh* X_PRESENTATION;
  XMLCh* X_TABLE;
  XMLCh* X_SCATTERPLOT;
  XMLCh* X_COLUMN;
  XMLCh* X_X;
  XMLCh* X_Y;
  XMLCh* X_BY;
  XMLCh* X_APPROXIMATION;
  XMLCh* X_FO;
  XMLCh* X_FOCE;
  XMLCh* X_LAPLACE;
  XMLCh* X_POP_SIZE;
  XMLCh* X_IS_ESTIMATION;
  XMLCh* X_IS_ETA_OUT;
  XMLCh* X_IS_RESTART;
  XMLCh* X_DATA_LABELS;
  XMLCh* X_FILENAME;
  XMLCh* X_NAME;
  XMLCh* X_SYNONYM;
  XMLCh* X_THETA;
  XMLCh* X_LENGTH;
  XMLCh* X_OMEGA;
  XMLCh* X_SIGMA;
  XMLCh* X_SIMULATION;
  XMLCh* X_SEED;
  XMLCh* X_POP_STAT;
  XMLCh* X_COVARIANCE_FORM;
  XMLCh* X_MITR;
  XMLCh* X_IND_STAT;
  //========================================
};

#endif
