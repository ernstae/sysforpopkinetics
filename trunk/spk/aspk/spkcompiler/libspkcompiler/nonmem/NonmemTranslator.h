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
  // Analyze the <pop_analysis> subtree and generate the source code
  // for the driver.
  //
  // @param pPopAnalysis A pointer to the <popAnalysis> node.
  //
   void parsePopAnalysis ( xercesc::DOMElement* pPopAnalysis );

  //
  // Analyze the <ind_analysis> subtree and generate the source code
  // for the driver.
  //
  // @param pIndAnalysis A pointer to the <indAnalysis> node.
  //
  void parseIndAnalysis ( xercesc::DOMElement* pIndAnalysis );

  //
  // Analyzie the <pred> subtree and generate the source code
  // for the Pred class.
  //
  // @param pPred A pointer to the <pred> node.
  //
  void parsePred( xercesc::DOMElement* pPred );

  //
  // Generate C++ source code for declaring and defining IndData class which
  // is a C++ representation of a single patient records.
  //
  void generateIndData( ) const;

  //
  // Generate C++ source code for declaring and defining DataSet class which
  // is a C++ representation of the set of patient records.
  //
  void generateDataSet( ) const;


  //
  // Generate C++ source code for Pred class.
  //
  void generatePred( const char* predDefFilename ) const;

  //
  // Generate C++ source code for the driver for population analysis.
  //
  void generatePopDriver( ) const;

  //
  // Generate C++ source code for the driver for individual analysis.
  //
  void generateIndDriver( ) const;

  // The header file name for the IndData template class.
  const char *fIndData_h;

  // The header file name for the DataSet template class.
  const char *fDataSet_h;

  // The name of file that contains fortran (ie. NONMEM TRAN) version of
  // the user defined $PRED.
  const char *fPredEqn_fortran;

  // The name of file that contains C++ version of the user defined $PRED 
  // (equations only) model.
  const char *fPredEqn_cpp;
  
  // The header file for the Pred template class.
  const char *fPred_h;

  // The header file for Omega class.
  const char *fOmega_h;

  // The definition file for Omega class.
  const char * fOmega_cpp;
 
  // The driver definition.
  const char * fDriver_cpp;

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
  XMLCh* X_COV_R;
  XMLCh* X_COV_RSR;
  XMLCh* X_COV_S;
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
