#ifndef NONMEMTRANSLATOR_H
#define NONMEMTRANSLATOR_H

#include "../ClientTranslator.h"
#include "../SymbolTable.h"

#include <xercesc/dom/DOMDocument.hpp>

class NonmemTranslator : public ClientTranslator
{
 public:
  NonmemTranslator( xercesc::DOMDocument* sourceIn, xercesc::DOMDocument* dataIn );
  ~NonmemTranslator();

  virtual void parseSource();

 protected:
  NonmemTranslator();
  NonmemTranslator( const NonmemTranslator & );
  NonmemTranslator& operator=( const NonmemTranslator& );
  
 private:
  int parsePopAnalysis ( xercesc::DOMElement* );  // return the pop. size
  void parseIndAnalysis ( xercesc::DOMElement* );
  void parsePred( xercesc::DOMElement*, SymbolTable& table, char [] );
  void parseTables( xercesc::DOMNodeList*, std::vector<int>& );
  void parseScatterplots( xercesc::DOMNodeList*, std::vector<int>& );
  void generateIndData();

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
};

#endif
