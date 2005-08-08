/**
 * @file ClientTranslator.h
 * Declare the ClientTranslator abstract class.
 */
#ifndef CLIENTTRANSLATOR
#define CLIENTTRANSLATOR

#include <xercesc/dom/DOMDocument.hpp>
#include <iostream>

#include "SymbolTable.h"

/**
 * Abstract class that provides interfaces to execute translations from
 * XML to C++.  Generated C++ source code files are stored in the
 * current working directory.
 */
class ClientTranslator{
 public:

  /**
   * Approximation method
   */
  enum APPROX     { FO, FOCE, LAPLACE };

  /**
   * Analysis type
   */
  enum TARGET     { IND, POP };

  /**
   * The constructor that takes arguments.
   *
   * @param sourceIn A pointer to the parsed SpkSourceML document tree.
   * @param dataIn   A pointer to the parsed SpkDataML document tree.
   */
  ClientTranslator( xercesc::DOMDocument * sourceIn, xercesc::DOMDocument * dataIn );

  /**
   * The destructor.
   */
  virtual ~ClientTranslator();

  /**
   * Parse both SpkDataML and SpkSourceML documents and generate C++ source code files.
   */
  void translate();

  /**
   * Insert the ID field (ie. node) in each record (subtree) if the data set lacks of it.
   * Returns the location in which the ID field can be found.
   */
  int insertID( xercesc::DOMElement* dataset );

  /**
   * Insert the MDV field (ie. node) in each record (subtree) if the data set lacks of it.
   * Returns the location in which the MDV field can be found.
   */
  int insertMDV( xercesc::DOMElement* dataset );

  /**
   * Insert the EVID field (ie. node) in each record (subtree) if the data set lacks of it.
   * Returns the location in which the EVID field can be found.
   * This routine assumes MDV is present or has been inserted by SPK Compiler in the data set.
   */
  int insertEVID( xercesc::DOMElement* dataset );

  /**
   * Parse the DOMDocument tree that represents
   * the SpkDataML document and register the foundings into the symbol table.
   *
   * Precondition: The symbol table has no *label* entries
   * (ie. parseSource() shall not have been completed).
   *
   * Postcondition: Upon the successful completion, the symbol table will be populated
   * with the data labels and their corresponding data values.
   * The labels however are not associated with (possible) synonyms yet
   * at this point.
   * 
   */
  void parseData();

  /**
   * Parse the DOMDocument tree that represents
   * the SpkSourceML document and register the foundings 
   * into the symbol table.
   *
   * Precondition: The symbol table contains the *label* entries
   * (ie. parseData() shall have been completed in advance).
   *
   * Postcondition: Upon the successful completion, the label entries in the symbol
   * table will be associated with possible synonyms.
   * The symbol table also will contain the user defined (scalar) variables
   * found in the model definition expressions and the NONMEM predefined
   * variables with the initial and boundary values found in the document.
   *
   */
  virtual void parseSource() = 0;

  /**
   * Determine the number of individuals in the population and the
   * type of analysis (population/individual).
   * The number of individuals is set in ourPopSize and 
   * the type of analysis is set in ourTarget.
   * The return value of this routine is the number of individuals.
   */
  virtual int detAnalysisType() = 0;

public:

  /**
   * Return a pointer to the (read-only) symbol table.
   *
   * @return t A pointer to the symbol table.
   */
  const SymbolTable* getSymbolTable() const;

  /**
   * Return a pointer to the (writable) symbol table.
   *
   * @return t A pointer to the symbol table.
   */
  SymbolTable* getSymbolTable();

 protected:
  /**
   * A pointer to the SpkSourceML parse tree.
   */
  xercesc::DOMDocument * source;

  /**
   * A pointer to the SpkDataML parse tree.
   */
  xercesc::DOMDocument * data;

  /**
   * The number of individuals in the population.
   */
  unsigned int ourPopSize;

  /**
   * Analysis type: Population or Individual
   */
  enum TARGET ourTarget;

  /**
   * Approximation method
   */
  enum APPROX ourApproximation;

  /**
   * The number of data recoreds for each individual.
   */
  std::valarray<int> ourN;

  /**
   * The symbol table.
   */
  SymbolTable table;

  /**
   * The default constructor.
   */
  ClientTranslator();
  /**
   * The copy constructor.
   */
  ClientTranslator( const ClientTranslator& );
  /**
   * The assignment operator.
   */
  ClientTranslator & operator=( const ClientTranslator& );

 private:

  int whereis( const XMLCh* label ) const;

  static const char * C_SPKDATA;     XMLCh* X_SPKDATA;
  static const char * C_VERSION;     XMLCh* X_VERSION;
  static const char * C_POINTONE;    XMLCh* X_POINTONE;
  static const char * C_TABLE;       XMLCh* X_TABLE;
  static const char * C_COLUMNS;     XMLCh* X_COLUMNS;
  static const char * C_ROWS;        XMLCh* X_ROWS;
  static const char * C_DESCRIPTION; XMLCh* X_DESCRIPTION;
  static const char * C_ROW;         XMLCh* X_ROW;
  static const char * C_POSITION;    XMLCh* X_POSITION;
  static const char * C_VALUE;       XMLCh* X_VALUE;
  static const char * C_TYPE;        XMLCh* X_TYPE;
  static const char * C_NUMERIC;     XMLCh* X_NUMERIC;
  static const char * C_ID;          XMLCh* X_ID;
  static const char * C_MDV;         XMLCh* X_MDV;
  static const char * C_EVID;        XMLCh* X_EVID;
};
#endif
