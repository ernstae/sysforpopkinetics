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
   * Return a pointer to the (read-only) symbol table.
   *
   * @return t A pointer to the symbol table.
   */
  const SymbolTable* getSymbolTable() const;

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
   * The symbol table.
   */
  SymbolTable stable;

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

};
#endif
