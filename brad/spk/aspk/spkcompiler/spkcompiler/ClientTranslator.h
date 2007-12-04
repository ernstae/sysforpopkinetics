/**
 * @file ClientTranslator.h
 * Declare the ClientTranslator class.
 */
#ifndef CLIENTTRANSLATOR
#define CLIENTTRANSLATOR

#include <xercesc/dom/DOMDocument.hpp>
#include <iostream>

#include "SymbolTable.h"

/**
 * Abstract class that provides interfaces to execute translations from
 * XML to C++.
 *
 * Parsing of SpkDataML documents are done by methods implemented
 * within this class.
 * ID, MDV and EVID data items are added to the parse tree when they
 * are found missing in the data set.
 *
 * Parsing of SpkSourceML documents are done by derived classes 
 * because the documents are client-specific.
 * 
 */
class ClientTranslator{
 public:

  /**
   * Approximation method
   */
  enum APPROX     { FO, 		            /**< First Order */
                    FOCE,                           /**< Expected Hessian */
                    LAPLACE,                        /**< Modified Laplace */
                    STD_TWO_STAGE,                  /**< Standard two stage */
                    GLOBAL_TWO_STAGE,               /**< Global two stage*/
                    ITERATIVE_TWO_STAGE,            /**< Iterative two stage */
                    MAP_BAYES_STANDARD_TWO_STAGE,   /**< Standard two stage with Bayesian objective */ 
                    MAP_BAYES_GLOBAL_TWO_STAGE,	    /**< Global two stage with Bayesian objective*/    
                    MAP_BAYES_ITERATIVE_TWO_STAGE,  /**< Iterative two stage with Bayesian objective */
                    NONPARAM_GRID,                  /**< Nonparametic with grid starting points */
                    NONPARAM_RANDOM_UNIFORM         /**< Nonparametic with random starting points */
                  };
  /** Likelihood evaluation method. */
  enum INTEG_METHOD { ADAPT, GRID, PLAIN, MISER, VEGAS };                                                                                

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
  ~ClientTranslator();

  /**
   * Parse both SpkDataML and SpkSourceML documents and generate C++ source code files.
   */
  void translate();

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
  virtual void parseData() = 0;

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
  inline const SymbolTable* getSymbolTable() const
    {
      return &table;
    }

  /**
   * Return a pointer to the (writable) symbol table.
   */
  inline SymbolTable* getSymbolTable()
    {
      return &table;
    }

  /**
   * Return a non-const pointer to the SpkSourceML parse tree.
   */
  inline xercesc::DOMDocument * getSourceTree()
    {
      return source;
    }
 
  /**
   * Return a constant pinter to the SpkSourceML parse tree.
   */
  inline const xercesc::DOMDocument * getSourceTree() const
    {
      return source;
    }

  /**
   * Return a non-const pointer to the SpkDataML parse tree.
   */
  inline xercesc::DOMDocument * getDataTree()
    {
      return data;
    }

  /**
   * Return a pointer to the SpkDataML parse tree.
   */
  inline const xercesc::DOMDocument * getDataTree() const
    {
      return data;
    }

  /**
   * Return the number of subjects in the population.
   */
  inline const unsigned int getPopSize() const
    {
      return popSize;
    }

  /**
   * Determines the type of analysis: population or individual.
   */
  inline const enum TARGET getTarget() const
    {
      return target;
    }

  /**
   * Determines the type of approximation.
   */
  inline const enum APPROX getApproximation() const
    {
      return approximation;
    }

 protected:

  /**
   * Set the number of subjects in the population.
   * @param n The number of subjects (>0).
   */
  inline void setPopSize( unsigned int n )
    {
      popSize = n;
    }

  /**
   * Set the type of analysis: population or individual.
   * @param targetIn A value of TARGET enumlator indicating the type of analysis.
   */
  inline void setTarget( enum TARGET targetIn )
    {
      target = targetIn;
    }

  /**
   * Set the type of approximation.
   * @param approx A value of APPROX enumlator indicating the type of approximation.
   */
  inline void setApproximation( enum APPROX approx )
    {
      approximation = approx;
    }

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
  SymbolTable table;

  /**
   * The number of individuals in the population.
   */
  unsigned int popSize;

  /**
   * Analysis type: Population or Individual
   */
  enum TARGET target;

  /**
   * Approximation method
   */
  enum APPROX approximation;

};
#endif
