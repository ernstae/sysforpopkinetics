#ifndef EXPTREEGENERATOR_H
#define EXPTREEGENERATOR_H

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "ExpNodeCarrier.h"
#include "SpkCompilerUtil.h"
#include "SymbolTable.h"
/**
 * @file ExpTreeGenerator.h
 *
 * Declares ExpTreeGenerator class
 */
/**

 */
class ExpTreeGenerator {

 public:
  /**
   * The default constructor.  
   * 
   * This constructor initializes exercesc::DOMPlatformUtils 
   * with locale=en_US if it has not done so yet.
   * Initialization of DOMPlatformUtil is required prior to 
   * the first use of xercesc DOM related features.
   * DOM features are used in building expression trees which
   * are implemented in terms of DOMDocument (as an xml object).
   *
   */
  ExpTreeGenerator();

  /**
   * The destructor. 
   *
   * The destructor releases resources allocated for objects
   * created/used during the object's life cycle.  
   * The DOMPlatformUtil shall not be terminated with the
   * call to this destructor.  It will be terminated automatically
   * as the process goes out of life cycle.
   */
  ~ExpTreeGenerator();

  /**
   * Create a new DOM-based parse tree.
   *
   * The resources allocated for the parse trees will be released
   * when the ExpTreeGenerator object goes out of life cycle.
   * 
   * @param name is a pointer to XMLCh character array that holds
   * a name used to label the tree root.
   *
   * @return a pointer to a new DOMDocument object.
   */
  DOMDocument* createTree( const XMLCh* name );

  /**
   * Create a new DOM-based parse tree.
   *
   * The resources allocated for the parse trees will be released
   * when the ExpTreeGenerator object goes out of life cycle.
   * 
   * @param name is a pointer to ANSII char character array that holds
   * a name used to label the tree root.
   *
   * @return a pointer to a new DOMDocument object.
   */
  DOMDocument* createTree( const char* name );


  /**
   * Create an empty ExpNodeCarrier data structure object.  
   *
   * The created object is 
   * empty in a sense that its @c node element is pointing to nothing.  The user
   * is responsible for creating a DOMElement object 
   * (using @c ExpTreeGenerator::getRoot()->createElement(const XMLCh*))
   * and set the @c node pointer to poin to it.
   *
   * @return a pointer to a ExpNodeCarrier data structure object.
   *
   */
  struct ExpNodeCarrier * createExpNodeCarrier( void );

  /**
   * Releases resources allocated for ExpNodeCarrier objects.
   *
   * This method immediately releases the resources allocated for those 
   * ExpNodeCarrier objects which were created
   * via ExpTreeGenerator::createExpNodeCarrier().
   *
   * @return the number of ExpNodeCarrier objects whose resources
   * are released.
   */
  int releaseExpNodeCarriers( void );

  /**
   * Releases resources allocated for DOMDocument-based parse tree objects.
   *
   * This method immediately releases the resources allocated for those 
   * DOMDocument objects which were created
   * via ExpTreeGenerator::createTree().
   *
   * @return the number of DOMDocument objects whose resources
   * are released.
   */  int releaseTrees( void );

  /**
   * Prints out a DOM based parse tree to the standard output.
   *
   * @param tree is a pointer to a DOMDocument document to be
   * printed out.
   */
  void printToStdout( const xercesc::DOMDocument* tree ) const;

  /**
   * Prints out the DOM-based tree to a file.
   *
   * @param tree is a pointer to a DOMDocument document to be printed out.
   * @param filename is a path to a file into which tree contents are written.
   */
  void printToFile( const DOMDocument * tree, const char* filename ) const;


 private:
  xercesc::DOMImplementation * impl;

  std::vector<struct ExpNodeCarrier*> nodes;
  std::vector<DOMDocument *> trees;
};


#endif
