#ifndef EXPTREEGENERATOR_H
#define EXPTREEGENERATOR_H

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "ExpNodeCarrier.h"
#include "SpkCompilerUtil.h"

/**
 * @file ExpTreeGenerator.h
 *
 * Declares ExpTreeGenerator class.
 */
/**
 * A class that encapusulate a DOM-based tree.
 * 
 * Initializing, allocating resources and releasing a DOM tree and its component nodes
 * require some readings and, as a result, it is easy to forget or do wrong
 * some of the processes.  This class ensures these resource aquisition and release
 * are done automatically and also provide a couple methods to manupulate tree
 * components for convenience.
 *
 */
class ExpTreeGenerator {

 public:
  /**
   * The default constructor.  Intialize DOM environment and create a valid DOM document.
   */
  ExpTreeGenerator();

  /**
   * The destructor. 
   *
   * The destrcutor ensures those resources allocated via the class methods to be released properly.
   */
  ~ExpTreeGenerator();

  /**
   * @return a pointer to a valid DOMDocument object.
   * The pointer to the object will be used to directly create/manipulate DOM elements by the user.
   */
  xercesc::DOMDocument * getRoot( void ) const;

  /**
   * Create an empty ExpNodeCarrier data structure object.  
   *
   * The created object is 
   * empty in a sense that its @c node element is pointing to nothing.  The user
   * is responsible for creating a DOMElement object (using @c ExpTreeGenerator::getRoot()->createElement(const XMLCh*))
   * and set the @c node pointer to poin to it.
   *
   * @return a pointer to a ExpNodeCarrier data structure object.
   *
   */
  struct ExpNodeCarrier * createExpNodeCarrier( void );

  /**
   * Releases resources allocated for ExpNodeCarrier objects.
   *
   * This method immediately releases the resources allocated for those ExpNodeCarrier objects which were created
   * via ExpTreeGenerator::createExpNodeCarrier().
   *
   * @return the number of ExpNodeCarrier objects whose allocated resources have been released.
   */
  int releaseExpNodeCarriers( void );

  /**
   * Prints out the DOM-based tree to the standard output.
   */
  void printToStdout( void ) const;

  /**
   * Prints out the DOM-based tree to a file.
   *
   * @param filename is a path to a file into which tree contents are written.
   */
  void printToFile( const char* filename ) const;


 private:
  xercesc::DOMImplementation * impl;
  xercesc::DOMDocument * doc;
  xercesc::DOMElement * root;
  XMLCh * core;
  XMLCh * unit;

  std::vector<struct ExpNodeCarrier*> nodes;
};
#endif
