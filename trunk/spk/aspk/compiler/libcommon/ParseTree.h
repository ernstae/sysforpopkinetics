#ifndef PARSETREE_H
#define PARSETREE_H

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "NodeCarrier.h"
#include "SpkCompilerUtil.h"

/**
 * @file ParseTree.h
 *
 * Declares ParseTree class.
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
class ParseTree {

 public:
  /**
   * The default constructor.  Intialize DOM environment and create a valid DOM document.
   */
  ParseTree();

  /**
   * The destructor. 
   *
   * The destrcutor ensures those resources allocated via the class methods to be released properly.
   */
  ~ParseTree();

  /**
   * @return a pointer to a valid DOMDocument object.
   * The pointer to the object will be used to directly create/manipulate DOM elements by the user.
   */
  xercesc::DOMDocument * handler( void ) const;

  /**
   * Create an empty NodeCarrier data structure object.  
   *
   * The created object is 
   * empty in a sense that its @c node element is pointing to nothing.  The user
   * is responsible for creating a DOMElement object (using @c ParseTree::hander()->createElement(const XMLCh*))
   * and set the @c node pointer to poin to it.
   *
   * @return a pointer to a NodeCarrier data structure object.
   *
   */
  struct NodeCarrier * createNodeCarrier( void );

  /**
   * Converts a C string to XMLCh string (ie. unicode-based string).
   *
   * The method allocates memory for a XMLCh string object, unicode version of the string held in @c c_str
   * and returns a pointer to it.  The ParseTree class ensures the memory allocated by this method
   * to be released properly.
   *
   * @param c_str is a pointer to a C character array that holds a string to be converted.
   * The string held in the array must be contiguous (no space)
   * and may contain alphabets, numbers, and underscores.
   *
   * @return a pointer to a XMLCh character array that has a string equivalent to the value in @c c_str.
   */
  //const XMLCh * createXmlString( const char * c_str );

  /**
   * Converts a XMLCh string to a C string (ie. native code-based string).
   *
   * The methods allocates memory for a C character array long enough to hold the string represented
   * in @c x_str.  The ParseTree class ensures the memory allocated by this method to be
   * released properly.
   *
   * @param x_str is a pointer to a XMLCh character array that holds a string to be convereted.
   * The string held in the array must be contiguous (no space)
   * and may contain alphabets, numbers, and underscores.
   * 
   * @return a pointer to a C character array that has a string equivalent to the value in @c x_str.
   */
  //const char  * createCString  ( const XMLCh * x_str );

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

  /**
   * Obtains the reference to a vector holding created XMLCh strings.
   *
   * The pointers to those XMLCh character arrays which were allocated memory via @c ParseTree::createXmlString()
   * are kept internally in a vector for bookkeeping.  
   * This methods returns a pointer to the vector.  The users are not allowed to modify the contents or the pointer
   * value; only to look at it.
   *
   * \note This method is available for internal debug purpose.
   */
  //const std::vector<XMLCh*> & debug_xml_strings( void ) const;

  /**
   * Obtains the reference to a vector holding created C strings. 
   *
   * The pointers to those C character arrays which were allocated memory via @c ParseTree::createCString()
   * are kept internally in a vector for bookkeeping.  
   * This methods returns a pointer to the vector.  The users are not allowed to modify the contents or the pointer
   * value; only to look at it.
   *
   * \note This method is available for internal debug purpose.
   */
  //const std::vector<char*>  & debug_c_strings( void ) const;

  /**
   * Releasing resources allocated for XMLCh strings.
   *
   * This method immediately releases the resources allocated for those XMLCh strings which were created
   * via ParseTree::createXmlString().
   *
   * @return the number of XMLCh strings whose allocated resources have been released.
   */
  //int releaseXmlStrings( void );

  /**
   * Releaseing reesources allocated for C strings.
   *
   * This method immediately releases the resources allocated for those C strings which were created
   * via @c ParseTree::createCString().
   *
   * @return the number of C strings whose allocated resources have been released.
   */
  //int releaseCStrings( void );

  /**
   * Releases resources allocated for NodeCarrier objects.
   *
   * This method immediately releases the resources allocated for those NodeCarrier objects which were created
   * via ParseTree::createNodeCarrier().
   *
   * @return the number of NodeCarrier objects whose allocated resources have been released.
   */
  int releaseNodeCarriers( void );

 private:
  xercesc::DOMImplementation * impl;
  xercesc::DOMDocument * doc;
  xercesc::DOMElement * root;
  XMLCh * core;
  XMLCh * unit;

  std::vector<struct NodeCarrier*> nodes;
};
#endif
