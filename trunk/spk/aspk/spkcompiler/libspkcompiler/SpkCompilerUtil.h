#ifndef SPKCOMPILERUTIL_H
#define SPKCOMPILERUTIL_H

/**
 * @file SpkCompilerUtil.h
 * Dealres SpkCompilerUtil class.
 */
/**
 * @example SpkCompilerUtilTest.cpp
 */
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <vector>

using namespace xercesc;

/**
 * A bundle of utility functions commonly used by the processes associated
 * with SpkML<->C++ compilation.
 * 
 */
class SpkCompilerUtil{
 public:

  SpkCompilerUtil();
  ~SpkCompilerUtil();

  /** 
   * Create an XMLCh (2 bytes) string from a C string.
   *
   * Dynamically allocate memory for an XMLCh array 
   * that holds a character array copied from c_str.
   * A pointer to the newly allocated XMLCh string is internally book-keeped
   * so that its resource is released properly when the
   * object goes out of scope.
   *
   * @param c_str points to a character array that holds the base string.
   * @return a read-only pointer to a read-only XMLCh (2 bytes) character array 
   * whose value is copied from c_str.
   */
  const XMLCh* const createXmlString( const char * c_str );

  /**
   * Create an C (1 byte) string from a C string.
   *
   * Dynamically allocate memory for an C array 
   * that holds an XMLCh character array copied from xml_str.
   * A pointer to the newly allocated C string is internally book-keeped
   * so that its resource is released properly when the
   * object goes out of scope.
   *
   * @param xml_str points to an XMLCh character array that holds the base string.
   * @return a read-only pointer to a read-only C (1 byte) character array 
   * whose value is copied from xml_str.
   */
  const char * const createCString( const XMLCh* xml_str );

#ifndef NDEBUG
  /**
   * Obtains the reference to a vector holding created XMLCh strings.
   *
   * The pointers to those XMLCh character arrays which were allocated memory via @c ExpTreeGenerator::createXmlString()
   * are kept internally in a vector for bookkeeping.  
   * This methods returns a pointer to the vector.  The users are not allowed to modify the contents or the pointer
   * value; only to look at it.
   *
   * \note This method is available for internal debug purpose.
   */
  const std::vector< XMLCh* > & debug_xml_strings();

  /**
   * Obtains the reference to a vector holding created C strings. 
   *
   * The pointers to those C character arrays which were allocated memory via @c ExpTreeGenerator::createCString()
   * are kept internally in a vector for bookkeeping.  
   * This methods returns a pointer to the vector.  The users are not allowed to modify the contents or the pointer
   * value; only to look at it.
   *
   * \note This method is available for internal debug purpose.
   */
  const std::vector< char*  > & debug_c_strings();
  int debug_call_releaseXmlStrings();
  int debug_call_releaseCStrings();
#endif

private:
  std::vector< XMLCh* > xml_strings;
  std::vector< char*  > c_strings;

  /**
   * Releases resources allocated for XML string objects.
   *
   * This method immediately releases the resources allocated for those XML string objects which were created
   * via ExpTreeGenerator::createXMLString().
   *
   * @return the number of XML string objects whose allocated resources have been released.
   */
  int releaseXmlStrings();

  /**
   * Releaseing reesources allocated for C strings.
   *
   * This method immediately releases the resources allocated for those C strings which were created
   * via @c ExpTreeGenerator::createCString().
   *
   * @return the number of C strings whose allocated resources have been released.
   */
  int releaseCStrings();
};

static SpkCompilerUtil spkcompiler_util;

/**
 * A macro for converting an ANSII character string to XMLCh string.
 */
#define X(c) spkcompiler_util.createXmlString(c)

/**
 * A macro for converting an XMLCh string to ANSII character string.
 */
#define C(x) spkcompiler_util.createCString(x)

/**
 * Trim leading and trailing white spaces.
 */
XMLCh* trim( const XMLCh* source );

#endif
