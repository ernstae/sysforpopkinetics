#ifndef SPKCOMPILERUTIL_H
#define SPKCOMPILERUTIL_H

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
  const std::vector< XMLCh* > & debug_xml_strings();
  const std::vector< char*  > & debug_c_strings();
  int debug_call_releaseXmlStrings();
  int debug_call_releaseCStrings();
#endif

private:
  std::vector< XMLCh* > xml_strings;
  std::vector< char*  > c_strings;

  int releaseXmlStrings();
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

#endif
