#ifndef CLIENTTRANSLATOR_H
#define CLIENTTRANSLATOR_H

#include <vector>

#include <xercesc/dom/DOM.hpp>
#include "SpkParameters.h"
/**
 * @file ClientTranslator.h
 * Declares ClientTranslator abstract class.
 */
/**
 * @example ClientTranslatorTest.cpp
 */
/**
 * ClientTranslator class defines the interfaces
 * to be implemented by sublasses specific to clients (ex. NONMEM, SAAM2...).
 *
 * These interfaces include a method to initiate the process of reading an
 * SpkML document and convert it to C++ source code files,
 * ways to acquire the names of these C++ files created and so on.
 */
class ClientTranslator
{
 public:
  /**
   * Collect and assemble information mined in the tree necessarily
   * to generate a SPK driver file and an SpkModel definition
   * files.
   *
   * When process is completed successfully, the vector returned
   * by getModelFilenameList() shall contain path names associated
   * C++ source code files generated during the translation.
   * Together, when they are linked to SPK Library, they must
   * drive one of SPK entry points and obtain analysis results.
   * 
   * When process is completed successfully, two sets of information 
   * besides C++ source code are
   * expected, but not required, to be assembled: A data set returned by
   * getSpkParameters() and a data set returned by getClientParameters().
   * The first set is composed in the form of SpkParameters data structure.
   * It has place holders like "popParIn" for the initial value for
   * the population parameter.  The second set is in an arbitrary form (void*).
   * It can contain any value in any form with any names for convenience.
   * This set will be primarily used for debugging purpose.
   * 
   * @param tree is the DOMDocument-based tree representation
   * of an SpkML input document.
   */
  virtual void translate ( xercesc::DOMDocument * tree ) = 0;

  /**
   * Obtain a pointer to an SpkParameters data structure object used
   * to record values expressed in such a way that SPK can understand.
   *
   * @return a SpkParameters data structured object that may or may
   * not have valid values associated with its declared variables.
   */
  virtual const struct SpkParameters * getSpkParameters() const = 0;

  /**
   * Obtain a pointer to an object used to record values
   * expressed i the client specific language/terms.
   *
   * For this reason, the data type of the returned value is void*.
   * The caller has to know which specific data type to cast into
   * in order to access the elements.
   * 
   * @return an arbitrary object that may or may not contain 
   * a set of information gathered during the translation process
   * and specific to the client.
   */
  virtual const void * getClientParameters() const = 0;

  /**
   * Obtain a list of character arrays holding the names of the
   * generated files that, together, define a subclass of SpkModel.
   * 
   * @return a vector of character arrays contaings paths followed by
   * the filenames associated with the C++ source code files defining
   * a SpkModel subclass.
   */
  virtual const std::vector< std::string > getFilenameList() const = 0;
};

#endif

