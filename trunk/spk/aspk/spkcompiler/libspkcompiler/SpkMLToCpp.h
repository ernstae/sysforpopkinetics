#ifndef SPKMLTOCPP_H
#define SPKMLTOCPP_H

#include <vector>

//#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "SpkParameters.h"
#include "ClientTranslator.h"
#include "client.h"
#include "SymbolTable.h"
//#include "ExpTreeGenerator.h"
#include "nonmem/explang.h"

/**
 * @file SpkMLToCpp.h
 * 
 * This header declares the SpkMLToCpp (SpkML->C++ compiler) class and
 * extern-s the global variables that serve as the communication
 * channel between the SpkML->C++ compiler and 
 * the expression language parser (and lexical analyizer)
 * which is internally used by the compiler to translate only
 * the algebraic expression portions found in the input
 * XML document.
 */
/**
 * @example SpkMLToCppTest.cpp
 */
extern "C"{
  /**
   * The lexical analyzer (tokenizer) for expressions.
   *
   * The function shall be implemented for each client type
   * because different clients use different grammers and
   * words to express expressions.
   *
   * @note This function is implemented in the form of
   * LEX definition.
   */
  int nm_lex(void);  
  
  /**
   * The syntax analyzer for expressions.
   *
   * The function is implemented for each client type 
   * because different clients use different grammers and 
   * words to express expressions.
   *
   * @note This function is implemented in the form of 
   * YACC definition.
   */
  int nm_parse(void);

  void nm_restart( FILE* );
};
/**
 * A global flag used by nm_parse() to indicate as to whether
 * nm_parse() emits debugging information as it proceeds.
 *
 * @note YACC (BISON in Linux) declares this flag.
 */
extern int nm_debug;

/**
 * A global pointer used by nm_lex() and nm_parse().  The caller of nm_lex()
 * sets the pointer to a FILE handler for the input (expressions) file.
 *
 * @note LEX (FLEX in Linux) declares this pointer.
 */
extern FILE *nm_in;
  
/**
 * A global counter used by nm_lex() to count the number of lines it
 * has read so far.
 *
 * 
 * nm_lex() only increments (hence, it does not initialize)
 * the counter from the value set prior to the call.
 * It will probably most make sense if the caller of nm_parse()
 * initialize this counter just before a call to nm_parse() which
 * in turn calls nm_lex() repetively until all contents in a file
 * are read.  One example of situations where the counter is
 * initialized to a value greater than zero is where
 * the file contains expressions extracted from another file
 * and the first expression appeared somewhere in the middle
 * of the original file.
 * 
 * @note This counter is actually declared in a file that defines nm_lex()
 * for each client type.
 */
extern int gSpkExpLines;


/**
 * A global counter used by nm_lex() and nm_parse() to keep track of
 * the number of syntax and semantics errors detected during the process.
 *
 * nm_lex() and nm_parse() only increment the counter from the value
 * set prior to the call to nm_parse() which in turn calls nm_lex() repectively.
 * It will probably most make sense if the caller of nm_parse()
 * initialize this counter just before a call to nm_parse()
 *
 * @note This counter is acutally declared in a file that defines nm_parse()
 * for each client type.
 */
extern int gSpkExpErrors;

/**
 * A global pointer to the symbol table used by nm_parse().
 *
 * nm_parse() inserts new user-defined variables discovered 
 * in a given set of expressions that are not found in the
 * table prior to the call.  In other words, nm_parse()
 * does not initialize or allocate memory and
 * only adds new variables.  nm_parse() also does not 
 * disturb the information about the variables already
 * found in the table prior to the call.
 *
 * @note This pointer is actually declared in a file that defines nm_parse()
 * for each client type.
 */
extern SymbolTable *gSpkExpSymbolTable;

/**
 * A global pointer to a DOM based parse tree used by nm_parse().
 *
 * nm_parse() builds a DOM based parse tree from a set of expressions
 * passed through nm_lex().  nm_parse() assumes this pointer points to an
 * already allocated area of memory for the tree.
 * In other words, nm_parse() does not do memory management
 * and only inserts new nodes below the
 * root node pointed by the pointer set prior to the call.
 *
 * @note This pointer is acutally declared in a file that defines 
 * a nm_parse() for a target client type.
 */
//extern xercesc::DOMDocument *gSpkExpTree;

/**
 * Global pointer to a FILE handler pointing to an open writable
 * file to which the results of nm_parse() is redirected.
 */
extern FILE * gSpkExpOutput;

/**
 * This is the top level class in the SPK Compiler package
 * that SPK Compiler Driver will instanciate.
 *
 * The object of this class determines the client
 * which created the input SpkML.xml document and
 * plugs in the appropriate SpkML->C++ translator.
 * The caller will execute the translator() method
 * and, upon the successful completion, use getXXX() 
 * methods to obtain the names of genrated C++ source files 
 * and the ponters to data structures which contain
 * values used to generated the C++ source files.
 *  
 */
class SpkMLToCpp
{
 public:
  /**
   * Initializes DOM and generates a DOM parse tree
   * from the SpkInML document.  The protected class member,
   * tree, will point to the parse tree upon the
   * sucessful completion of parsing process.
   *
   * @param SpkInML is a file path to the SpkML xml document
   * to be translated into C++ code.  The grammer for
   * an SpkML document is defined per client.
   * 
   */
  SpkMLToCpp( const char* SpkInML );
  ~SpkMLToCpp();

  /**
   * Translates the information stored in the DOM
   * parse tree pointed by a protected class member, tree, 
   * generated from the SpkInML document
   * to C++ source code and store the results into
   * files.
   * 
   * During the process, it gathers information 
   * from the input, which may or may not be expressed
   * in the client specific language/terms
   * and converts them so that SPK can understand. 
   * The raw information --- expressed in the
   * client-specific terms/language --- are recorded into
   * ClientParameters and their corresponding pieces of
   * information expressed in SPK terms/language are
   * recorded into SpkParameters. 
   * 
   * Upon the successful completion, a number of C++ source
   * code files will be generated.  One of them is a driver
   * that executes fitPopulation() or fitIndividual(),
   * depending on the client's request.  The rest, as together,
   * defines the whole body or a part of a subclass of SpkModel 
   * class and a class that holds individiduals' data, IndData.
   * The names of these generatd files can be obtained
   * through getFilenameList().
   * 
   * If errors are detected during the translation process,
   * an SpkException object will be thrown.
   *
   * @return a pointer to the ClientTranslator object whose
   * "virtual void translate()" was used to perform this operation.
   */
  const ClientTranslator* translate() const;

  /**
   * Obtain the vector containg pointers to character arrays,
   * each containing a file name.  These files, as together,
   * defines the entire body or a part of a subclass of SpkModel class,
   * IndData class which holds individiduals' measurements and
   * the driver.
   *
   * @return a vector containing filenames, together define
   * a subclass of SpkModel class, a data structure which
   * will capture measurement data and a driver
   * if translate() has been
   * completed successfully prior to the call.  If not,
   * the values are undetermined.
   */
  const std::vector<std::string> getFilenameList() const;
  
 protected:

  SpkMLToCpp();
  SpkMLToCpp( const SpkMLToCpp& right );
  const SpkMLToCpp& operator=( const SpkMLToCpp& right );

 private:
  /**
   * Initialize DOM Utitilies.  
   *
   * Upon the successful completion, parser points to a legal
   * DOM parser.
   * 
   * @note DOM must be initialized successfully prior to the use of 
   * any DOM utilities/tools.
   */
  void                        initializeDOM     () const;

  /**
   * Terminate DOM Utilities.
   *
   * Upon the successful completion, the resources allocated for
   * DOM parser and utilities shall be released.
   */
  void                        terminateDOM      () const;

  /**
   * Discover the client whose key information is barried in the input SpkML document.
   *
   * Upon the successful completion, who, shall
   * be set to an enum value corresponding to the discovered client.
   */
  enum client::type           discoverClient    ( const xercesc::DOMDocument* tree ) const;

  /**
   * Build a DOM parse tree from the input SpkML xml document for further
   * analysis.
   * 
   * Upon the succesfful completion, tree points to a legal DOM parse tree generated
   * from parseing the input SpkML xml document.
   */
  xercesc::DOMDocument      * buildTreeFromSpkML( const char * inputSpkMLIn );

  /**
   * Create an instance of ClientTranslator, appropriate to the client type.
   * The client type must be discovered prior to the call.
   *
   * Upon the successfull completion, client_translator points to a legal
   * instance of ClientTranslator class, corresponding to the client.
   */
  ClientTranslator          * createTranslator  ( enum client::type ) const;

  /**
   * The pointer to a character array specifying the name of
   * input SpkML (xml) file.
   */
  const char                * inputSpkML;

  /**
   * The enumulator value indicating the type of client
   * that originated the input SpkML file.
   */
  enum client::type           who;

  /**
   * The pointer to a client specific translator, an instance
   * of ClientTranslator class.
   */
  ClientTranslator          * client_translator;

  /**
   * The pointer to a DOM parser which builds the expression
   * tree.
   */
  xercesc::XercesDOMParser  * parser;

  /**
   * The pointer to a DOM parse tree used to represent
   * the input SpkML document.
   */
  xercesc::DOMDocument      * tree;
};

#endif

