#ifndef SPKMLTOCPP_H
#define SPKMLTOCPP_H

#include <vector>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "SpkParameters.h"
#include "ClientTranslator.h"
#include "client.h"
#include "SymbolTable.h"
#include "ExpTreeGenerator.h"
#include "explang.h"

/**
 * @file SpkMLToCpp.h
 * 
 * This header declares the SpkMLToCpp (SpkML->C++ compiler) class and
 * extern-s the global variables that serve as the communication
 * channel between the SpkML->C++ compiler and 
 * the expression language parser (and lexical analyizer).
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
  int yylex(void);  
  
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
  int yyparse(void);
};
/**
 * A global flag used by yyparse() to indicate as to whether
 * yyparse() emits debugging information as it proceeds.
 *
 * @note YACC (BISON in Linux) declares this flag.
 */
extern int yydebug;

/**
 * A global pointer used by yylex() and yyparse().  The caller of yylex()
 * sets the pointer to a FILE handler for the input (expressions) file.
 *
 * @note LEX (FLEX in Linux) declares this pointer.
 */
extern FILE *yyin;
  
/**
 * A global counter used by yylex() to count the number of lines it
 * has read so far.
 *
 * 
 * yylex() only increments (hence, it does not initialize)
 * the counter from the value set prior to the call.
 * It will probably most make sense if the caller of yyparse()
 * initialize this counter just before a call to yyparse() which
 * in turn calls yylex() repetively until all contents in a file
 * are read.  One example of situations where the counter is
 * initialized to a value greater than zero is where
 * the file contains expressions extracted from another file
 * and the first expression appeared somewhere in the middle
 * of the original file.
 * 
 * @note This counter is actually declared in a file that defines yylex()
 * for each client type.
 */
extern int gSpkExpLines;


/**
 * A global counter used by yylex() and yyparse() to keep track of
 * the number of syntax and semantics errors detected during the process.
 *
 * yylex() and yyparse() only increment the counter from the value
 * set prior to the call to yyparse() which in turn calls yylex() repectively.
 * It will probably most make sense if the caller of yyparse()
 * initialize this counter just before a call to yyparse()
 *
 * @note This counter is acutally declared in a file that defines yyparse()
 * for each client type.
 */
extern int gSpkExpErrors;

/**
 * A global pointer to the symbol table used by yyparse().
 *
 * yyparse() inserts new user-defined variables discovered 
 * in a given set of expressions that are not found in the
 * table prior to the call.  In other words, yyparse()
 * does not initialize or allocate memory and
 * only adds new variables.  yyparse() also does not 
 * disturb the information about the variables already
 * found in the table prior to the call.
 *
 * @note This pointer is actually declared in a file that defines yyparse()
 * for each client type.
 */
extern SymbolTable *gSpkExpSymbolTable;

/**
 * A global pointer to a DOM based parse tree used by yyparse().
 *
 * yyparse() builds a DOM based parse tree from a set of expressions
 * passed through yylex().  yyparse() assumes this pointer points to an
 * already allocated area of memory for the tree.
 * In other words, yyparse() does not do memory management
 * and only inserts new nodes below the
 * root node pointed by the pointer set prior to the call.
 *
 * @note This pointer is acutally declared in a file that defines yyparse()
 * for each client type.
 */
extern xercesc::DOMDocument *gSpkExpTree;

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
   * The only legal constructor which takes a character
   * array containing the name of input SpkML xml document.
   *
   * During the initialization process, it converts the
   * document to a DOM parse tree and discover the client
   * from the econtents.
   */
  SpkMLToCpp( const char* inputSpkpMLIn );
  ~SpkMLToCpp();

  /**
   * Translates the input SpkML document (which may have been
   * converted to a DOM parse tree) to C++ code files.
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
   * depending on the cleint's request.  The rest, as together,
   * define a subclass of SpkModel class.
   * The names of these generatd files can be obtained
   * through getDriverDefFileName() and getModelDefFileNames().
   * 
   * If errors are detected during the translation process,
   * an SpkException object will be thrown.
   */
  void translate();

  /**
   * Obtain a pointer to an SpkParameters data structure 
   * object used to record values expressed in such a way
   * that SPK can understand.
   * 
   * @return a SpkParameter data structure object which
   * lists values gathered druing a translation process if
   * translate() has been completed successfully prior to
   * the call,  If not, the values are undetermined.
   */
  const struct SpkParameters * getSpkParameters() const;

  /**
   * Obtain a pointer to an 
   * object used to record values expressed in the client
   * specific language/terms.  For this reason, the data type
   * of the returned value is void*.  The caller has to know
   * which specific data type to cast into in order to
   * access the elements.
   *
   * @return a void pointer to an object which
   * lists values gathered during a translation process if
   * translate() has been completed successfully prior to
   * the call.  If not, the values are undetermined.
   */
  const void * getClientParameters() const;

  /**
   * Obtain a pointer to a character array specifying the name
   * of the generated SPK driver file.
   *
   * @return a pointer to a character array specifying the name
   * of the generated SPK driver file if translate() has been
   * completed successfully prior to the call.  If not,
   * the value is undertermined.
   */
  const char * getDriverFilename() const;

  /**
   * Obtain the vector containg pointers to character arrays,
   * each containing a file name.  These files, as together,
   * defines a subclass of SpkModel class.
   *
   * @return a vector containing filenames, together define
   * a subclass of SpkModel class if translate() has been
   * completed successfully prior to the call.  If not,
   * the values are undetermined.
   */
  const std::vector<const char *> getModelFilenameList() const;
  
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
   * Discover the client, which is specified in the input SpkML document.
   *
   * Upon the successful completion, who, shall
   * be set to an enum value corresponding to the discovered client.
   */
  enum client::type           discoverClient    ( const xercesc::DOMDocument* tree ) const;

  /**
   * Build a DOM parse tree from the input SpkML xml document.
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

