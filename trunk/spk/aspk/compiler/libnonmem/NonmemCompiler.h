#ifndef NONMEMCOMPILER_H
#define NONMEMCOMPILER_H

#include <map>
#include "../libcommon/SpkCompiler.h"
#include "../libnonmem/explang.tab.h"

extern "C"{
  int yylex(void);  
  int yyparse(void);
};
extern int yydebug;
extern FILE *yyin;
  
extern int gSpkExpLines;


/**
 * A global counter to keep track of # of errors detected during a call to yyparse().
 *
 * This counter is declared and initialized in a file that specifies and defines yyparse().
 * The version of yyparse() for analyzing NONMEM abbriviated code
 * is specified in @c SpkCompier/NonmemAbbToC++/nmabb.y.
 *
 * \note The documentation for the yyparse specification found in @c nmabb.y 
 * is not visible because DOXYGEN does not support YACC.
 */
extern int gSpkExpErrors;

/**
 * A global pointer to the symbol table.
 *
 * This table is used to bookkeep symbols found in the NONMEM-like control files.
 * These symbols include both keywords (symbols predefined/reserved by the NMTRAN)
 * and arbitary symbols defined by the end user.
 */
extern SymbolTable *gSpkExpSymbolTable;

/**
 * A global pointer to a DOM document (tree).
 *
 * The DOMDocument object pointed by this pointer is created within the util (ExpTreeGenerator) object.
 * This is used to call tools like DOMDocument::createElement() to directly create DOM elements.
 *
 * \todo Perhaps this pointer should not be exposed or should be accessed always though the
 * global pointer (util) to the ExpTreeGenerator object.
 */
extern DOMDocument *gSpkExpTree;

/**
 * A global pointer to a ExpTreeGenerator object (providing a set of utilities re. DOM-based tree).
 *
 * The ExpTreeGenerator object pointed by this pointer is used to create and initialze DOM-based
 * parse tree built during the syntax recoginition process.  The class provides
 * means to properly allocate and release resources required to generate
 * tree and its components as well as other utilities to print out the tree contents
 * to a file or standard output and possibly more.
 */
extern ExpTreeGenerator *gSpkExpTreeGenerator;

class Column{
public:
  char* label;
  char* synonym;
  std::valarray<double> values;
public:
  Column() : values(3), label(0), synonym(0){}
  Column( int nMeasurements ) : values(nMeasurements), label(0), synonym(0)
  {
  }
};
class Data{
public:
  char* id;
  std::vector<Column> columns;
public:
  Data() : id(0){}
  Data( int nColumns ) : id(0), columns(nColumns)
  {
  }
  ~Data()
  {
  }
};

/**
 * Specialization of SpkCompiler for NONMEM.
 * 
 * This class specializes in translating SpkInML documents emitted
 * by NONMEM Model Design Agent.
 */
class NonmemCompiler : public SpkCompiler{
 public:
  NonmemCompiler( const char* SpkInML_filename );
  ~NonmemCompiler( );

  virtual void interpret();

  virtual const std::set<std::string> emit();

  virtual const std::set<std::string> getFileNames() const;

  virtual const std::string getDriverFileName() const;

  const std::valarray<bool>   getThetaFixed() const;
  const std::valarray<double> getThetaIn() const;
  const std::valarray<double> getThetaLow() const;
  const std::valarray<double> getThetaUp() const;

  const std::valarray<bool>   getOmegaFixed() const;
  const std::valarray<double> getOmegaIn() const;

  const std::valarray<bool>   getSigmaFixed() const;
  const std::valarray<double> getSigmaIn() const;

  const std::valarray<bool>   getEtaFixed() const;
  const std::valarray<double> getEtaIn() const;

  const char* whichCannedModel() const;

 protected:
  NonmemCompiler();
  NonmemCompiler( const NonmemCompiler & );
  NonmemCompiler& operator=( const NonmemCompiler & );

 private:
  /**
   * Interpret <content> section in the input SpkInML document.
   * 
   * <content> section contains general information regarding
   * versions, client, the level of estimation and so on.
   */
  void interpretContent();

  /**
   * Interpret <driver> section in the input SpkInML document.
   * 
   * <driver> section contains constraints to both population and
   * individual level optimizations.  There are also
   * user requests for statistics computations and results
   * delivery.
   *
   * Upon the completion of this function,
   * the mapping between NONMEM variables and SPK variables
   * shall be stored in ParameterMapping.
   * Such mapping includes THETA, OMEGA and SIGMA vs.
   * SPK's population parameter, alpha (or population parameter),
   * and ETA vs. b (or individual parameter).
   * 
   */
  void interpretDriver();
  const std::set<std::string> emitDriver();
  //std::set<std::string> driverFiles;


  /**
   * Interpret <data> section in the input SpkInML document.
   *
   * Extract data sets that constitute to "y" in SPK terminology
   * and place it in the "measurementsAll" vector in
   * the spkSymbols data structure inherited from the base
   * SpkCompiler class.
   * It also figures out the mapping between the identifiers
   * associated with individuals and the order of process 
   * and place it in the "order_id_mapping" map.
   * All data sets, y equivalent, time, dosages and etc., are
   * placed in the "allData" map, with the data set identifier
   * as their keys.
   */
  void interpretData();
  /*
  std::map< std::string, int > IDs;
  typedef std::map<const char*, std::valarray<double> > DataColumn;
  std::vector<DataColumn>  dataColumns;
  */
  std::vector<Data> data;

  /**
   * Interpret <model> section in the input SpkInML document
   * and, at the same time, emit C++ source code for
   * a subclass of SpkModel, namely UserModel.
   *
   * Interact with data structures that hold information
   * gathered though interpretData() and interpretDriver()
   * processes and assemble them into a subclass of
   * SpkModel.  It will use/call the expression translator
   * to interpret the expressions written in NONMEM Abbriviated
   * (FORTRAN like) language.
   */
  void interpretModel();
  const std::set<std::string> emitModel();
  //std::set<std::string> modelFiles;

 /**
  * Readonly character array keeping the name of will-be-generated driver file.
  */
  const std::string driverFileName;
  
 /**
  * A list of generated files including driver file.
  */
  std::set<std::string> fileNames;

  /**
   *
   */
  bool isCannedModelUsed;

  /**
   *
   */
  enum BaseModel { NONE, 
		   ADVAN1, ADVAN2, ADVAN3, ADVAN4, ADVAN5, 
		   ADVAN6, ADVAN7, ADVAN8, ADVAN9, ADVAN10, 
		   ADVAN11, ADVAN12 };
  enum BaseModel baseModel;
  enum BaseModel setCannedModel( const char* canned );

  /**
   * theta
   */
  std::valarray<bool>   thetaFixed;
  std::valarray<double> thetaIn;
  std::valarray<double> thetaLow;
  std::valarray<double> thetaUp;
  /**
   * Omega
   */
  std::valarray<bool>   omegaFixed;
  std::valarray<double> omegaIn;

  /**
   * Sigma
   */
  std::valarray<bool>   sigmaFixed;
  std::valarray<double> sigmaIn;

  /**
   * eta
   */
  std::valarray<bool>   etaFixed;
  std::valarray<double> etaIn;
};

#endif
