/**
 * @file NonmemTranslator.h
 * Declare NonmemTranslator class.
 */
#ifndef NONMEMTRANSLATOR_H
#define NONMEMTRANSLATOR_H

#include "../ClientTranslator.h"
#include "../SymbolTable.h"
#include "CompModelInfo.h"
#include "nonmem.h"
#include "explang.h"
#include "XmlConstants.h"
#include <iostream>
#include <vector>

#include <xercesc/dom/DOMDocument.hpp>

//========================================
// The global variables used by
// yyparse() and yylex() (or equivalent).
//----------------------------------------
/**
 * The global counter for errors encountered during the expression parsing.
 * 
 * The expression lexical analyzier (nm_lex()) and the parser (nm_parse())
 * increment this counter when they encounter syntax errors.
 * The counter is initialized to zero at the beginning of run-time process.
 * 
 * This global variable is defined in explang.ypp.
 */
extern "C" int           gSpkExpErrors;

/**
 * A global placeholder for syntax error messages.
 *
 * The expression lexcal analyizer (nm_lex()) and the parser (nm_parse())
 * store messages when they diagnose syntax errors.
 * This pointer is initialized to NULL at the beginning of run-time process
 * and it is the user's responsibly to allocate and deallocate 
 * the memory.
 *
 * This global variable is defined in explang.ypp. 
 */
extern "C" char*         gSpkExpErrorMessages;

/**
 * The global counter for lines that have been read so far during the expression parsing.
 * 
 * The expression lexical analyzier (nm_lex()) increments this counter.
 * This counter is initialized to zero at the beginning of run-time process.
 *
 * This global variable is defined in explang.ypp.
 */
extern "C" int           gSpkExpLines;

/**
 *
 * The global symbol table.
 *
 * This pointer is initialized to NULL at the beginning of 
 * run-time process and it is the user's responsibility
 * to allocate and deallocate the memory.
 *
 * This global variable is defined in explang.ypp.
 */
extern "C" SymbolTable * gSpkExpSymbolTable;

/**
 * Global pointer to a FILE handler to which output is redirected.
 *
 * The expression parser (nm_parse()) redirects its output text (C++ source code) 
 * to the file pointed by the handler.  The pointer is initilized to
 * NULL at the beginning of run-time process and it is the user's
 * responsibility to open and close the resource.
 *
 * This global variable is defined in explang.ypp.
 */
extern "C" FILE *        gSpkExpOutput;

/**
 * The global flag indicating as to whether a variable T appear in the right
 * hand side of an assignment statement.
 *
 * The expression parser (nm_parse()) sets this flag TRUE when
 * it finds a variable named "T" on the right hand side of an assignment
 * statement.
 *
 * This global variable is defined in explang.ypp.
 */
extern "C" bool          gSpkIsTInRhs;

/**
 * The file hander pointing to the input file to read in.
 *
 * The expression lexical analyzier (nm_lex()) reads characters
 * from the file pointed by this handler.
 *
 * This global variable is defined in explang.ypp.
 */
extern "C" FILE *        nm_in;

extern "C"{
  /**
   * The NONMEM expression parser.
   *
   * This module reads in expressions via the lexical analyizer (nm_lex()),
   * analyizes them and converts them to fit to C++ syntax.
   *
   * This function is defined in explang.ypp.
   */
  int nm_parse(void);

  /**
   * The error handler for the expression parser.
   *
   * This error handler puts the #message into #gSpkExpErrorMessages
   * and increments #gSpkExpErrors.
   *
   * This function is defined in explang.ypp.
   */
  int nm_error( const char* message );

  /**
   * @var nm_lex(void)
   *
   * The lexical analyzer for NONMEM expression.
   * The input string stream nm_lex() reads in is assumed to 
   * be pointed by a FILE handler, @a nm_in.
   *
   * @note The code for this function is generated from
   * a LEX (FLEX) specification file, lex_explang.l.
   */
};
/**
 * NonmemTranslator is an implementation of ClientTranslator abstract class.
 *
 * This class encapsulates features necessary for parsing, analysing
 * and translating a pair of an SpkSourceML and an SpkDataML documents
 * written from the view point of NONMEM users.  
 *
 * @note for developers: Prefix "my" indicates variables local to this class.
 * Prefix "our" indicates variables held by the super class.
 *
 * @note for developers: Variables prefixed by "my" or "our" are placeholders
 * for information gathered from user's input files.
 */
class NonmemTranslator : public ClientTranslator
{
 private:

  /**
   * A data structure bundling XML tags and attribute names used in the user input XML files.
   */
  const struct XmlConstants XML;

public:
  /**
   * Model specification types.  Only PRED and ADVAN6 are currently supported (as of December 23, 2005).
   */
  enum MODEL_SPEC { PRED=0,   /**< PRED: Analytical model */
		    ADVAN1=1, /**< ADVAN 1: One compartment linear model */
		    ADVAN2,   /**< ADVAN 2: One compartment linear model with first order absorpotion */
		    ADVAN3,   /**< ADVAN 3: Two compartment linear model */
		    ADVAN4,   /**< ADVAN 4: Two compartment linear model with first order absorption */
		    ADVAN5,   /**< ADVAN 5: General linear model */
		    ADVAN6,   /**< ADVAN 6: General nonlinear model */
		    ADVAN7,   /**< ADVAN 7: General linear model with real eigenvalues */
		    ADVAN8,   /**< ADVAN 8: General nonlinear model with stiff differential equations */
		    ADVAN9,   /**< ADVAN 9: General nonlinear model with equilibrium compartments */
		    ADVAN10,  /**< ADVAN 10: One compartment model with Michaelis-Menten elimination */
		    ADVAN11,  /**< ADVAN 11: Three compartment linear model */
		    ADVAN12   /**< ADVAN 12: Three compartment linear model with first order absorption */
                  };
  /**
   * PK parameter translation unit.
   */
  enum TRANS { TRANS1=1, 
	       TRANS2, 
	       TRANS3, 
	       TRANS4, 
	       TRANS5, 
	       TRANS6 };

  /**
   * Constructor.
   *
   * All strings of type XMLCh* or XMLString that will be used within the member
   * functions will be created (ie the resources are allocated dynamically)
   * here.
   *
   * @param sourceIn A pointer to the SpkSourceML document.
   * @param dataIn   A pointer to the SpkDataML document.  This is passed to the super class.
   */
  NonmemTranslator( xercesc::DOMDocument* sourceIn, xercesc::DOMDocument* dataIn );

  /**
   * Destructor.
   *
   * All strings of type XMLCh* or XMLString that were created in the construction
   * (ie the resources were allocated dynamically) are destroyed properly
   * here. 
   */
  ~NonmemTranslator();

  /**
   * The implementation of ClientTranslator::parseSource() 
   * particular to NONMEM-user-based inputs.
   *
   * Upon the successful completion, the following files will be generated
   * in the current working directory:
   * <dl> 
   *     <dt>driver.cpp</dt>
   *     <dd>A file that defines main().  It initializes the parameters and
   *         executes fitPopulation()/fitIndividual(), simulate() if requested,
   *         and popStatistics()/indStatistics() if requested.  Upon
   *         the completion of these function executions, it generates
   *         a result.xml that contains the computational results.
   *     </dd>
   *     <dt>DataSet.h</dt>
   *     <dd>A template class, DataSet, is declared and defined in this file.
   *         The object of this class represents the whole data set (from the data file).
   *     </dd>
   *     <dt>IndData.h</dt>
   *     <dd>A template class, IndData, is declared and defined in this file.
   *         The object of this class represents an individual's data set.
   *     </dd>
   *     <dt>Pred.h</dt>
   *     <dd>A template class, Pred, is declared and defined in this file.
   *         This class encapusulates the NONMEM PRED block.
   *     </dd>
   *     <dt>NonmemPars.h</dt>
   *     <dd>A namespace, NonmemPars, is declared in this file.
   *        The namespace defines the user given values such as
   *        the initial parameter estimates.
   *     </dd>
   *     <dt>Makefile</dt>
   *     <dd>A Makefile that builds an executable, driver, from the above files.
   *     </dd>
   *   </dl>
   */
  virtual void parseSource();

  /**
   * Parse the DOMDocument tree that represents
   * the SpkDataML document and register the foundings into the symbol table.
   *
   * Precondition: The symbol table has no *label* entries
   * (ie parseSource() shall not have been completed).
   *
   * Postcondition: Upon the successful completion, the symbol table will be populated
   * with the data labels and their corresponding data values.
   * The labels however are not associated with (possible) synonyms yet
   * at this point.
   * 
   */
  virtual void parseData();

 /**
  * Returns a pointer handler to the CompModelInfo object that has captured 
  * the information about a compartmental model.
  */
 inline const CompModelInfo& getCompModel() const { return *myCompModel; };
 
 private:
  /**
   * Determine the type of analysis, population or individual, and
   * the number of subjects.  The parent class variable, ourTarget 
   * is set to either enum values POP or IND indicating population
   * or individual analysis, respectively.  Another parent class
   * variable getPopSize() is set to the number of subjects.
   * The number of subjects is also returned as the function value.
   */
  virtual int detAnalysisType();

  /**
   * Determine the type of model specifications, PRED or ADVAN.
   */
  enum MODEL_SPEC detModelType();

 protected:

  /** (prohibited) Default constructor. */
  NonmemTranslator();

  /** (prohibited) Copy constructor. */
  NonmemTranslator( const NonmemTranslator & );

  /** (prohibited) Assignment constructor. */
  NonmemTranslator& operator=( const NonmemTranslator& );

 private:
  /** The filename for a Make that builds a runtime. */
  static const char * fMakefile;

  /** The header file name for the IndData template class. */
  static const char * fIndData_h;

  /** The header file name for the DataSet template class. */
  static const char * fDataSet_h;

  /**
   *  The name of file that contains fortran version of
   * the user defined $PRED.
   */
  static const char * fPredEqn_fortran;

  /** The name of file that contains C++ version of the user defined $PRED 
   * (equations only) model.
   */
  static const char * fPredEqn_cpp;
  
  /** The header file for the Pred template class. */
  static const char * fPred_h;

  /** The name of file that contains fortran version of
   * the user defined $DES.
   */
  static const char * fDiffEqn_fortran;

  /** The name of file that contains C++ version of the user defined $DES 
   * (equations only) model.
   */
  static const char * fDiffEqn_cpp;
  
  /** The header file for the OdePred template class. */
  static const char * fOdePred_h;

  /** The header file for the IdentPred template class. */
  static const char * fIdentPred_h;

  /** The name of file that contains fortran version of
   * the user defined $PK.
   */
  static const char * fPkEqn_fortran;

  /** The name of file that contains C++ version of the user defined $PK 
   * (equations only) model.
   */
  static const char * fPkEqn_cpp;

  /** The name of file that contains fortran version of
   * the user defined $ERROR.
   */
  static const char * fErrorEqn_fortran;

  /** The name of file that contains C++ version of the user defined $DES 
   * (equations only) model.
   */
  static const char * fErrorEqn_cpp;

  /** The NonmemPars namespace definition. */
  static const char * fNonmemPars_h;

  /** The MontePars namespace definition. */
  static const char * fMontePars_h;

  /** The halfCvec template function definition. */
  static const char * fHalfCvec_h;

  /** The SPK population level optimization driver definition. */
  static const char * fFitDriver_cpp;

  /** The SPK individual level optimization driver definition. */
  static const char * fIndDriver_cpp;

  /** The Monte Carlo driver definition. */
  static const char * fMonteDriver_cpp;

  /** A temporary file for storing (long) runtime error messages.  The
   * compiler use only the name of the file, which will be inserted
   * into the generated driver.cpp.
   */
  static const char * fSpkRuntimeLongError_tmp;

  /** The checkpoint file. */
  static const char * fCheckpoint_xml;

  /** The result XML. */
  static const char * fResult_xml;

  //=========================================================
  // constant strings used as <tag> names and values
  //---------------------------------------------------------

  /** Determine the location of the label in label_list.
   *
   * @return The index to the element in the label list when the label is found.  
   *         -1 if it fails to found.
   * @param label_list The list of lables.
   * @param label The label to be found.
   */
  int whereis( xercesc::DOMNodeList* label_list, const XMLCh* label ) const;
  //========================================

 private:

  /**
   * Insert the ID field into the data set if it were missing.
   * @return The location in which the ID field was inserted.
   * @param dataset The data set to which the ID field is inserted.
   * @param labels The corresponding label list to which the change should be reflected.
   */
  int insertID( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels );

  /**
   * Insert the MDV field into the data set if it were missing.
   * @note This routine assumes AMT and EVID fields are present in the data set.
   * @return The location in which the MDV field was inserted.
   * @param dataset The data set to which the MDV field is inserted.
   * @param labels The corresponding label list to which the change should be reflected.
   * @param posAMT The position of AMT field in the data set, counting from the left (>=0).
   * @param posEVID The position of EVID field in the data set, counting from the left (>=0).
   */
  int insertMDV( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels, int posAMT, int posEVID );

  /**
   * Insert the EVID field into the data set if it were missing.
   * @note This routine assumes AMT and MDV fields are present in the data set.
   * @return The location in which the EVID field was inserted.
   * @param dataset The data set to which the EVID field is inserted.
   * @param labels The corresponding label list to which the change should be reflected.
   * @param posAMT The position of AMT field in the data set, counting from the left (>=0).
   * @param posMDV The position of MDV field in the data set, counting from the left (>=0).
   */
  int insertEVID( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels, int posAMT, int posMDV );

  /**
   * Insert the AMT field into the data set if it were missing.
   * @return the location in which the AMT field was inserted.
   * @param dataset The data set to which the AMT field is inserted.
   * @param labels The corresponding label list to which the change should be reflected.
   */
  int insertAMT( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels );

  /**
   * Remove data columns labeled as "DROP" from the data set.  
   * @return The number of columns removed.
   * @param dataset The data set from which the DROP field is removed.
   * @param labels The corresponding label list to which the change should be reflected.
   */
  int removeDrop( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels );

  /**
   * Remove data columns labeled as "SKIP" from the data set.  
   * @return The number of columns removed.
   * @param dataset The data set from which the SKIP field is removed.
   * @param labels The corresponding label list to which the change should be reflected.
   */
  int removeSkip( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels );

  /**
   * Analyze the &lt;pop_analysis&gt; subtree.
   *
   * @param pPopAnalysis A pointer to the &lt;popAnalysis&gt; node.
   */
   void parsePopAnalysis ( const xercesc::DOMElement* pPopAnalysis );

  /**
   * Analyze the &lt;ind_analysis&gt; subtree.
   *
   * @param pIndAnalysis A pointer to the &lt;indAnalysis&gt; node.
   */
  void parseIndAnalysis ( const xercesc::DOMElement* pIndAnalysis );

  /**
   * Analyze the &lt;pred&gt; subtree.
   *
   * @param pPred A pointer to the &lt;pred&gt; node.
   */
  void parsePred( const xercesc::DOMElement* pPred );

  /**
   * Analyze the ADVAN 1 - 12 specification.
   *
   * @param advan A canned model
   * @param trans A translation method
   * @param model A pointer to &lt;model&gt; element.
   */
  void parseAdvan( enum MODEL_SPEC advan,
		   enum TRANS      trans,
		   const xercesc::DOMElement* model );
  /**
   * Analyze the &lt;comp_model&gt; subtree.
   * @return #of compartments.
   * @param comp_model A pointer to &lt;comp_model&gt; tree.
   * @param tolRel The relative toleranace as for the number of digits
   *               that are required to be accurate in the computation of 
   *               the drug amount in compartment.
   */
  int parseCompModel( const xercesc:: DOMElement* comp_model, double tolRel );

  /**
   * Analyze the &lt;pk&gt; subtree.
   *
   * @param pk A pointer to $PK specification.
   */
  void parsePK( const xercesc::DOMElement* pk );

  /**
   * Analyze the &lt;diffeqn&gt; substree.
   *
   * @param diffeqn A pointer to &lt;diffeqn&gt; tree.
   */
  void parseDiffEqn( const xercesc::DOMElement* diffeqn );

  /**
   * Analyze the &lt;error&gt; subtree.
   *
   * @param error A pointer to &lt;error&gt; tree.
   */
  void parseError( const xercesc::DOMElement* error );

  /**
   * Analyze the &lt;monte_carlo&gt; subtree.
   *
   * @param pMonte A point to the &lt;monte_carlo&gt; node.
   */
  void parseMonte( const xercesc::DOMElement* pMonte );
  
  /**
   * Generate IndData.h, defining IndData class.
   *
   * IndData class is a C++ representation of a subject's records.
   */
  void generateIndData( ) const;

  /**
   * Generate DataSet.h, defining DataSet class.
   * DataSet class is a C++ representation of the set of all subjects' 
   * data records.
   */
  void generateDataSet( ) const;

  /**
   * Generate Pred.h, defining Pred class.
   * 
   * @param fPredEqn_cpp The name of file containing the C++ translation of PRED equations.
   */
  void generatePred( const char* fPredEqn_cpp ) const;

  /**
   * Generate OdePred.h or IdentPred.h, defining OdePred or IdentPred
   * classes, respectively.
   *
   * @param fPkEqn_cpp The name of file containing the C++ translation of PK definition.
     @param fDiffEqn_cpp The name of file containing the C++ translation of DES definition.
   * @param fErrorEqn_cpp The name of file containing the C++ translation of ERROR definition.
   * @param isIdent If this is equal to true, then the generated file will be IdentPred.h.
   */
  void generateOdePred( const char* fPkEqn_cpp, 
	                const char* fDiffEqn_cpp, 
			const char* fErrorEqn_cpp,
			bool        isIdent ) const;
  /**
   * Generate NonmemPars.h for NonmemPars namespace.
   * NonmemPars namespace declares parameters given by the NONMEM-savvy user.
   */
  void generateNonmemParsNamespace() const;

  /**
   * Generate MontePars.h for MontePars namespace.
   * MontePars namespace declares parameters needed by Likelihood problems.
   */
  void generateMonteParsNamespace() const;

  /**
   * Generate fitDriver.cpp for population analysis.
   */
  void generatePopDriver( ) const;

  /**
   * Generate fitDriver.cpp for individual analysis.
   */
  void generateIndDriver( ) const;

  /**
   * Generate fitDriver.cpp for individual identifiability.
   */
  void generateIdentDriver( ) const;

  /**
   * Generate Makefile.SPK, a makefile that builds a job driver.
   */
  void generateMakefile() const;

private:
  /**
   * Pointer to the symbol tabel held in the super class
   */
  SymbolTable * table;

  //
  // Place holders to hold values/info gathered through parsings.
  // The reason why this is just locally defined, as opposed to
  // defined as a NonmemTranslator class member, is 
  // to hide the existence since it's just an object of convenience.
  //
  /** The type of model requested by the user. */
  enum MODEL_SPEC   myModelSpec;

  /** The type of likelihood method requested by the user.*/
  enum INTEG_METHOD myIntegMethod;
 
  /** The type of TRANS requested by the user. */
  enum TRANS        myTrans;
 
  /** A description of the job written by the user. */
  char             *myDescription;

  /** The type of parameter estimation method requested by the user. */
  bool              myIsEstimate;

  /** True if the user requested data simulation. */
  bool              myIsSimulate;

  /** True if the user requested calculating statistics at the end of parameter estimation. */
  bool              myIsStat; 

  /** True if the user requested individual identifiability. */
  bool              myIsIdent;

  /** True if the user requested the nonparametric method. */
  bool              myIsNonparam;

  /** Number of initial measure points per grid side for the nonparametric method. */
  unsigned int      myNonparamGridMeasurePointPerSideIn;

  /** Number of initial random measure points for the nonparametric method. */
  unsigned int      myNonparamRandomMeasurePointIn;

  /** True if the user requested the likelihood estimation. */
  bool              myIsMonte;

  /** True if the user requested writing the individual level checkpoint file. */
  bool              myIndWriteCheckpoint;

  /** True if the user requested writing the population level checkpoint file. */ 
  bool              myPopWriteCheckpoint;

  /** True if the CMT data item was found missing from the data set. */
  bool              myIsMissingCmt;  // for ADVANs

  /** True if the PCMT data item was found missing from the data set. */
  bool              myIsMissingPcmt; // for ADVANs

  /** True if the RATE data item was found missing from the data set. */
  bool              myIsMissingRate; // for ADVANs

  /** The number of SUBPROBLEM's. */
  unsigned int      mySubproblemsN; 

  /** True if POSTHOC is requested. */  
  bool              myIsPosthoc;
  
  /** True if Warm-start is requested. */
  bool              myIsRestart;

  /** The length of THETA vector. */
  unsigned int      myThetaLen;

  /** The dimension of OMEGA square matrix. */
  std::valarray<unsigned int>      myOmegaDim;

  /** The order of OMEGA matrix. */
  std::valarray<unsigned int>      myOmegaOrder;

  /** The matrix structure of OMEGA. */
  std::valarray<Symbol::Structure> myOmegaStruct;

  /** True if the OMEGA block is constrained eqaul to the previous block */
  std::valarray<bool>              myOmegaSameAsPrev;

  /** The dimension of SIGMA matrix. */
  std::valarray<unsigned int>      mySigmaDim;

  /** The order of SIGMA matrix. */
  std::valarray<unsigned int>      mySigmaOrder;

  /** The matrix structure of SIGMA. */
  std::valarray<Symbol::Structure> mySigmaStruct;

  /** True if the SIGMA block is constrained eqaul to the previous block */
  std::valarray<bool>              mySigmaSameAsPrev;

   /** The length of ETA vector. */
  int               myEtaLen;

  /** The length of EPS vector. */
  int               myEpsLen;

  /** A list of #of integration evaluations (for likelihood problems only). */
  std::vector<unsigned int> myIntegNumberEvals;

  /** The number of integration evalutions (for likelihood problems only). */
  unsigned int      myIntegNEvals;

  /** The number of significant digits. */
  unsigned int      mySigDigits;

  /** The maximum number of iterations at the population level optimization.*/
  unsigned int      myPopMitr;

  /** The maximum number of iterations at the individual level optimziation. */
  unsigned int      myIndMitr;

  /** The population level convergence criteria. */
  double            myPopEpsilon;

  /** The individual level convergence criteria. */
  double            myIndEpsilon;

  /** The population level trace level. */
  int               myPopTraceLevel;

  /** The individual level trace level. */
  int               myIndTraceLevel;

  /** The seed value used for data simulation (if requested). */
  unsigned int      mySeed;

  /** The form of covariance used to compute statistics. */
  std::string       myCovForm;

  /** True if the user requested the computation of standard error. */
  bool              myIsStderr;

  /** True if the user requested the computation of correlation matrix. */
  bool              myIsCorrelation;

  /** True if the user requested the computation of covariance. */
  bool              myIsCov;

  /** True if the user requested the computation of the inverse of the covariance. */
  bool              myIsInvCov;

  /** True if the user requested the computation of confidence interval. */
  bool              myIsConfidence;

  /** True if the user requested the computation of coefficients of correlation. */
  bool              myIsCoefficient;

  /** The numbers of data records.  myRecordNums[i] is the number of i-th individual's data records.*/
  std::valarray<int> myRecordNums;

  /** The data structure to hold information about a compartmental model.  NULL if no Compartmental model is used.*/
  CompModelInfo    *myCompModel;
};

#endif
