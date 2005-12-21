/**
 * @file NonmemTranslator.h
 * Declare NonmemTranslator class.
 */
#ifndef NONMEMTRANSLATOR_H
#define NONMEMTRANSLATOR_H

#include "../ClientTranslator.h"
#include "../SymbolTable.h"
#include "NonmemKeyword.h"
#include "CompModelInfo.h"
#include "XmlConstants.h"
#include <iostream>
#include <vector>

#include <xercesc/dom/DOMDocument.hpp>

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
 public:

  /**
   * XML tags and attribute names.
   */
  const struct XmlConstants XML;

  /**
   * NONMEM Keywords.
   */
  struct NonmemKeyword NMKey;
  /**
   * Model specification types.
   */
  enum MODEL_SPEC { PRED=0, 
		    ADVAN1=1, 
		    ADVAN2, 
		    ADVAN3,
		    ADVAN4, 
		    ADVAN5, 
		    ADVAN6,
		    ADVAN7, 
		    ADVAN8, 
		    ADVAN9, 
		    ADVAN10, 
		    ADVAN11, 
		    ADVAN12 };
  /**
   * Predefined variable sets for ADVAN.
   */
  enum TRANS { TRANS1=1, 
	       TRANS2, 
	       TRANS3, 
	       TRANS4, 
	       TRANS5, 
	       TRANS6 };

  /** Likelihood evaluation method. */
  enum INTEG_METHOD { ANALYTIC, GRID, PLAIN, MISER, VEGAS };


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
  * Returns a pointer handler to the CompModelInfo object that captures the information
  * necessary to define a compartmental model.
  */
 inline const CompModelInfo& getCompModel() const { return *myCompModel; };
 
 protected:
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

  /** The SPK optimization driver definition. */
  static const char * fFitDriver_cpp;

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

  /** Determine the location of the label found in the list.
   *
   * @return The index to the element in the label list when the label is found.  
   *         -1 if it fails to found.
   * @param labels The list of lables.
   * @param label The label to be found.
   */
  int whereis( xercesc::DOMNodeList* labels, const XMLCh* label ) const;
  //========================================

 private:

  /**
   * Insert the ID field in each record (subtree) if the data set lacks of it.
   * Returns the location in which the ID field can be found.
   */
  int insertID( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels );

  /**
   * Insert the MDV field in each record (subtree) if the data set lacks of it.
   * Returns the location in which the MDV field can be found.
   */
  int insertMDV( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels, int posAMT, int posEVID );

  /**
   * Insert the EVID field in each record (subtree) if the data set lacks of it.
   * Returns the location in which the EVID field can be found.
   * This routine assumes MDV is present or has been inserted by SPK Compiler in the data set.
   */
  int insertEVID( xercesc::DOMElement* dataset, xercesc::DOMNodeList* labels, int posAMT, int posMDV );

  /**
   * Insert the AMT field in each record (subtree) if the data set lacks of it.
   * @return the location in which the AMT field can be found.
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
   void parsePopAnalysis ( xercesc::DOMElement* pPopAnalysis );

  /**
   * Analyze the &lt;ind_analysis&gt; subtree.
   *
   * @param pIndAnalysis A pointer to the &lt;indAnalysis&gt; node.
   */
  void parseIndAnalysis ( xercesc::DOMElement* pIndAnalysis );

  /**
   * Analyzie the PRED specification.
   *
   * @param pPred A pointer to the &lt;pred&gt; node.
   */
  void parsePred( xercesc::DOMElement* pPred );

  /**
   * Analyze ADVAN 1 - 12 specification.
   *
   * @param advan A canned model
   * @param trans A translation method
   * @param model A pointer to &lt;model&gt; element.
   */
  void parseAdvan( enum MODEL_SPEC advan,
		   enum TRANS      trans,
		   const xercesc::DOMElement* model );
  /**
   * Analyze &lt;comp_model&gt; specification.
   * @return #of compartments.
   * @param comp_model A pointer to &lt;comp_model&gt; tree.
   * @param tolRel The relative toleranace as for the number of digits
   *               that are required to be accurate in the computation of 
   *               the drug amount in compartment.
   */
  int parseCompModel( const xercesc:: DOMElement* comp_model, double tolRel );

  /**
   * Analyze &lt;pk&gt; specification.
   *
   * @param pk A pointer to $PK specification.
   */
  void parsePK( const xercesc::DOMElement* pk );

  /**
   * Analyze &lt;diffeqn&gt; specification.
   *
   * @param diffeqn A pointer to &lt;diffeqn&gt; tree.
   */
  void parseDiffEqn( const xercesc::DOMElement* diffeqn );

  /**
   * Analyze &lt;error&gt; specification.
   *
   * @param error A pointer to &lt;error&gt; tree.
   */
  void parseError( const xercesc::DOMElement* error );

  /**
   * Analyze the &lt;monte_carlo&gt; subtree.
   *
   * @param pMonte A point to the &lt;monte_carlo&gt; node.
   */
  void parseMonte( xercesc::DOMElement* pMonte );
  
  /**
   * Generate C++ source code for declaring and defining IndData class which
   * is a C++ representation of a single patient records.
   */
  void generateIndData( ) const;

  /**
   * Generate C++ source code for declaring and defining DataSet class which
   * is a C++ representation of the set of patient records.
   */
  void generateDataSet( ) const;

  /**
   * Generate C++ source code for Pred class.
   */
  void generatePred( const char* predDefFilename ) const;

  /**
   * Generate C++ soruce code for OdePred class.
   */
  void generateOdePred( const char* fPkEqn_cpp, 
	                const char* fDiffEqn_cpp, 
			const char* fErrorEqn_cpp ) const;
  /**
   * Generate C++ source code for NonmemPars namespace.
   */
  void generateNonmemParsNamespace() const;

  /**
   * Generate C++ source code for MontePars namespace.
   */
  void generateMonteParsNamespace() const;

  /**
   * Generate C++ source code for the driver for population analysis.
   */
  void generatePopDriver( ) const;

  /**
   * Generate C++ source code for the driver for individual analysis.
   */
  void generateIndDriver( ) const;

  /**
   * Generate a Makefile that builds an executable called, driver, 
   * from all the generated files.
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

  /** The type of likehood method requested by the user.*/
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
  unsigned int      myOmegaDim;

  /** The order of OMEGA matrix. */
  unsigned int      myOmegaOrder;

  /** The matrix structure of OMEGA. */
  Symbol::Structure myOmegaStruct;

  /** The dimension of SIGMA matrix. */
  unsigned int      mySigmaDim;

  /** The order of SIGMA matrix. */
  int               mySigmaOrder;

  /** The matrix structure of SIGMA. */
  Symbol::Structure mySigmaStruct;

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
