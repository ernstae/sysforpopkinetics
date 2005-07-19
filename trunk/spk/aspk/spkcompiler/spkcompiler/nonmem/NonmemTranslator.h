/**
 * @file NonmemTranslator.h
 * Declare NonmemTranslator class.
 */
#ifndef NONMEMTRANSLATOR_H
#define NONMEMTRANSLATOR_H

#include "../ClientTranslator.h"
#include "../SymbolTable.h"

#include <iostream>
#include <vector>

#include <xercesc/dom/DOMDocument.hpp>

/**
 * NonmemTranslator is an implementation of ClientTranslator abstract class.
 *
 * This class encapsulates functionarities necessary for parsing, analysing
 * and translating a pair of an SpkSourceML and an SpkDataML documents
 * written from the view point of NONMEM users.
 */
class NonmemTranslator : public ClientTranslator
{
 public:

  /**
   * Model specification types
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
   * Predefined variable sets
   */
  enum TRANS { TRANS1=1, 
	       TRANS2, 
	       TRANS3, 
	       TRANS4, 
	       TRANS5, 
	       TRANS6 };

  enum INTEG_METHOD { ANALYTIC, GRID, PLAIN, MISER };

  /**
   * Predefined words
   */
  struct NonmemKeyword{
    std::string THETA;
    std::string ETA;
    std::string EPS;
    std::string OMEGA;
    std::string SIGMA;
    std::string PRED;
    std::string RES;
    std::string WRES;
    std::string ETARES;
    std::string WETARES;
    std::string IPRED;
    std::string IRES;
    std::string IWRES;
    std::string IETARES;
    std::string IWETARES;
    std::string PPRED;
    std::string PRES;
    std::string PWRES;
    std::string PETARES;
    std::string PWETARES;
    std::string CPRED;
    std::string CRES;
    std::string CWRES;
    std::string CETARES;
    std::string CWETARES;
    std::string DV;
    std::string ORGDV;
    std::string MDV;
    std::string ID;
    std::string F;
    std::string Y;

    std::string T;
    std::string P;
    std::string A;
    std::string DADT;
  };

  /**
   * These are the default string values for NONMEM-predefined
   * variables.  The default values will be used when these variables do
   * not appear in either <label> or <model> in the sourceML document.
   */
  NonmemKeyword DefaultStr;

  /**
   * These are used as insensitive search keys to find the values of
   * NONMEM-predefined variables in the symbol table or to be extracted
   * as C++ variable names when cases are supposed to be insensitive.
   */
  NonmemKeyword KeyStr;

  /**
   * These will hold the actual (NONMEM-predefined) variable names that
   * may be composed of arbitrary mixture of letters in upper/lower
   * cases.  These values would be determined from the <label> values or
   * variable names used within <model> definition in the sourceML
   * document.
   */
  NonmemKeyword UserStr;

  /**
   * The only legal constructor.
   *
   * All strings of type XMLCh* or XMLString that will be used within the member
   * functions will be created (ie. the resources are allocated dynamically)
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
   * (ie. the resources were allocated dynamically) are destroyed properly
   * here. 
   */
  ~NonmemTranslator();

  /**
   * The implementation of ClientTranslator::parseSource() 
   * particular to NONMEM-user-based inputs.
   *
   * Upon the successful completion, the following files will be generated
   * in the current working directory:
   * 
   * @htmlonly
   *   <dl>
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
   * @endhtmlonly
   */
  virtual void parseSource();

 protected:
  /**
   * Determine the type of analysis, population or individual, and
   * the number of subjects.  The parent class variable, ourTarget 
   * is set to either enum values POP or IND indicating population
   * or individual analysis, respectively.  Another parent class
   * variable ourPopSize is set to the number of subjects.
   * The number of subjects is also returned as the function value.
   */
  virtual int detAnalysisType();

 protected:
  NonmemTranslator();
  NonmemTranslator( const NonmemTranslator & );
  NonmemTranslator& operator=( const NonmemTranslator& );
  

 protected:
  // The filename for a Make that builds a runtime.
  static const char * fMakefile;

  // The header file name for the IndData template class.
  static const char * fIndData_h;

  // The header file name for the DataSet template class.
  static const char * fDataSet_h;

  // The name of file that contains fortran (ie. NONMEM TRAN) version of
  // the user defined $PRED.
  static const char * fPredEqn_fortran;

  // The name of file that contains C++ version of the user defined $PRED 
  // (equations only) model.
  static const char * fPredEqn_cpp;
  
  // The header file for the Pred template class.
  static const char * fPred_h;

  // The name of file that contains fortran (ie. NONMEM TRAN) version of
  // the user defined $DES.
  static const char * fDiffEqn_fortran;

  // The name of file that contains C++ version of the user defined $DES 
  // (equations only) model.
  static const char * fDiffEqn_cpp;
  
  // The header file for the ODEPred template class.
  static const char * fODEPred_h;

  // The name of file that contains fortran (ie. NONMEM TRAN) version of
  // the user defined $PK.
  static const char * fPkEqn_fortran;

  // The name of file that contains C++ version of the user defined $PK 
  // (equations only) model.
  static const char * fPkEqn_cpp;

  // The name of file that contains fortran (ie. NONMEM TRAN) version of
  // the user defined $ERROR.
  static const char * fErrorEqn_fortran;

  // The name of file that contains C++ version of the user defined $DES 
  // (equations only) model.
  static const char * fErrorEqn_cpp;

  // The NonmemPars namespace definition.
  static const char * fNonmemPars_h;

  // The MontePars namespace definition.
  static const char * fMontePars_h;

  // The halfCvec template function definition.
  static const char * fHalfCvec_h;

  // The SPK optimization driver definition.
  static const char * fFitDriver_cpp;

  // The Monte Carlo driver definition.
  static const char * fMonteDriver_cpp;

  // A temporary file for storing (long) runtime error messages.  The
  // compiler use only the name of the file, which will be inserted
  // into the generated driver.cpp.
  static const char * fSpkRuntimeLongError_tmp;

  // The checkpoint file.
  static const char * fCheckpoint_xml;

  // The result XML
  static const char * fResult_xml;

 protected:
  //=========================================================
  // constant strings used as <tag> names and values
  //---------------------------------------------------------
  // SpkSourceML attributes  
  XMLCh* X_DESCRIPTION;                static const char* C_DESCRIPTION;
  XMLCh* X_YES;                        static const char* C_YES;             
  XMLCh* X_NO;                         static const char* C_NO;
  XMLCh* X_FIXED;                      static const char* C_FIXED;
  XMLCh* X_IN;                         static const char* C_IN;
  XMLCh* X_LOW;                        static const char* C_LOW;
  XMLCh* X_UP;                         static const char* C_UP;
  XMLCh* X_DIAGONAL;                   static const char* C_DIAGONAL;
  XMLCh* X_BLOCK;                      static const char* C_BLOCK;
  XMLCh* X_VALUE;                      static const char* C_VALUE;
  XMLCh* X_STRUCT;                     static const char* C_STRUCT;
  XMLCh* X_DIMENSION;                  static const char* C_DIMENSION;
  XMLCh* X_LABEL;                      static const char* C_LABEL;
  XMLCh* X_LABELS;                     static const char* C_LABELS;
  XMLCh* X_COV_R;                      static const char* C_COV_R;
  XMLCh* X_COV_RSR;                    static const char* C_COV_RSR;
  XMLCh* X_COV_S;                      static const char* C_COV_S;
  XMLCh* X_COV_H;                      static const char* C_COV_H;
  XMLCh* X_COV_HSH;                    static const char* C_COV_HSH;
  XMLCh* X_IS_ERR_OUT;                 static const char* C_IS_STDERROR_OUT;
  XMLCh* X_IS_CORR_OUT;                static const char* C_IS_CORRELATION_OUT;
  XMLCh* X_IS_COV_OUT;                 static const char* C_IS_COVARIANCE_OUT;
  XMLCh* X_IS_INV_COV_OUT;             static const char* C_IS_INVERSE_COVARIANCE_OUT;
  XMLCh* X_IS_COEF_OUT;                static const char* C_IS_COEFFICIENT_OUT;
  XMLCh* X_IS_CONF_OUT;                static const char* C_IS_CONFIDENCE_OUT;
  
  // SpkSourceML tags
  XMLCh* X_NONMEM;                     static const char* C_NONMEM;
  XMLCh* X_POP_ANALYSIS;               static const char* C_POP_ANALYSIS;
  XMLCh* X_IND_ANALYSIS;               static const char* C_IND_ANALYSIS;
  XMLCh* X_CONSTRAINT;                 static const char* C_CONSTRAINT;
  XMLCh* X_MODEL;                      static const char* C_MODEL;
  XMLCh* X_ADVAN;                      static const char* C_ADVAN;
  XMLCh* X_TRANS;                      static const char* C_TRANS;
  XMLCh* X_PRED;                       static const char* C_PRED;
  XMLCh* X_COMP_MODEL;                 static const char* C_COMP_MODEL;
  XMLCh* X_DIFFEQN;                    static const char* C_DIFFEQN;
  XMLCh* X_PK;                         static const char* C_PK;
  XMLCh* X_ERROR;                      static const char* C_ERROR;
  XMLCh* X_MONTE_CARLO;                static const char* C_MONTE_CARLO;
  XMLCh* X_PRESENTATION;               static const char* C_PRESENTATION;
  XMLCh* X_TABLE;                      static const char* C_TABLE;
  XMLCh* X_SCATTERPLOT;                static const char* C_SCATTERPLOT;
  XMLCh* X_COLUMN;                     static const char* C_COLUMN;
  XMLCh* X_X;                          static const char* C_X;
  XMLCh* X_Y;                          static const char* C_Y;
  XMLCh* X_SPLIT;                      static const char* C_SPLIT;
  XMLCh* X_APPROXIMATION;              static const char* C_APPROXIMATION;
  XMLCh* X_FO;                         static const char* C_FO;
  XMLCh* X_FOCE;                       static const char* C_FOCE;
  XMLCh* X_LAPLACE;                    static const char* C_LAPLACE;
  XMLCh* X_METHOD;                     static const char* C_METHOD;
  XMLCh* X_ANALYTIC;                   static const char* C_ANALYTIC;
  XMLCh* X_GRID;                       static const char* C_GRID;
  XMLCh* X_MISER;                      static const char* C_MISER;
  XMLCh* X_PLAIN;                      static const char* C_PLAIN;
  XMLCh* X_NUMBEREVAL;                 static const char* C_NUMBEREVAL;
  XMLCh* X_POP_SIZE;                   static const char* C_POP_SIZE;
  XMLCh* X_IS_ESTIMATION;              static const char* C_IS_ESTIMATION;
  XMLCh* X_IS_ETA_OUT;                 static const char* C_IS_ETA_OUT;
  XMLCh* X_IS_RESTART;                 static const char* C_IS_RESTART;
  XMLCh* X_DATA_LABELS;                static const char* C_DATA_LABELS;
  XMLCh* X_FILENAME;                   static const char* C_FILENAME;
  XMLCh* X_NAME;                       static const char* C_NAME;
  XMLCh* X_SYNONYM;                    static const char* C_SYNONYM;
  XMLCh* X_THETA;                      static const char* C_THETA;
  XMLCh* X_LENGTH;                     static const char* C_LENGTH;
  XMLCh* X_OMEGA;                      static const char* C_OMEGA;
  XMLCh* X_SIGMA;                      static const char* C_SIGMA;
  XMLCh* X_SIMULATION;                 static const char* C_SIMULATION;
  XMLCh* X_SEED;                       static const char* C_SEED;
  XMLCh* X_POP_STAT;                   static const char* C_POP_STAT;
  XMLCh* X_COVARIANCE_FORM;            static const char* C_COVARIANCE_FORM;
  XMLCh* X_MITR;                       static const char* C_MITR;
  XMLCh* X_IND_STAT;                   static const char* C_IND_STAT;
  XMLCh* X_SIG_DIGITS;                 static const char* C_SIG_DIGITS;
  XMLCh* X_SUBPROBLEMS;                static const char* C_SUBPROBLEMS;

  // SpkReportML attributes
  XMLCh* X_ELAPSEDTIME;                static const char* C_ELAPSEDTIME;
  XMLCh* X_NEMBER_EVAL;                static const char* C_NUMBER_EVAL;
  XMLCh* X_SUBPROBLEM;                 static const char* C_SUBPROBLEM;
 
  // SpkReportML tags
  XMLCh* X_SPKREPORT;                  static const char* C_SPKREPORT;
  XMLCh* X_ERROR_LIST;                 static const char* C_ERROR_LIST;
  XMLCh* X_WARNING_LIST;               static const char* C_WARNING_LIST;
  XMLCh* X_PRESENTATION_DATA;          static const char* C_PRESENTATION_DATA;
  XMLCh* X_POP_ANALYSIS_RESULT;        static const char* C_POP_ANALYSIS_RESULT;
  XMLCh* X_IND_ANALYSIS_RESULT;        static const char* C_IND_ANALYSIS_RESULT;
  XMLCh* X_OPT_TRACE_OUT;              static const char* C_OPT_TRACE_OUT;
  XMLCh* X_POP_MONTE_RESULT;           static const char* C_POP_MONTE_RESULT;
  XMLCh* X_MESSAGE;                    static const char* C_MESSAGE;
  XMLCh* X_FILE_NAME;                  static const char* C_FILE_NAME;
  XMLCh* X_LINE_NUMBER;                static const char* C_LINE_NUMBER;
  XMLCh* X_WARNING;                    static const char* C_WARNING;
  XMLCh* X_POP_OPT_RESULT;             static const char* C_POP_OPT_RESULT;
  XMLCh* X_IND_OPT_RESULT;             static const char* C_IND_OPT_RESULT;
  XMLCh* X_POP_STAT_RESULT;            static const char* C_POP_STAT_RESULT;
  XMLCh* X_IND_STAT_RESULT;            static const char* C_IND_STAT_RESULT;
  XMLCh* X_POP_OBJ_OUT;                static const char* C_POP_OBJ_OUT;
  XMLCh* X_IND_OBJ_OUT;                static const char* C_IND_OBJ_OUT;
  XMLCh* X_THETA_IN;                   static const char* C_THETA_IN;
  XMLCh* X_THETA_OUT;                  static const char* C_THETA_OUT;
  XMLCh* X_OMEGA_IN;                   static const char* C_OMEGA_IN;
  XMLCh* X_OMEGA_OUT;                  static const char* C_OMEGA_OUT;
  XMLCh* X_SIGMA_IN;                   static const char* C_SIGMA_IN;
  XMLCh* X_SIGMA_OUT;                  static const char* C_SIGMA_OUT;
  XMLCh* X_POP_STDERROR_OUT;           static const char* C_POP_STDERROR_OUT;
  XMLCh* X_POP_COVARIANCE_OUT;         static const char* C_POP_COVARIANCE_OUT;
  XMLCh* X_POP_INVERSE_COVARIANCE_OUT; static const char* C_POP_INVERSE_COVARIANCE_OUT;
  XMLCh* X_POP_CORRELATION_OUT;        static const char* C_POP_CORRELATION_OUT;
  XMLCh* X_POP_COEFFICIENT_OUT;        static const char* C_POP_COEFFICIENT_OUT;
  XMLCh* X_POP_CONFIDENCE_OUT;         static const char* C_POP_CONFIDENCE_OUT;
  XMLCh* X_IND_STDERROR_OUT;           static const char* C_IND_STDERROR_OUT;
  XMLCh* X_IND_COVARIANCE_OUT;         static const char* C_IND_COVARIANCE_OUT;
  XMLCh* X_IND_INVERSE_COVARIANCE_OUT; static const char* C_IND_INVERSE_COVARIANCE_OUT;
  XMLCh* X_IND_CORRELATION_OUT;        static const char* C_IND_CORRELATION_OUT;
  XMLCh* X_IND_COEFFICIENT_OUT;        static const char* C_IND_COEFFICIENT_OUT;
  XMLCh* X_IND_CONFIDENCE_OUT;         static const char* C_IND_CONFIDENCE_OUT;

  //========================================

 private:
  //
  // Analyze the <pop_analysis> subtree.
  //
  // @param pPopAnalysis A pointer to the <popAnalysis> node.
  //
   void parsePopAnalysis ( xercesc::DOMElement* pPopAnalysis );

  //
  // Analyze the <ind_analysis> subtree.
  //
  // @param pIndAnalysis A pointer to the <indAnalysis> node.
  //
  void parseIndAnalysis ( xercesc::DOMElement* pIndAnalysis );

  //
  // Analyzie the PRED specification.
  //
  // @param pPred A pointer to the <pred> node.
  //
  void parsePred( xercesc::DOMElement* pPred );

  //
  // Analyze ADVAN 1 - 12.
  //
  // @param advan A canned model
  // @param trans A translation method
  // @param model A pointer to <model> element.
  //
  void parseAdvan( enum MODEL_SPEC advan,
		   enum TRANS      trans,
		   const xercesc::DOMElement* model );
  //
  // Specialized to analyze ADVAN 6 - nonlinear ODE
  //
  // @param trans A translation method
  // @param comp_model The compartmental model definition - equivalent of $MODEL
  // @param pk The pharmacokinetic parameters definition - equivalent of $PK
  // @param diffeqn The system of differential equations - equivalent of $DES
  // @param error The error model definition - equivalent of $ERROR
  //
  void parseAdvan6( enum TRANS trans,
		    const xercesc::DOMElement* comp_model,
		    const xercesc::DOMElement* pk,
		    const xercesc::DOMElement* diffeqn,
		    const xercesc::DOMElement* error );

  //
  // Analyze <comp_model>, i.e. $MODEL
  //
  // @param comp_model A pointer to <comp_model> tree.
  //
  void parseCompModel( const xercesc:: DOMElement* comp_model );

  //
  // Analyze <pk>, i.e. $PK.
  //
  // @param A pointer to a <PK> tree.
  //
  void parsePK( const xercesc::DOMElement* pk );

  //
  // Analyze <diffeqn>, i.e. $DES
  //
  // @param diffeqn A pointer to <diffeqn> tree.
  //
  void parseDiffEqn( const xercesc::DOMElement* diffeqn );

  //
  // Analyze <error>, i.e. $ERROR
  //
  // @param error A pointer to <error> tree.
  //
  void parseError( const xercesc::DOMElement* error );

  //
  // Analyze the <monte_carlo> subtree.
  //
  // @param pMonte A point to the <monte_carlo> node.
  //
  void parseMonte( xercesc::DOMElement* pMonte );
  
  //
  // Generate C++ source code for declaring and defining IndData class which
  // is a C++ representation of a single patient records.
  //
  void generateIndData( ) const;

  //
  // Generate C++ source code for declaring and defining DataSet class which
  // is a C++ representation of the set of patient records.
  //
  void generateDataSet( ) const;


  //
  // Generate C++ source code for Pred class.
  //
  void generatePred( const char* predDefFilename ) const;

  //
  // Generate C++ soruce code for ODEPred class.
  //
  void generateODEPred( const char* fPkEqn_cpp, 
	                const char* fDiffEqn_cpp, 
			const char* fErrorEqn_cpp ) const;
  //
  // Generate C++ source code for NonmemPars namespace.
  //
  void generateNonmemParsNamespace() const;

  //
  // Generate C++ source code for MontePars namespace.
  //
  void generateMonteParsNamespace() const;

  //
  // Generate C++ source code for the driver for population analysis.
  //
  void generatePopDriver( ) const;

  //
  // Generate C++ source code for the driver for individual analysis.
  //
  void generateIndDriver( ) const;

  //
  // Count the number of occurences of the text on the LHS of equations.
  //
  int countStrInLhs( const char * str,  const char * para );

  //
  // Generate a Makefile that builds an executable called, driver, 
  // from all the generated files.
  //
  void generateMakefile() const;

private:
  //
  // Pointer to the central symbol tabel held in the super class
  //
  SymbolTable * table;

  //
  // Place holders to hold values/info gathered through parsings.
  // The reason why this is just locally defined, as opposed to
  // defined as a NonmemTranslator class member, is 
  // to hide the existence since it's just an object of convenience.
  //
  enum MODEL_SPEC   myModelSpec;
  enum INTEG_METHOD myIntegMethod;
  enum TRANS        myTrans;

  char             *myDescription;
  bool              myIsEstimate;
  bool              myIsSimulate;
  bool              myIsStat; 
  bool              myIsMonte;
  bool              myIndWriteCheckpoint;
  bool              myPopWriteCheckpoint;

  unsigned int      mySubproblemsN;   
  bool              myIsPosthoc;
  bool              myIsRestart;
  unsigned int      myThetaLen;
  unsigned int      myOmegaDim;
  unsigned int      myOmegaOrder;
  Symbol::Structure myOmegaStruct;
  unsigned int      mySigmaDim;
  int               mySigmaOrder;
  Symbol::Structure mySigmaStruct;
  int               myEtaLen;
  int               myEpsLen;
  std::vector<unsigned int> myIntegNumberEvals;
  unsigned int      myIntegNEvals;
  unsigned int      mySigDigits;
  unsigned int      myPopMitr;
  unsigned int      myIndMitr;
  double            myPopEpsilon;
  double            myIndEpsilon;
  int               myPopTraceLevel;
  int               myIndTraceLevel;
  unsigned int      mySeed;

  std::string       myCovForm;
  bool              myIsStderr;
  bool              myIsCorrelation;
  bool              myIsCov;
  bool              myIsInvCov;
  bool              myIsConfidence;
  bool              myIsCoefficient;
  std::valarray<int> myRecordNums;
};

#endif
