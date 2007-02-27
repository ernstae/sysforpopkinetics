/**
 * @file XmlConstants.h
 * Declare the XmlConstants data structure.
 */
#ifndef XMLCONSTANTS_H
#define XMLCONSTANTS_H

#include <xercesc/dom/DOMDocument.hpp>

/**
 * Data structure bundling XML tags and attribute names/values.
 */
struct XmlConstants{
  /**
   * Default constructor
   */
  XmlConstants();

  /**
   * Destructor
   */
  ~XmlConstants();

  /** C array version. */
  static const char * C_ADVAN;
  /** XMLCh array version. */
  XMLCh* X_ADVAN;

  /** C array version. */
  static const char * C_AMT;
  /** XMLCh array version. */
  XMLCh* X_AMT;

  /** C array version. */
  static const char * C_ADAPT;
  /** XMLCh array version. */
  XMLCh* X_ADAPT;

  /** C array version. */
  static const char * C_APPROXIMATION;
  /** XMLCh array version. */
  XMLCh* X_APPROXIMATION;

  /** C array version. */
  static const char * C_AUTO_GENERATE_METHOD;
  /** XMLCh array version. */
  XMLCh* X_AUTO_GENERATE_METHOD;

  /** C array version. */
  static const char * C_BLOCK;
  /** XMLCh array version. */
  XMLCh* X_BLOCK;

  /** C array version. */
  static const char * C_COLUMN;
  /** XMLCh array version. */
  XMLCh* X_COLUMN;

  /** C array version. */
  static const char * C_COLUMNS;
  /** XMLCh array version. */
  XMLCh* X_COLUMNS;

  /** C array version. */
  static const char * C_COMP_MODEL;
  /** XMLCh array version. */
  XMLCh* X_COMP_MODEL;

  /** C array version. */
  static const char * C_COMPARTMENT;
  /** XMLCh array version. */
  XMLCh* X_COMPARTMENT;

  /** C array version. */
  static const char * C_CONSTRAINT;
  /** XMLCh array version. */
  XMLCh* X_CONSTRAINT;

  /** C array version. */
  static const char * C_COV_H;
  /** XMLCh array version. */
  XMLCh* X_COV_H;

  /** C array version. */
  static const char * C_COV_HSH;
  /** XMLCh array version. */
  XMLCh* X_COV_HSH;

  /** C array version. */
  static const char * C_COV_R;
  /** XMLCh array version. */
  XMLCh* X_COV_R;

  /** C array version. */
  static const char * C_COV_RSR;
  /** XMLCh array version. */
  XMLCh* X_COV_RSR;

  /** C array version. */
  static const char * C_COV_S;
  /** XMLCh array version. */
  XMLCh* X_COV_S;

  /** C array version. */
  static const char * C_COVARIANCE_FORM;
  /** XMLCh array version. */
  XMLCh* X_COVARIANCE_FORM;

  /** C array version. */
  static const char * C_DATA_LABELS;
  /** XMLCh array version. */
  XMLCh* X_DATA_LABELS;

  /** C array version. */
  static const char * C_DEFAULT_DOSE;  
  /** XMLCh array version. */
  XMLCh* X_DEFAULT_DOSE;

  /** C array version. */
  static const char * C_DEFAULT_OBSERVATION;
  /** XMLCh array version. */
  XMLCh* X_DEFAULT_OBSERVATION;

  /** C array version. */
  static const char * C_DESCRIPTION;
  /** XMLCh array version. */
  XMLCh* X_DESCRIPTION;

  /** C array version. */
  static const char * C_DIAGONAL;
  /** XMLCh array version. */
  XMLCh* X_DIAGONAL;

  /** C array version. */
  static const char * C_DIFFEQN;
  /** XMLCh array version. */
  XMLCh* X_DIFFEQN;

  /** C array version. */
  static const char * C_DIMENSION;
  /** XMLCh array version. */
  XMLCh* X_DIMENSION;

  /** C array version. */
  static const char * C_DROP;
  /** XMLCh array version. */
  XMLCh* X_DROP;

  /** C array version. */
  static const char * C_ELAPSEDTIME;
  /** XMLCh array version. */
  XMLCh* X_ELAPSEDTIME;

  /** C array version. */
  static const char * C_EQUILIBRIM;
  /** XMLCh array version. */
  XMLCh* X_EQUILIBRIM;

  /** C array version. */
  static const char * C_ERROR;
  /** XMLCh array version. */
  XMLCh* X_ERROR;

  /** C array version. */
  static const char * C_ERROR_LIST;
  /** XMLCh array version. */
  XMLCh* X_ERROR_LIST;

  /** C array version. */
  static const char * C_EXCLUDE;
  /** XMLCh array version. */
  XMLCh* X_EXCLUDE;

  /** C array version. */
  static const char * C_EVID;
  /** XMLCh array version. */
  XMLCh* X_EVID;

  /** C array version. */
  static const char * C_FILE_NAME;
  /** XMLCh array version. */
  XMLCh* X_FILE_NAME;

  /** C array version. */
  static const char * C_FILENAME;
  /** XMLCh array version. */
  XMLCh* X_FILENAME;

  /** C array version. */
  static const char * C_FIXED;
  /** XMLCh array version. */
  XMLCh* X_FIXED;

  /** C array version. */
  static const char * C_FO;
  /** XMLCh array version. */
  XMLCh* X_FO;

  /** C array version. */
  static const char * C_FOCE;
  /** XMLCh array version. */
  XMLCh* X_FOCE;

  /** C array version. */
  static const char * C_GLOBAL_TWO_STAGE;
  /** XMLCh array version. */
  XMLCh* X_GLOBAL_TWO_STAGE;

  /** C array version. */
  static const char * C_GRID;
  /** XMLCh array version. */
  XMLCh* X_GRID;

  /** C array version. */
  static const char * C_ID;
  /** XMLCh array version. */
  XMLCh* X_ID;

  /** C array version. */
  static const char * C_IN;
  /** XMLCh array version. */
  XMLCh* X_IN;

  /** C array version. */
  static const char * C_IND_ANALYSIS;
  /** XMLCh array version. */
  XMLCh* X_IND_ANALYSIS;

  /** C array version. */
  static const char * C_IND_ANALYSIS_RESULT;
  /** XMLCh array version. */
  XMLCh* X_IND_ANALYSIS_RESULT;

  /** C array version. */
  static const char * C_IND_COEFFICIENT_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_COEFFICIENT_OUT;

  /** C array version. */
  static const char * C_IND_CONFIDENCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_CONFIDENCE_OUT;
  /** C array version. */
  static const char * C_IND_CORRELATION_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_CORRELATION_OUT;

  /** C array version. */
  static const char * C_IND_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_COVARIANCE_OUT;

  /** C array version. */
  static const char * C_IND_INVERSE_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_INVERSE_COVARIANCE_OUT;

  /** C array version. */
  static const char * C_IND_OBJ_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_OBJ_OUT;

  /** C array version. */
  static const char * C_IND_OPT_RESULT;
  /** XMLCh array version. */
  XMLCh* X_IND_OPT_RESULT;

  /** C array version. */
  static const char * C_IND_STAT;
  /** XMLCh array version. */
  XMLCh* X_IND_STAT;

  /** C array version. */
  static const char * C_IND_STAT_RESULT;
  /** XMLCh array version. */
  XMLCh* X_IND_STAT_RESULT;

  /** C array version. */
  static const char * C_IND_STDERROR_OUT;
  /** XMLCh array version. */
  XMLCh* X_IND_STDERROR_OUT;

  /** C array version. */
  static const char * C_IND_IDENT_NUMBER_OF_SOLUTIONS;
  /** XMLCh array version. */
  XMLCh* X_IND_IDENT_NUMBER_OF_SOLUTIONS;

  /** C array version. */
  static const char * C_IND_IDENT_STATUS;
  /** XMLCh array version. */
  XMLCh* X_IND_IDENT_STATUS;

  /** C array version. */
  static const char * C_INITIAL_OFF;
  /** XMLCh array version. */
  XMLCh* X_INITIAL_OFF;

  /** C array version. */
  static const char * C_IS_COEFFICIENT_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_COEF_OUT;

  /** C array version. */
  static const char * C_IS_CONFIDENCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_CONF_OUT;

  /** C array version. */
  static const char * C_IS_CORRELATION_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_CORR_OUT;

  /** C array version. */
  static const char * C_IS_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_COV_OUT;

  /** C array version. */
  static const char * C_IS_ESTIMATION;
  /** XMLCh array version. */
  XMLCh* X_IS_ESTIMATION;

  /** C array version. */
  static const char * C_IS_IDENTIFIABILITY;
  /** XMLCh array version. */
  XMLCh* X_IS_IDENTIFIABILITY;

  /** C array version. */
  static const char * C_IS_ETA_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_ETA_OUT;

  /** C array version. */
  static const char * C_IS_INVERSE_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_INV_COV_OUT;

  /** C array version. */
  static const char * C_IS_RESTART;
  /** XMLCh array version. */
  XMLCh* X_IS_RESTART;

  /** C array version. */
  static const char * C_IS_STDERROR_OUT;
  /** XMLCh array version. */
  XMLCh* X_IS_ERR_OUT;

  /** C array version. */
  static const char * C_ITERATIVE_TWO_STAGE;
  /** XMLCh array version. */
  XMLCh* X_ITERATIVE_TWO_STAGE;

  /** C array version. */
  static const char * C_LABEL;
  /** XMLCh array version. */
  XMLCh* X_LABEL;

  /** C array version. */
  static const char * C_LABELS;
  /** XMLCh array version. */
  XMLCh* X_LABELS;

  /** C array version. */
  static const char * C_LAPLACE;
  /** XMLCh array version. */
  XMLCh* X_LAPLACE;

  /** C array version. */
  static const char * C_LENGTH;
  /** XMLCh array version. */
  XMLCh* X_LENGTH;

  /** C array version. */
  static const char * C_LINE_NUMBER;
  /** XMLCh array version. */
  XMLCh* X_LINE_NUMBER;

  /** C array version. */
  static const char * C_LOW;
  /** XMLCh array version. */
  XMLCh* X_LOW;

  /** C array version. */
  static const char * C_MITR;
  /** XMLCh array version. */
  XMLCh* X_MITR;

  /** C array version. */
  static const char * C_MDV;
  /** XMLCh array version. */
  XMLCh* X_MDV;

  /** C array version. */
  static const char * C_MEASURE_POINTS_IN;
  /** XMLCh array version. */
  XMLCh* X_MEASURE_POINTS_IN;

  /** C array version. */
  static const char * C_MESSAGE;
  /** XMLCh array version. */
  XMLCh* X_MESSAGE;

  /** C array version. */
  static const char * C_METHOD;
  /** XMLCh array version. */
  XMLCh* X_METHOD;

  /** C array version. */
  static const char * C_MISER;
  /** XMLCh array version. */
  XMLCh* X_MISER;

  /** C array version. */
  static const char * C_MODEL;
  /** XMLCh array version. */
  XMLCh* X_MODEL;

  /** C array version. */
  static const char * C_MONTE_CARLO;
  /** XMLCh array version. */
  XMLCh* X_MONTE_CARLO;

  /** C array version. */
  static const char * C_NAME;
  /** XMLCh array version. */
  XMLCh* X_NAME;

  /** C array version. */
  static const char * C_NCOMPARTMENTS;
  /** XMLCh array version. */
  XMLCh* X_NCOMPARTMENTS;

  /** C array version. */
  static const char * C_NEQUILIBRIMS;
  /** XMLCh array version. */
  XMLCh* X_NEQUILIBRIMS;

  /** C array version. */
  static const char * C_NO;
  /** XMLCh array version. */
  XMLCh* X_NO;

  /** C array version. */
  static const char * C_NO_DOSE;
  /** XMLCh array version. */
  XMLCh* X_NO_DOSE;

  /** C array version. */
  static const char * C_NO_OFF;
  /** XMLCh array version. */
  XMLCh* X_NO_OFF;

  /** C array version. */
  static const char * C_NONMEM;
  /** XMLCh array version. */
  XMLCh* X_NONMEM;

  /** C array version. */
  static const char * C_NONPARAMETRIC;
  /** XMLCh array version. */
  XMLCh* X_NONPARAMETRIC;

  /** C array version. */
  static const char * C_NONPARAMETRIC_INFO;
  /** XMLCh array version. */
  XMLCh* X_NONPARAMETRIC_INFO;

  /** C array version. */
  static const char * C_NONPARAMETRIC_RESULT;
  /** XMLCh array version. */
  XMLCh* X_NONPARAMETRIC_RESULT;

  /** C array version. */
  static const char * C_NPARAMETERS;
  /** XMLCh array version. */
  XMLCh* X_NPARAMETERS;

  /** C array version. */
  static const char * C_NUMBER_EVAL;
  /** XMLCh array version. */
  XMLCh* X_NEMBER_EVAL;

  /** C array version. */
  static const char * C_NUMBER_OF_POINTS;
  /** XMLCh array version. */
  XMLCh* X_NUMBER_OF_POINTS;

  /** C array version. */
  static const char * C_NUMBEREVAL;
  /** XMLCh array version. */
  XMLCh* X_NUMBEREVAL;

  /** C array version. */
  static const char * C_NUMERIC;
  /** XMLCh array version. */
  XMLCh* X_NUMERIC;

  /** C array version. */
  static const char * C_OMEGA;
  /** XMLCh array version. */
  XMLCh* X_OMEGA;

  /** C array version. */
  static const char * C_OMEGA_IN;
  /** XMLCh array version. */
  XMLCh* X_OMEGA_IN;

  /** C array version. */
  static const char * C_OMEGA_OUT;
  /** XMLCh array version. */
  XMLCh* X_OMEGA_OUT;

  /** C array version. */
  static const char * C_OPT_TRACE_OUT;
  /** XMLCh array version. */
  XMLCh* X_OPT_TRACE_OUT;

  /** C array version. */
  static const char * C_PK;
  /** XMLCh array version. */
  XMLCh* X_PK;

  /** C array version. */
  static const char * C_PLAIN;
  /** XMLCh array version. */
  XMLCh* X_PLAIN;

  /** C array version. */
  static const char * C_POINTONE;
  /** XMLCh array version. */
  XMLCh* X_POINTONE;

  /** C array version. */
  static const char * C_POINTS_PER_DIMENSION;
  /** XMLCh array version. */
  XMLCh* X_POINTS_PER_DIMENSION;

  /** C array version. */
  static const char * C_POSITION;
  /** XMLCh array version */
  XMLCh* X_POSITION;

  /** C array version. */
  static const char * C_POP_ANALYSIS;
  /** XMLCh array version. */
  XMLCh* X_POP_ANALYSIS;

  /** C array version. */
  static const char * C_POP_ANALYSIS_RESULT;
  /** XMLCh array version. */
  XMLCh* X_POP_ANALYSIS_RESULT;

  /** C array version. */
  static const char * C_POP_COEFFICIENT_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_COEFFICIENT_OUT;

  /** C array version. */
  static const char * C_POP_CONFIDENCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_CONFIDENCE_OUT;

  /** C array version. */
  static const char * C_POP_CORRELATION_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_CORRELATION_OUT;

  /** C array version. */
  static const char * C_POP_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_COVARIANCE_OUT;

  /** C array version. */
  static const char * C_POP_INVERSE_COVARIANCE_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_INVERSE_COVARIANCE_OUT;

  /** C array version. */
  static const char * C_POP_MONTE_RESULT;
  /** XMLCh array version. */
  XMLCh* X_POP_MONTE_RESULT;

  /** C array version. */
  static const char * C_POP_OBJ_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_OBJ_OUT;

  /** C array version. */
  static const char * C_POP_OPT_RESULT;
  /** XMLCh array version. */
  XMLCh* X_POP_OPT_RESULT;

  /** C array version. */
  static const char * C_POP_SIZE;
  /** XMLCh array version. */
  XMLCh* X_POP_SIZE;

  /** C array version. */
  static const char * C_POP_STAT;
  /** XMLCh array version. */
  XMLCh* X_POP_STAT;

  /** C array version. */
  static const char * C_POP_STAT_RESULT;
  /** XMLCh array version. */
  XMLCh* X_POP_STAT_RESULT;

  /** C array version. */
  static const char * C_POP_STDERROR_OUT;
  /** XMLCh array version. */
  XMLCh* X_POP_STDERROR_OUT;

  /** C array version. */
  static const char * C_PRED;
  /** XMLCh array version. */
  XMLCh* X_PRED;

  /** C array version. */
  static const char * C_PRESENTATION;
  /** XMLCh array version. */
  XMLCh* X_PRESENTATION;

  /** C array version. */
  static const char * C_PRESENTATION_DATA;
  /** XMLCh array version. */
  XMLCh* X_PRESENTATION_DATA;

  /** C array version. */
  static const char * C_RANDOM_UNIFORM;
  /** XMLCh array version. */
  XMLCh* X_RANDOM_UNIFORM;

  /** C array version. */
  static const char * C_ROW;
  /** XMLCh array version. */
  XMLCh* X_ROW;

  /** C array version. */
  static const char * C_ROWS;
  /** XMLCh array version. */
  XMLCh* X_ROWS;

  /** C array version. */
  static const char * C_SAME_AS_PREVIOUS;
  /** XMLCh array version. */
  XMLCh* X_SAME_AS_PREVIOUS;

  /** C array version. */
  static const char * C_SCATTERPLOT;
  /** XMLCh array version. */
  XMLCh* X_SCATTERPLOT;

  /** C array version. */
  static const char * C_SEED;
  /** XMLCh array version. */
  XMLCh* X_SEED;

  /** C array version. */
  static const char * C_SIG_DIGITS;
  /** XMLCh array version. */
  XMLCh* X_SIG_DIGITS;

  /** C array version. */
  static const char * C_SIGMA;
  /** XMLCh array version. */
  XMLCh* X_SIGMA;

  /** C array version. */
  static const char * C_SIGMA_IN;
  /** XMLCh array version. */
  XMLCh* X_SIGMA_IN;

  /** C array version. */
  static const char * C_SIGMA_OUT;
  /** XMLCh array version. */
  XMLCh* X_SIGMA_OUT;

  /** C array version. */
  static const char * C_SIMULATION;
  /** XMLCh array version. */
  XMLCh* X_SIMULATION;

  /** C array version. */
  static const char * C_SKIP;
  /** XMLCh array version. */
  XMLCh* X_SKIP;

  /** C array version. */
  static const char *  C_SPKDATA; 
  /** XMLCh array version. */
  XMLCh * X_SPKDATA;   

  /** C array version. */
  static const char * C_SPKREPORT;
  /** XMLCh array version. */
  XMLCh* X_SPKREPORT;

  /** C array version. */
  static const char * C_SPLIT;
  /** XMLCh array version. */
  XMLCh* X_SPLIT;

  /** C array version. */
  static const char * C_SUBPROBLEM;
  /** XMLCh array version. */
  XMLCh* X_SUBPROBLEM;

  /** C array version. */
  static const char * C_STD_TWO_STAGE;
  /** XMLCh array version. */
  XMLCh* X_STD_TWO_STAGE;

  /** C array version. */
  static const char * C_STRUCT;
  /** XMLCh array version. */
  XMLCh* X_STRUCT;

  /** C array version. */
  static const char * C_SUBPROBLEMS;
  /** XMLCh array version. */
  XMLCh* X_SUBPROBLEMS;

  /** C array version. */
  static const char * C_SYNONYM;
  /** XMLCh array version. */
  XMLCh* X_SYNONYM;

  /** C array version. */
  static const char * C_TABLE;
  /** XMLCh array version. */
  XMLCh* X_TABLE;

  /** C array version. */
  static const char * C_THETA;
  /** XMLCh array version. */
  XMLCh* X_THETA;

  /** C array version. */
  static const char * C_THETA_IN;
  /** XMLCh array version. */
  XMLCh* X_THETA_IN;

  /** C array version. */
  static const char * C_THETA_OUT;
  /** XMLCh array version. */
  XMLCh* X_THETA_OUT;

  /** C array version. */
  static const char * C_TOLERANCE;
  /** XMLCh array version. */
  XMLCh* X_TOLERANCE;

  /** C array version. */
  static const char * C_TRANS;
  /** XMLCh array version. */
  XMLCh* X_TRANS;

  /** C array version. */
  static const char * C_TYPE;
  /** XMLCh array version. */
  XMLCh* X_TYPE;

  /** C array version. */
  static const char * C_UP;
  /** XMLCh array version. */
  XMLCh* X_UP;

  /** C array version. */
  static const char * C_VEGAS;
  /** XMLCh array version. */
  XMLCh* X_VEGAS;

  /** C array version. */
  static const char * C_VERSION;
  /** XMLCh array version. */
  XMLCh* X_VERSION;

  /** C array version. */
  static const char * C_VALUE;
  /** XMLCh array version. */
  XMLCh* X_VALUE;

  /** C array version. */
  static const char * C_WARNING;
  /** XMLCh array version. */
  XMLCh* X_WARNING;

  /** C array version. */
  static const char * C_WARNING_LIST;
  /** XMLCh array version. */
  XMLCh* X_WARNING_LIST;

  /** C array version. */
  static const char * C_X;
  /** XMLCh array version. */
  XMLCh* X_X;

  /** C array version. */
  static const char * C_Y;
  /** XMLCh array version. */
  XMLCh* X_Y;

  /** C array version. */
  static const char * C_YES;    
  /** XMLCh array version. */
  XMLCh* X_YES;
};

#endif
