#ifndef XMLCONSTANTS_H
#define XMLCONSTANTS_H

#include <xercesc/dom/DOMDocument.hpp>
                                                                                                        
struct XmlConstants{
  XmlConstants();
  ~XmlConstants();

  static const char*  C_SPKDATA;
  XMLCh * X_SPKDATA;   

  static const char * C_VERSION;
  XMLCh* X_VERSION;

  static const char * C_POINTONE;
  XMLCh* X_POINTONE;

  static const char * C_TABLE;
  XMLCh* X_TABLE;

  static const char * C_COLUMNS;
  XMLCh* X_COLUMNS;

  static const char * C_ROWS;
  XMLCh* X_ROWS;

  static const char * C_DESCRIPTION;
  XMLCh* X_DESCRIPTION;

  static const char * C_ROW;
  XMLCh* X_ROW;

  static const char * C_POSITION;
  XMLCh* X_POSITION;

  static const char * C_VALUE;
  XMLCh* X_VALUE;

  static const char * C_TYPE;
  XMLCh* X_TYPE;

  static const char * C_NUMERIC;
  XMLCh* X_NUMERIC;

  static const char * C_ID;
  XMLCh* X_ID;

  static const char * C_MDV;
  XMLCh* X_MDV;

  static const char * C_EVID;
  XMLCh* X_EVID;

  static const char * C_AMT;
  XMLCh* X_AMT;

  static const char * C_DROP;
  XMLCh* X_DROP;

  static const char * C_SKIP;
  XMLCh* X_SKIP;

  // SpkSourceML attributes  

  static const char* C_YES;    
  XMLCh* X_YES;

  static const char* C_NO;
  XMLCh* X_NO;

  static const char* C_FIXED;
  XMLCh* X_FIXED;

  static const char* C_IN;
  XMLCh* X_IN;

  static const char* C_LOW;
  XMLCh* X_LOW;

  static const char* C_UP;
  XMLCh* X_UP;

  static const char* C_DIAGONAL;
  XMLCh* X_DIAGONAL;

  static const char* C_BLOCK;
  XMLCh* X_BLOCK;

  static const char* C_STRUCT;
  XMLCh* X_STRUCT;

  static const char* C_DIMENSION;
  XMLCh* X_DIMENSION;

  static const char* C_LABEL;
  XMLCh* X_LABEL;

  static const char* C_LABELS;
  XMLCh* X_LABELS;

  static const char* C_COV_R;
  XMLCh* X_COV_R;

  static const char* C_COV_RSR;
  XMLCh* X_COV_RSR;

  static const char* C_COV_S;
  XMLCh* X_COV_S;

  static const char* C_COV_H;
  XMLCh* X_COV_H;

  static const char* C_COV_HSH;
  XMLCh* X_COV_HSH;

  static const char* C_IS_STDERROR_OUT;
  XMLCh* X_IS_ERR_OUT;

  static const char* C_IS_CORRELATION_OUT;
  XMLCh* X_IS_CORR_OUT;

  static const char* C_IS_COVARIANCE_OUT;
  XMLCh* X_IS_COV_OUT;

  static const char* C_IS_INVERSE_COVARIANCE_OUT;
  XMLCh* X_IS_INV_COV_OUT;

  static const char* C_IS_COEFFICIENT_OUT;
  XMLCh* X_IS_COEF_OUT;

  static const char* C_IS_CONFIDENCE_OUT;
  XMLCh* X_IS_CONF_OUT;

  static const char* C_NCOMPARTMENTS;
  XMLCh* X_NCOMPARTMENTS;

  static const char* C_NPARAMETERS;
  XMLCh* X_NPARAMETERS;

  static const char* C_NEQUILIBRIMS;
  XMLCh* X_NEQUILIBRIMS;

  static const char* C_INITIAL_OFF;
  XMLCh* X_INITIAL_OFF;

  static const char* C_NO_OFF;
  XMLCh* X_NO_OFF;

  static const char* C_NO_DOSE;
  XMLCh* X_NO_DOSE;

  static const char* C_EQUILIBRIM;
  XMLCh* X_EQUILIBRIM;

  static const char* C_EXCLUDE;
  XMLCh* X_EXCLUDE;

  static const char* C_DEFAULT_OBSERVATION;
  XMLCh* X_DEFAULT_OBSERVATION;

  static const char* C_DEFAULT_DOSE;  
  XMLCh* X_DEFAULT_DOSE;

  static const char* C_TOLERANCE;
  XMLCh* X_TOLERANCE;

  // SpkSourceML tags
                     static const char* C_NONMEM;
  XMLCh* X_NONMEM;
               static const char* C_POP_ANALYSIS;
  XMLCh* X_POP_ANALYSIS;
               static const char* C_IND_ANALYSIS;
  XMLCh* X_IND_ANALYSIS;
                 static const char* C_CONSTRAINT;
  XMLCh* X_CONSTRAINT;

                      static const char* C_MODEL;
  XMLCh* X_MODEL;
                      static const char* C_ADVAN;
  XMLCh* X_ADVAN;
                      static const char* C_TRANS;
  XMLCh* X_TRANS;
                       static const char* C_PRED;
  XMLCh* X_PRED;
                 static const char* C_COMP_MODEL;
  XMLCh* X_COMP_MODEL;
                static const char* C_COMPARTMENT;
  XMLCh* X_COMPARTMENT;
                    static const char* C_DIFFEQN;
  XMLCh* X_DIFFEQN;
                         static const char* C_PK;
  XMLCh* X_PK;
                      static const char* C_ERROR;
  XMLCh* X_ERROR;
                static const char* C_MONTE_CARLO;
  XMLCh* X_MONTE_CARLO;
               static const char* C_PRESENTATION;
  XMLCh* X_PRESENTATION;

                static const char* C_SCATTERPLOT;
  XMLCh* X_SCATTERPLOT;
                     static const char* C_COLUMN;
  XMLCh* X_COLUMN;
                          static const char* C_X;
  XMLCh* X_X;
                          static const char* C_Y;
  XMLCh* X_Y;
                      static const char* C_SPLIT;
  XMLCh* X_SPLIT;
              static const char* C_APPROXIMATION;
  XMLCh* X_APPROXIMATION;
                         static const char* C_FO;
  XMLCh* X_FO;
                       static const char* C_FOCE;
  XMLCh* X_FOCE;
                    static const char* C_LAPLACE;
  XMLCh* X_LAPLACE;
              static const char* C_STD_TWO_STAGE;
  XMLCh* X_STD_TWO_STAGE;

           static const char* C_GLOBAL_TWO_STAGE;
  XMLCh* X_GLOBAL_TWO_STAGE;

        static const char* C_ITERATIVE_TWO_STAGE;
  XMLCh* X_ITERATIVE_TWO_STAGE;

                     static const char* C_METHOD;
  XMLCh* X_METHOD;

                   static const char* C_ANALYTIC;
  XMLCh* X_ANALYTIC;

                       static const char* C_GRID;
  XMLCh* X_GRID;

                      static const char* C_MISER;
  XMLCh* X_MISER;

                      static const char* C_PLAIN;
  XMLCh* X_PLAIN;

                      static const char* C_VEGAS;
  XMLCh* X_VEGAS;

                 static const char* C_NUMBEREVAL;
  XMLCh* X_NUMBEREVAL;

                   static const char* C_POP_SIZE;
  XMLCh* X_POP_SIZE;

              static const char* C_IS_ESTIMATION;
  XMLCh* X_IS_ESTIMATION;

                 static const char* C_IS_ETA_OUT;
  XMLCh* X_IS_ETA_OUT;

                 static const char* C_IS_RESTART;
  XMLCh* X_IS_RESTART;

                static const char* C_DATA_LABELS;
  XMLCh* X_DATA_LABELS;

                   static const char* C_FILENAME;
  XMLCh* X_FILENAME;

                       static const char* C_NAME;
  XMLCh* X_NAME;
                    static const char* C_SYNONYM;
  XMLCh* X_SYNONYM;

                      static const char* C_THETA;
  XMLCh* X_THETA;

                     static const char* C_LENGTH;
  XMLCh* X_LENGTH;

                      static const char* C_OMEGA;
  XMLCh* X_OMEGA;

                      static const char* C_SIGMA;
  XMLCh* X_SIGMA;

                 static const char* C_SIMULATION;
  XMLCh* X_SIMULATION;

                       static const char* C_SEED;
  XMLCh* X_SEED;
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


};

#endif
