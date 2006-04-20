/**
 * @file XmlConstants.cpp
 * Define the XmlConstants data structure.
 */
#include "XmlConstants.h"
#include "SpkCompilerException.h"
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
using namespace xercesc;

/** &lt;spkdata&gt; */
const char* XmlConstants::C_SPKDATA                   = "spkdata";
const char* XmlConstants::C_VERSION                   = "version";
const char* XmlConstants::C_POINTONE                  = "0.1" ;
const char* XmlConstants::C_TABLE                     = "table";
const char* XmlConstants::C_COLUMNS                   = "columns";
const char* XmlConstants::C_ROWS                      = "rows";
const char* XmlConstants::C_DESCRIPTION               = "description";
const char* XmlConstants::C_ROW                       = "row";
const char* XmlConstants::C_POSITION                  = "position";
const char* XmlConstants::C_VALUE                     = "value";
const char* XmlConstants::C_TYPE                      = "type";
const char* XmlConstants::C_NUMERIC                   = "numeric";

const char* XmlConstants::C_THETA                     = "theta";
const char* XmlConstants::C_OMEGA                     = "omega";
const char* XmlConstants::C_SIGMA                     = "sigma";
const char* XmlConstants::C_PRED                      = "pred";
const char* XmlConstants::C_YES                       = "yes";
const char* XmlConstants::C_NO                        = "no";
const char* XmlConstants::C_FIXED                     = "fixed";
const char* XmlConstants::C_IN                        = "in";
const char* XmlConstants::C_LOW                       = "low";
const char* XmlConstants::C_UP                        = "up";
const char* XmlConstants::C_DIAGONAL                  = "diagonal";
const char* XmlConstants::C_BLOCK                     = "block";
const char* XmlConstants::C_STRUCT                    = "struct";
const char* XmlConstants::C_DIMENSION                 = "dimension";
const char* XmlConstants::C_SAME_AS_PREVIOUS          = "same_as_previous";
const char* XmlConstants::C_LABEL                     = "label";
const char* XmlConstants::C_LABELS                    = "labels";
const char* XmlConstants::C_COV_R                     = "r";
const char* XmlConstants::C_COV_RSR                   = "rsr";
const char* XmlConstants::C_COV_S                     = "s";
const char* XmlConstants::C_COV_H                     = "h";
const char* XmlConstants::C_COV_HSH                   = "hsh";
const char* XmlConstants::C_NONMEM                    = "nonmem";
const char* XmlConstants::C_POP_ANALYSIS              = "pop_analysis";
const char* XmlConstants::C_IND_ANALYSIS              = "ind_analysis";
const char* XmlConstants::C_CONSTRAINT                = "constraint";
const char* XmlConstants::C_MODEL                     = "model";
const char* XmlConstants::C_ADVAN                     = "advan";
const char* XmlConstants::C_TRANS                     = "trans";
const char* XmlConstants::C_COMP_MODEL                = "comp_model";
const char* XmlConstants::C_COMPARTMENT               = "compartment";
const char* XmlConstants::C_DIFFEQN                   = "diffeqn";
const char* XmlConstants::C_PK                        = "pk";
const char* XmlConstants::C_ERROR                     = "error";
const char* XmlConstants::C_PRESENTATION              = "presentation";
const char* XmlConstants::C_SCATTERPLOT               = "scatterplot";
const char* XmlConstants::C_COLUMN                    = "column";
const char* XmlConstants::C_X                         = "x";
const char* XmlConstants::C_Y                         = "y";
const char* XmlConstants::C_SPLIT                     = "split";
const char* XmlConstants::C_APPROXIMATION             = "approximation";
const char* XmlConstants::C_FO                        = "fo";
const char* XmlConstants::C_FOCE                      = "foce";
const char* XmlConstants::C_LAPLACE                   = "laplace";
const char* XmlConstants::C_STD_TWO_STAGE             = "std_two_stage";
const char* XmlConstants::C_GLOBAL_TWO_STAGE          = "global_two_stage";
const char* XmlConstants::C_ITERATIVE_TWO_STAGE       = "iterative_two_stage";
const char* XmlConstants::C_MONTE_CARLO               = "monte_carlo";
const char* XmlConstants::C_METHOD                    = "method";
const char* XmlConstants::C_ADAPT                     = "adapt";
const char* XmlConstants::C_GRID                      = "grid";
const char* XmlConstants::C_MISER                     = "miser";
const char* XmlConstants::C_PLAIN                     = "plain";
const char* XmlConstants::C_VEGAS                     = "vegas";
const char* XmlConstants::C_NUMBEREVAL                = "number_eval";
const char* XmlConstants::C_POP_SIZE                  = "pop_size";
const char* XmlConstants::C_IS_ESTIMATION             = "is_estimation";
const char* XmlConstants::C_IS_ETA_OUT                = "is_eta_out";
const char* XmlConstants::C_IS_RESTART                = "is_restart";
const char* XmlConstants::C_DATA_LABELS               = "data_labels";
const char* XmlConstants::C_FILENAME                  = "filename";
const char* XmlConstants::C_NAME                      = "name";
const char* XmlConstants::C_SYNONYM                   = "synonym";
const char* XmlConstants::C_LENGTH                    = "length";
const char* XmlConstants::C_SIMULATION                = "simulation";
const char* XmlConstants::C_SEED                      = "seed";
const char* XmlConstants::C_SUBPROBLEMS               = "subproblems";
const char* XmlConstants::C_POP_STAT                  = "pop_stat";
const char* XmlConstants::C_COVARIANCE_FORM           = "covariance_form";
const char* XmlConstants::C_MITR                      = "mitr";
const char* XmlConstants::C_IND_STAT                  = "ind_stat";
const char* XmlConstants::C_SIG_DIGITS                = "sig_digits";
const char* XmlConstants::C_IS_STDERROR_OUT           = "is_stderror_out";
const char* XmlConstants::C_IS_CORRELATION_OUT        = "is_correlation_out";
const char* XmlConstants::C_IS_COVARIANCE_OUT         = "is_covariance_out";
const char* XmlConstants::C_IS_INVERSE_COVARIANCE_OUT = "is_inverse_covariance_out";
const char* XmlConstants::C_IS_COEFFICIENT_OUT        = "is_coefficient_out";
const char* XmlConstants::C_IS_CONFIDENCE_OUT         = "is_confidence_out";
const char* XmlConstants::C_SPKREPORT                 = "spkreport";
const char* XmlConstants::C_ELAPSEDTIME               = "elapsedtime";
const char* XmlConstants::C_NUMBER_EVAL               = "number_eval";
const char* XmlConstants::C_SUBPROBLEM                = "subproblem";
const char* XmlConstants::C_ERROR_LIST                = "error_list";
const char* XmlConstants::C_WARNING_LIST              = "warning_list";
const char* XmlConstants::C_POP_ANALYSIS_RESULT       = "pop_analysis_result";
const char* XmlConstants::C_IND_ANALYSIS_RESULT       = "ind_analysis_result";
const char* XmlConstants::C_OPT_TRACE_OUT             = "opt_trace_out";
const char* XmlConstants::C_POP_MONTE_RESULT          = "pop_monte_result";
const char* XmlConstants::C_MESSAGE                   = "message";
const char* XmlConstants::C_FILE_NAME                 = "file_name";
const char* XmlConstants::C_LINE_NUMBER               = "line_number";
const char* XmlConstants::C_WARNING                   = "warning";
const char* XmlConstants::C_POP_OPT_RESULT            = "pop_opt_result";
const char* XmlConstants::C_POP_STAT_RESULT           = "pop_stat_result";
const char* XmlConstants::C_IND_OPT_RESULT            = "ind_opt_result";
const char* XmlConstants::C_IND_STAT_RESULT           = "ind_stat_result";
const char* XmlConstants::C_POP_OBJ_OUT               = "pop_obj_out";
const char* XmlConstants::C_IND_OBJ_OUT               = "ind_obj_out";
const char* XmlConstants::C_THETA_IN                  = "theta_in";
const char* XmlConstants::C_THETA_OUT                 = "theta_out";
const char* XmlConstants::C_OMEGA_IN                  = "omega_in";
const char* XmlConstants::C_OMEGA_OUT                 = "omega_out";
const char* XmlConstants::C_SIGMA_IN                  = "sigma_in";
const char* XmlConstants::C_SIGMA_OUT                 = "sigma_out";
const char* XmlConstants::C_POP_STDERROR_OUT          = "pop_stderror_out";
const char* XmlConstants::C_POP_COVARIANCE_OUT        = "pop_convariance_out";
const char* XmlConstants::C_POP_INVERSE_COVARIANCE_OUT= "pop_inverse_covariance_out";
const char* XmlConstants::C_POP_CORRELATION_OUT       = "pop_correlation_out";
const char* XmlConstants::C_POP_COEFFICIENT_OUT       = "pop_coefficient_out";
const char* XmlConstants::C_POP_CONFIDENCE_OUT        = "pop_confidence_out";
const char* XmlConstants::C_IND_STDERROR_OUT          = "ind_stderror_out";
const char* XmlConstants::C_IND_COVARIANCE_OUT        = "ind_covariance_out";
const char* XmlConstants::C_IND_INVERSE_COVARIANCE_OUT= "ind_inverse_covariance_out";
const char* XmlConstants::C_IND_CORRELATION_OUT       = "ind_correlation_out";
const char* XmlConstants::C_IND_COEFFICIENT_OUT       = "ind_coefficient_out";
const char* XmlConstants::C_IND_CONFIDENCE_OUT        = "ind_confidence_out";
const char* XmlConstants::C_PRESENTATION_DATA         = "presentation_data";
const char* XmlConstants::C_NCOMPARTMENTS             = "ncompartments";
const char* XmlConstants::C_NPARAMETERS               = "nparameters";
const char* XmlConstants::C_NEQUILIBRIMS              = "nequilibrims";
const char* XmlConstants::C_INITIAL_OFF               = "initial_off";
const char* XmlConstants::C_NO_OFF                    = "no_off";
const char* XmlConstants::C_NO_DOSE                   = "no_dose";
const char* XmlConstants::C_EQUILIBRIM                = "equilibrim";
const char* XmlConstants::C_EXCLUDE                   = "exclude";
const char* XmlConstants::C_DEFAULT_OBSERVATION       = "default_observation";
const char* XmlConstants::C_DEFAULT_DOSE              = "default_dose";
const char* XmlConstants::C_TOLERANCE                 = "tolerance";

//==================================================================================
// XML source/data element values
//==================================================================================
const char* XmlConstants::C_ID                        = "ID";
const char* XmlConstants::C_MDV                       = "MDV";
const char* XmlConstants::C_EVID                      = "EVID";
const char* XmlConstants::C_AMT                       = "AMT";
const char* XmlConstants::C_DROP                      = "DROP";
const char* XmlConstants::C_SKIP                      = "SKIP";
XmlConstants::XmlConstants()
{
   X_SPKDATA = XMLString::transcode( C_SPKDATA );
   assert( X_SPKDATA != NULL );
  X_VERSION                    = XMLString::transcode( C_VERSION );
  X_POINTONE                   = XMLString::transcode( C_POINTONE );
  X_TABLE                      = XMLString::transcode( C_TABLE );
  X_COLUMNS                    = XMLString::transcode( C_COLUMNS );
  X_ROWS                       = XMLString::transcode( C_ROWS );
  X_DESCRIPTION                = XMLString::transcode( C_DESCRIPTION );
  X_ROW                        = XMLString::transcode( C_ROW );
  X_POSITION                   = XMLString::transcode( C_POSITION );
  X_VALUE                      = XMLString::transcode( C_VALUE );
  X_TYPE                       = XMLString::transcode( C_TYPE );
  X_NUMERIC                    = XMLString::transcode( C_NUMERIC );
  X_ID                         = XMLString::transcode( C_ID );
  X_MDV                        = XMLString::transcode( C_MDV );
  X_EVID                       = XMLString::transcode( C_EVID );
  X_AMT                        = XMLString::transcode( C_AMT );
  X_DROP                       = XMLString::transcode( C_DROP );
  X_SKIP                       = XMLString::transcode( C_SKIP );
  // SpkSourceML tags
    //  X_DESCRIPTION                = XMLString::transcode( C_DESCRIPTION );
  X_IN                         = XMLString::transcode( C_IN );
  X_NONMEM                     = XMLString::transcode( C_NONMEM );
  X_POP_ANALYSIS               = XMLString::transcode( C_POP_ANALYSIS );
  X_IND_ANALYSIS               = XMLString::transcode( C_IND_ANALYSIS );
  X_CONSTRAINT                 = XMLString::transcode( C_CONSTRAINT );
  X_MONTE_CARLO                = XMLString::transcode( C_MONTE_CARLO );
  X_MODEL                      = XMLString::transcode( C_MODEL );
  X_ADVAN                      = XMLString::transcode( C_ADVAN );
  X_TRANS                      = XMLString::transcode( C_TRANS );
  X_PRED                       = XMLString::transcode( C_PRED );
  X_COMP_MODEL                 = XMLString::transcode( C_COMP_MODEL );
  X_COMPARTMENT                = XMLString::transcode( C_COMPARTMENT );
  X_DIFFEQN                    = XMLString::transcode( C_DIFFEQN );
  X_PK                         = XMLString::transcode( C_PK );
  X_ERROR                      = XMLString::transcode( C_ERROR );
  X_PRESENTATION               = XMLString::transcode( C_PRESENTATION );
  //  X_TABLE                      = XMLString::transcode( C_TABLE );
  X_SCATTERPLOT                = XMLString::transcode( C_SCATTERPLOT );
  X_COLUMN                     = XMLString::transcode( C_COLUMN );
  X_LOW                        = XMLString::transcode( C_LOW );
  X_UP                         = XMLString::transcode( C_UP );
  X_LABEL                      = XMLString::transcode( C_LABEL );
  X_LABELS                     = XMLString::transcode( C_LABELS );
  X_X                          = XMLString::transcode( C_X );
  X_Y                          = XMLString::transcode( C_Y );
  X_SPLIT                      = XMLString::transcode( C_SPLIT );
  X_THETA                      = XMLString::transcode( C_THETA );
  X_OMEGA                      = XMLString::transcode( C_OMEGA );
  X_SIGMA                      = XMLString::transcode( C_SIGMA );
  X_SIMULATION                 = XMLString::transcode( C_SIMULATION );
  X_POP_STAT                   = XMLString::transcode( C_POP_STAT );
  X_IND_STAT                   = XMLString::transcode( C_IND_STAT );
  X_NCOMPARTMENTS              = XMLString::transcode( C_NCOMPARTMENTS );
  X_NPARAMETERS                = XMLString::transcode( C_NPARAMETERS );
  X_NEQUILIBRIMS               = XMLString::transcode( C_NEQUILIBRIMS );
  X_INITIAL_OFF                = XMLString::transcode( C_INITIAL_OFF );
  X_NO_OFF                     = XMLString::transcode( C_NO_OFF );
  X_NO_DOSE                    = XMLString::transcode( C_NO_DOSE );
  X_EQUILIBRIM                 = XMLString::transcode( C_EQUILIBRIM );
  X_EXCLUDE                    = XMLString::transcode( C_EXCLUDE );
  X_DEFAULT_OBSERVATION        = XMLString::transcode( C_DEFAULT_OBSERVATION );
  X_DEFAULT_DOSE               = XMLString::transcode( C_DEFAULT_DOSE );
  X_TOLERANCE                  = XMLString::transcode( C_TOLERANCE );

  // SpkSourceML attributes
  X_FIXED                      = XMLString::transcode( C_FIXED );
  //  X_VALUE                      = XMLString::transcode( C_VALUE );
  X_STRUCT                     = XMLString::transcode( C_STRUCT );
  X_DIMENSION                  = XMLString::transcode( C_DIMENSION );
  X_SAME_AS_PREVIOUS           = XMLString::transcode( C_SAME_AS_PREVIOUS );
  X_IS_ERR_OUT                 = XMLString::transcode( C_IS_STDERROR_OUT );
  X_IS_CORR_OUT                = XMLString::transcode( C_IS_CORRELATION_OUT );
  X_IS_COV_OUT                 = XMLString::transcode( C_IS_COVARIANCE_OUT );
  X_IS_INV_COV_OUT             = XMLString::transcode( C_IS_INVERSE_COVARIANCE_OUT );
  X_IS_COEF_OUT                = XMLString::transcode( C_IS_COEFFICIENT_OUT );
  X_IS_CONF_OUT                = XMLString::transcode( C_IS_CONFIDENCE_OUT );
  X_APPROXIMATION              = XMLString::transcode( C_APPROXIMATION );
  X_METHOD                     = XMLString::transcode( C_METHOD );
  X_NUMBEREVAL                 = XMLString::transcode( C_NUMBEREVAL );
  X_POP_SIZE                   = XMLString::transcode( C_POP_SIZE  );
  X_IS_ESTIMATION              = XMLString::transcode( C_IS_ESTIMATION );
  X_IS_ETA_OUT                 = XMLString::transcode( C_IS_ETA_OUT );
  X_IS_RESTART                 = XMLString::transcode( C_IS_RESTART );
  X_DATA_LABELS                = XMLString::transcode( C_DATA_LABELS );
  X_FILENAME                   = XMLString::transcode( C_FILENAME );
  X_NAME                       = XMLString::transcode( C_NAME );
  X_SYNONYM                    = XMLString::transcode( C_SYNONYM );
  X_LENGTH                     = XMLString::transcode( C_LENGTH );
  X_SEED                       = XMLString::transcode( C_SEED );
  X_SUBPROBLEMS                = XMLString::transcode( C_SUBPROBLEMS );
  X_COVARIANCE_FORM            = XMLString::transcode( C_COVARIANCE_FORM );
  X_MITR                       = XMLString::transcode( C_MITR );
  X_SIG_DIGITS                 = XMLString::transcode( C_SIG_DIGITS );

  // SpkSourceML attribute values
  X_YES                        = XMLString::transcode( C_YES );
  X_NO                         = XMLString::transcode( C_NO );
  X_DIAGONAL                   = XMLString::transcode( C_DIAGONAL );
  X_BLOCK                      = XMLString::transcode( C_BLOCK );
  X_COV_R                      = XMLString::transcode( C_COV_R );
  X_COV_RSR                    = XMLString::transcode( C_COV_RSR );
  X_COV_S                      = XMLString::transcode( C_COV_S );
  X_COV_H                      = XMLString::transcode( C_COV_H );
  X_COV_HSH                    = XMLString::transcode( C_COV_HSH );
  X_FO                         = XMLString::transcode( C_FO );
  X_FOCE                       = XMLString::transcode( C_FOCE );
  X_LAPLACE                    = XMLString::transcode( C_LAPLACE );
  X_STD_TWO_STAGE              = XMLString::transcode( C_STD_TWO_STAGE );
  X_GLOBAL_TWO_STAGE           = XMLString::transcode( C_GLOBAL_TWO_STAGE );
  X_ITERATIVE_TWO_STAGE        = XMLString::transcode( C_ITERATIVE_TWO_STAGE );
  X_ADAPT                      = XMLString::transcode( C_ADAPT );
  X_GRID                       = XMLString::transcode( C_GRID );
  X_PLAIN                      = XMLString::transcode( C_PLAIN );
  X_MISER                      = XMLString::transcode( C_MISER );
  X_VEGAS                      = XMLString::transcode( C_VEGAS );

  // SpkReportML attribute
  X_SPKREPORT                  = XMLString::transcode( C_SPKREPORT );
  X_ELAPSEDTIME                = XMLString::transcode( C_ELAPSEDTIME );
  X_NEMBER_EVAL                = XMLString::transcode( C_NUMBER_EVAL );
  X_SUBPROBLEM                 = XMLString::transcode( C_SUBPROBLEM );
 
  // SpkReportML tags
  X_ERROR_LIST                 = XMLString::transcode( C_ERROR_LIST );
  X_WARNING_LIST               = XMLString::transcode( C_WARNING_LIST );
  X_POP_ANALYSIS_RESULT        = XMLString::transcode( C_POP_ANALYSIS_RESULT );
  X_IND_ANALYSIS_RESULT        = XMLString::transcode( C_IND_ANALYSIS_RESULT );
  X_OPT_TRACE_OUT              = XMLString::transcode( C_OPT_TRACE_OUT );
  X_POP_MONTE_RESULT           = XMLString::transcode( C_POP_MONTE_RESULT );
  X_MESSAGE                    = XMLString::transcode( C_MESSAGE );
  X_FILE_NAME                  = XMLString::transcode( C_FILE_NAME );
  X_LINE_NUMBER                = XMLString::transcode( C_LINE_NUMBER );
  X_WARNING                    = XMLString::transcode( C_WARNING );
  X_POP_OPT_RESULT             = XMLString::transcode( C_POP_OPT_RESULT );
  X_IND_OPT_RESULT             = XMLString::transcode( C_IND_OPT_RESULT );
  X_POP_STAT_RESULT            = XMLString::transcode( C_POP_STAT_RESULT );
  X_IND_STAT_RESULT            = XMLString::transcode( C_IND_STAT_RESULT );
  X_POP_OBJ_OUT                = XMLString::transcode( C_POP_OBJ_OUT );
  X_IND_OBJ_OUT                = XMLString::transcode( C_IND_OBJ_OUT );
  X_THETA_IN                   = XMLString::transcode( C_THETA_IN );
  X_THETA_OUT                  = XMLString::transcode( C_THETA_OUT );
  X_OMEGA_IN                   = XMLString::transcode( C_OMEGA_IN );
  X_OMEGA_OUT                  = XMLString::transcode( C_OMEGA_OUT );
  X_SIGMA_IN                   = XMLString::transcode( C_SIGMA_IN );
  X_SIGMA_OUT                  = XMLString::transcode( C_SIGMA_OUT );
  X_POP_STDERROR_OUT           = XMLString::transcode( C_POP_STDERROR_OUT );
  X_POP_COVARIANCE_OUT         = XMLString::transcode( C_POP_COVARIANCE_OUT );
  X_POP_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_POP_INVERSE_COVARIANCE_OUT );
  X_POP_CORRELATION_OUT        = XMLString::transcode( C_POP_CORRELATION_OUT );
  X_POP_COEFFICIENT_OUT        = XMLString::transcode( C_POP_COEFFICIENT_OUT );
  X_POP_CONFIDENCE_OUT         = XMLString::transcode( C_POP_CONFIDENCE_OUT );
  X_IND_STDERROR_OUT           = XMLString::transcode( C_IND_STDERROR_OUT );
  X_IND_COVARIANCE_OUT         = XMLString::transcode( C_IND_COVARIANCE_OUT );
  X_IND_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_IND_INVERSE_COVARIANCE_OUT );
  X_IND_CORRELATION_OUT        = XMLString::transcode( C_IND_CORRELATION_OUT );
  X_IND_COEFFICIENT_OUT        = XMLString::transcode( C_IND_COEFFICIENT_OUT );
  X_IND_CONFIDENCE_OUT         = XMLString::transcode( C_IND_CONFIDENCE_OUT );
  X_PRESENTATION_DATA          = XMLString::transcode( C_PRESENTATION_DATA );
}
XmlConstants::~XmlConstants()
{
  XMLString::release( &X_SPKDATA );
  XMLString::release( &X_VERSION );
  XMLString::release( &X_POINTONE );
  XMLString::release( &X_TABLE );
  XMLString::release( &X_COLUMNS );
  XMLString::release( &X_ROWS );
  XMLString::release( &X_DESCRIPTION );
  XMLString::release( &X_ROW );
  XMLString::release( &X_POSITION );
  XMLString::release( &X_VALUE );
  XMLString::release( &X_TYPE );
  XMLString::release( &X_NUMERIC );
  XMLString::release( &X_ID );
  XMLString::release( &X_MDV );
  XMLString::release( &X_EVID );
  XMLString::release( &X_AMT );
  XMLString::release( &X_DROP );
  XMLString::release( &X_SKIP );
  //  XMLString::release( &X_DESCRIPTION );
  XMLString::release( &X_YES );
  XMLString::release( &X_NO );
  XMLString::release( &X_FIXED );
  XMLString::release( &X_IN );
  XMLString::release( &X_LOW );
  XMLString::release( &X_UP );
  XMLString::release( &X_DIAGONAL );
  XMLString::release( &X_BLOCK );
  //  XMLString::release( &X_VALUE );
  XMLString::release( &X_STRUCT );
  XMLString::release( &X_DIMENSION );
  XMLString::release( &X_SAME_AS_PREVIOUS );
  XMLString::release( &X_LABEL );
  XMLString::release( &X_COV_R );
  XMLString::release( &X_COV_RSR );
  XMLString::release( &X_COV_S );
  XMLString::release( &X_COV_H );
  XMLString::release( &X_COV_HSH );
  XMLString::release( &X_IS_ERR_OUT );
  XMLString::release( &X_IS_CORR_OUT );
  XMLString::release( &X_IS_COV_OUT );
  XMLString::release( &X_IS_INV_COV_OUT );
  XMLString::release( &X_IS_COEF_OUT );
  XMLString::release( &X_IS_CONF_OUT );
  
  XMLString::release( &X_NONMEM );
  XMLString::release( &X_POP_ANALYSIS );
  XMLString::release( &X_IND_ANALYSIS );
  XMLString::release( &X_CONSTRAINT );
  XMLString::release( &X_MODEL );
  XMLString::release( &X_ADVAN );
  XMLString::release( &X_TRANS );
  XMLString::release( &X_PRED );
  XMLString::release( &X_COMP_MODEL );
  XMLString::release( &X_COMPARTMENT );
  XMLString::release( &X_DIFFEQN );
  XMLString::release( &X_PK );
  XMLString::release( &X_ERROR );
  XMLString::release( &X_MONTE_CARLO );
  XMLString::release( &X_PRESENTATION );
  //  XMLString::release( &X_TABLE );
  XMLString::release( &X_SCATTERPLOT );
  XMLString::release( &X_COLUMN );
  XMLString::release( &X_X );
  XMLString::release( &X_Y );
  XMLString::release( &X_SPLIT );
  XMLString::release( &X_APPROXIMATION );
  XMLString::release( &X_FO );
  XMLString::release( &X_FOCE );
  XMLString::release( &X_LAPLACE );
  XMLString::release( &X_ADAPT );
  XMLString::release( &X_GRID );
  XMLString::release( &X_PLAIN );
  XMLString::release( &X_MISER );
  XMLString::release( &X_VEGAS );
  XMLString::release( &X_NUMBEREVAL );
  XMLString::release( &X_POP_SIZE );
  XMLString::release( &X_IS_ESTIMATION );
  XMLString::release( &X_IS_ETA_OUT );
  XMLString::release( &X_IS_RESTART );
  XMLString::release( &X_DATA_LABELS );
  XMLString::release( &X_FILENAME );
  XMLString::release( &X_NAME );
  XMLString::release( &X_SYNONYM );
  XMLString::release( &X_THETA );
  XMLString::release( &X_LENGTH );
  XMLString::release( &X_OMEGA );
  XMLString::release( &X_SIGMA );
  XMLString::release( &X_SIMULATION );
  XMLString::release( &X_SEED );
  XMLString::release( &X_POP_STAT );
  XMLString::release( &X_COVARIANCE_FORM );
  XMLString::release( &X_MITR );
  XMLString::release( &X_IND_STAT );
  XMLString::release( &X_SIG_DIGITS );
  XMLString::release( &X_SUBPROBLEMS );

  // SpkReportML attribute
  XMLString::release( &X_ELAPSEDTIME );
  XMLString::release( &X_NEMBER_EVAL );
  XMLString::release( &X_SUBPROBLEM );
 
  // SpkReportML tags
  XMLString::release( &X_ERROR_LIST );
  XMLString::release( &X_WARNING_LIST );
  XMLString::release( &X_POP_ANALYSIS_RESULT );
  XMLString::release( &X_IND_ANALYSIS_RESULT );
  XMLString::release( &X_OPT_TRACE_OUT );
  XMLString::release( &X_POP_MONTE_RESULT );
  XMLString::release( &X_MESSAGE );
  XMLString::release( &X_FILE_NAME );
  XMLString::release( &X_LINE_NUMBER );
  XMLString::release( &X_WARNING );
  XMLString::release( &X_POP_OPT_RESULT );
  XMLString::release( &X_IND_OPT_RESULT );
  XMLString::release( &X_POP_STAT_RESULT );
  XMLString::release( &X_IND_STAT_RESULT );
  XMLString::release( &X_POP_OBJ_OUT );
  XMLString::release( &X_IND_OBJ_OUT );
  XMLString::release( &X_THETA_IN );
  XMLString::release( &X_THETA_OUT );
  XMLString::release( &X_OMEGA_IN );
  XMLString::release( &X_OMEGA_OUT );
  XMLString::release( &X_SIGMA_IN );
  XMLString::release( &X_SIGMA_OUT );
  XMLString::release( &X_POP_STDERROR_OUT );
  XMLString::release( &X_POP_COVARIANCE_OUT );
  XMLString::release( &X_POP_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_POP_CORRELATION_OUT );
  XMLString::release( &X_POP_COEFFICIENT_OUT );
  XMLString::release( &X_POP_CONFIDENCE_OUT );
  XMLString::release( &X_IND_STDERROR_OUT );
  XMLString::release( &X_IND_COVARIANCE_OUT );
  XMLString::release( &X_IND_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_IND_CORRELATION_OUT );
  XMLString::release( &X_IND_COEFFICIENT_OUT );
  XMLString::release( &X_IND_CONFIDENCE_OUT );
  XMLString::release( &X_PRESENTATION_DATA );

  XMLString::release( &X_NCOMPARTMENTS );
  XMLString::release( &X_NPARAMETERS );
  XMLString::release( &X_NEQUILIBRIMS );
  XMLString::release( &X_INITIAL_OFF );
  XMLString::release( &X_NO_OFF );
  XMLString::release( &X_NO_DOSE );
  XMLString::release( &X_EQUILIBRIM );
  XMLString::release( &X_EXCLUDE );
  XMLString::release( &X_DEFAULT_OBSERVATION );
  XMLString::release( &X_DEFAULT_DOSE );
  XMLString::release( &X_TOLERANCE );
}
