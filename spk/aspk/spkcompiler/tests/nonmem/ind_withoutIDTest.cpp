#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_withoutIDTest.h"
#include <spkcompiler/series.h>
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <spkcompiler/nonmem/NonmemTranslator.h>
#include <spkcompiler/SymbolTable.h>

using namespace std;
using namespace CppUnit;
using namespace xercesc;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// A test based upon a linear regression problem defined at
// http://http://www.itl.nist.gov/div898/strd/lls/data/Norris.shtml.
//
// This test requests parameter estimation and statistics but not Monte Carlo.
// 
// Procedure: 	Linear Least Squares Regression
// Certification Method & Definitions
// Data: 	1 Response Variable (y)
// 1 Predictor Variable (x)
// 36 Observations
// Lower Level of Difficulty
// Observed Data
// Data file (ASCII Format)
// Additional Information
//
// Model: 	Linear Class
// 2 Parameters ( beta0, beta1 )
//
//     y = beta0 + beta1 * x + e
// 
// --- Certified Regression Statistics ---
//
// Parameter 	
//           Estimate 	    Standard Deviation
//           of Estimate
//    -0.262323073774029    0.232818234301152
//     1.00211681802045     0.429796848199937E-03
//
// Residual
// Standard Deviation 	    0.884796396144373 		
// R-Squared 	            0.999993745883712 		
//
// 
// --- Certified Analysis of Variance Table ---
//
//   Source   | Degrees |                       |                          |
//     of     |   of    |    Sum of Squares     |      Mean Squares        |    F Statistics
// Variation  | Freedom |                       |                          |
// -----------+---------+-----------------------+--------------------------+-------------------
// Regression |    1 	| 4255954.13232369 	|  4255954.13232369        |  5436385.54079785
// Residual   |   34 	|      26.6173985294224 |        0.782864662630069 |	
//
//
// --- Data:     y     x
//             130    60
//             131    61
//             132    62
//             133    63
//             134    64
//             135    65
//             136    66
//             137    67
//             138    68
//             139    69
//             140    70
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
namespace{
  const unsigned int MAXCHARS = 64;

  const char * testName;
  char fIndData_h[]       = "IndData.h";
  char fDataSet_h[]       = "DataSet.h";
  char fPred_h[]          = "Pred.h";
  char fPredEqn_cpp[]     = "predEqn.cpp";
  char fNonmemPars_h[]    = "NonmemPars.h";
  char fMontePars_h[]     = "MontePars.h";
  char fMonteDriver_cpp[] = "monteDriver.cpp";
  char fFitDriver_cpp[]   = "fitDriver.cpp";
  char fMakefile[]        = "Makefile.SPK";
  char fDriver[]          = "driver";
  char fReportML[]        = "result.xml";
  char fSavedReportML[]   = "saved_result.xml";
  char fTraceOut[]        = "trace_output";

  char fPrefix              [MAXCHARS];
  char fDataML              [MAXCHARS];
  char fSourceML            [MAXCHARS];
  char fNonmemParsDriver    [MAXCHARS];
  char fNonmemParsDriver_cpp[MAXCHARS];
  char fMonteParsDriver     [MAXCHARS];
  char fMonteParsDriver_cpp [MAXCHARS];
  char fIndDataDriver       [MAXCHARS];
  char fIndDataDriver_cpp   [MAXCHARS];
  char fDataSetDriver       [MAXCHARS];
  char fDataSetDriver_cpp   [MAXCHARS];
  char fPredDriver          [MAXCHARS];
  char fPredDriver_cpp      [MAXCHARS];

  char SPKLIB[]     = "spk";
  char SPKPREDLIB[] = "spkpred";
  char SPKOPTLIB[]  = "spkopt";
  char ATLASLIB[]   = "atlas_lapack";
  char CBLASLIB[]   = "cblas";
  char CLAPACKLIB[] = "atlas";
  char PTHREADLIB[] = "pthread";
  char MLIB[]       = "m";
  char LDPATH[]     = "-Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest";
  char CPPFLAG[]    = "-g -I/usr/local/include/spktest";
  char LDFLAG[514];

  char MY_ASSERT_EQUAL[] =
"#include <iostream> \n \
#include <sys/signal.h> \n \
#define MY_ASSERT_EQUAL( expected, actual ) \\\n \
if( actual != expected ) \\\n \
 { \\\n \
   std::cerr << __FILE__ << \"(\" << __LINE__ << \"): but was \" << actual << std::endl; \\\n \
   raise( SIGABRT ); \\\n \
} \\\n\n";

  
  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = true;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const char *strID   = "ID";
  const char *strTIME = "TiMe";
  const char *strDV   = "DV";
  const char *strCP   = "CP";
  const char *strMDV  = "MDV";
  const char *label[] = { strDV, strTIME, strMDV };
  map<const char*, const char*> label_alias;
  int nLabels         = 3;

  //============================================
  // <Data Set>  --- SKIP ID
  //
  //   ID       DV=CP    TIME    MDV
  /*
        (1)       0.1     0.2      0
        (1)     338.8   337.4      0
        (1)     118.1   118.2      0
        (1)     888.0   884.6      0
        (1)       9.2    10.1      0
        (1)     228.1   226.5      0
        (1)     668.5   666.3      0
        (1)     998.5   996.3      0
        (1)     449.1   448.6      0
        (1)     778.9   777.0      0
        (1)     559.2   558.2      0
        (1)       0.3     0.4      0
        (1)       0.1     0.6      0 
        (1)     778.1   775.5      0
        (1)     668.8   666.9      0
        (1)     339.3   338.0      0
        (1)     448.9   447.5      0
        (1)      10.8    11.6      0
        (1)     557.7   556.0      0
        (1)     228.3   228.1      0
        (1)     998.0   995.8      0
        (1)     888.0   887.6      0
        (1)     119.6   120.2      0
        (1)       0.3     0.3      0
        (1)       0.6     0.3      0
        (1)     557.6   556.8      0
        (1)     339.3   339.1      0
        (1)     888.0   887.2      0
        (1)     998.5   999.0      0
        (1)     778.9   779.0      0
        (1)      10.2    11.1      0
        (1)     117.6   118.3      0
        (1)     228.9   229.2      0
        (1)     668.4   669.1      0
        (1)     449.2   448.9      0
        (1)       0.2     0.5      0
   */
  //============================================
  const int    nRecords   =  36;
  const int    nFixed     =  0;
  const int    nItems     =  3;
  const double record0[]  = { 0.1, 0.2, 0 };
  const double record1[]  = { 338.8, 337.4, 0 };
  const double record2[]  = { 118.1, 118.2, 0 };
  const double record3[]  = { 888.0, 884.6, 0 };
  const double record4[]  = { 9.2, 10.1, 0 };
  const double record5[]  = { 228.1, 226.5, 0 };
  const double record6[]  = { 668.5, 666.3, 0 };
  const double record7[]  = { 998.5, 996.3, 0 };
  const double record8[]  = { 449.1, 448.6, 0 };
  const double record9[]  = { 778.9, 777.0, 0 };
  const double record10[] = { 559.2, 558.2, 0 };
  const double record11[] = { 0.3, 0.4, 0 };
  const double record12[] = { 0.1, 0.6, 0 };
  const double record13[] = { 778.1, 775.5, 0 };
  const double record14[] = { 668.8, 666.9, 0 };
  const double record15[] = { 339.3, 338.0, 0 };
  const double record16[] = { 448.9, 447.5, 0 };
  const double record17[] = { 10.8, 11.6, 0 };
  const double record18[] = { 557.7, 556.0, 0 };
  const double record19[] = { 228.3, 228.1, 0 };
  const double record20[] = { 998.0, 995.8, 0 };
  const double record21[] = { 888.8, 887.6, 0 };
  const double record22[] = { 119.6, 120.2, 0 };
  const double record23[] = { 0.3, 0.3, 0 };
  const double record24[] = { 0.6, 0.3, 0 };
  const double record25[] = { 557.6, 556.8, 0 };
  const double record26[] = { 339.3, 339.1, 0 };
  const double record27[] = { 888.0, 887.2, 0 };
  const double record28[] = { 998.5, 999.0, 0 };
  const double record29[] = { 778.9, 779.0, 0 };
  const double record30[] = { 10.2, 11.1, 0 };
  const double record31[] = { 117.6, 118.3, 0 };
  const double record32[] = { 228.9, 229.2, 0 };
  const double record33[] = { 668.4, 669.1, 0 };
  const double record34[] = { 449.2, 448.9, 0 };
  const double record35[] = { 0.2, 0.5, 0 };
  //  const double record36[] = { 0.0, 0.0, 1 };

  double const * record[nRecords];

  //============================================
  // Define NONMEM keywords
  //============================================
  const char *strTHETA = "THETA";
  const char *strOMEGA = "OMEGA";
  const char *strSIGMA = "SIGMA";
  const char *strETA   = "ETA";
  const char *strEPS   = "EPS";
  const char *strPRED  = "PRED";
  const char *strRES   = "RES";
  const char *strWRES  = "WRES";
  const char *strF     = "F";
  const char *strY     = "Y";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen = 2;
  const double theta_in [ thetaLen ]   = { 0.2,  1.0 };
  const double theta_up [ thetaLen ]   = { 2.0, 10.0 };
  const double theta_low[ thetaLen ]   = { 0.02, 0.1 };
  const bool   theta_fix[ thetaLen ]   = { false, false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal --- 1 dimensional! --- matrix.
  //============================================
  const int    omegaDim                = 1;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
  const int    omegaOrder              = 1;
  const double omega_in[ omegaOrder ]  = { 1.0 };
  const bool   omega_fix[ omegaOrder ] = { false };

  //============================================
  // The SPK Compiler determines the initial
  // values for eta, the variance of data
  // in the individual analysis case.
  // The size of this vector is determined by
  // the order of Omega matrix.
  //============================================
  const int etaLen = omegaOrder;

  //============================================
  // EPS is irrevalent in the individual 
  // analysis case.  It'll be ignored.
  //============================================
  const int epsLen = 0;

  //============================================
  // Make requests for statistics.
  //============================================
  const bool ind_stderr         = true;
  const bool ind_coefficient    = true;
  const bool ind_confidence     = true;
  const bool ind_covariance     = true;
  const bool ind_inv_covariance = true;
  const bool ind_correlation    = true;

  //============================================
  // Make a request on data simulation.
  //============================================
  const bool isSimulate         = false;
  const int  seed               = -1;
  const bool onlySimulation     = false;
  const int  subproblems        = 1;

  //============================================
  // PRED model based on Norris
  //
  // b0 = THETA(1)
  // b1 = THETA(2)
  // x = TIME
  // F = b0 + b1 * x = THETA(1) + THETA(2)*TIME
  // Y = F + ETA(1)
  //============================================
  const char PRED[] = "b0 = THETA(1)\nb1 = THETA(2)\nx = TiMe\nF = b0 + b1 * x\nY = F + ETA(1)\n";


  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  const double nm_obj       =  46.4087;
  const double nm_theta[]   = { 0.02, 1.00171 };
  const double nm_omega[]   = { 0.771353 };

  // Standard error
  // With SPK's parameterization:
  //
  // theta(1)    0.2311  
  // theta(2)    0.000426625  
  // Omega(1,1)  0.117851
  // 
  //                            theta(1)  theta(2)  Omega(1,1)
  //const double nm_stderr[]  = {  }; 
                              
  //
  // Covariance
  // With SPK's parameterization:
  //
  //                theta(1)     theta(2)     Omega(1,1)
  //            /                                         \
  // theta(1)   |   0.0534072   -7.62941e-05  0.0         |
  // theta(2)   |  -7.62941e-05  1.82009e-07  0.0         |
  // Omega(1,1) |   0.0          0.0          0.0138889   |
  //            \                                         /
  //
  //const double nm_cov[]     = {   };

  // Inverse of covariance
  //
  //               theta(1)      theta(2)     Omega(1,1)
  //            /                                         \
  // theta(1)   |  0.0534072    -7.62941e-05  0.0         |
  // theta(2)   | -7.62941e-05   1.82009e-07  0.0         |
  // Omega(1,1) |  0.0           0.0          0.0138889   |
  //            \                                         /
  // 
  //const double nm_inv_cov[] = {  };


  // Correlation matrix
  // With SPK's parameterization:
  //
  //               theta(1)      theta(2)     Omega(1,1)
  //            /                                        \
  // theta(1)   |   1.0         -0.773828     0.0        |
  // theta(2)   |  -0.773828     1.0          0.0        |
  // Omega(1,1) |   0.0          0.0          0.0        |
  //            \                                        /
  //
  //const double nm_corr[];

  // Coefficient of variation
  // With SPK's parameterization
  //
  // theta(1)   1155.5
  // theta(2)      0.0425895
  // Omega(1,1)  -90.791
  //
  //const double nm_cv[];

  // Confidence interval
  // with SPK's parameterization:
  //
  // theta(1)    -0.45045  ~ 0.49045
  // theta(2)    -1.00085  ~ 1.00258
  // Omega(1,1)  -0.369714 ~ 0.110105
  // 
  // const double nm_ci[];

  const double nm_pred[]    = {  };
  //============================================
  // XML strings
  //============================================
  XMLCh * X_ERROR_MESSAGES;
  XMLCh * X_IND_ANALYSIS_RESULT;
  XMLCh * X_PRESENTATION_DATA;
  XMLCh * X_IND_STDERROR_OUT;
  XMLCh * X_IND_COVARIANCE_OUT;
  XMLCh * X_IND_INVERSE_COVARIANCE_OUT;
  XMLCh * X_IND_CORRELATION_OUT;
  XMLCh * X_IND_COEFFICIENT_OUT;
  XMLCh * X_IND_CONFIDENCE_OUT;
  XMLCh * X_VALUE;
};

void ind_withoutIDTest::setUp()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Initializing the XML
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException& toCatch )
    {
      char buf[MAXCHARS + 1];
      sprintf( buf, "Error during Xerces-c initialization.\nException message: %s.\n", 
               XMLString::transcode( toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,               testName );
  sprintf( fMonteParsDriver,      "%s_MonteParsDriver",      fPrefix );
  sprintf( fMonteParsDriver_cpp,  "%s_MonteParsDriver.cpp",  fPrefix );
  sprintf( fNonmemParsDriver,     "%s_NonmemParsDriver",     fPrefix );
  sprintf( fNonmemParsDriver_cpp, "%s_NonmemParsDriver.cpp", fPrefix );
  sprintf( fIndDataDriver,        "%s_IndDataDriver",        fPrefix );
  sprintf( fIndDataDriver_cpp,    "%s_IndDataDriver.cpp",    fPrefix );
  sprintf( fDataML,               "%s_dataML.xml",           fPrefix );
  sprintf( fSourceML,             "%s_sourceML.xml",         fPrefix );
  sprintf( fDataSetDriver,        "%s_DataSetDriver",        fPrefix );
  sprintf( fDataSetDriver_cpp,    "%s_DataSetDriver.cpp",    fPrefix );
  sprintf( fPredDriver,           "%s_PredDriver",           fPrefix );
  sprintf( fPredDriver_cpp,       "%s_PredDriver.cpp",       fPrefix );

  sprintf( LDFLAG, "%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
	   LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB );

  // TIME doesn't have an alias
  label_alias[strTIME] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;

  // MDV doesn't have an alias.
  label_alias[strMDV]  = NULL;

  record[0]   = record0;
  record[1]   = record1;
  record[2]   = record2;
  record[3]   = record3;
  record[4]   = record4;
  record[5]   = record5;
  record[6]   = record6;
  record[7]   = record7;
  record[8]   = record8;
  record[9]   = record9;
  record[10]  = record10;
  record[11]  = record11;
  record[12]  = record12;
  record[13]  = record13;
  record[14]  = record14;
  record[15]  = record15;
  record[16]  = record16;
  record[17]  = record17;
  record[18]  = record18;
  record[19]  = record19;
  record[20]  = record20;
  record[21]  = record21;
  record[22]  = record22;
  record[23]  = record23;
  record[24]  = record24;
  record[25]  = record25;
  record[26]  = record26;
  record[27]  = record27;
  record[28]  = record28;
  record[29]  = record29;
  record[30]  = record30;
  record[31]  = record31;
  record[32]  = record32;
  record[33]  = record33;
  record[34]  = record34;
  record[35]  = record35;
  //  record[36]  = record36;

  X_ERROR_MESSAGES             = XMLString::transcode( "error_messages" );
  X_IND_ANALYSIS_RESULT        = XMLString::transcode( "ind_analysis_result" );
  X_PRESENTATION_DATA          = XMLString::transcode( "presentation_data" );
  X_IND_STDERROR_OUT           = XMLString::transcode( "ind_stderror_out" );
  X_IND_COVARIANCE_OUT         = XMLString::transcode( "ind_covariance_out" );
  X_IND_INVERSE_COVARIANCE_OUT = XMLString::transcode( "ind_inverse_covariance_out" );
  X_IND_CORRELATION_OUT        = XMLString::transcode( "ind_correlation_out" );
  X_IND_COEFFICIENT_OUT        = XMLString::transcode( "ind_coefficient_out" );
  X_IND_CONFIDENCE_OUT         = XMLString::transcode( "ind_confidence_out" );
  X_VALUE                      = XMLString::transcode( "value" );


  createDataML();
  createSourceML();
  parse();
}
void ind_withoutIDTest::tearDown()
{
  XMLString::release( &X_ERROR_MESSAGES );
  XMLString::release( &X_IND_ANALYSIS_RESULT );
  XMLString::release( &X_PRESENTATION_DATA );
  XMLString::release( &X_IND_STDERROR_OUT );
  XMLString::release( &X_IND_COVARIANCE_OUT );
  XMLString::release( &X_IND_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_IND_CORRELATION_OUT );
  XMLString::release( &X_IND_COEFFICIENT_OUT );
  XMLString::release( &X_IND_CONFIDENCE_OUT );
  XMLString::release( &X_VALUE );
  
  if( okToClean )
    {
      remove( fDataML );
      remove( fSourceML );
      remove( fMonteParsDriver );
      remove( fMonteParsDriver_cpp );
      remove( fNonmemParsDriver );
      remove( fNonmemParsDriver_cpp );
      remove( fIndDataDriver );
      remove( fIndDataDriver_cpp );
      remove( fDataSetDriver );
      remove( fDataSetDriver_cpp );
      remove( fPredDriver );
      remove( fPredDriver_cpp );
      remove( fMontePars_h );
      remove( fNonmemPars_h );
      remove( fDriver );
      remove( fFitDriver_cpp );
      remove( fMonteDriver_cpp );
      remove( fIndData_h );
      remove( fDataSet_h );
      remove( fPred_h );
      remove( fPredEqn_cpp );
      remove( fMakefile );
      remove( fReportML );
      remove( fSavedReportML );
      remove( fTraceOut );
    }
  
  XMLPlatformUtils::Terminate();

}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void ind_withoutIDTest::createDataML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ofstream oData( fDataML );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nLabels << "\" rows=\"" << nRecords + 1 << "\">" << endl;
  oData << "<description>" << endl;
  oData << "The data set (without ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;

  // Labels
  oData << "<row position=\"1\">" << endl;
  for( int i=0; i<nItems; i++ )
    {
      oData << "<value type=\"string\">" << label[i] << "</value>" << endl;
    }
  oData << "</row>" << endl;

  // Values
  for( int i=0; i<nRecords; i++ )
    {
      oData << "<row position=\"" << i+2 << "\">" << endl;
      oData << "<value type=\"string\">"  << record[i][0] << "</value>" << endl;
      for( int j=1; j<nItems; j++ )
	{
	  oData << "<value type=\"numeric\">" << record[i][j] << "</value>" << endl;
	}
      oData << "</row>" << endl;
    }
  oData << "</table>" << endl;
  oData << "</spkdata>" << endl;
  oData.close();

  xercesc::XercesDOMParser *dataParser = new xercesc::XercesDOMParser;
  dataParser->setValidationScheme( XercesDOMParser::Val_Auto );
  dataParser->setDoNamespaces( true );
  dataParser->setDoSchema( true );
  dataParser->setValidationSchemaFullChecking( true );
  dataParser->setCreateEntityReferenceNodes( true );
  
  try{
    dataParser->parse( fDataML );
    data = dataParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fDataML, XMLString::transcode(e.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      XMLCh errText[MAXCHARS + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, MAXCHARS))
	{
          XMLPlatformUtils::Terminate();
          char buf[MAXCHARS + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fDataML, e.code, XMLString::transcode(errText) );
	  
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fDataML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
 
void ind_withoutIDTest::createSourceML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preparation for creating a sourceML document
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  //============================================
  // Create an sourceML based upon the
  // parameters set so far.
  //============================================
  ofstream oSource( fSourceML );
  CPPUNIT_ASSERT( oSource.good() );

  oSource << "<spksource>" << endl;
  oSource << "<nonmem>" << endl;
      
  oSource << "<constraint>" << endl;

  // default: is_eta_out=no, is_restart=yes
  oSource << "<ind_analysis ";
  oSource << "mitr=\"" << mitr << "\" ";
  oSource << "is_estimation=\"" << (isEstimate? "yes" : "no") << "\">" << endl;

  oSource << "<data_labels>" << endl;
  for( int i=0; i<nLabels; i++ )
    {
      oSource << "<label name=\"" << label[i] << "\"";
      if( label_alias[ label[i] ] != NULL )
	oSource << " synonym=\"" << label_alias[ label[i] ] << "\"";
      oSource << "/>" << endl;
    }
  oSource << "</data_labels>" << endl;

  oSource << "<theta length=\"" << thetaLen << "\">" << endl;
  oSource << "<in>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      oSource << "<value";
      oSource << " fixed=\"" << (theta_fix[i]? "yes" : "no") << "\"";
      oSource << ">" << theta_in[i] << "</value>" << endl;
    }
  oSource << "</in>" << endl;
  oSource << "<low>" << endl;
  for( int i=0; i<thetaLen; i++ )
    oSource << "<value>" << theta_low[i] << "</value>" << endl;
  oSource << "</low>" << endl;
  oSource << "<up>" << endl;
  for( int i=0; i<thetaLen; i++ )
    oSource << "<value>" << theta_up[i] << "</value>" << endl;
  oSource << "</up>" << endl;
  oSource << "</theta>" << endl;

  oSource << "<omega struct=\"";
  oSource << (omegaStruct==Symbol::DIAGONAL? "diagonal" : "block");
  oSource << "\" dimension=\"";
  oSource << omegaDim << "\">" << endl;
  oSource << "<in>" << endl;

  for( int i=0; i<omegaOrder; i++ )
    {
      oSource << "<value";
      oSource << " fixed=\"" << (omega_fix[i]? "yes" : "no") << "\"";
      oSource << ">" << omega_in[i] << "</value>" << endl;
    }
  oSource << "</in>" << endl;
  oSource << "</omega>" << endl;

  oSource << "<ind_stat ";
  oSource << "is_stderror_out=\""           << (ind_stderr?         "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance?     "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_confidence_out=\""         << (ind_confidence?     "yes":"no") << "\" ";
  oSource << "is_coefficient_out=\""        << (ind_coefficient?    "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation?    "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"";
      if( onlySimulation )
	oSource << " only_simulation=\"yes\"";
      if( subproblems > 1 )
        oSource << " subproblems=\"" << subproblems << "\"";
      oSource << "/>" << endl;
    }
  oSource << "</ind_analysis>" << endl;
  oSource << "</constraint>" << endl;

  oSource << "<model>" << endl;
  oSource << "<pred>" << endl;
  oSource << "   " << PRED << endl;
  oSource << "</pred>" << endl;
  oSource << "</model>" << endl;

  oSource << "<presentation/>" << endl;
    
  oSource << "</nonmem>" << endl;
  oSource << "</spksource>" << endl;
  oSource.close();

  //============================================
  // Build a parse tree from the sourceML
  // document.
  //============================================
  xercesc::XercesDOMParser *sourceParser = new xercesc::XercesDOMParser;
  sourceParser->setValidationScheme( XercesDOMParser::Val_Auto );
  sourceParser->setDoNamespaces( true );
  sourceParser->setDoSchema( true );
  sourceParser->setValidationSchemaFullChecking( true );
  sourceParser->setCreateEntityReferenceNodes( true );
  
  try{
    sourceParser->parse( fSourceML );
    source = sourceParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fSourceML, XMLString::transcode(e.getMessage() ) );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
     }
  catch( const DOMException& e )
    {
      
      XMLCh errText[MAXCHARS + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, MAXCHARS))
	{
          XMLPlatformUtils::Terminate();
          char buf[MAXCHARS + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fSourceML, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fSourceML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Translation
// 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ind_withoutIDTest::parse()
{
  //============================================
  // Instanciate a NonmemTranslator object, 
  // passing the pointers to the sourceML 
  // document tree and the dataML document tree.
  //============================================
  NonmemTranslator xlator( source, data );

  //============================================
  // Determine the type of analysis and 
  // the number of subjects.
  //============================================
  xlator.detAnalysisType();

  //============================================
  // Parse the dataML document
  //============================================
  xlator.parseData();
  SymbolTable *table = xlator.getSymbolTable();

  // ID, TIME, DV were in the data set.  So, they should be in the symbol table already.
  Symbol * id   = table->findi( strID );
  CPPUNIT_ASSERT( id != Symbol::empty() );
  Symbol * time = table->findi( strTIME );
  CPPUNIT_ASSERT( time != Symbol::empty() );
  Symbol * dv   = table->findi( strDV );
  CPPUNIT_ASSERT( dv != Symbol::empty() );

  //============================================
  // Parse the sourceML document
  //============================================
  xlator.parseSource();

  // MDV and CP (=DV) were not in the data set; they must be added to the symbol table.
  Symbol * mdv   = table->findi( strMDV );
  CPPUNIT_ASSERT( mdv != Symbol::empty() );
  Symbol * cp   = table->findi( strCP );
  CPPUNIT_ASSERT( cp != Symbol::empty() );

  // THETA, OMEGA, ETA must be registered for individual analysis.
  Symbol * theta = table->findi( strTHETA );
  CPPUNIT_ASSERT( theta != Symbol::empty() );
  Symbol * omega = table->findi( strOMEGA );
  CPPUNIT_ASSERT( omega != Symbol::empty() );
  Symbol * eta = table->findi( strETA );
  CPPUNIT_ASSERT( eta != Symbol::empty() );

  //============================================
  // Check existence/absence of generated files
  // NonmemPars.h
  // MontePars.h
  // IndData.h
  // DataSet.h
  // Pred.h
  // Makefile.SPK
  // Makefile.MC
  // fitDriver.cpp
  // monteDriver.cpp
  // ==========================================
  FILE * nonmemPars = fopen( fNonmemPars_h, "r" );
  CPPUNIT_ASSERT( nonmemPars != NULL );
  fclose( nonmemPars );

  FILE * montePars = fopen( fMontePars_h, "r" );
  CPPUNIT_ASSERT( montePars == NULL );
  
  FILE * indData = fopen( fIndData_h, "r" );
  CPPUNIT_ASSERT( indData != NULL );
  fclose( indData );

  FILE * dataSet = fopen( fDataSet_h, "r" );
  CPPUNIT_ASSERT( dataSet != NULL );
  fclose( dataSet );

  FILE * pred = fopen( fPred_h, "r" );
  CPPUNIT_ASSERT( pred != NULL );
  fclose( pred );

  FILE * makeSPK = fopen( fMakefile, "r" );
  CPPUNIT_ASSERT( makeSPK != NULL );
  fclose( makeSPK );
 
  FILE * fitDriver = fopen( fFitDriver_cpp, "r" );
  CPPUNIT_ASSERT( fitDriver != NULL );
  fclose( fitDriver );

  FILE * monteDriver = fopen( fMonteDriver_cpp, "r" );
  CPPUNIT_ASSERT( monteDriver == NULL );
}
void ind_withoutIDTest::testNonmemPars_h()
{
  //============================================
  // Test if NonmemPars declares/defines
  // variables as required.
  //============================================
  printf( "\n--- %s ---\n", fNonmemParsDriver );
  ofstream o ( fNonmemParsDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <iostream>" << endl;
  o << "#include \"NonmemPars.h\"" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "using namespace std;" << endl;
  o << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   MY_ASSERT_EQUAL( NonmemPars::nTheta, " << thetaLen << " );" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      o << "   MY_ASSERT_EQUAL( NonmemPars::thetaUp [" << i << "], " << theta_up [i] << " );" << endl;
      o << "   MY_ASSERT_EQUAL( NonmemPars::thetaLow[" << i << "], " << theta_low[i] << " );" << endl;
      o << "   MY_ASSERT_EQUAL( NonmemPars::thetaIn [" << i << "], " << theta_in [i] << " );" << endl;
      o << "   MY_ASSERT_EQUAL( NonmemPars::thetaFixed[" << i << "], " << theta_fix[i] << " );" << endl;
    }						  
  o << "   MY_ASSERT_EQUAL( NonmemPars::omegaDim, " << omegaDim << " );" << endl;
  o << "   MY_ASSERT_EQUAL( NonmemPars::omegaOrder, " << omegaOrder << " );" << endl;
  for( int i=0; i<omegaOrder; i++ )
    {
      o << "   MY_ASSERT_EQUAL( NonmemPars::omegaIn [" << i << "], " << omega_in [i] << " );" << endl;
    }						  
  o << "   MY_ASSERT_EQUAL( NonmemPars::nEta, " << etaLen << " );" << endl;
  for( int i=0; i<etaLen; i++ )
    {
      o << "   MY_ASSERT_EQUAL( NonmemPars::etaIn [" << i << "], 0.0 );" << endl;
    }						  
  o << "   MY_ASSERT_EQUAL( NonmemPars::seed, " << seed << " );" << endl;
  o << "}" << endl;

  o.close();

  char command[512];

  // Build the test driver.
  sprintf( command, "g++ %s -o %s %s %s",
           fNonmemParsDriver_cpp, 
           fNonmemParsDriver, 
           LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char mess[128];
      sprintf( mess, "Failed to build %s.", fNonmemParsDriver );
      CPPUNIT_ASSERT_MESSAGE( mess, false );
    }

  // Run the test driver
  sprintf( command, "./%s", fNonmemParsDriver );
  if( system( command ) != 0 )
    {
      char mess[128];
      sprintf( mess, "%s abnormally terminated.", fNonmemParsDriver );
      CPPUNIT_ASSERT_MESSAGE( mess, false );      
    }
  remove( fNonmemParsDriver );
  //remove( fNonmemParsDriver_cpp );
}
void ind_withoutIDTest::testIndDataClass()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test IndData class to see if it has all necessary 
  // variables declared and sized.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //============================================
  // Check read-only Data Item values
  // * ID
  // * TIME
  // * CP/DV
  // * MDV (registered by the Compiler)
  //
  // Check PK Parameters
  // * theta
  // * Omega
  // * eta (registered by the Compiler)
  //
  // Check the variables appeared on the left hand side 
  // of equations in the PRED definition.
  // * f
  //
  // Check other registered-by-the-compiler variables
  // * PRED
  // * WRES
  // * RES
  //============================================
  printf( "\n--- %s ---\n", fIndDataDriver );
  ofstream o( fIndDataDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <vector>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include \"IndData.h\"" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   const int n = " << nRecords << ";" << endl;
  o << "   const int thetaLen = " << thetaLen << ";" << endl;
  o << "   const int etaLen = " << etaLen << ";" << endl;
  o << "   vector<char*>  a_id(n);" << endl;
  o << "   vector<double> a_time(n);" << endl;
  o << "   vector<double> a_dv(n);" << endl;
  o << "   vector<double> a_mdv(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_id  [" << i << "] = \"" << 1 << "\";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][0] << ";" << endl;
    o << "   a_time[" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << record[i][2] << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_dv, a_time, a_mdv );" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << 1 << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", A." << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strMDV  << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to Pred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,   A." << strETA   << "[" << i << "].size() );" << endl;
      o << endl;
    }
  o << endl;  

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, A." << strRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPRED << ".size() );" << endl;

  o << "   MY_ASSERT_EQUAL( n, A." << strF    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strY    << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nFixed << ", y.size() );" << endl;
  o << "   A.compResiduals();" << endl;
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( A." << strMDV << "[j] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( A." << strDV << "[j], y[k] );" << endl;
  o << "         MY_ASSERT_EQUAL( A." << strDV << "[j]-A." << strPRED << "[j], A." << strRES << "[j] );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[256];
  sprintf( command, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void ind_withoutIDTest::testDataSetClass()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fDataSetDriver );
  ofstream o( fDataSetDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <string>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   const int n = set.getN()[0];" << endl;
  o << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << 1 << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strMDV  << "[" << i << "] );" << endl;
    }

  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( " << thetaLen << ", set.data[0]->" << strTHETA << "[j].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( " << etaLen   << ", set.data[0]->" << strETA   << "[j].size() );" << endl;
  o << "   }" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strY    << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   set.compAllResiduals( );" << endl;  
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( set.data[0]->" << strMDV << "[j] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[j], y[k] );" << endl;
  o << "         MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[j]-set.data[0]->" << strPRED << "[j], set.data[0]->" << strRES << "[j] );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;

  o << endl;
  o << "  return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[256];
  sprintf( command, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withoutIDTest::testPredClass()
{ 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test Pred class to see if it has defined eval() properly.
  // Especially, the proper elements of the dependent variable-
  // vector given as an argument are replaced by the computed 
  // value of Y(j) and F(j).
  // Also, make sure the currently computed values, all of them,
  // are stored in memory for potential retrieval from the 
  // outside.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fPredDriver );
  ofstream o( fPredDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"Pred.h\"" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "#include <CppAD/CppAD.h>" << endl;
  o << "#include <spkpred/PredBase.h>" << endl;
  o << "#include <vector>" << endl;
  o << "#include <iostream>" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "using namespace std;" << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   bool ok = true;" << endl;
  o << "   DataSet< CppAD::AD<double> > set;" << endl;
  o << "   Pred< CppAD::AD<double> > pred( &set );" << endl;
  o << "   const int who         = 0;" << endl;
  o << "   const int n           = set.getN()[0]; // #of measurements" << endl;
  o << "   const int thetaLen    = " << thetaLen << ";" << endl;
  o << "   const int etaLen      = " << etaLen << ";" << endl;
  o << "   const int epsLen      = " << epsLen << ";" << endl;
  o << "   const int thetaOffset = 0;" << endl;
  o << "   const int etaOffset   = thetaLen;" << endl;
  o << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  o << "   const int fOffset     = 0;" << endl;
  o << "   const int yOffset     = n;" << endl;
  o << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  o << "   vector< CppAD::AD<double> > depVar( n*2 );" << endl;
  o << "   fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  o << "   fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  o << "   const double C1 = 1.0;" << endl;
  o << "   const double C2 = 2.0;" << endl;
  //---------------------------------------------------------------------------------
  // A complete iteration over j
  //
  o << endl;
  o << "   double expectedF1[n];" << endl;
  o << "   double expectedY1[n];" << endl;
  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  o << "      indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
  o << "      indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  o << "      pred.eval( thetaOffset, thetaLen," << endl;
  o << "                 etaOffset,   etaLen," << endl;
  o << "                 epsOffset,   epsLen ," << endl;
  o << "                 fOffset,     n, " << endl;
  o << "                 yOffset,     n, " << endl;
  o << "                 who, j, " << endl;
  o << "                 indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "      double actualF = CppAD::Value(depVar[ fOffset + j ]);"  << endl;
  o << "      expectedF1[j]   = CppAD::Value(indepVar[thetaOffset+0] + indepVar[thetaOffset+1] * set.data[who]->" << strTIME << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF1[j], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "      double actualY = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  o << "      expectedY1[j]   = expectedF1[j] + CppAD::Value( indepVar[etaOffset+0] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedY1[j], actualY );" << endl;
  o << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF1[j], set.data[who]->" << strPRED << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF1[j], set.data[who]->" << strF << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedY1[j], set.data[who]->" << strY<< "[j] );" << endl;
  o << "   }" << endl;
  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  //---------------------------------------------------------------------------------
  // Incomplete iteration over j
  //
  o << "   assert( n>1 );" << endl;
  o << "   double expectedF2[n];" << endl;
  o << "   double expectedY2[n];" << endl;
  o << "   for( int j=0; j<1; j++ )" << endl;
  o << "   {" << endl;
  o << "      indepVar[thetaOffset+0] = C2*j; // theta(0)" << endl;
  o << "      indepVar[etaOffset  +0] = C2*j; // eta(0)" << endl;
  o << "      pred.eval( thetaOffset, thetaLen," << endl;
  o << "                 etaOffset,   etaLen," << endl;
  o << "                 epsOffset,   epsLen ," << endl;
  o << "                 fOffset,     n, " << endl;
  o << "                 yOffset,     n, " << endl;
  o << "                 who, j, " << endl;
  o << "                 indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "      double actualF = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  o << "      expectedF2[j]  = CppAD::Value(indepVar[thetaOffset+0] + indepVar[thetaOffset+1] * set.data[who]->";
  o << strTIME << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF2[j], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  o << "      expectedY2[j] = expectedF2[j] + CppAD::Value( indepVar[etaOffset+0] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedY2[j], actualY );" << endl;
  o << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF1[j], set.data[who]->" << strPRED << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedF1[j], set.data[who]->" << strF << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedY1[j], set.data[who]->" << strY << "[j] );" << endl;
  o << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  o << "   return !ok;" << endl;
  o << "}" << endl;
  o.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s %s %s", fPredDriver_cpp, fPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fPredDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withoutIDTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fDriver );
  int  exitcode      = 0;
  char command[256];
  sprintf( command, "make -f %s test", fMakefile );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fFitDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s > %s", fDriver, fTraceOut );

  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for convergence problem <%d>!", fDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode == 2 )
    {
      char message[256];
      sprintf( message, "%s failed due to inproper file access permission <%d>!", fDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode > 2 )
    {
      char message[256];
      sprintf( message, 
               "%s failed for reasons other than convergence propblem or access permission <%d>!", 
               fDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
     }

  if( rename( fReportML, fSavedReportML ) != 0 )
    {
      char message[256];
      sprintf( message, 
               "Failed to rename %s to %s!",
               fReportML, fSavedReportML );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withoutIDTest::testReportML()
{
  const double scale = 0.05;

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Parse the generated reportML document.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  xercesc::XercesDOMParser *reportParser = new xercesc::XercesDOMParser;
  reportParser->setValidationScheme( XercesDOMParser::Val_Auto );
  reportParser->setDoNamespaces( true );
  reportParser->setDoSchema( true );
  reportParser->setValidationSchemaFullChecking( true );
  reportParser->setCreateEntityReferenceNodes( true );
  
  try{
    reportParser->parse( fSavedReportML );
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fReportML, XMLString::transcode(e.getMessage() ) );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      
      XMLCh errText[MAXCHARS + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, MAXCHARS))
	{
          XMLPlatformUtils::Terminate();
          char buf[MAXCHARS + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fReportML, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fReportML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

  report = reportParser->getDocument();
  CPPUNIT_ASSERT( report );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify if any error was caught during the runtime.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *error_messages = report->getElementsByTagName( X_ERROR_MESSAGES );
  CPPUNIT_ASSERT( error_messages->getLength() == 0 );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the objective value.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double obj_out = 0.0;
  DOMNodeList * objOut_list = report->getElementsByTagName( XMLString::transcode("ind_obj_out" ) );
  if( objOut_list->getLength() > 0 )
    {
      DOMElement* objOut = dynamic_cast<DOMElement*>( objOut_list->item(0) );
      DOMNodeList* value_list = objOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( 1, n );
      obj_out = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );      
      //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale * nm_obj );
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for theta
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double theta_out[thetaLen];
  DOMNodeList * thetaOut_list = report->getElementsByTagName( XMLString::transcode("theta_out" ) );
  if( thetaOut_list->getLength() > 0 )
    {
      DOMElement* thetaOut = dynamic_cast<DOMElement*>( thetaOut_list->item(0) );
      DOMNodeList* value_list = thetaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( thetaLen, n );
      for( int i=0; i<n; i++ )
	{
	  theta_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], scale * nm_theta[i] );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for Omega
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double omega_out[omegaOrder];
  DOMNodeList * omegaOut_list = report->getElementsByTagName( XMLString::transcode("omega_out" ) );
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list = omegaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
      for( int i=0; i<+n; i++ )
	{
	  omega_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], scale * nm_omega[i] );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Grab a pointer to the top of "ind_stat_result" sub-tree.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *ind_analysis_result = report->getElementsByTagName( X_IND_ANALYSIS_RESULT );
  CPPUNIT_ASSERT( ind_analysis_result->getLength() == 1 );
  DOMElement *ind_stat_result = dynamic_cast<DOMElement*>( ind_analysis_result->item( 0 ) );
  CPPUNIT_ASSERT( ind_stat_result != NULL );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the standard error of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> se_val;
  DOMNodeList * se_list = ind_stat_result->getElementsByTagName( X_IND_STDERROR_OUT );
  if( se_list->getLength() == 1 )
    {
      DOMElement * se = dynamic_cast<DOMElement*>( se_list->item(0) );
      CPPUNIT_ASSERT( se != NULL );
      DOMNodeList * value_list = se->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      se_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  se_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "se[%d] = %f\n", i, se_val[i] );
	//CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_stderr[i], se_val[i], scale * nm_stderr[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the covariance of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cov_val;
  vector<double> inv_cov_val;
  int covLen = series(1,1,omegaOrder+thetaLen);
  DOMNodeList * cov_list =ind_stat_result->getElementsByTagName(  X_IND_COVARIANCE_OUT ) ;
  if( cov_list->getLength() == 1 )
    {
      DOMElement * cov = dynamic_cast<DOMElement*>( cov_list->item(0) );
      CPPUNIT_ASSERT( cov != NULL );
      DOMNodeList * value_list = cov->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      cov_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cov_val[i] = atof( XMLString::transcode( x_val ) );
	CPPUNIT_ASSERT_EQUAL( covLen, n );

	//printf( "cov[%d] = %f\n", i, cov_val[i] );
        //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cov[i], cov_val[i], scale * nm_cov[i] );
      }
    }
  DOMNodeList * invcov_list =ind_stat_result->getElementsByTagName(  X_IND_INVERSE_COVARIANCE_OUT ) ;
  if( invcov_list->getLength() == 1 )
    {
      DOMElement * invcov = dynamic_cast<DOMElement*>( invcov_list->item(0) );
      CPPUNIT_ASSERT( invcov != NULL );
      DOMNodeList * value_list = invcov->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      inv_cov_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  inv_cov_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "inv_cov[%d] = %f\n", i, inv_cov_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the confidence interval for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> ci_val;
  DOMNodeList * ci_list =ind_stat_result->getElementsByTagName(  X_IND_CONFIDENCE_OUT ) ;
  if( ci_list->getLength() == 1 )
    {
      DOMElement * ci = dynamic_cast<DOMElement*>( ci_list->item(0) );
      CPPUNIT_ASSERT( ci != NULL );
      DOMNodeList * value_list = ci->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      ci_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  ci_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "ci[%d] = %f\n", i, ci_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the coefficient of variation for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cv_val;
  DOMNodeList * cv_list =ind_stat_result->getElementsByTagName(  X_IND_COEFFICIENT_OUT ) ;
  if( cv_list->getLength() == 1 )
    {
      DOMElement * cv = dynamic_cast<DOMElement*>( cv_list->item(0) );
      CPPUNIT_ASSERT( cv != NULL );
      DOMNodeList * value_list = cv->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      cv_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cv_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "cv[%d] = %f\n", i, cv_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the correlation matrix for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cor_val;
  DOMNodeList * cor_list =ind_stat_result->getElementsByTagName(  X_IND_CORRELATION_OUT ) ;
  if( cor_list->getLength() == 1 )
    {
      DOMElement * cor = dynamic_cast<DOMElement*>( cor_list->item(0) );
      CPPUNIT_ASSERT( cor != NULL );
      DOMNodeList * value_list = cor->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      cor_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cor_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "cor[%d] = %f\n", i, cor_val[i] );
      }
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * ind_withoutIDTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_withoutIDTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testNonmemPars_h", 
	 &ind_withoutIDTest::testNonmemPars_h ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testIndDataClass", 
	 &ind_withoutIDTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testDataSetClass", 
	 &ind_withoutIDTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testPredClass", 
	 &ind_withoutIDTest::testPredClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testDriver", 
	 &ind_withoutIDTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testReportML", 
	 &ind_withoutIDTest::testReportML ) );

  return suiteOfTests;
}

