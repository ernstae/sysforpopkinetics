#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_onlysim_NonmemTranslatorTest.h"
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
// Standard Deviation 	     0.884796396144373 		
// R-Squared 	     0.999993745883712 		
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
  const unsigned int maxChars = 2047;

  char description[] = "Individual Analysis - only simulation";
  char fPrefix[128];
  char fData[128];
  char fSource[128];
  char fIndDataDriver[128];
  char fIndDataDriver_cpp[128];
  char fDataSetDriver[128];
  char fDataSetDriver_cpp[128];
  char fPredDriver[128];
  char fPredDriver_cpp[128];
  char fDriver[128];
  char fDriver_cpp[128];
  char fReport_xml[128];

  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = false;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const char *strID   = "ID";
  const char *strTIME = "TIME";
  const char *strDV   = "DV";
  const char *strCP   = "CP";
  const char *strMDV  = "MDV";
  const char *label[] = { strID, strDV, strTIME, strMDV };
  map<const char*, const char*> label_alias;
  int nLabels         = 4;

  //============================================
  // <Data Set>
  //
  //   ID     DV=CP    TIME    MDV
  /*
        1       0.1     0.2      0
        1     338.8   337.4      0
        1     118.1   118.2      0
        1     888.0   884.6      0
        1       9.2    10.1      0
        1     228.1   226.5      0
        1     668.5   666.3      0
        1     998.5   996.3      0
        1     449.1   448.6      0
        1     778.9   777.0      0
        1     559.2   558.2      0
        1       0.3     0.4      0
        1       0.1     0.6      0 
        1     778.1   775.5      0
        1     668.8   666.9      0
        1     339.3   338.0      0
        1     448.9   447.5      0
        1      10.8    11.6      0
        1     557.7   556.0      0
        1     228.3   228.1      0
        1     998.0   995.8      0
        1     888.0   887.6      0
        1     119.6   120.2      0
        1       0.3     0.3      0
        1       0.6     0.3      0
        1     557.6   556.8      0
        1     339.3   339.1      0
        1     888.0   887.2      0
        1     998.5   999.0      0
        1     778.9   779.0      0
        1      10.2    11.1      0
        1     117.6   118.3      0
        1     228.9   229.2      0
        1     668.4   669.1      0
        1     449.2   448.9      0
        1       0.2     0.5      0
   */
  //============================================
  const int    nRecords  =  36;
  const int    nItems    =  4;
  const double record0[] = { 1, 0.1, 0.2, 0 };
  const double record1[] = { 1, 338.8, 337.4, 0 };
  const double record2[] = { 1, 118.1, 118.2, 0 };
  const double record3[] = { 1, 888.0, 884.6, 0 };
  const double record4[] = { 1, 9.2, 10.1, 0 };
  const double record5[] = { 1, 228.1, 226.5, 0 };
  const double record6[] = { 1, 668.5, 666.3, 0 };
  const double record7[] = { 1, 998.5, 996.3, 0 };
  const double record8[] = { 1, 449.1, 448.6, 0 };
  const double record9[] = { 1, 778.9, 777.0, 0 };
  const double record10[] = { 1, 559.2, 558.2, 0 };
  const double record11[] = { 1, 0.3, 0.4, 0 };
  const double record12[] = { 1, 0.1, 0.6, 0 };
  const double record13[] = { 1, 778.1, 775.5, 0 };
  const double record14[] = { 1, 668.8, 666.9, 0 };
  const double record15[] = { 1, 339.3, 338.0, 0 };
  const double record16[] = { 1, 448.9, 447.5, 0 };
  const double record17[] = { 1, 10.8, 11.6, 0 };
  const double record18[] = { 1, 557.7, 556.0, 0 };
  const double record19[] = { 1, 228.3, 228.1, 0 };
  const double record20[] = { 1, 998.0, 995.8, 0 };
  const double record21[] = { 1, 888.8, 887.6, 0 };
  const double record22[] = { 1, 119.6, 120.2, 0 };
  const double record23[] = { 1, 0.3, 0.3, 0 };
  const double record24[] = { 1, 0.6, 0.3, 0 };
  const double record25[] = { 1, 557.6, 556.8, 0 };
  const double record26[] = { 1, 339.3, 339.1, 0 };
  const double record27[] = { 1, 888.0, 887.2, 0 };
  const double record28[] = { 1, 998.5, 999.0, 0 };
  const double record29[] = { 1, 778.9, 779.0, 0 };
  const double record30[] = { 1, 10.2, 11.1, 0 };
  const double record31[] = { 1, 117.6, 118.3, 0 };
  const double record32[] = { 1, 228.9, 229.2, 0 };
  const double record33[] = { 1, 668.4, 669.1, 0 };
  const double record34[] = { 1, 449.2, 448.9, 0 };
  const double record35[] = { 1, 0.2, 0.5, 0 };

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
  const bool   theta_fix[ thetaLen ]   = { false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal --- 1 dimensional! --- matrix.
  //============================================
  const int    omegaDim                = 1;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
  const int    omegaOrder              = 1;
  const double omega_in [ omegaOrder ] = { 1.0 };
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
  const bool ind_stderr         = false;
  const bool ind_coefficient    = false;
  const bool ind_confidence     = false;
  const bool ind_covariance     = false;
  const bool ind_inv_covariance = false;
  const bool ind_correlation    = false;

  //============================================
  // Make a request on data simulation.
  //============================================
  const bool isSimulate         = true;
  const int  seed               = 1;
  const bool onlySimulation     = true;
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
  const char PRED[] = "b0 = THETA(1)\nb1 = THETA(2)\nx = TIME\nF = b0 + b1 * x\nY = F + ETA(1)\n";


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
  XMLCh * X_DATA_LABELS;
  XMLCh * X_ROW;
  XMLCh * X_ROWS;
  XMLCh * X_COLUMNS;
  XMLCh * X_NAME;
  XMLCh * X_POSITION;
  XMLCh * X_LABEL;
};

void ind_onlysim_NonmemTranslatorTest::setUp()
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
      char buf[maxChars + 1];
      sprintf( buf, "Error during Xerces-c initialization.\nException message: %s.\n", 
               XMLString::transcode( toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

  okToClean = false;

  sprintf( fDriver, "driver" );
  sprintf( fDriver_cpp, "driver.cpp" );
  sprintf( fReport_xml, "result.xml" );

  label_alias[strID]   = NULL;
  label_alias[strTIME] = NULL;
  label_alias[strDV]   = strCP;
  label_alias[strMDV]  = NULL;

  record[0]  = record0;
  record[1]  = record1;
  record[2]  = record2;
  record[3]  = record3;
  record[4]  = record4;
  record[5]  = record5;
  record[6]  = record6;
  record[7]  = record7;
  record[8]  = record8;
  record[9]  = record9;
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
  X_ROWS                       = XMLString::transcode( "rows" );
  X_COLUMNS                    = XMLString::transcode( "columns" );
  X_DATA_LABELS                = XMLString::transcode( "data_labels" );
  X_ROW                        = XMLString::transcode( "row" );
  X_POSITION                   = XMLString::transcode( "position" );
  X_LABEL                      = XMLString::transcode( "label" );
  X_NAME                       = XMLString::transcode( "name" );

  createDataML();
  createSourceML();
  parse();
}
void ind_onlysim_NonmemTranslatorTest::tearDown()
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
  XMLString::release( &X_ROWS );
  XMLString::release( &X_COLUMNS );
  XMLString::release( &X_DATA_LABELS );
  XMLString::release( &X_ROW );
  XMLString::release( &X_POSITION );
  XMLString::release( &X_NAME );
  
  if( okToClean )
    {
      remove( fData );
      remove( fSource );
      remove( fIndDataDriver );
      remove( fIndDataDriver_cpp );
      remove( fDataSetDriver );
      remove( fDataSetDriver_cpp );
      remove( fPredDriver );
      remove( fPredDriver_cpp );
      remove( fDriver );
      remove( fDriver_cpp );
      remove( "IndData.h" );
      remove( "DataSet.h" );
      remove( "Pred.h" );
      remove( "predEqn.cpp" );
      remove( "generatedMakefile" );
      remove( "result.xml" );
    }
  
  XMLPlatformUtils::Terminate();

}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void ind_onlysim_NonmemTranslatorTest::createDataML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sprintf( fPrefix, "indOnlySim" );
  sprintf( fData, "%s_dataML.xml", fPrefix );
  ofstream oData( fData );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nLabels << "\" rows=\"" << nRecords + 1 << "\">" << endl;
  oData << "<description>" << description << "</description>" << endl;
  oData << "<row position=\"1\">" << endl;
  for( int i=0; i<nItems; i++ )
    {
      oData << "<value type=\"string\">" << label[i] << "</value>" << endl;
    }
  oData << "</row>" << endl;
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
    dataParser->parse( fData );
    data = dataParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fData, XMLString::transcode(e.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      XMLCh errText[maxChars + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
	{
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fData, e.code, XMLString::transcode(errText) );
	  
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fData );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
 
void ind_onlysim_NonmemTranslatorTest::createSourceML()
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
  sprintf( fSource, "%s_sourceML.xml", fPrefix );
  ofstream oSource( fSource );
  CPPUNIT_ASSERT( oSource.good() );

  oSource << "<spksource>" << endl;
  oSource << "<nonmem>" << endl;
  oSource << "<constraint>" << endl;

  // default: is_eta_out=no, is_restart=yes
  oSource << "<ind_analysis ";
  oSource << "mitr=\"" << mitr << "\" ";
  oSource << "is_estimation=\"" << (isEstimate? "yes" : "no") << "\">" << endl;

  oSource << "<description>"  << description << "</description>" << endl;

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
  oSource << "is_stderror_out=\""           << (ind_stderr? "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_confidence_out=\""         << (ind_confidence? "yes":"no") << "\" ";
  oSource << "is_coefficient_out=\""        << (ind_coefficient? "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"";
      oSource << " only_simulation=\"" << (onlySimulation? "yes":"no") << "\"";
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
    sourceParser->parse( fSource );
    source = sourceParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fSource, XMLString::transcode(e.getMessage() ) );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
     }
  catch( const DOMException& e )
    {
      
      XMLCh errText[maxChars + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
	{
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fSource, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fSource );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Translation
// 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ind_onlysim_NonmemTranslatorTest::parse()
{
  //
  //============================================
  // Instanciate a NonmemTranslator object, 
  // passing the pointers to the sourceML 
  // document tree and the dataML document tree.
  //============================================
  NonmemTranslator xlator( source, data );

  //============================================
  // Parse the dataML document
  //============================================
  xlator.parseData();
  SymbolTable *table = xlator.getSymbolTable();

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

  Symbol * mdv   = table->findi( strMDV );
  CPPUNIT_ASSERT( mdv != Symbol::empty() );
  Symbol * cp   = table->findi( strCP );
  CPPUNIT_ASSERT( cp != Symbol::empty() );
}

void ind_onlysim_NonmemTranslatorTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  int  exitcode      = 0;
  char command[256];
  sprintf( command, "make -f generatedMakefile test" );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fDriver );
  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematically, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for convergence problem <%d>!", fDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, true );
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
      sprintf( message, "%s failed for reasons other than convergence propblem or access permission <%d>!", fDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, true );
     }
}
void ind_onlysim_NonmemTranslatorTest::testReportML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the results
  // In particular, for this test, verify the PRED defined 
  // variable values reported in the XML if they
  // match the values evaluated at THETA(initial) and ETA(simulated).
  // Also, check if WRES are all zero.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  const double eps = 1.0/10.0;

  xercesc::XercesDOMParser *reportParser = new xercesc::XercesDOMParser;
  reportParser->setValidationScheme( XercesDOMParser::Val_Auto );
  reportParser->setDoNamespaces( true );
  reportParser->setDoSchema( true );
  reportParser->setValidationSchemaFullChecking( true );
  reportParser->setCreateEntityReferenceNodes( true );
  
  try{
    reportParser->parse( fReport_xml );
    report = reportParser->getDocument();
    CPPUNIT_ASSERT_MESSAGE( "The XMLDocument pointer is null.", report!=NULL );
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred while parsing %s.\n   Message: %s\n",
	       fReport_xml, XMLString::transcode(e.getMessage() ) );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      
      XMLCh errText[maxChars + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
	{
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error while parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fReport_xml, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred while parsing %s.\n", fReport_xml );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  DOMNodeList *error_messages = report->getElementsByTagName( X_ERROR_MESSAGES );
  if( error_messages->getLength() > 0 )
    CPPUNIT_ASSERT( error_messages->getLength() == 0 );

  /*
  DOMNodeList * thetaOut_list = report->getElementsByTagName( XMLString::transcode("theta_out" ) );
  DOMNodeList * omegaOut_list = report->getElementsByTagName( XMLString::transcode("omega_out" ) );
  double theta_out[ thetaLen ];
  double omega_out[ omegaOrder ];

  CPPUNIT_ASSERT( thetaOut_list->getLength() == 1 );
    {
      DOMElement* thetaOut = dynamic_cast<DOMElement*>( thetaOut_list->item(0) );
      DOMNodeList* value_list = thetaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( thetaLen, n );
      for( int i=0; i<n; i++ )
	{
	  theta_out[i] = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );
	  printf( "theta[%d] = %f\n", i, theta_out[i] );
	  CPPUNIT_ASSERT_EQUAL( theta_in[i], theta_out[i] );
	}
    }
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list = omegaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
      for( int i=0; i<omegaOrder; i++ )
	{
	  omega_out[i] = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );
	  printf( "omegaOut[%d] = %f\n", i, omega_out );
	  CPPUNIT_ASSERT_EQUAL( omega_in[i], omega_out[i] );
	}
    }
  */
  //
  // <presentation_data rows="xxx" columns="yyy">
  //    <data_labels>
  //       <label name="x"/>
  //       <label name="y"/>
  //       ...
  //    </data_labels>
  //    <row position="1">
  //       <value ref="x">1.0</value>
  //       <value ref="y">2.0</value>
  //       ...
  //    </row>
  //    ...
  // </presentation>
  //
  DOMNodeList * presentation_data_list = report->getElementsByTagName( X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data_list->getLength() == 1 );
  DOMElement * presentation_data = dynamic_cast<DOMElement*>( presentation_data_list->item(0) );

  DOMNodeList * data_labels_list = presentation_data->getElementsByTagName( X_DATA_LABELS );
  CPPUNIT_ASSERT( data_labels_list->getLength() == 1 );
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );

  // ID, DV, TIME, MDV, WRES, RES, THETA, ETA, PRED, F, Y, b0, b1, x
  map<string, bool> itemlist;

  itemlist[ "ID"       ] = false;
  itemlist[ "DV"       ] = false; 
  itemlist[ "SIMDV"    ] = false;
  itemlist[ "TIME"     ] = false;
  itemlist[ "MDV"      ] = false;
  itemlist[ "WRES"     ] = false;
  itemlist[ "RES"      ] = false;
  itemlist[ "THETA(1)" ] = false;
  itemlist[ "THETA(2)" ] = false;
  itemlist[ "ETA(1)"   ] = false;
  itemlist[ "PRED"     ] = false;
  itemlist[ "F"        ] = false;
  itemlist[ "Y"        ] = false;
  itemlist[ "b0"       ] = false;
  itemlist[ "b1"       ] = false;
  itemlist[ "x"        ] = false;

  map<string, bool>::iterator itr;
  
  int nPresentItems = 16;
  DOMNodeList * label_list = data_labels->getElementsByTagName( X_LABEL );
  CPPUNIT_ASSERT_EQUAL( nPresentItems, static_cast<int>( label_list->getLength() ) );

  // Go through <label>s in <data_label> and see if they match the list of expected labels.
  for( int i=0; i<nPresentItems; i++ )
    {
      DOMElement * label = dynamic_cast<DOMElement*>( label_list->item(i));
      CPPUNIT_ASSERT( label->hasAttribute( X_NAME ) );
      const XMLCh * x_name = label->getAttribute( X_NAME );
      CPPUNIT_ASSERT( XMLString::stringLen( x_name ) > 0 );
      string name( XMLString::transcode( x_name ) );

      itr = itemlist.find( name );
      CPPUNIT_ASSERT_MESSAGE( name, itr != itemlist.end() );
      itr->second = true;
    }

  itr = itemlist.begin();
  for( int i=0; i<nPresentItems; i++, itr++ )
    {
      CPPUNIT_ASSERT_MESSAGE( itr->first, itr->second );
    }

  


  
  okToClean = true;
}

CppUnit::Test * ind_onlysim_NonmemTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_onlysim_NonmemTranslatorTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_onlysim_NonmemTranslatorTest>(
         "testDriver", 
	 &ind_onlysim_NonmemTranslatorTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_onlysim_NonmemTranslatorTest>(
         "testReportML", 
	 &ind_onlysim_NonmemTranslatorTest::testReportML ) );

  return suiteOfTests;
}

