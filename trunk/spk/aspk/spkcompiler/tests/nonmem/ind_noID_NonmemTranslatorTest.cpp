#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_noID_NonmemTranslatorTest.h"
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

namespace{
  const unsigned int maxChars = 2047;
  bool okToClear = false;

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
  const bool isEstimate = true;

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
  //   (ID)   DV=CP    TIME    MDV
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
  const bool ind_coefficent     = true;
  const bool ind_confidence     = true;
  const bool ind_covariance     = true;
  const bool ind_inv_covariance = true;
  const bool ind_correlation    = true;

  //============================================
  // Make a request on data simulation.
  //============================================
  const bool isSimulate         = false;
  const int  seed               = 1;
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

void ind_noID_NonmemTranslatorTest::setUp()
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

  createDataML();
  createSourceML();
  parse();

}
void ind_noID_NonmemTranslatorTest::tearDown()
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
  /*  
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
  */  
  XMLPlatformUtils::Terminate();

}

//******************************************************************************
//
// Create a dataML containing a data set with no ID field.
//
//******************************************************************************
void ind_noID_NonmemTranslatorTest::createDataML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with no ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sprintf( fPrefix, "indWithNoID" );
  sprintf( fData, "%s_dataML.xml", fPrefix );
  ofstream oData( fData );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nItems-1 << "\" rows=\"" << nRecords+1 << "\">" << endl;
  oData << "<description>" << endl;
  oData << "The data set (with no ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;
  oData << "<row position=\"1\">" << endl;
  for( int i=1; i<nItems; i++ )
    {
      oData << "<value type=\"string\">" << label[i] << "</value>" << endl;
    }
  oData << "</row>" << endl;
  for( int i=0; i<nRecords; i++ )
    {
      oData << "<row position=\"" << i+2 << "\">" << endl;
      for( int j=1; j<nItems; j++ )
	oData << "<value type=\"numeric\">" << record[i][j] << "</value>" << endl;
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
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Create a sourceML document
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ind_noID_NonmemTranslatorTest::createSourceML()
{  
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

  oSource << "<data_labels>" << endl;
  for( int i=1; i<nItems; i++ )
    {
      oSource << "<label name=\"" << label[i] << "\"";
      if( label_alias[ label[i] ] != NULL )
	{
	  oSource << " synonym=\"" << label_alias[label[i]] << "\"";
	}
      oSource << "/>" << endl;
    }
  oSource << "</data_labels>" << endl;

  oSource << "<theta length=\"" << thetaLen << "\">" << endl;
  oSource << "<in>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      oSource << "<value";
      if( theta_fix[i] )
	oSource << " fixed=\"yes\"";
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
      if( omega_fix[i] )
	oSource << " fixed=\"yes\"";
      oSource << ">" << omega_in[i] << "</value>" << endl;
    }
  oSource << "</in>" << endl;
  oSource << "</omega>" << endl;

  oSource << "<ind_stat ";
  oSource << "is_standarderr_out=\""        << (ind_stderr?         "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance?     "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation?    "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
    }
  oSource << "</ind_analysis>" << endl;
  oSource << "</constraint>" << endl;

  oSource << "<model>" << endl;
  oSource << "<pred>" << endl;
  oSource << "   " << PRED << endl;
  oSource << "</pred>" << endl;
  oSource << "</model>" << endl;
      
  oSource << "<presentation>" << endl;
  oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
  oSource << "<column label=\"" << strTIME  << "\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(1)\" appearance_order=\"2\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(3)\" appearance_order=\"4\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(2)\" appearance_order=\"3\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<table header=\"every\">" << endl;
  oSource << "<column label=\"" << strTIME << "\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"" << strCP   << "\" appearance_order=\"2\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<scatterplot>" << endl;
  oSource << "<x label=\"" << strTIME << "\"/>" << endl;
  oSource << "<y label=\"" << strPRED << "\"/>" << endl;
  oSource << "</scatterplot>" << endl;
  oSource << "</presentation>" << endl;
      
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
void ind_noID_NonmemTranslatorTest::parse()
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
  Symbol * mdv   = table->findi( strMDV );
  CPPUNIT_ASSERT( mdv != Symbol::empty() );

  //============================================
  // Parse the sourceML document
  //============================================
  xlator.parseSource();

  Symbol * cp   = table->findi( strCP );
  CPPUNIT_ASSERT( cp != Symbol::empty() );
}
void ind_noID_NonmemTranslatorTest::testIndDataClass()
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
  sprintf( fIndDataDriver, "%s_IndDataDriver", fPrefix );
  sprintf( fIndDataDriver_cpp, "%s_IndDataDriver.cpp", fPrefix );
  ofstream oIndDataDriver( fIndDataDriver_cpp );
  CPPUNIT_ASSERT( oIndDataDriver.good() );

  oIndDataDriver << "#include <string>" << endl;
  oIndDataDriver << "#include <iostream>" << endl;
  oIndDataDriver << "#include <sys/signal.h>" << endl;
  oIndDataDriver << "#include \"IndData.h\"" << endl;
  oIndDataDriver << "using namespace std;" << endl;
  oIndDataDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oIndDataDriver << "   if( actual != expected ) \\" << endl;
  oIndDataDriver << "   { \\" << endl;
  oIndDataDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oIndDataDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oIndDataDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oIndDataDriver << "      raise( SIGABRT ); \\" << endl;
  oIndDataDriver << "   } " << endl;
  oIndDataDriver << endl;
  oIndDataDriver << "int main()" << endl;
  oIndDataDriver << "{" << endl;
  oIndDataDriver << "   const int n = " << nRecords << ";" << endl;
  oIndDataDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oIndDataDriver << "   const int etaLen = " << etaLen << ";" << endl;
  oIndDataDriver << "   vector<char*> a_id(n);" << endl;
  oIndDataDriver << "   vector<double> a_time(n);" << endl;
  oIndDataDriver << "   vector<double> a_dv(n);" << endl;
  oIndDataDriver << "   vector<double> a_mdv(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    oIndDataDriver << "   a_id["   << i << "] = \"" << record[i][0] << "\";" << endl;
    oIndDataDriver << "   a_dv["   << i << "] = "   << record[i][1] << ";" << endl;
    oIndDataDriver << "   a_time[" << i << "] = "   << record[i][2] << ";" << endl;
    oIndDataDriver << "   a_mdv["  << i << "] = "   << record[i][3] << ";" << endl;
  }

  oIndDataDriver << "   IndData<double> A( n, a_id, a_dv, a_time, a_mdv );" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      oIndDataDriver << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strCP   << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strDV   << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strTIME << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", A." << strMDV  << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to Pred::eval().
      oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "["<< i << "].size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   A." << strETA   << "[" << i << "].size() );" << endl;
    }
  

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strWRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strPRED << ".size() );" << endl;

  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strF << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strY << ".size() );" << endl;
  oIndDataDriver << "}" << endl;

  oIndDataDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fIndDataDriver_cpp, fIndDataDriver );
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
void ind_noID_NonmemTranslatorTest::testDataSetClass()
{  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sprintf( fDataSetDriver, "%s_DataSetDriver", fPrefix );
  sprintf( fDataSetDriver_cpp, "%s_DataSetDriver.cpp", fPrefix );
  ofstream oDataSetDriver( fDataSetDriver_cpp );
  CPPUNIT_ASSERT( oDataSetDriver.good() );

  oDataSetDriver << "#include <string>" << endl;
  oDataSetDriver << "#include <iostream>" << endl;
  oDataSetDriver << "#include <sys/signal.h>" << endl;
  oDataSetDriver << "#include \"DataSet.h\"" << endl;
  oDataSetDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oDataSetDriver << "   if( actual != expected ) \\" << endl;
  oDataSetDriver << "   { \\" << endl;
  oDataSetDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oDataSetDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oDataSetDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oDataSetDriver << "      raise( SIGABRT ); \\" << endl;
  oDataSetDriver << "   } " << endl;
  oDataSetDriver << endl;  oDataSetDriver << "using namespace std;" << endl;
  oDataSetDriver << "int main()" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   const int n = " << nRecords << ";" << endl;
  oDataSetDriver << "   DataSet<double> set;" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", set.data[0]->" << strMDV  << "[" << i << "] );" << endl;
    }

  oDataSetDriver << "for( int j=0; j<n; j++ )" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( " << thetaLen << ", set.data[0]->" << strTHETA << "[j].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( " << etaLen   << ", set.data[0]->" << strETA << "[j].size() );" << endl;
  oDataSetDriver << "}" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oDataSetDriver << "MY_ASSERT_EQUAL( n, set.data[0]->" << strRES  << ".size() );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, set.data[0]->" << strWRES << ".size() );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED << ".size() );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, set.data[0]->" << strF    << ".size() );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, set.data[0]->" << strY    << ".size() );" << endl;
  oDataSetDriver << endl;

  oDataSetDriver << "return 0;" << endl;
  oDataSetDriver << "}" << endl;
  
  oDataSetDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fDataSetDriver_cpp, fDataSetDriver );
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

CppUnit::Test * ind_noID_NonmemTranslatorTest::suite()
 {
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_noID_NonmemTranslatorTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_noID_NonmemTranslatorTest>(
         "testIndDataClass", 
	 &ind_noID_NonmemTranslatorTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_noID_NonmemTranslatorTest>(
         "testDataSetClass", 
	 &ind_noID_NonmemTranslatorTest::testDataSetClass ) );

  return suiteOfTests;
}

