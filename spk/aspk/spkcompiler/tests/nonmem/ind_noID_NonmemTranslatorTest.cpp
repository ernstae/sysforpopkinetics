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
  const char approx[]   = "foce";

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const char *strID   = "ID";
  const char *strTIME = "TIME";
  const char *strDV   = "DV";
  const char *strCP   = "CP";
  const char *strMDV  = "MDV";
  const char *label[] = { strID, strTIME, strDV, strMDV };
  map<const char*, const char*> label_alias;
  int nLabels         = 4;

  //============================================
  // <Data Set>
  //
  //   ID     TIME     DV=CP    MDV
  //    1      0.0      0.0      0
  //    1      1.0     10.0      0
  //    1      2.0     20.0      0
  //    1      2.5     30.0      0
  //============================================
  /*
  const int    nRecords  =  4;
  const int    nItems    =  4;
  const double record0[] = { 1, 0.0,  0.0, 0 };
  const double record1[] = { 1, 1.0, 10.0, 0 };
  const double record2[] = { 1, 2.0, 20.0, 0 };
  const double record3[] = { 1, 3.0, 30.0, 0 }; 
  */
  const int    nRecords  =  3;
  const int    nItems    =  4;
  const double record0[] = { 1, 0.0, 1.8, 0 };
  const double record1[] = { 1, 1.0, 2.0, 0 };
  const double record2[] = { 1, 2.0, 2.2, 0 };

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
  const int    thetaLen = 1;
  const double theta_in [ thetaLen ]   = {  2.0 };
  const double theta_up [ thetaLen ]   = {  4.0 };
  const double theta_low[ thetaLen ]   = { -4.0 };
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
  const double omega_in[ omegaOrder ]  = { 2.0 };
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
  // The PRED model
  //============================================
  const char PRED[] = "F = THETA(1)\nY = EXP( ETA(1) )\n";

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

  record[0]  = record0;
  record[1]  = record1;
  record[2]  = record2;

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
  oSource << "<ind_analysis approximation=\"" << approx << "\" ";
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
  oIndDataDriver << "   vector<char*>  a_" << strID << "(n);" << endl;
  for( int j=0; j<nRecords; j++ )
    oIndDataDriver << "   a_" << strID << "[" << j << "] = \"" << record[j][0] << "\";" << endl;
  for( int j=1; j<nItems; j++ )
    {    
      oIndDataDriver << "   vector<double>  a_" << label[j] << "(n);" << endl;
      for( int i=0; i<nRecords; i++ )
	{
	  oIndDataDriver << "   a_" << label[j] << "[" << i << "] = " << record[i][j] << ";" << endl;
	}
    }

  oIndDataDriver << "   IndData<double> A( n";
  for( int i=0; i<nLabels; i++ )
    oIndDataDriver << ", a_" << label[i];
  oIndDataDriver << " );" << endl;

  for( int i=0; i<nRecords; i++ )
    {
      oIndDataDriver << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( "  << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( "  << record[i][2] << ", A." << strCP   << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( "  << record[i][2] << ", A." << strDV   << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( "  << record[i][3] << ", A." << strMDV  << "[" << i << "] );" << endl;
    }
  
  // There have to be placeholders for the current values of theta/eta for
  // each call to Pred::eval().
  oIndDataDriver << "   const int thetaLen   = " << thetaLen << ";" << endl;
  oIndDataDriver << "   const int etaLen     = " << etaLen << ";" << endl;
  for( int i=0; i<thetaLen; i++ )
    oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[" << i << "].size() );" << endl;
  for( int i=0; i<etaLen; i++ )
    oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[" << i << "].size() );" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strWRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strPRED << ".size() );" << endl;

  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strF << ".size() );" << endl;
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
  oDataSetDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oDataSetDriver << "   const int etaLen   = " << etaLen << ";" << endl;
  oDataSetDriver << "   DataSet<double> set;" << endl;
  for( int i=0; i<nRecords; i++ )
  {
    oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
    oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
    oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
    oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
    oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", set.data[0]->" << strMDV  << "[" << i << "] );" << endl;
  }

  for( int i=0; i<thetaLen; i++ )
    oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[0]->" << strTHETA << "[" << i << "].size() );" << endl;
  for( int i=0; i<etaLen; i++ )
    oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[0]->" << strETA   << "[" << i << "].size() );" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strRES  << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strWRES << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED << ".size() );" << endl;

  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF    << ".size() );" << endl;
  oDataSetDriver << endl;
  /*
  oDataSetDriver << "valarray<double> y;" << endl;
  oDataSetDriver << "valarray<int>    N;" << endl;
  oDataSetDriver << "int total = set.getMeasurements( y, N );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, total );" << endl;
  oDataSetDriver << "MY_ASSERT_EQUAL( n, N[0] );" << endl;
  oDataSetDriver << "for( int i=0; i<n; i++ )" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[i], y[i] );" << endl;
  oDataSetDriver << "}" << endl;
  */
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
void ind_noID_NonmemTranslatorTest::testPredClass()
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
  sprintf( fPredDriver, "%s_PredDriver", fPrefix );
  sprintf( fPredDriver_cpp, "%s_PredDriver.cpp", fPrefix );
  ofstream oPredDriver( fPredDriver_cpp );
  CPPUNIT_ASSERT( oPredDriver.good() );

  oPredDriver << "#include \"Pred.h\"" << endl;
  oPredDriver << "#include \"DataSet.h\"" << endl;
  oPredDriver << "#include <cppad/include/CppAD.h>" << endl;
  oPredDriver << "#include <spkpred/PredBase.h>" << endl;
  oPredDriver << "#include <sys/signal.h>" << endl;
  oPredDriver << "#include <vector>" << endl;
  oPredDriver << "using namespace std;" << endl;
  oPredDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oPredDriver << "   if( actual != expected ) \\" << endl;
  oPredDriver << "   { \\" << endl;
  oPredDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oPredDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oPredDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oPredDriver << "      raise( SIGABRT ); \\" << endl;
  oPredDriver << "   } " << endl;
  oPredDriver << "int main()" << endl;
  oPredDriver << "{" << endl;
  oPredDriver << "   bool ok = true;" << endl;
  oPredDriver << "   DataSet< CppAD::AD<double> > set;" << endl;
  oPredDriver << "   Pred< CppAD::AD<double> > pred( &set );" << endl;
  oPredDriver << "   const int who         = 0;" << endl;
  oPredDriver << "   const int n           = " << nRecords << "; // #of measurements" << endl;
  oPredDriver << "   const int thetaLen    = " << thetaLen << ";" << endl;
  oPredDriver << "   const int etaLen      = " << etaLen << ";" << endl;
  oPredDriver << "   const int epsLen      = " << epsLen << ";" << endl;
  oPredDriver << "   const int thetaOffset = 0;" << endl;
  oPredDriver << "   const int etaOffset   = thetaLen;" << endl;
  oPredDriver << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  oPredDriver << "   const int fOffset     = 0;" << endl;
  oPredDriver << "   const int yOffset     = n;" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > depVar( n*2 );" << endl;
  oPredDriver << "   fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  oPredDriver << "   fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  oPredDriver << "   const double C1 = 1.0;" << endl;
  oPredDriver << "   const double C2 = 2.0;" << endl;
  //---------------------------------------------------------------------------------
  // A complete iteration over j
  //
  oPredDriver << endl;
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      indepVar[thetaOffset+0] = C1*j; // theta(0)" << endl;
  oPredDriver << "      indepVar[etaOffset  +0] = C1*j; // eta(0)" << endl;
  oPredDriver << "      pred.eval( thetaOffset, thetaLen," << endl;
  oPredDriver << "                 etaOffset,   etaLen," << endl;
  oPredDriver << "                 epsOffset,   epsLen ," << endl;
  oPredDriver << "                 fOffset,     n, " << endl;
  oPredDriver << "                 yOffset,     n, " << endl;
  oPredDriver << "                 who, j, " << endl;
  oPredDriver << "                 indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualF   = CppAD::Value(depVar[ fOffset + j ]);"  << endl;
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0]);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = exp( CppAD::Value(indepVar[etaOffset+0]) );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY )" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strF << "[j] );" << endl;
  oPredDriver << "   }" << endl;
  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  //---------------------------------------------------------------------------------
  // Incomplete iteration over j
  //
  oPredDriver << "   assert( n>1 );" << endl;
  oPredDriver << "   for( int j=0; j<1; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      indepVar[thetaOffset+0] = C2*j; // theta(0)" << endl;
  oPredDriver << "      indepVar[etaOffset  +0] = C2*j; // eta(0)" << endl;
  oPredDriver << "      pred.eval( thetaOffset, thetaLen," << endl;
  oPredDriver << "                 etaOffset,   etaLen," << endl;
  oPredDriver << "                 epsOffset,   epsLen ," << endl;
  oPredDriver << "                 fOffset,     n, " << endl;
  oPredDriver << "                 yOffset,     n, " << endl;
  oPredDriver << "                 who, j, " << endl;
  oPredDriver << "                 indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualF   = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0]); " << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = exp( CppAD::Value(indepVar[etaOffset+0]) );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strF << "[j] );" << endl;
  oPredDriver << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  oPredDriver << "   return !ok;" << endl;
  oPredDriver << "}" << endl;
  oPredDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fPredDriver_cpp, fPredDriver );
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
void ind_noID_NonmemTranslatorTest::testDriver()
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
  // or mathematially, the return code of anything other than 2 is ignored here.
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
void ind_noID_NonmemTranslatorTest::testReportML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the results
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  xercesc::XercesDOMParser *reportParser = new xercesc::XercesDOMParser;
  reportParser->setValidationScheme( XercesDOMParser::Val_Auto );
  reportParser->setDoNamespaces( true );
  reportParser->setDoSchema( true );
  reportParser->setValidationSchemaFullChecking( true );
  reportParser->setCreateEntityReferenceNodes( true );
  
  try{
    reportParser->parse( fReport_xml );
    report = reportParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
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
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fReport_xml, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fReport_xml );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

  DOMNodeList *error_messages = report->getElementsByTagName( X_ERROR_MESSAGES );
  if( error_messages->getLength() > 0 )

  CPPUNIT_ASSERT( error_messages->getLength() == 0 );

  okToClean = true;
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
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_noID_NonmemTranslatorTest>(
         "testPredClass", 
	 &ind_noID_NonmemTranslatorTest::testPredClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_noID_NonmemTranslatorTest>(
         "testDriver", 
	 &ind_noID_NonmemTranslatorTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_noID_NonmemTranslatorTest>(
         "testReportML", 
	 &ind_noID_NonmemTranslatorTest::testReportML ) );

  return suiteOfTests;
}

