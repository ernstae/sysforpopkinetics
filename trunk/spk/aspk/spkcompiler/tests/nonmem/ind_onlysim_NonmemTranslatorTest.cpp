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

namespace{
  const unsigned int maxChars = 2047;

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
  const int    nRecords  =  4;
  const int    nItems    =  4;
  const double record0[] = { 1, 0.0,  0.0, 0 };
  const double record1[] = { 1, 1.0, 10.0, 0 };
  const double record2[] = { 1, 2.0, 20.0, 0 };
  const double record3[] = { 1, 3.0, 30.0, 0 }; 
  /*
  const int    nRecords  =  3;
  const int    nItems    =  4;
  const double record0[] = { 1, 0.0, 1.8, 0 };
  const double record1[] = { 1, 1.0, 2.0, 0 };
  const double record2[] = { 1, 2.0, 2.2, 0 };
  */
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
  const bool ind_stderr         = false;
  const bool ind_coefficent     = false;
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

  record[0]  = record0;
  record[1]  = record1;
  record[2]  = record2;
  record[3]  = record3;

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
  sprintf( fPrefix, "indWithID" );
  sprintf( fData, "%s_dataML", fPrefix );
  ofstream oData( fData );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nLabels << "\" rows=\"" << nRecords + 1 << "\">" << endl;
  oData << "<description>" << endl;
  oData << "The data set (with ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;
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
  oSource << "<ind_analysis approximation=\"" << approx << "\" ";
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
  oSource << "is_standarderr_out=\""        << (ind_stderr? "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

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
void ind_onlysim_NonmemTranslatorTest::testIndDataClass()
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
  strcpy( fIndDataDriver, "indWithID_IndDataDriver" );
  strcpy( fIndDataDriver_cpp, "indWithID_IndDataDriver.cpp" );
  ofstream oIndDataDriver( fIndDataDriver_cpp );
  CPPUNIT_ASSERT( oIndDataDriver.good() );

  oIndDataDriver << "#include <vector>" << endl;
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
  oIndDataDriver << "   vector<double> a_cp(n);" << endl;
  oIndDataDriver << "   vector<double> a_mdv(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    oIndDataDriver << "   a_id["   << i << "] = \"" << record[i][0] << "\";" << endl;
    oIndDataDriver << "   a_time[" << i << "] = "   << record[i][1] << ";" << endl;
    oIndDataDriver << "   a_cp["   << i << "] = "   << record[i][2] << ";" << endl;
    oIndDataDriver << "   a_mdv["  << i << "] = "   << record[i][3] << ";" << endl;
  }

  oIndDataDriver << "   IndData<double> A( n, a_id, a_time, a_cp, a_mdv );" << endl;

  for( int i=0; i<nRecords; i++ )
    {
      oIndDataDriver << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      // CP=DV
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strCP   << "[" << i << "] );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strDV   << "[" << i << "] );" << endl;
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
  oIndDataDriver << "}" << endl;
  oIndDataDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fIndDataDriver_cpp, fIndDataDriver );
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
void ind_onlysim_NonmemTranslatorTest::testDataSetClass()
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
  oDataSetDriver << "#include <sys/signal.h>" << endl;
  oDataSetDriver << "#include <iostream>" << endl;
  oDataSetDriver << "#include \"DataSet.h\"" << endl;
  oDataSetDriver << "using namespace std;" << endl;
  oDataSetDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oDataSetDriver << "   if( actual != expected ) \\" << endl;
  oDataSetDriver << "   { \\" << endl;
  oDataSetDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oDataSetDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oDataSetDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oDataSetDriver << "      raise( SIGABRT ); \\" << endl;
  oDataSetDriver << "   } " << endl;
  oDataSetDriver << endl;  
  oDataSetDriver << "int main()" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   const int n = " << nRecords << ";" << endl;
  oDataSetDriver << "   DataSet<double> set;" << endl;
  for( int i=0; i<nRecords; i++ )
    {
      oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
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
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fDataSetDriver_cpp, fDataSetDriver );
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
void ind_onlysim_NonmemTranslatorTest::testPredClass()
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
  oPredDriver << "#include <vector>" << endl;
  oPredDriver << "#include <iostream>" << endl;
  oPredDriver << "#include <sys/signal.h>" << endl;
  oPredDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oPredDriver << "   if( actual != expected ) \\" << endl;
  oPredDriver << "   { \\" << endl;
  oPredDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oPredDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oPredDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oPredDriver << "      raise( SIGABRT ); \\" << endl;
  oPredDriver << "   } " << endl;
  oPredDriver << endl;  
  oPredDriver << "using namespace std;" << endl;
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
  oPredDriver << "      indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  oPredDriver << "      indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
  oPredDriver << "      indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  oPredDriver << "      pred.eval( thetaOffset, thetaLen," << endl;
  oPredDriver << "                 etaOffset,   etaLen," << endl;
  oPredDriver << "                 epsOffset,   epsLen ," << endl;
  oPredDriver << "                 fOffset,     n, " << endl;
  oPredDriver << "                 yOffset,     n, " << endl;
  oPredDriver << "                 who, j, " << endl;
  oPredDriver << "                 indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualF   = CppAD::Value(depVar[ fOffset + j ]);"  << endl;
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = exp( CppAD::Value(indepVar[etaOffset+0]) );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
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
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0]);" << endl;
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
void ind_onlysim_NonmemTranslatorTest::testReportML()
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
  /*
  DOMNodeList * bObj_list     = report->getElementsByTagName( XMLString::transcode("ind_obj_out" ) );
  DOMNodeList * thetaOut_list = report->getElementsByTagName( XMLString::transcode("theta_out" ) );
  DOMNodeList * omegaOut_list = report->getElementsByTagName( XMLString::transcode("omega_out" ) );
  vector<double> bOut(thetaLen+omegaOrder);
  if( thetaOut_list->getLength() > 0 )
    {
      DOMElement* thetaOut = dynamic_cast<DOMElement*>( thetaOut_list->item(0) );
      DOMNodeList* value_list = thetaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( thetaLen, n );
      for( int i=0; i<n; i++ )
	{
	  bOut[i] = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );
	  printf( "bOut[%d] = %f\n", i, bOut[i] );
	}
    }
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list = omegaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
      for( int i=thetaLen; i<thetaLen+n; i++ )
	{
	  bOut[i] = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );
	  printf( "bOut[%d] = %f\n", i, bOut[i] );
	}
    }

  const double eps = 1.0/10.0;

  
  //        /  2.0      \
  // bOut = |           |
  //        \  0.026667 /
  //
  double bOut_expected[] = { 2.0, 0.02667 };
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut_expected[0], bOut[0], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut_expected[1], bOut[1], eps );

  //
  // bCov = [ 0.5 * { R_b^t * (RInv kron RInv * R_b) } + f_b^t * ( RInv * f_b ) ]^-1
  // 
  //         /                \
  // bCov =  | 0.00889    --- |
  //         |  0.0       0.0 |
  //         \                /
  //
  double cov_expected[] = { 0.008889, 0.0, 0.0 };
  
  //
  // bSE  = [ sqrt( bCov(1,1), sqrt( bCov(2,2) ]
  //
  double se_expected[]  = { sqrt( cov_expected[0] ), sqrt( cov_expected[2] ) }; 

  //
  // bCV  = [ bSE(1) / b(1) * 100.0, bSE(2) / b(2) * 100.0 ]
  //
  double cv_expected[]  = { se_expected[0] / bOut_expected[0] * 100.0, se_expected[1] / bOut_expected[1] * 100.0 };

  //        /             \
  // bCor = |  1.0   ---  |
  //        |  0.0   1.0  |
  //        \             /
  //
  double cor_expected[] = { 1.0, 0.0, 1.0 };

  //        /                                                         \
  // bCI  = |  b(1) - (12.706 * bSE(1))    ---                        |      
  //        |  b(2) - (12.706 * bSE(2))    b(2) - (12.706 * bSE(2))   |
  //        \                                                         /
  //
  double ci_expected[] = { bOut_expected[0]-(12.706+se_expected[0]),
			   bOut_expected[1]-(12.706*se_expected[1]),
                           bOut_expected[0]-(12.706*se_expected[0]),
                           bOut_expected[1]-(12.706*se_expected[1])
                         };

  vector<double> se_val;
  vector<double> inv_cov_val;
  vector<double> cov_val;
  vector<double> ci_val;
  vector<double> cv_val;
  vector<double> cor_val;

  DOMNodeList *ind_analysis_result = report->getElementsByTagName( X_IND_ANALYSIS_RESULT );
  CPPUNIT_ASSERT( ind_analysis_result->getLength() == 1 );
  DOMElement *ind_stat_result = dynamic_cast<DOMElement*>( ind_analysis_result->item( 0 ) );
  CPPUNIT_ASSERT( ind_stat_result != NULL );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( se_expected[i], se_val[i], eps );
	printf( "se[%d] = %f\n", i, se_val[i] );
      }
    }
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
	printf( "cov[%d] = %f\n", i, cov_val[i] );
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
	printf( "inv_cov[%d] = %f\n", i, inv_cov_val[i] );
      }
    }
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
	printf( "cor[%d] = %f\n", i, cor_val[i] );
      }
    }
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
	printf( "cv[%d] = %f\n", i, cv_val[i] );
      }
    }
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
	printf( "ci[%d] = %f\n", i, ci_val[i] );
      }
    }
  */


  DOMNodeList *presentation_data = report->getElementsByTagName( X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * ind_onlysim_NonmemTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_onlysim_NonmemTranslatorTest" );


  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_onlysim_NonmemTranslatorTest>(
         "testIndDataClass", 
	 &ind_onlysim_NonmemTranslatorTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_onlysim_NonmemTranslatorTest>(
         "testDataSetClass", 
	 &ind_onlysim_NonmemTranslatorTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_onlysim_NonmemTranslatorTest>(
         "testPredClass", 
	 &ind_onlysim_NonmemTranslatorTest::testPredClass ) );
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

