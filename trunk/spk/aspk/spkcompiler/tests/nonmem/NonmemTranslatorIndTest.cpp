#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "NonmemTranslatorIndTest.h"
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
};

void NonmemTranslatorIndTest::setUp()
{
}
void NonmemTranslatorIndTest::tearDown()
{
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
// <Data Set>
//
//   ID     TIME     CP=DV
//    1      0.0      0.0 
//    1      1.0     10.0
//    1      2.0     20.0
//
// <Parameter Estimation Constraints>
// theta (initial):  {  5 }
// theta (upper):    { 10 }
// theta (lower):    {  0 }
// Omega (initial):  /   \
//                   | 1 |
//                   \   /  treat it as a diagonal case
//
// <Statistics values request>
// standard error:            yes
// coefficent of variation    yes
// confidence interval        yes
// covariance                 yes
// inverse of covariance      no
// correlation matrix         yes
//
// <Data simulation>
// Only simulation?           no
// #of subproblems            1
// Seed                       1
//
// <Model>
//  F= THETA(1) + ETA(1)*TIME
//******************************************************************************
void NonmemTranslatorIndTest::testParseIndSource()
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  const char fData[] = "NonmemTranslatorIndTest.dataML";
  ofstream oData( fData );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"3\" rows=\"4\">" << endl;
  oData << "<description>" << endl;
  oData << "The data set (with ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;
  oData << "<row position=\"1\">" << endl;
  oData << "<value type=\"string\">ID</value>" << endl;
  oData << "<value type=\"string\">TIME</value>" << endl;
  oData << "<value type=\"string\">DV</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"2\">" << endl;
  oData << "<value type=\"string\">" << endl;
  oData << "1" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "0.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "0.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"3\">" << endl;
  oData << "<value type=\"string\">" << endl;
  oData << "1" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "1.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "10.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"4\">" << endl;
  oData << "<value type=\"string\">" << endl;
  oData << "1" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "2.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "20.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
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
  

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preparation for creating a sourceML document
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //============================================
  // Optimizer controls
  //============================================
  const char approx[]   = "foce";
  const int  mitr       = 100;
  const bool isEstimate = true;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const string strID   = "ID";
  const string strTIME = "TIME";
  const string strCP   = "CP";
  const string strDV   = "DV";
  map<string, string> labels;
  labels[ strID ]   = "";
  labels[ strTIME ] = "";
  labels[ strCP ]   = "DV";
  int nLabels    = labels.size();

  //============================================
  // Define NONMEM keywords
  //============================================
  const string strTHETA = "THETA";
  const string strOMEGA = "OMEGA";
  const string strSIGMA = "SIGMA";
  const string strETA   = "ETA";
  const string strEPS   = "EPS";
  const string strPRED  = "PRED";
  const string strRES   = "RES";
  const string strWRES  = "WRES";
  const string strMDV   = "MDV";
  const string strF     = "F";
  const string strY     = "Y";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen                = 1;
  const double theta_in [ thetaLen ]   = {  5.0 };
  const double theta_up [ thetaLen ]   = { 10.0 };
  const double theta_low[ thetaLen ]   = {  0.0 };
  const bool   theta_fix[ thetaLen ]   = { false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal --- 1 dimensional! --- matrix.
  //============================================
  const int    omegaDim                = 1;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
  assert( omegaStruct == Symbol::DIAGONAL );
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
//  const bool isSimulate         = true;
  const bool isSimulate         = false;
  const int  seed               = 1;
  const bool onlySimulation     = false;
  const int  subproblems        = 1;
  
  //============================================
  // Create an sourceML based upon the
  // parameters set so far.
  //============================================
  const char fSource[] = "NonmemTranslatorIndTest.sourceML";
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
  oSource << "<label name=\"" << strID << "\"/>" << endl;
  oSource << "<label name=\"" << strTIME << "\"/>" << endl;
  oSource << "<label name=\"" << strCP << "\" synonym=\"" << strDV << "\"/>" << endl;
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
  oSource << "is_standarderr_out=\""        << (ind_stderr? "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
    }
  oSource << "</ind_analysis>" << endl;
  oSource << "</constraint>" << endl;

  oSource << "<model>" << endl;
  oSource << "<pred>" << endl;
  oSource << "   " << strF << " = " << strTHETA << "(1) + " << strETA << "(1) * " << strTIME << endl;
  oSource << "</pred>" << endl;
  oSource << "</model>" << endl;
      
  oSource << "<presentation>" << endl;
  oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
  oSource << "<column label=\"" << strTIME << "\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(1)\" appearance_order=\"2\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(3)\" appearance_order=\"4\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(2)\" appearance_order=\"3\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<table header=\"every\">" << endl;
  oSource << "<column label=\"" << strTIME << "\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"" << strDV << "\" appearance_order=\"2\"/>" << endl;
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Translation
  // 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  char fIndDataDriver[]     = "indWithID_IndDataDriver";
  char fIndDataDriver_cpp[] = "indWithID_IndDataDriver.cpp";
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
  oIndDataDriver << "   const int n = 3;" << endl;
  oIndDataDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oIndDataDriver << "   const int etaLen = " << etaLen << ";" << endl;
  oIndDataDriver << "   vector<char*> a_id(n);" << endl;
  oIndDataDriver << "   char id1[] = \"1\";" << endl;
  oIndDataDriver << "   a_id[0] = id1;" << endl;
  oIndDataDriver << "   a_id[1] = id1;" << endl;
  oIndDataDriver << "   a_id[2] = id1;" << endl;
  oIndDataDriver << "   vector<double> a_time(n);" << endl;
  oIndDataDriver << "   a_time[0] = 0.0;" << endl;
  oIndDataDriver << "   a_time[1] = 1.0;" << endl;
  oIndDataDriver << "   a_time[2] = 2.0;" << endl;
  oIndDataDriver << "   vector<double> a_cp(n);" << endl;
  oIndDataDriver << "   a_cp[0] = 0.0;" << endl;
  oIndDataDriver << "   a_cp[1] = 10.0;" << endl;
  oIndDataDriver << "   a_cp[2] = 20.0;" << endl;
  oIndDataDriver << "   vector<double> a_mdv(n);" << endl;
  oIndDataDriver << "   a_mdv[0] = 0;" << endl;
  oIndDataDriver << "   a_mdv[1] = 0;" << endl;
  oIndDataDriver << "   a_mdv[2] = 0;" << endl;

  oIndDataDriver << "   IndData<double> A( n, a_id, a_time, a_cp, a_mdv );" << endl;

  oIndDataDriver << "   assert( strcmp( A." << strID << "[0], id1 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( A." << strID << "[1], id1 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( A." << strID << "[2], id1 ) == 0 );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strTIME << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  1.0, A." << strTIME << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  2.0, A." << strTIME << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strCP << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, A." << strCP << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, A." << strCP << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strDV << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, A." << strDV << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, A." << strDV << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[2] );" << endl;
  
  // There have to be placeholders for the current values of theta/eta for
  // each call to Pred::eval().
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[2].size() );" << endl;
  //  oIndDataDriver << "   MY_ASSERT_EQUAL( omegaOrder, A." << strOMEGA << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[2].size() );" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strWRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strPRED << ".size() );" << endl;

  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strF << ".size() );" << endl;
  oIndDataDriver << "}" << endl;
  oIndDataDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s", fIndDataDriver_cpp, fIndDataDriver );
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
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  char fDataSetDriver[]     = "indWithID_DataSetDriver";
  char fDataSetDriver_cpp[] = "indWithID_DataSetDriver.cpp";
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
  oDataSetDriver << "   const int n = 3;" << endl;
  oDataSetDriver << "   DataSet<double> set;" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[0], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[1], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[2], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strTIME << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  1.0, set.data[0]->" << strTIME << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  2.0, set.data[0]->" << strTIME << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strCP << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[0]->" << strCP << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[0]->" << strCP << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strDV << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[0]->" << strDV << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[0]->" << strDV << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strMDV << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strMDV << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strMDV << "[2] );" << endl;

  oDataSetDriver << "for( int j=0; j<n; j++ )" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( " << thetaLen << ", set.data[0]->" << strTHETA << "[j].size() );" << endl;
  //  oDataSetDriver << "   MY_ASSERT_EQUAL( " << omegaOrder << ", set.data[0]->" << strOMEGaO << "[j].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( " << etaLen << ", set.data[0]->" << strETA << "[j].size() );" << endl;
  oDataSetDriver << "}" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strRES << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strWRES << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED << ".size() );" << endl;

  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF << ".size() );" << endl;
  oDataSetDriver << "}" << endl;
  
  oDataSetDriver.close();

  sprintf( command, "g++ -g %s -o %s", fDataSetDriver_cpp, fDataSetDriver );
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
  char fPredDriver[]     = "indWithID_PredDriver";
  char fPredDriver_cpp[] = "indWithID_PredDriver.cpp";
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
  oPredDriver << "   const int n           = 3; // #of measurements" << endl;
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
  oPredDriver << "      double actualF   = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0] " << endl;
  oPredDriver << "                       + indepVar[etaOffset+0]*set.data[who]->" << strTIME << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = 0.0;" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j) + (C1*j) * CppAD::Value(set.data[who]->" << strTIME << "[j]);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( set.data[who]->" << strDV << "[j] - expectedPred, set.data[who]->" << strRES << "[j] );" << endl;
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
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0] " << endl;
  oPredDriver << "                       + indepVar[etaOffset+0]*set.data[who]->" << strTIME << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = 0.0;" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j) + (C1*j) * CppAD::Value(set.data[who]->" << strTIME << "[j]);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( set.data[who]->" << strDV << "[j] - expectedPred, set.data[who]->" << strRES << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strF << "[j] );" << endl;
  oPredDriver << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  oPredDriver << "   return !ok;" << endl;
  oPredDriver << "}" << endl;
  oPredDriver.close();

  sprintf( command, "g++ -g %s -o %s", fPredDriver_cpp, fPredDriver );
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
  

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  char fDriver[]     = "driver";
  char fDriver_cpp[] = "driver.cpp";
  int  exitcode      = 0;
  sprintf( command, "make -f generatedMakefile" );
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

  remove( fData );
  remove( fSource );
  remove( fIndDataDriver );
  remove( fIndDataDriver_cpp );
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
  remove( fPredDriver );
  remove( fPredDriver_cpp );
  remove( fDriver );
  remove( "driver.cpp" );
  remove( "IndData.h" );
  remove( "DataSet.h" );
  remove( "Pred.h" );
  remove( "predEqn.cpp" );
  remove( "generatedMakefile" );

  XMLPlatformUtils::Terminate();

}
//******************************************************************************
//
// Test a problem that takes a data set WITHOUT! ID field filled in.
//
// <Data Set>
//
//   TIME     CP=DV
//    0.0      0.0 
//    1.0     10.0
//    2.0     20.0
//
// <Parameter Estimation Constraints>
// theta (initial):  {  5 }
// theta (upper):    { 10 }
// theta (lower):    {  0 }
// Omega (initial):  /   \
//                   | 1 |
//                   \   /  treat it as a diagonal case
//
// <Statistics values request>
// standard error:            yes
// coefficent of variation    yes
// confidence interval        yes
// covariance                 yes
// inverse of covariance      no
// correlation matrix         yes
//
// <Data simulation>
// Only simulation?           no
// #of subproblems            1
// Seed                       1
//
// <Model>
//  F= THETA(1) + ETA(1)*TIME
//******************************************************************************
void NonmemTranslatorIndTest::testParseIndNoID()
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with no ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  const char fData[] = "NonmemTranslatorIndNoIDTest.dataML";
  ofstream oData( fData );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"2\" rows=\"4\">" << endl;
  oData << "<description>" << endl;
  oData << "The data set (with no ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;
  oData << "<row position=\"1\">" << endl;
  oData << "<value type=\"string\">TIME</value>" << endl;
  oData << "<value type=\"string\">DV</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"2\">" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "0.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "0.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"3\">" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "1.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "10.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
  oData << "<row position=\"4\">" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "2.0" << endl;
  oData << "</value>" << endl;
  oData << "<value type=\"numeric\">" << endl;
  oData << "20.0" << endl;
  oData << "</value>" << endl;
  oData << "</row>" << endl;
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
  

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preparation for creating a sourceML document
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //============================================
  // Optimizer controls
  //============================================
  const char approx[]   = "foce";
  const int  mitr       = 100;
  const bool isEstimate = true;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const string strID   = "ID";
  const string strTIME = "TIME";
  const string strCP   = "CP";
  const string strDV   = "DV";
  map<string, string> labels;
  labels[strID]   = "";
  labels[strTIME] = "";
  labels[strCP]   = strDV;
  int nLabels     = labels.size();

  //============================================
  // Define NONMEM keywords
  //============================================
  const string strTHETA = "THETA";
  const string strOMEGA = "OMEGA";
  const string strSIGMA = "SIGMA";
  const string strETA   = "ETA";
  const string strEPS   = "EPS";
  const string strPRED  = "PRED";
  const string strRES   = "RES";
  const string strWRES  = "WRES";
  const string strMDV   = "MDV";
  const string strF     = "F";
  const string strY     = "Y";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen                = 1;
  const double theta_in [ thetaLen ]   = {  5.0 };
  const double theta_up [ thetaLen ]   = { 10.0 };
  const double theta_low[ thetaLen ]   = {  0.0 };
  const bool   theta_fix[ thetaLen ]   = { false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal --- 1 dimensional! --- matrix.
  //============================================
  const int    omegaDim                = 1;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
  assert( omegaStruct == Symbol::DIAGONAL );
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
//  const bool isSimulate         = true;
  const bool isSimulate         = false;
  const int  seed               = 1;
  const bool onlySimulation     = false;
  const int  subproblems        = 1;
  
  //============================================
  // Create an sourceML based upon the
  // parameters set so far.
  //============================================
  const char fSource[] = "NonmemTranslatorIndNoIDTest.sourceML";
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
  oSource << "<label name=\"TIME\"/>" << endl;
  oSource << "<label name=\"CP\" synonym=\"DV\"/>" << endl;
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
  oSource << "is_standarderr_out=\""        << (ind_stderr? "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
    }
  oSource << "</ind_analysis>" << endl;
  oSource << "</constraint>" << endl;

  oSource << "<model>" << endl;
  oSource << "<pred>" << endl;
  oSource << "   F= THETA(1) + ETA(1)*TIME" << endl;
  oSource << "</pred>" << endl;
  oSource << "</model>" << endl;
      
  oSource << "<presentation>" << endl;
  oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
  oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"THETA(1)\" appearance_order=\"2\"/>" << endl;
  oSource << "<column label=\"THETA(3)\" appearance_order=\"4\"/>" << endl;
  oSource << "<column label=\"THETA(2)\" appearance_order=\"3\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<table header=\"every\">" << endl;
  oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
  oSource << "<column label=\"DV\" appearance_order=\"2\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<scatterplot>" << endl;
  oSource << "<x label=\"TIME\"/>" << endl;
  oSource << "<y label=\"PRED\"/>" << endl;
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Translation
  // 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

  Symbol * id   = table->findi( "ID" );
  CPPUNIT_ASSERT( id != Symbol::empty() );
  Symbol * time = table->findi( "TIME" );
  CPPUNIT_ASSERT( time != Symbol::empty() );
  Symbol * dv   = table->findi( "DV" );
  CPPUNIT_ASSERT( dv != Symbol::empty() );

  //============================================
  // Parse the sourceML document
  //============================================
  xlator.parseSource();

  Symbol * mdv   = table->findi( "MDV" );
  CPPUNIT_ASSERT( mdv != Symbol::empty() );
  Symbol * cp   = table->findi( "CP" );
  CPPUNIT_ASSERT( cp != Symbol::empty() );

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
  char fIndDataDriver[]     = "indNoID_IndDataDriver";
  char fIndDataDriver_cpp[] = "indNoID_IndDataDriver.cpp";
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
  oIndDataDriver << "   const int n = 3;" << endl;
  oIndDataDriver << "   vector<char*> a_id(n);" << endl;
  oIndDataDriver << "   char id1[] = \"1\";" << endl;
  oIndDataDriver << "   a_id[0] = id1;" << endl;
  oIndDataDriver << "   a_id[1] = id1;" << endl;
  oIndDataDriver << "   a_id[2] = id1;" << endl;
  oIndDataDriver << "   vector<double> a_time(n);" << endl;
  oIndDataDriver << "   a_time[0] = 0.0;" << endl;
  oIndDataDriver << "   a_time[1] = 1.0;" << endl;
  oIndDataDriver << "   a_time[2] = 2.0;" << endl;
  oIndDataDriver << "   vector<double> a_cp(n);" << endl;
  oIndDataDriver << "   a_cp[0] = 0.0;" << endl;
  oIndDataDriver << "   a_cp[1] = 10.0;" << endl;
  oIndDataDriver << "   a_cp[2] = 20.0;" << endl;
  oIndDataDriver << "   vector<double> a_mdv(n);" << endl;
  oIndDataDriver << "   a_mdv[0] = 0;" << endl;
  oIndDataDriver << "   a_mdv[1] = 0;" << endl;
  oIndDataDriver << "   a_mdv[2] = 0;" << endl;

  oIndDataDriver << "   IndData<double> A( n, a_id, a_time, a_cp, a_mdv );" << endl;

  oIndDataDriver << "   assert( strcmp( A." << strID << "[0], id1 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( A." << strID << "[1], id1 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( A." << strID << "[2], id1 ) == 0 );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strTIME << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  1.0, A." << strTIME << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  2.0, A." << strTIME << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strCP << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, A." << strCP << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, A." << strCP << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0.0, A." << strDV << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, A." << strDV << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, A." << strDV << "[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL(  0,   A." << strMDV << "[2] );" << endl;
  
  // There have to be placeholders for the current values of theta/eta for
  // each call to Pred::eval().
  oIndDataDriver << "   const int thetaLen   = " << thetaLen << ";" << endl;
  oIndDataDriver << "   const int etaLen     = " << etaLen << ";" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[2].size() );" << endl;
  //  oIndDataDriver << "   MY_ASSERT_EQUAL( omegaOrder,  A." << strOMEGA << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen, A." << strETA << "[2].size() );" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strWRES << ".size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strPRED << ".size() );" << endl;

  oIndDataDriver << "   MY_ASSERT_EQUAL( n, A." << strF << ".size() );" << endl;
  oIndDataDriver << "}" << endl;
  oIndDataDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s", fIndDataDriver_cpp, fIndDataDriver );
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
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  char fDataSetDriver[]     = "indNoID_DataSetDriver";
  char fDataSetDriver_cpp[] = "indNoID_DataSetDriver.cpp";
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
  oDataSetDriver << "   const int n = 3;" << endl;
  oDataSetDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oDataSetDriver << "   const int etaLen   = " << etaLen << ";" << endl;
  oDataSetDriver << "   DataSet<double> set;" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[0], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[1], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[0]->" << strID << "[2], \"1\" ) == 0 );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strTIME << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  1.0, set.data[0]->" << strTIME << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  2.0, set.data[0]->" << strTIME << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strCP << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[0]->" << strCP << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[0]->" << strCP << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0.0, set.data[0]->" << strDV << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[0]->" << strDV << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[0]->" << strDV << "[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0,   set.data[0]->" << strMDV << "[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0,   set.data[0]->" << strMDV << "[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL(  0,   set.data[0]->" << strMDV << "[2] );" << endl;

  oDataSetDriver << "for( int j=0; j<n; j++ )" << endl;
  oDataSetDriver << "{" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[0]->" << strTHETA << "[j].size() );" << endl;
  //  oDataSetDriver << "   MY_ASSERT_EQUAL( omegaOrder, set.data[0]->" << strOMEGA << "[j].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen, set.data[0]->" << strETA << "[j].size() );" << endl;
  oDataSetDriver << "}" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strRES << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strWRES << ".size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED << ".size() );" << endl;

  oDataSetDriver << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF << ".size() );" << endl;
  oDataSetDriver << "}" << endl;
  
  oDataSetDriver.close();

  sprintf( command, "g++ -g %s -o %s", fDataSetDriver_cpp, fDataSetDriver );
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
  char fPredDriver[]     = "indNoID_PredDriver";
  char fPredDriver_cpp[] = "indNoID_PredDriver.cpp";
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
  oPredDriver << "   const int n           = 3; // #of measurements" << endl;
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
  oPredDriver << "      double actualF   = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0] " << endl;
  oPredDriver << "                       + indepVar[etaOffset+0]*set.data[who]->";
  oPredDriver << strTIME << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = 0.0;" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY )" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j) + (C1*j) * CppAD::Value(set.data[who]->" << strTIME << "[j]);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( set.data[who]->" << strDV << "[j] - expectedPred, set.data[who]->" << strRES << "[j] );" << endl;
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
  oPredDriver << "      double expectedF = CppAD::Value(indepVar[thetaOffset+0] " << endl;
  oPredDriver << "                       + indepVar[etaOffset+0]*set.data[who]->";
  oPredDriver << strTIME << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "      double actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "      double expectedY = 0.0;" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
  oPredDriver << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  oPredDriver << "   for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      double expectedPred = (C1*j) + (C1*j) * CppAD::Value(set.data[who]->" << strTIME << "[j]);" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA << "[j][0] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strPRED << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( set.data[who]->" << strDV << "[j] - expectedPred, set.data[who]->" << strRES << "[j] );" << endl;
  oPredDriver << "      MY_ASSERT_EQUAL( expectedPred, set.data[who]->" << strF << "[j] );" << endl;
  oPredDriver << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  oPredDriver << "   return !ok;" << endl;
  oPredDriver << "}" << endl;
  oPredDriver.close();

  sprintf( command, "g++ -g %s -o %s", fPredDriver_cpp, fPredDriver );
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
  

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  char fDriver[]     = "driver";
  char fDriver_cpp[] = "driver.cpp";
  int  exitcode      = 0;

  sprintf( command, "make -f generatedMakefile" );
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
  remove( fData );
  remove( fSource );
  remove( fIndDataDriver );
  remove( fIndDataDriver_cpp );
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
  remove( fPredDriver );
  remove( fPredDriver_cpp );
  remove( fDriver );
  remove( "driver.cpp" );
  remove( "IndData.h" );
  remove( "DataSet.h" );
  remove( "Pred.h" );
  remove( "predEqn.cpp" );
  remove( "generatedMakefile" );

  XMLPlatformUtils::Terminate();

}

CppUnit::Test * NonmemTranslatorIndTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemTranslatorIndTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorIndTest>(
         "testParseIndSource", 
	 &NonmemTranslatorIndTest::testParseIndSource ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorIndTest>(
         "testParseIndNoID", 
	 &NonmemTranslatorIndTest::testParseIndNoID ) );
  return suiteOfTests;
}

