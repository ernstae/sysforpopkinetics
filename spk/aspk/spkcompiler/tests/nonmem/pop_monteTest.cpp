#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_monteTest.h"
#include "spkcompiler/series.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "spkcompiler/nonmem/NonmemTranslator.h"
#include "spkcompiler/SymbolTable.h"
#include "spkcompiler/SpkCompilerException.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

namespace{ 
  const unsigned int MAXCHARS = 64;

  const char * testName;
  char fIndData_h[]       = "IndData.h";
  char fDataSet_h[]       = "DataSet.h";
  char fPred_h[]          = "Pred.h";
  char fPredEqn_cpp[]     = "predEqn.cpp";
  char fNonmemPars_h[]    = "NonmemPars.h";
  char fMontePars_h[]     = "MontePars.h";
  char fMakefile[]        = "Makefile.SPK";
  char fDriver_cpp[]      = "monteDriver.cpp";
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
  char XERCESCLIB[] = "xerces-c";
  char LDPATH[]     = "../../spkcompiler/libcommon.a ../../spkcompiler/nonmem/libnonmem.a -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest";
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest";
  char LDFLAG[514];

  char MY_ASSERT_EQUAL[] =
"#include <iostream> \n \
#include <sys/signal.h> \n \
#define MY_ASSERT_EQUAL( expected, actual ) \\\n \
if( actual != expected ) \\\n \
 { \\\n \
   std::cerr << __FILE__ << \"(\" << __LINE__ << \"): expected \" << expected << \" but was \" << actual << std::endl; \\\n \
   raise( SIGABRT ); \\\n \
} \\\n\n";

  //============================================
  // Monte Carlo integration controls
  //
  // Warning: "analytic" method, which is
  // intened for internal testing use only,
  // accepts nEta=nB=1.  Since the 
  //============================================
  const bool isMonte = true;
  const char monteMethod[] = "grid";
  int  monteNEvals = 0;
  vector<int>  monteNumberEvals(1);

  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = false;
  const char method[]   = "fo";
  const int  sig_digits = 3;

  //============================================
  // <Data Set>
  //
  //   ID      TIME     CP=DV    (MDV)
  //   1       0.0       0.0      0
  //   2       0.0       0.0      0
  //   2       1.0      10.0      0
  //   3       0.0       0.0      0
  //   3       1.0      10.0      0
  //   3       2.0      20.0      0
  //   4       0.0       0.0      0
  //   4       1.0      10.0      0
  //   4       2.0      20.0      0
  //   4       3.0      25.0      0
  //============================================
  map<const char*, const char*> label_alias;
  const char *strID         = "ID";
  const char *strTIME       = "TiMe";
  const char *strDV         = "DV";
  const char *strCP         = "CP";
  const char *strMDV        = "MDV";
  const char *label[]       = { strID, strDV, strTIME, strMDV };
  const int    nLabels      = 4;
  const int    nIndividuals = 4;
  const int    nRecords     = 10;
  const int    nFixed       = 0;
  const int    nItems       = nLabels;
  valarray<int> N( nIndividuals );
  const double record0[] = { 1, 0.0,  0.0, 0 };
  const double record1[] = { 2, 0.0,  0.0, 0 };
  const double record2[] = { 2, 1.0, 10.0, 0 };
  const double record3[] = { 3, 0.0,  0.0, 0 };
  const double record4[] = { 3, 1.0, 10.0, 0 };
  const double record5[] = { 3, 2.0, 20.0, 0 };
  const double record6[] = { 4, 0.0,  0.0, 0 };
  const double record7[] = { 4, 1.0, 10.0, 0 };
  const double record8[] = { 4, 2.0, 20.0, 0 };
  const double record9[] = { 4, 3.0, 25.0, 0 };
  double const * record[nRecords];

  //============================================
  // Define NONMEM keywords
  //============================================
  const char *strTHETA  = "THETA";
  const char *strOMEGA  = "OMEGA";
  const char *strSIGMA  = "SIGMA";
  const char *strETA    = "ETA";
  const char *strEPS    = "EPS";
  const char *strPRED   = "PRED";
  const char *strPPRED  = "PPRED";
  const char *strPRES   = "PRES";
  const char *strPWRES  = "PWRES";
  const char *strF      = "F";
  const char *strY      = "Y";

  //============================================
  // User defined words
  //============================================
  const char * strKA    = "ka";
  const char * strKE    = "ke";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen = 3;
  const double theta_in [ thetaLen ]   = {  1.0,  2.0,  3.0 };
  const double theta_up [ thetaLen ]   = { 11.0, 12.0, 13.0 };
  const double theta_low[ thetaLen ]   = { -9.0, -8.0, -7.0 };
  const bool   theta_fix[ thetaLen ]   = { false, false, false };

  //============================================
  // The SPK Compiler determines the initial
  // values for eta, the variance of data
  // in the individual analysis case.
  // The size of this vector is determined by
  // the order of Omega matrix.
  //============================================
  const int etaLen = 2;
  const double eta_in [ etaLen ] = { 0.0, 0.0 };
  const bool   eta_fix[ etaLen ] = { false, false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal matrix.
  //============================================
  const int    omegaDim                = etaLen;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
  const int    omegaOrder              = (omegaStruct==Symbol::DIAGONAL? 
					  omegaDim : omegaDim * (omegaDim+1)/2 );
  const double omega_in [ omegaOrder ] = { 1.0, 2.0 };
  const bool   omega_fix[ omegaOrder ] = { false, false };

  //============================================
  // EPS is irrevalent in the individual 
  // analysis case.  It'll be ignored.
  //============================================
  const int epsLen = 2;
   
  //============================================
  // The SPK Compiler decides the constraints
  // of Sigma matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal matrix.
  //============================================
  const int    sigmaDim                = epsLen;
  const Symbol::Structure sigmaStruct  = Symbol::DIAGONAL;
  const int    sigmaOrder              = (sigmaStruct==Symbol::DIAGONAL? 
					  sigmaDim : sigmaDim * (sigmaDim+1)/2 );
  const double sigma_in [ sigmaOrder ] = { 1.0, 1.0 };
  const bool   sigma_fix[ sigmaOrder ] = { false, false };

  //============================================
  // Make requests for statistics.
  //============================================
  const string covForm          = "r";
  const bool pop_stderr         = false;
  const bool pop_coefficient    = false;
  const bool pop_confidence     = false;
  const bool pop_covariance     = false;
  const bool pop_inv_covariance = false;
  const bool pop_correlation    = false;

  //============================================
  // Make a request on data simulation.
  //============================================
  const bool isSimulate         = false;
  const int  seed               = -1;
  const bool onlySimulation     = false;
  const int  subproblems        = 1;

  //============================================
  // PRED model
  //
  // KA=THETA(1) + ETA(1)
  // KE=THETA(2) + ETA(2)
  // F=KE*KA
  // Y=F+EPS(1)+EPS(2)
  //============================================
  const char PRED[]     = "ka = THETA(1) + ETA(1)\nke = THETA(2) + ETA(2)\nF = ke * ka\nY = F + EPS(1) + EPS(2)\n";

  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  /*
  const double nm_obj       =  46.4087;
  const double nm_theta[]   = { 0.02, 1.00171 };
  const double nm_omega[]   = { 0.771353 };
  */

  const double nm_pred[]    = {  };
  //============================================
  // XML strings
  //============================================
  XMLCh * X_ERROR_MESSAGES;
  XMLCh * X_POP_ANALYSIS_RESULT;
  XMLCh * X_POP_MONTE_RESULT;
  XMLCh * X_PRESENTATION_DATA;
  XMLCh * X_POP_STDERROR_OUT;
  XMLCh * X_POP_COVARIANCE_OUT;
  XMLCh * X_POP_INVERSE_COVARIANCE_OUT;
  XMLCh * X_POP_CORRELATION_OUT;
  XMLCh * X_POP_COEFFICIENT_OUT;
  XMLCh * X_POP_CONFIDENCE_OUT;
  XMLCh * X_VALUE;
};

void pop_monteTest::setUp()
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
  catch( ... )
    {
      char buf[MAXCHARS + 1];
      sprintf( buf, "Unknown rror during Xerces-c initialization.\nException message.\n" );
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

  sprintf( LDFLAG, "%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
	   LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB );

  // ID doesn't have an alias
  label_alias[strID]   = NULL;

  // TIME doesn't have an alias
  label_alias[strTIME] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;

  // MDV doesn't have an alias.
  label_alias[strMDV]  = NULL;

  // #of records for each individual
  N[0] = 1;
  N[1] = 2;
  N[2] = 3;
  N[3] = 4;

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

  X_POP_ANALYSIS_RESULT        = XMLString::transcode( "pop_analysis_result" );
  X_POP_MONTE_RESULT           = XMLString::transcode( "pop_monte_result" );
  X_PRESENTATION_DATA          = XMLString::transcode( "presentation_data" );
  X_POP_STDERROR_OUT           = XMLString::transcode( "ind_stderror_out" );
  X_POP_COVARIANCE_OUT         = XMLString::transcode( "ind_covariance_out" );
  X_POP_INVERSE_COVARIANCE_OUT = XMLString::transcode( "ind_inverse_covariance_out" );
  X_POP_CORRELATION_OUT        = XMLString::transcode( "ind_correlation_out" );
  X_POP_COEFFICIENT_OUT        = XMLString::transcode( "ind_coefficient_out" );
  X_POP_CONFIDENCE_OUT         = XMLString::transcode( "ind_confidence_out" );
  X_VALUE                      = XMLString::transcode( "value" );
  X_ERROR_MESSAGES             = XMLString::transcode( "error_messages" );

  int defaultEvals = 0;
  if( strcmp( monteMethod, "grid" ) == 0 )
  {
     monteNEvals = etaLen;
     defaultEvals = 10;
  }
  else
  {
     monteNEvals = 1;
     
     if( strcmp( monteMethod, "plain" ) == 0 
         || strcmp( monteMethod, "miser" ) == 0 ) 
         defaultEvals = 1000;
     else //if( strcmp( monteMethod, "analytic" ) == 0 )
         defaultEvals = 1;
  }
  monteNumberEvals.resize( monteNEvals );
  fill( monteNumberEvals.begin(), monteNumberEvals.end(), defaultEvals);   

  createDataML();
  createSourceML();
  parse();
}
void pop_monteTest::tearDown()
{
  XMLString::release( &X_ERROR_MESSAGES );
  XMLString::release( &X_POP_ANALYSIS_RESULT );
  XMLString::release( &X_POP_MONTE_RESULT );
  XMLString::release( &X_PRESENTATION_DATA );
  XMLString::release( &X_POP_STDERROR_OUT );
  XMLString::release( &X_POP_COVARIANCE_OUT );
  XMLString::release( &X_POP_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_POP_CORRELATION_OUT );
  XMLString::release( &X_POP_COEFFICIENT_OUT );
  XMLString::release( &X_POP_CONFIDENCE_OUT );
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
      remove( fIndData_h );
      remove( fDataSet_h );
      remove( fPred_h );
      remove( fPredEqn_cpp );
      remove( fMakefile );
      remove( fReportML );
      remove( fSavedReportML );
      remove( fTraceOut );

      remove( "MapMonte.h" );
      remove( "MapMonte.cpp" );
      remove( "MapBay.h" );
      remove( "MapBay.cpp" );
      remove( "GridIntegral.h" );
      remove( "GridIntegral.cpp" );
      remove( "Gsl2SpkError.h" );
      remove( "Gsl2SpkError.cpp" );
      remove( "MontePopObj.h" );
      remove( "MontePopObj.cpp" );
      remove( "monteDriver.cpp" );
      remove( "monteDriver" );
    }
  XMLPlatformUtils::Terminate();
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void pop_monteTest::createDataML()
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
  oData << "The data set for the population analysis test" << endl;
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
    dataParser->parse( fDataML );
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
  data = dataParser->getDocument();
  assert( data );
}
 
void pop_monteTest::createSourceML()
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

  if( isMonte )
  {
     oSource << "<monte_carlo method=\"" << monteMethod << "\">";
     oSource << "   <number_eval>" << endl;
     for( int i=0; i<monteNEvals; i++ )
        oSource << "      <value>" << monteNumberEvals[i] << "</value>" << endl;
     oSource << "   </number_eval>" << endl;
     oSource << "</monte_carlo>" << endl;
  }
  oSource << "<constraint>" << endl;

  // default: is_eta_out=no, is_restart=yes
  oSource << "<pop_analysis ";
  oSource << "mitr=\"" << mitr << "\" ";
  oSource << "is_estimation=\"" << (isEstimate? "yes" : "no") << "\" ";
  oSource << "approximation=\"" << method << "\" ";
  oSource << "pop_size=\"" << nIndividuals << "\"";
  oSource << ">" << endl;

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

  oSource << "<sigma struct=\"";
  oSource << (sigmaStruct==Symbol::DIAGONAL? "diagonal" : "block");
  oSource << "\" dimension=\"";
  oSource << sigmaDim << "\">" << endl;
  oSource << "<in>" << endl;

  for( int i=0; i<sigmaOrder; i++ )
    {
      oSource << "<value";
      oSource << " fixed=\"" << (sigma_fix[i]? "yes" : "no") << "\"";
      oSource << ">" << sigma_in[i] << "</value>" << endl;
    }
  oSource << "</in>" << endl;
  oSource << "</sigma>" << endl;

  oSource << "<pop_stat ";
  oSource << "covariance_form=\""           << covForm                          << "\" ";
  oSource << "is_stderror_out=\""           << (pop_stderr?         "yes":"no") << "\" ";
  oSource << "is_covariance_out=\""         << (pop_covariance?     "yes":"no") << "\" ";
  oSource << "is_inverse_covariance_out=\"" << (pop_inv_covariance? "yes":"no") << "\" ";
  oSource << "is_confidence_out=\""         << (pop_confidence?     "yes":"no") << "\" ";
  oSource << "is_coefficient_out=\""        << (pop_coefficient?    "yes":"no") << "\" ";
  oSource << "is_correlation_out=\""        << (pop_correlation?    "yes":"no") << "\"/>" << endl;

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"";
      if( onlySimulation )
	oSource << " only_simulation=\"yes\"";
      if( subproblems > 1 )
        oSource << " subproblems=\"" << subproblems << "\"";
      oSource << "/>" << endl;
    }
  oSource << "</pop_analysis>" << endl;
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
void pop_monteTest::parse()
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
  try{
    xlator.parseData();
  }
  catch( const SpkCompilerException & e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "Failed to parse the data xml.", false );
    }
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
  try{
    xlator.parseSource();
  }
  catch( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "Failed to parse the source xml.", false );
    }

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
  // driver.cpp
  // ==========================================
  FILE * nonmemPars = fopen( fNonmemPars_h, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing NonmemPars.h", nonmemPars != NULL );
  fclose( nonmemPars );

  FILE * montePars = fopen( fMontePars_h, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing MontePars.h", montePars != NULL );
  
  FILE * indData = fopen( fIndData_h, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing IndData.h", indData != NULL );
  fclose( indData );

  FILE * dataSet = fopen( fDataSet_h, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing DataSet.h", dataSet != NULL );
  fclose( dataSet );

  FILE * pred = fopen( fPred_h, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing Pred.h", pred != NULL );
  fclose( pred );

  FILE * makeSPK = fopen( fMakefile, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing Makefile.SPK", makeSPK != NULL );
  fclose( makeSPK );
}
void pop_monteTest::testNonmemPars_h()
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
}
void pop_monteTest::testMontePars_h()
{
  //============================================
  // Test if MontePars declares/defines
  // variables as required.
  //============================================
  printf( "\n--- %s ---\n", fMonteParsDriver );
  ofstream o ( fMonteParsDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <iostream>" << endl;
  o << "#include \"MontePars.h\"" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "using namespace std;" << endl;
  o << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   MY_ASSERT_EQUAL( MontePars::" << monteMethod << ", MontePars::method );" << endl;
  o << "   MY_ASSERT_EQUAL( " << monteNEvals << ", MontePars::nEval );" << endl;
  o << "}" << endl;
  o.close();

  char command[512];

  // Build the test driver.
  sprintf( command, "g++ %s -o %s %s %s",
           fMonteParsDriver_cpp, 
           fMonteParsDriver, 
           LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char mess[128];
      sprintf( mess, "Failed to build %s.", fMonteParsDriver );
      CPPUNIT_ASSERT_MESSAGE( mess, false );
    }

  // Run the test driver
  sprintf( command, "./%s", fMonteParsDriver );
  if( system( command ) != 0 )
    {
      char mess[128];
      sprintf( mess, "%s abnormally terminated.", fMonteParsDriver );
      CPPUNIT_ASSERT_MESSAGE( mess, false );      
    }
}
void pop_monteTest::testDriver()
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
      sprintf( message, "Compilation of the generated %s failed!", fDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s > %s", fDriver, fTraceOut );

  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  remove( "keep_monteDriver.cpp" );
  system( "link monteDriver.cpp keep_monteDriver.cpp" );
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for computation problem <%d>!", fDriver, exitcode );
      
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
               fDriver, 
               exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, true );
    }
  if( rename( fReportML, fSavedReportML ) != 0 )
  {
     char message[256];
     sprintf( message, "Failed to rename %s to %s!", fReportML, fSavedReportML );
     CPPUNIT_ASSERT_MESSAGE( message, false );
  }
}
void pop_monteTest::testReportML()
{
  printf( "\n--- %s ---\n", "Report Test" );
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
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fSavedReportML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  
  report = reportParser->getDocument();
  CPPUNIT_ASSERT( report );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify if any error was caught during the runtime.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *error_messages;
  
  error_messages = report->getElementsByTagName( X_ERROR_MESSAGES );
  CPPUNIT_ASSERT( error_messages->getLength() == 0 );
   
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the objective value.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double obj_out = 0.0;
  DOMNodeList * objOut_list = report->getElementsByTagName( XMLString::transcode( "ind_obj_out" ) );
  if( objOut_list->getLength() > 0 )
    {
      DOMElement* objOut = dynamic_cast<DOMElement*>( objOut_list->item(0) );
      DOMNodeList* value_list = objOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( 1, n );
      obj_out = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );      
      // CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale * nm_obj );
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
  // Grab a pointer to the top of "pop_monte_result" sub-tree.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *pop_monte_result = report->getElementsByTagName( X_POP_MONTE_RESULT );
  CPPUNIT_ASSERT( pop_monte_result->getLength() == 1 );
  DOMElement *ind_stat_result = dynamic_cast<DOMElement*>( pop_monte_result->item( 0 ) );
  CPPUNIT_ASSERT( ind_stat_result != NULL );

  okToClean = true;
}

CppUnit::Test * pop_monteTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_monteTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_monteTest>(
         "testNonmemPars_h", 
	 &pop_monteTest::testNonmemPars_h ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_monteTest>(
         "testMontePars_h", 
	 &pop_monteTest::testMontePars_h ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_monteTest>(
         "testDriver", 
	 &pop_monteTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_monteTest>(
         "testReportML", 
	 &pop_monteTest::testReportML ) );
  return suiteOfTests;
}

