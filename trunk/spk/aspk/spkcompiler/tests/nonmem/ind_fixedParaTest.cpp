#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_fixedParaTest.h"
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
#include <spkcompiler/SpkCompilerException.h>

using namespace std;
using namespace CppUnit;
using namespace xercesc;
/*
Job Identification number: 100

Job Description: y(t) =  a * exp( - b * t ) + e(t) <single compartment with loss>

            Low   In   Up
THETA(1) = { 0.1, 1.0, 10.0 }
THETA(2) = { 0.1, 1.0, 10.0 }

OMEGA(1) = 0.001

PRED
   A = THETA(1)
   B = THETA(2)
   T = TIME
   E = ETA(1)
   F = A * EXP( - B * T  ) 
   Y = F + E

DATA

   TIME    DV
   0.00    1.00
   0.20    0.81873
   0.40    0.67032
   0.60    0.54881
   0.80    0.44933
   1.00    0.36788

Objective: -11.5207

Final estimate of Theta: { 0.974, 0.880 }
Final estimate of Omega: { 0.00126 }

                         THATA(1)   THATA(2)   THETA(3)
Standard error:         { 2.93e-2,  6.91e-2,   7.26e-4 }
Coef of Var:            { 3.00e+0,  7.84e+0,   5.77e+1 }
95% Conf interval(low): { 8.80e-1,  6.61e-1,  -1.05e-3 }
                 (up) : { 1.07e+1,  1.10e+1,   3.57e-3 }


=====================================================================================   
 */

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
  char fDriver_cpp[]      = "fitDriver.cpp";
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
   std::cerr << __FILE__ << \"(\" << __LINE__ << \"): Expected \" << expected << \" but was \" << actual << std::endl; \\\n \
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
  const char *strTIME = "TIME";
  const char *strDV   = "DV";
  const char *strMDV  = "MDV";
  const char *label[] = { strTIME, strDV };
  map<const char*, const char*> label_alias;
  int nLabels         = 2;

  //============================================
  // <Data Set>
  //
  //   TIME    DV
  //   0.00    1.00
  //   0.20    0.81873
  //   0.40    0.67032
  //   0.60    0.54881
  //   0.80    0.44933
  //   1.00    0.36788
  //
  //============================================
  const int    nRecords   =  6;
  const int    nItems     =  2;
  const double record0[]  = { 0.00,   1.00    };
  const double record1[]  = { 0.20,   0.81873 };
  const double record2[]  = { 0.40,   0.67032 };
  const double record3[]  = { 0.60,   0.54881 };
  const double record4[]  = { 0.80,   0.44933 };
  const double record5[]  = { 1.00,   0.36788 };

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
  const double theta_low[ thetaLen ]   = {  0.1,   0.1 };
  const double theta_in [ thetaLen ]   = {  1.0,   1.0 };
  const double theta_up [ thetaLen ]   = { 10.0,  10.0 };
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
  const double omega_in [ omegaOrder ]  = { 0.001 };
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
  // A = THETA(1)
  // B = THETA(2)
  // T = TIME
  // E = ETA(1)
  // F = A * EXP( - B * T  ) 
  // Y = F + E
  //============================================
  const char PRED[] = "   A = THETA(1)\n \
   B = THETA(2)\n \
   T = TIME\n \
   E = ETA(1)\n \
   F = A * EXP( - B * T  )\n \
   Y = F + E\n";

  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  const double nm_obj       =  -29.0251;
  const double nm_theta[]   = { 1.0, 1.0 };
  const double nm_omega[]   = { 1e-5 };

  const double nm_stderr[]  = { 0.00264409, 0.00639371,  5.7735e-6 };
  const double nm_coef[]    = { 0.264409,   0.639372,   57.735 };
  const double nm_conf[]    = { 0.991586,   0.97954,    -8.37129e-6 , 1.00841,    1.02034,     2.83713e-5 };
  const double nm_cov[]     = { 6.9912e-6,  1.17294e-5,  4.08796e-5,  0.0,        3.33333e-11 };
  const double nm_invcov[]  = { 275808,    -79136.5,     47168.4,     0,          0,           3e10 };
  const double nm_corr[]    = { 1,          0.693822,    1,           0,          0,           1 };
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

void ind_fixedParaTest::setUp()
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
  sprintf( fDataML,               "%s_dataML",               fPrefix );
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
  label_alias[strDV]   = NULL;


  record[0]   = record0;
  record[1]   = record1;
  record[2]   = record2;
  record[3]   = record3;
  record[4]   = record4;
  record[5]   = record5;

  X_IND_ANALYSIS_RESULT        = XMLString::transcode( "ind_analysis_result" );
  X_PRESENTATION_DATA          = XMLString::transcode( "presentation_data" );
  X_IND_STDERROR_OUT           = XMLString::transcode( "ind_stderror_out" );
  X_IND_COVARIANCE_OUT         = XMLString::transcode( "ind_covariance_out" );
  X_IND_INVERSE_COVARIANCE_OUT = XMLString::transcode( "ind_inverse_covariance_out" );
  X_IND_CORRELATION_OUT        = XMLString::transcode( "ind_correlation_out" );
  X_IND_COEFFICIENT_OUT        = XMLString::transcode( "ind_coefficient_out" );
  X_IND_CONFIDENCE_OUT         = XMLString::transcode( "ind_confidence_out" );
  X_VALUE                      = XMLString::transcode( "value" );
  X_ERROR_MESSAGES             = XMLString::transcode( "error_messages" );

  createDataML();
  createSourceML();
  parse();
}
void ind_fixedParaTest::tearDown()
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
void ind_fixedParaTest::createDataML()
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
 
void ind_fixedParaTest::createSourceML()
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
void ind_fixedParaTest::parse()
{
  //============================================
  // Instanciate a NonmemTranslator object, 
  // passing the pointers to the sourceML 
  // document tree and the dataML document tree.
  //============================================
  NonmemTranslator xlator( source, data );

  //============================================
  // Parse the dataML document
  //============================================
  try{
    xlator.parseData();
  }
  catch( const SpkCompilerException & e )
    {
      cerr << e << endl;
      throw;
    }
  SymbolTable *table = xlator.getSymbolTable();

  // ID, TIME, DV were in the data set.  So, they should be in the symbol table already.
  Symbol * id   = table->findi( strID );
  CPPUNIT_ASSERT( id != Symbol::empty() );
  Symbol * dv   = table->findi( strDV );
  CPPUNIT_ASSERT( dv != Symbol::empty() );
  Symbol * time   = table->findi( strTIME );
  CPPUNIT_ASSERT( time != Symbol::empty() );

  //============================================
  // Parse the sourceML document
  //============================================
  try{
    xlator.parseSource();
  }
  catch( const SpkCompilerError & e )
    {
      cerr << e << endl;
      throw;
    }

  // MDV and CP (=DV) were not in the data set; they must be added to the symbol table.
  Symbol * mdv   = table->findi( strMDV );
  CPPUNIT_ASSERT( mdv != Symbol::empty() );

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
  CPPUNIT_ASSERT_MESSAGE( "Missing MontePars.h", montePars == NULL );
  
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
 
  FILE * fitDriver = fopen( fDriver_cpp, "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing fitDriver.cpp", fitDriver != NULL );
  fclose( fitDriver );
}

void ind_fixedParaTest::testDriver()
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
void ind_fixedParaTest::testReportML()
{
  const double scale = 0.001;

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
      CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale );
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
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], scale );
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
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], scale );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_stderr[i], se_val[i], scale );
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


	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cov[i], cov_val[i], scale );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_invcov[i], inv_cov_val[i], scale );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_conf[i], ci_val[i], scale );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_coef[i], cv_val[i], scale );
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
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_corr[i], cor_val[i], scale );
      }
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * ind_fixedParaTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_fixedParaTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_fixedParaTest>(
         "testDriver", 
	 &ind_fixedParaTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_fixedParaTest>(
         "testReportML", 
	 &ind_fixedParaTest::testReportML ) );

  return suiteOfTests;
}

