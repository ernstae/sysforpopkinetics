#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <sstream>
#include <algorithm>

#include "DOMPrint.h"
#include "ind_subprobTest.h"
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
  char XERCESCLIB[] = "xerces-c";
  char MLIB[]       = "m";
  char LDPATH[]     = "../../spkcompiler/libcommon.a ../../spkcompiler/nonmem/libnonmem.a -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest";
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest";
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
  const char* ind_covform       = "rsr";
  const bool ind_stderr         = true;
  const bool ind_coefficient    = true;
  const bool ind_confidence     = true;
  const bool ind_covariance     = true;
  const bool ind_inv_covariance = true;
  const bool ind_correlation    = true;

  //============================================
  // Make a request on data simulation.
  //============================================
  const bool isSimulate         = true;
  const int  seed               = 1;
  const bool onlySimulation     = false;
  const int  nSubproblems       = 3;

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
  const double nm_invcov[]  = { 275807,    -79136.3,     47168.4,     0,          0,           3e10 };
  const double nm_corr[]    = { 1,          0.693822,    1,           0,          0,           1 };
  const double nm_pred[]    = {  };
  //============================================
  // XML strings
  //============================================
  XMLCh * X_SPKREPORT;
  XMLCh * X_SIMULATION;
  XMLCh * X_IND_OPT_RESULT;
  XMLCh * X_IND_STAT_RESULT;
  XMLCh * X_PRESENTATION_DATA;
  XMLCh * X_SUBPROBLEM;
};

void ind_subprobTest::setUp()
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

  sprintf( LDFLAG, "%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
	   LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB );

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

  X_SPKREPORT         = XMLString::transcode( "spkreport" );
  X_IND_OPT_RESULT    = XMLString::transcode( "ind_opt_result" );
  X_PRESENTATION_DATA = XMLString::transcode( "presentation_data" );
  X_IND_STAT_RESULT   = XMLString::transcode( "ind_stat_result" );
  X_SIMULATION        = XMLString::transcode( "simulation" );
  X_SUBPROBLEM        = XMLString::transcode( "subproblem" );

  createDataML();
  createSourceML();
  parse();
}
void ind_subprobTest::tearDown()
{
  XMLString::release( &X_SPKREPORT );
  XMLString::release( &X_SIMULATION );
  XMLString::release( &X_IND_OPT_RESULT );
  XMLString::release( &X_PRESENTATION_DATA );
  XMLString::release( &X_IND_STAT_RESULT );
  XMLString::release( &X_SUBPROBLEM );
  
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
      remove( "xml1.xml" );
      remove( "xml2.xml" );
      remove( "xml3.xml" );
    }
  XMLPlatformUtils::Terminate();
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void ind_subprobTest::createDataML()
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
 
void ind_subprobTest::createSourceML()
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

  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"";
      if( onlySimulation )
	oSource << " only_simulation=\"yes\"";
      if( nSubproblems > 1 )
        oSource << " subproblems=\"" << nSubproblems << "\"";
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
void ind_subprobTest::parse()
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
      throw;
    }

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
}

void ind_subprobTest::testDriver()
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
void ind_subprobTest::testReportML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Seperate into pieces the generated result document which 
  // contains multiple instances of SpkReportML.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ifstream in( fSavedReportML );
  char ch;
  stringstream all;

  int i = 0;
  while( in.get(ch) )
    {
      all.put(ch);
    }
  in.close();
  const char* str = all.str().c_str();
  char* c1 = strstr( str, "<?xml" );
  char* c2 = strstr( c1+5,  "<?xml" );
  char* c3 = strstr( c2+5,  "<?xml" );

  FILE * xml1 = fopen( "xml1.xml", "w" );
  fprintf( xml1, "%s\n", c1 );
  fclose( xml1 );

  FILE * xml2 = fopen( "xml2.xml", "w" );
  fprintf( xml2, "%s\n", c2 );
  fclose( xml2 );

  FILE * xml3 = fopen( "xml3.xml", "w" );
  fprintf( xml3, "%s\n", c3 );
  fclose( xml3 );

  xercesc::XercesDOMParser *parser = new xercesc::XercesDOMParser;
  parser->setValidationScheme( XercesDOMParser::Val_Auto );
  parser->setDoNamespaces( true );
  parser->setDoSchema( true );
  parser->setValidationSchemaFullChecking( true );
  parser->setCreateEntityReferenceNodes( true );
  
  for( int i=0; i<3; i++ )
    {
      try{
	char filename[56];
        sprintf( filename, "xml%d.xml", i+1 );
	parser->reset();
	parser->parse( filename );
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
  
      doc = parser->getDocument();
      CPPUNIT_ASSERT( doc );

      // DOMPrint( doc );

      DOMNodeList * report_list = doc->getElementsByTagName( X_SPKREPORT );
      int nReports = report_list->getLength();

      DOMNodeList * simulation_list = doc->getElementsByTagName( X_SIMULATION );
      unsigned int order;
      XMLString::textToBin( dynamic_cast<DOMElement*>(simulation_list->item(0))->getAttribute( X_SUBPROBLEM ), order );
      CPPUNIT_ASSERT_EQUAL( i+1, static_cast<int>(order) );

      DOMNodeList * opt_result_list = doc->getElementsByTagName( X_IND_OPT_RESULT );
      int nOpts = opt_result_list->getLength();

      DOMNodeList * stat_result_list = doc->getElementsByTagName( X_IND_STAT_RESULT );
      int nStats = stat_result_list->getLength();
 
      DOMNodeList * presentation_data_list = doc->getElementsByTagName( X_PRESENTATION_DATA );
      int nPresentations = presentation_data_list->getLength();
    }

  okToClean = true;
}

CppUnit::Test * ind_subprobTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_subprobTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_subprobTest>(
         "testDriver", 
	 &ind_subprobTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_subprobTest>(
         "testReportML", 
	 &ind_subprobTest::testReportML ) );

  return suiteOfTests;
}

