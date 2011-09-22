#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_withIDTest.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "../../spkcompiler/nonmem/NonmemTranslator.h"
#include "../../spkcompiler/series.h"
#include "../../spkcompiler/SymbolTable.h"
#include "../../spkcompiler/SpkCompilerException.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// A test based upon a non-linear regression problem defined in job# 299.
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// NONMEM Control file
/*
$PROBLEM NIST MISRA1A
$DATA misra1a.dat
$INPUT DV TIME
$PRED 
B1 = THETA(1)
B2 = THETA(2)
F = B1*(1-EXP(-B2*TIME))
Y = F + ETA(1)
$THETA 
(50.0,500,5000)
(.00001,0.0001,0.001)
$OMEGA DIAGONAL(1) 0.001
$ESTIMATION METHOD=0 NOPOSTHOC SIGDIGITS=3 MAXEVALS=450 PRINT=5
$COVARIANCE
 */
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// NONMEM data file
/*
  1.0070E+01  7.7600E+01
  1.4730E+01  1.1490E+02
  1.7940E+01  1.4110E+02
  2.3930E+01  1.9080E+02
  2.9610E+01  2.3990E+02
  3.5180E+01  2.8900E+02
  4.0020E+01  3.3280E+02
  4.4820E+01  3.7840E+02
  5.0760E+01  4.3480E+02
  5.5050E+01  4.7730E+02
  6.1010E+01  5.3680E+02
  6.6400E+01  5.9310E+02
  7.5470E+01  6.8910E+02
  8.1780E+01  7.6000E+02
 */
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// NONMEM Result
/*
  ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 **************************************************        -52.102     **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                  FINAL PARAMETER ESTIMATE                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


            TH 1      TH 2
 
         2.39E+02  5.51E-04
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1
 
 ETA1
+        8.90E-03
 

 */
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

namespace{
  const unsigned int MAXCHARS = 256;

  const char * testName;
  char fSavedReportML[]   = "saved_result.xml";
  char fTraceOut[]        = "trace_output";
  char fFitDriver[]       = "driver";
  char fReportML[]        = "result.xml";
  char fMakefile[]        = "Makefile.SPK";
  char fFitDriver_cpp[]   = "fitDriver.cpp";

  char fPrefix              [MAXCHARS+1];
  char fDataML              [MAXCHARS+1];
  char fSourceML            [MAXCHARS+1];
  char fNonmemParsDriver    [MAXCHARS+1];
  char fNonmemParsDriver_cpp[MAXCHARS+1];
  char fMonteParsDriver     [MAXCHARS+1];
  char fMonteParsDriver_cpp [MAXCHARS+1];
  char fIndDataDriver       [MAXCHARS+1];
  char fIndDataDriver_cpp   [MAXCHARS+1];
  char fDataSetDriver       [MAXCHARS+1];
  char fDataSetDriver_cpp   [MAXCHARS+1];
  char fPredDriver          [MAXCHARS+1];
  char fPredDriver_cpp      [MAXCHARS+1];

  char SPKLIB[]     = "spk";
  char SPKPREDLIB[] = "spkpred";
  char SPKOPTLIB[]  = "QN01Box";
  char ATLASLIB[]   = "lapack_atlas";
  char CBLASLIB[]   = "cblas";
  char CLAPACKLIB[] = "atlas";
  char PTHREADLIB[] = "pthread";
  char MLIB[]       = "m";
  char XERCESCLIB[]  = "xerces-c";
  char CLNLIB[]      = "cln";
  char GINACLIB[]    = "ginac";
  char BADLIB[]      = "bad";
  char BAPLIB[]      = "bap";
  char BAVLIB[]      = "bav";
  char BA0LIB[]      = "ba0";
  char GSLLIB[]      = "gsl";
  char GSLCBLASLIB[] = "gslcblas";

  char LDPATH[]     = "../../spkcompiler/libcommon.a ../../spkcompiler/nonmem/libnonmem.a -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -L/usr/lib/atlas";
#ifndef SPK_RELEASE
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD -I/usr/local/include";
#else
  char CPPFLAG[]    = "-O3 -Dspk_release -DNDEBUG-I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD -I/usr/local/include";
#endif
  const unsigned int LDFLAG_MAXCHARS = 512;
  char LDFLAG[LDFLAG_MAXCHARS+1];

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
  const int  mitr       = 450;
  const bool isEstimate = true;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const char *strID   = "ID";
  const char *strTIME = "TiMe";
  const char *strDV   = "DV";
  const char *strCP   = "CP";
  const char *strAMT  = "AMT";
  const char *strMDV  = "MDV";
  const char *strEVID = "EVID";
  const char *label[] = { strID, strDV, strTIME };
  map<const char*, const char*> label_alias;
  int nLabels         = 3;

  //============================================
  // <Data Set>
  //============================================
  const int    nRecords   =  14;
  const int    nFixed     =  0;
  const int    nItems     =  3;
  const double record0[]  = { 1, 1.0070E+01, 7.7600E+01 };
  const double record1[]  = { 1, 1.4730E+01, 1.1490E+02 };
  const double record2[]  = { 1, 1.7940E+01, 1.4110E+02 };
  const double record3[]  = { 1, 2.3930E+01, 1.9080E+02 };
  const double record4[]  = { 1, 2.9610E+01, 2.3990E+02 };
  const double record5[]  = { 1, 3.5180E+01, 2.8900E+02 };
  const double record6[]  = { 1, 4.0020E+01, 3.3280E+02 };
  const double record7[]  = { 1, 4.4820E+01, 3.7840E+02 };
  const double record8[]  = { 1, 5.0760E+01, 4.3480E+02 };
  const double record9[]  = { 1, 5.5050E+01, 4.7730E+02 };
  const double record10[] = { 1, 6.1010E+01, 5.3680E+02 };
  const double record11[] = { 1, 6.6400E+01, 5.9310E+02 };
  const double record12[] = { 1, 7.5470E+01, 6.8910E+02 };
  const double record13[] = { 1, 8.1780E+01, 7.6000E+02 };

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
  const char *strIPRED  = "IPRED";
  const char *strIRES   = "IRES";
  const char *strIWRES  = "IWRES";
  const char *strF      = "F";
  const char *strY      = "Y";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen = 2;
  const double theta_low[ thetaLen ]   = {   50.0, 0.00001 };
  const double theta_in [ thetaLen ]   = {  500.0, 0.0001 };
  const double theta_up [ thetaLen ]   = { 5000.0, 0.001 };
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
  const double omega_in[ omegaOrder ]  = { 0.001 };
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
  const char *ind_covform       = "rsr";
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
  // PRED
  //
  // B1 = THETA(1)
  // B2 = THETA(2)
  // 
  // F = B1 * (1.0-EXP(-B2*TIME)) 
  // Y = F + ETA(1)
  //============================================
  const char PREDEQN[] = "B1 = THETA(1)\n \
B2 = THETA(2)\n \
F = B1*(1-EXP(-B2*TiMe))\n \
Y = F + ETA(1)\n";

  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  const double nm_obj       =  -52.102;
  const double nm_theta[]   = { 2.39E+02, 5.51E-04 };
  const double nm_omega[]   = { 8.90E-03 };
};

void ind_withIDTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy  ( fPrefix,               testName );
  snprintf( fMonteParsDriver,      MAXCHARS, "%s_MonteParsDriver",      fPrefix );
  snprintf( fMonteParsDriver_cpp,  MAXCHARS, "%s_MonteParsDriver.cpp",  fPrefix );
  snprintf( fNonmemParsDriver,     MAXCHARS, "%s_NonmemParsDriver",     fPrefix );
  snprintf( fNonmemParsDriver_cpp, MAXCHARS, "%s_NonmemParsDriver.cpp", fPrefix );
  snprintf( fIndDataDriver,        MAXCHARS, "%s_IndDataDriver",        fPrefix );
  snprintf( fIndDataDriver_cpp,    MAXCHARS, "%s_IndDataDriver.cpp",    fPrefix );
  snprintf( fDataML,               MAXCHARS, "%s_dataML",               fPrefix );
  snprintf( fSourceML,             MAXCHARS, "%s_sourceML.xml",         fPrefix );
  snprintf( fDataSetDriver,        MAXCHARS, "%s_DataSetDriver",        fPrefix );
  snprintf( fDataSetDriver_cpp,    MAXCHARS, "%s_DataSetDriver.cpp",    fPrefix );
  snprintf( fPredDriver,           MAXCHARS, "%s_PredDriver",           fPrefix );
  snprintf( fPredDriver_cpp,       MAXCHARS, "%s_PredDriver.cpp",       fPrefix );

  snprintf( LDFLAG, LDFLAG_MAXCHARS, "%s -l%s -l%s  -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
     LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB, CLNLIB, GINACLIB, BADLIB, BAPLIB, BAVLIB, BA0LIB, GSLLIB, GSLCBLASLIB );

  // ID doesn't have an alias
  label_alias[strID]   = NULL;

  // TIME doesn't have an alias
  label_alias[strTIME] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;

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

  createDataML();
  createSourceML();
  try{
    parse();
  }
  catch( const SpkCompilerException & e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "Failed to compile.", false );
    }
}
void ind_withIDTest::tearDown()
{
  if( okToClean )
    {
      remove( fDataML );
      remove( fSourceML );
      remove( fReportML );
      remove( fFitDriver );
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
      remove( fMakefile );
      remove( fSavedReportML );
      remove( fTraceOut );
    }
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void ind_withIDTest::createDataML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document (with ID)
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ofstream oData( fDataML );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nLabels << "\" rows=\"" << nRecords +1 << "\">" << endl;
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
      snprintf( buf, MAXCHARS, "An error occurred during parsing %s.\n   Message: %s\n",
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
	  snprintf( buf, MAXCHARS, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
		   fDataML, e.code, XMLString::transcode(errText) );
	  
	  CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      snprintf( buf, MAXCHARS, "An unknown error occurred during parsing %s.\n", fDataML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
 
void ind_withIDTest::createSourceML()
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
  oSource << "covariance_form=\""           << ind_covform << "\" ";
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
  oSource << "   " << PREDEQN << endl;
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
      snprintf( buf, MAXCHARS, "An error occurred during parsing %s.\n   Message: %s\n",
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
	  snprintf( buf, MAXCHARS, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
		   fSourceML, e.code, XMLString::transcode(errText) );
	  CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      snprintf( buf, MAXCHARS, "An unknown error occurred during parsing %s.\n", fSourceML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Translation
// 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ind_withIDTest::parse()
{
  //============================================
  // Instanciate a NonmemTranslator object, 
  // passing the pointers to the sourceML 
  // document tree and the dataML document tree.
  //============================================
  NonmemTranslator xlator( source, data );

  try{
    xlator.translate();
  }
  catch( const SpkCompilerException & e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "Failed to compile.", false );
    }
  catch(...)
    {
      CPPUNIT_ASSERT_MESSAGE( "Failed to compile.", false );
    }

  SymbolTable *table = xlator.getSymbolTable();

  // ID, TIME, DV were in the data set.  So, they should be in the symbol table already.
  Symbol * id   = table->find( strID );
  CPPUNIT_ASSERT( id != Symbol::empty() );
  Symbol * time = table->find( strTIME );
  CPPUNIT_ASSERT( time != Symbol::empty() );
  Symbol * dv   = table->find( strDV );
  CPPUNIT_ASSERT( dv != Symbol::empty() );

  Symbol * cp   = table->find( strCP );
  CPPUNIT_ASSERT( cp != Symbol::empty() );

  // THETA, OMEGA, ETA must be registered for individual analysis.
  Symbol * theta = table->find( strTHETA );
  CPPUNIT_ASSERT( theta != Symbol::empty() );
  Symbol * omega = table->find( strOMEGA );
  CPPUNIT_ASSERT( omega != Symbol::empty() );
  Symbol * eta = table->find( strETA );
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
  FILE * nonmemPars = fopen( "NonmemPars.h", "r" );
  assert( nonmemPars != NULL );
  CPPUNIT_ASSERT_MESSAGE( "Missing NonmemPars.h", nonmemPars != NULL );
  fclose( nonmemPars );

  FILE * montePars = fopen("MontePars.h", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing MontePars.h", montePars == NULL );

  FILE * indData = fopen( "IndData.h", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing IndData.h", indData != NULL );
  fclose( indData );

  FILE * dataSet = fopen( "DataSet.h", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing DataSet.h", dataSet != NULL );
  fclose( dataSet );

  FILE * pred = fopen( "Pred.h", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing Pred.h", pred != NULL );
  fclose( pred );

  FILE * makeSPK = fopen( "Makefile.SPK", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing Makefile.SPK", makeSPK != NULL );
  fclose( makeSPK );
 
  FILE * fitDriver = fopen( "fitDriver.cpp", "r" );
  CPPUNIT_ASSERT_MESSAGE( "Missing fitDriver.cpp", fitDriver != NULL );
  fclose( fitDriver );
}
void ind_withIDTest::testNonmemPars_h()
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
  snprintf( command, 512, "g++ %s -o %s %s %s",
	   fNonmemParsDriver_cpp, 
	   fNonmemParsDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Failed to build %s.", fNonmemParsDriver );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  // Run the test driver
  snprintf( command, 512, "./%s", fNonmemParsDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "%s abnormally terminated.", fNonmemParsDriver );
      CPPUNIT_ASSERT_MESSAGE( message, false );      
    }
}
void ind_withIDTest::testIndDataClass()
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
  // * IPRED
  // * IWRES
  // * IRES
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
  o << "   vector<double> a_amt(n);" << endl;
  o << "   vector<double> a_mdv(n);" << endl;
  o << "   vector<int>    a_evid(n);" << endl;

  for( int i=0; i<nRecords; i++ )
    {
      o << "   a_id  [" << i << "] = \"" << record[i][0] << "\";" << endl;
      o << "   a_dv  [" << i << "] = "   << record[i][1] << ";" << endl;
      o << "   a_time[" << i << "] = "   << record[i][2] << ";" << endl;
      o << "   a_amt [" << i << "] = "   << 0.0 << ";" << endl;
      o << "   a_mdv [" << i << "] = "   << 0.0 << ";" << endl;
      o << "   a_evid[" << i << "] = "   << 0  << ";" << endl;
    }

  o << "   IndData<double> A( n, a_id, a_dv, a_time, a_amt, a_mdv, a_evid );" << endl;

  // { ID, DV=CP, TIME }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A."   << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A."   << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A."   << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0 << ", A." << strAMT  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0 << ", A." << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0   << ", A." << strEVID << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to Pred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,   A." << strETA   << "[" << i << "].size() );" << endl;
      o << endl;
    }
  o << endl;  

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, A." << strIRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIPRED << ".size() );" << endl;

  o << "   MY_ASSERT_EQUAL( n, A." << strF    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strY    << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nFixed << ", y.size() );" << endl;
  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( y[j], A." << strDV   << "[j] );" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withIDTest::testDataSetClass()
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

  // { ID, DV=CP, TIME }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
    }

  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( " << thetaLen << ", set.data[0]->" << strTHETA << "[j].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( " << etaLen   << ", set.data[0]->" << strETA   << "[j].size() );" << endl;
  o << "   }" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIPRED << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strY    << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "       MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[j], y[j] );" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withIDTest::testPredClass()
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
  o << "      expectedF1[j]   = CppAD::Value(indepVar[thetaOffset+0] * (1.0 - exp(-indepVar[thetaOffset+1] * set.data[who]->" << strTIME << "[j] ) ) );" << endl;
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
  o << "      expectedF2[j]   = CppAD::Value(indepVar[thetaOffset+0] * (1.0 -exp( -indepVar[thetaOffset+1] * set.data[who]->" << strTIME << "[j] ) ) );" << endl;
  //o << "      expectedF2[j]  = CppAD::Value(indepVar[thetaOffset+0] + indepVar[thetaOffset+1] * set.data[who]->";
  //o << strTIME << "[j] );" << endl;
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
  o << "      MY_ASSERT_EQUAL( expectedF1[j], set.data[who]->" << strF << "[j] );" << endl;
  o << "      MY_ASSERT_EQUAL( expectedY1[j], set.data[who]->" << strY << "[j] );" << endl;
  o << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  o << "   return !ok;" << endl;
  o << "}" << endl;
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fPredDriver_cpp, fPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withIDTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", "fitDriver" );
  int  exitcode      = 0;
  char command[512];
  snprintf( command, 512, "make -f %s debug", fMakefile );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fFitDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s > %s", fFitDriver, fTraceOut );

  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "%s failed for convergence problem <%d>!", fFitDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode == 2 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "%s failed due to inproper file access permission <%d>!", fFitDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode > 2 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, 
	       "%s failed for reasons other than convergence propblem or access permission <%d>!", 
	       fFitDriver, 
	       exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, true );
    }
  if( rename( fReportML, fSavedReportML ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Failed to rename %s to %s!", fReportML, fSavedReportML );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withIDTest::testReportML()
{
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
      snprintf( buf, MAXCHARS, "An error occurred during parsing %s.\n   Message: %s\n",
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
	  snprintf( buf, MAXCHARS, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
		   fReportML, e.code, XMLString::transcode(errText) );
	  CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      snprintf( buf, MAXCHARS, "An unknown error occurred during parsing %s.\n", fSavedReportML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  
  report = reportParser->getDocument();
  CPPUNIT_ASSERT( report );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify if any error was caught during the runtime.
  // The <eroor_list> tag should appear even when there's no error.
  // However, it should not contain any error message.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *error_list;
  
  error_list = report->getElementsByTagName ( XML.X_ERROR_LIST );
  CPPUNIT_ASSERT_EQUAL( 1, (int)error_list->getLength() );
  DOMElement* error = dynamic_cast<DOMElement*>( error_list->item(0) );
  const XMLCh* error_message = error->getFirstChild()->getNodeValue();
  CPPUNIT_ASSERT_MESSAGE( "<error_list> should have been empty.", XMLString::isAllWhiteSpace( error_message ) );
   
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for theta
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double theta_out[thetaLen];
  DOMNodeList * thetaOut_list = report->getElementsByTagName( XML.X_THETA_OUT );
  if( thetaOut_list->getLength() > 0 )
    {
      DOMElement* thetaOut = dynamic_cast<DOMElement*>( thetaOut_list->item(0) );
      DOMNodeList* value_list = thetaOut->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( thetaLen, n );
      for( int i=0; i<n; i++ )
	{
	  theta_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], nm_theta[i] / 1000.0 * 5.0 );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for Omega
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double omega_out[omegaOrder];
  DOMNodeList * omegaOut_list = report->getElementsByTagName( XML.X_OMEGA_OUT );
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list = omegaOut->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
      for( int i=0; i<+n; i++ )
	{
	  omega_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], nm_omega[i] / 1000.0 * 5.0 );
	}
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( XML.X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * ind_withIDTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_withIDTest"  );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testNonmemPars_h", 
								&ind_withIDTest::testNonmemPars_h ) );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testIndDataClass", 
								&ind_withIDTest::testIndDataClass ) );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testDataSetClass", 
								&ind_withIDTest::testDataSetClass ) );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testPredClass", 
								&ind_withIDTest::testPredClass ) );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testDriver", 
								&ind_withIDTest::testDriver ) );
  suiteOfTests->addTest( 
			new CppUnit::TestCaller<ind_withIDTest>(
								"testReportML", 
								&ind_withIDTest::testReportML ) );
  return suiteOfTests;
}

