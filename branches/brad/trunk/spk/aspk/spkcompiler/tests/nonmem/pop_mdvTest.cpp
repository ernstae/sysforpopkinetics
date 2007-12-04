#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_mdvTest.h"
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

namespace{ 
  const unsigned int MAXCHARS = 512;

  const char * testName;
  char fSavedReportML[]   = "saved_result.xml";
  char fTraceOut[]        = "trace_output";
  char fFitDriver[]       = "driver";
  char fReportML[]        = "result.xml";
  char fFitDriver_cpp[]   = "fitDriver.cpp";
  char fMakefile[]        = "Makefile.SPK";

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
  char CPPFLAG[]    = "-O3 -Dspk_release -DNDEBUG -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD -I/usr/local/include";
#endif
  const unsigned int LDFLAG_MAXCHARS = 512;
  char LDFLAG[LDFLAG_MAXCHARS+1];

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
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = true;
  const char method[]   = "fo";
  const int  sig_digits = 3;

  //============================================
  // <Data Set>
  //
  //   ID      TIME     CP=DV   MDV
  //   1       0.0       0.0      1
  //   1       0.0       0.0      0
  //   2       0.0       0.0      1
  //   2       0.0       0.0      0
  //   2       1.0      10.0      0
  //   3       0.0       0.0      1
  //   3       0.0       0.0      0
  //   3       1.0      10.0      0
  //   3       2.0      20.0      0
  //   4       0.0       0.0      1
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
  const char *strAMT        = "AMT";
  const char *strEVID       = "EVID";
  const char *label[]       = { strID, strDV, strTIME, strMDV };
  const int    nLabels      = 4;
  const int    nIndividuals = 4;
  const int    nRecords     = 14;
  const int    nFixed       = 0;
  const int    nDVs         = 10;
  const int    nMDVs        = 4;
  const int    nItems       = nLabels;
  valarray<int> N( nIndividuals );
  const double record0 [] = { 1, 0.0,  0.0, 1 };
  const double record1 [] = { 1, 0.0,  0.0, 0 };
  const double record2 [] = { 2, 0.0,  0.0, 1 };
  const double record3 [] = { 2, 0.0,  0.0, 0 };
  const double record4 [] = { 2, 1.0, 10.0, 0 };
  const double record5 [] = { 3, 0.0,  0.0, 1 };
  const double record6 [] = { 3, 0.0,  0.0, 0 };
  const double record7 [] = { 3, 1.0, 10.0, 0 };
  const double record8 [] = { 3, 2.0, 20.0, 0 };
  const double record9 [] = { 4, 0.0,  0.0, 1 };
  const double record10[] = { 4, 0.0,  0.0, 0 };
  const double record11[] = { 4, 1.0, 10.0, 0 };
  const double record12[] = { 4, 2.0, 20.0, 0 };
  const double record13[] = { 4, 3.0, 25.0, 0 };
  double const * record[nRecords];

  //============================================
  // Define NONMEM keywords
  //============================================
  const char *strTHETA    = "THETA";
  const char *strOMEGA    = "OMEGA";
  const char *strSIGMA    = "SIGMA";
  const char *strETA      = "ETA";
  const char *strEPS      = "EPS";
  const char *strPRED     = "PRED";
  const char *strIPRED    = "IPRED";
  const char *strIRES     = "IRES";
  const char *strIWRES    = "IWRES";
  const char *strIETARES  = "IETARES";
  const char *strIWETARES = "IWETARES";
  const char *strPPRED    = "PPRED";
  const char *strPRES     = "PRES";
  const char *strPWRES    = "PWRES";
  const char *strPETARES  = "PETARES";
  const char *strPWETARES = "PWETARES";
  const char *strF        = "F";
  const char *strY        = "Y";

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
  const double eta_in  [ etaLen ] = { 0.0, 0.0 };
  const bool   eta_fix [ etaLen ] = { false, false };

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
  const char PREDEQN[]     = "ka = THETA(1) + ETA(1)\nke = THETA(2) + ETA(2)\nF = ke * ka\nY = F + EPS(1) + EPS(2)\n";

};
void pop_mdvTest::setUp()
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
  snprintf( fDataML,               MAXCHARS, "%s_dataML.xml",           fPrefix );
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

  // MDV doesn't have an alias.
  label_alias[strMDV]  = NULL;

  // #of records for each individual
  N[0] = 2;
  N[1] = 3;
  N[2] = 4;
  N[3] = 5;

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
  parse();
}
void pop_mdvTest::tearDown()
{
  if( okToClean )
    {
      remove( fDataML );
      remove( fSourceML );
      remove( fReportML );
      remove( fFitDriver );
      remove( fFitDriver_cpp );
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
void pop_mdvTest::createDataML()
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
  data = dataParser->getDocument();
  assert( data );
}
 
void pop_mdvTest::createSourceML()
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

  // default: is_eta_out=yes, is_restart=yes
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
void pop_mdvTest::parse()
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
}

void pop_mdvTest::testIndDataClass()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test IndData class to see if it has all necessary 
  // variables declared and sized.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  o << "   vector<double> a_amt(n);" << endl;
  o << "   vector<int>    a_evid(n);" << endl;
 
  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_id  [" << i << "] = \"" << record[i][0] << "\";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_time[" << i << "] = "   << record[i][2] << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << record[i][3] << ";" << endl;
    o << "   a_amt [" << i << "] = "   << 0.0 << ";" << endl;
    o << "   a_evid[" << i << "] = "   << 0   << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_dv, a_time, a_mdv, a_amt, a_evid );" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", A." << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0          << ", A." << strAMT  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0            << ", A." << strEVID << "[" << i << "] );" << endl;
      o << endl;
    }
  o << endl;  

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nDVs-nFixed << ", y.size() );" << endl;
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( A." << strMDV << "[j] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( A." << strDV << "[j], y[k] );" << endl;
  o << "         MY_ASSERT_EQUAL( j, A.getRecordIndex( k ) );" << endl;
  o << "         MY_ASSERT_EQUAL( k, A.getMeasurementIndex( j ) );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 1024, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void pop_mdvTest::testDataSetClass()
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
  o << "   const int n = " << nRecords << ";" << endl;
  o << "   const int thetaLen = " << thetaLen << ";" << endl;
  o << "   const int etaLen = " << etaLen << ";" << endl;
  o << "   const int nIndividuals = " << nIndividuals << ";" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   valarray<int> N = set.getN();" << endl;
  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nMDVs << ", y.size() );" << endl;
  o << "   for( int j=0, k=0, l=0; j<nIndividuals; j++ )" << endl;
  o << "   {" << endl;
  o << "      for( int i=0; i<set.getNRecords(j); i++, l++)" << endl;
  o << "      {" << endl;
  o << "         if( set.data[j]->" << strMDV << "[i] == 0 )" << endl;
  o << "         {" << endl;
  o << "            MY_ASSERT_EQUAL( set.data[j]->" << strDV << "[i], y[k] );" << endl;
  o << "            MY_ASSERT_EQUAL( k, set.getMeasurementIndex(l) );" << endl;
  o << "            MY_ASSERT_EQUAL( l, set.getRecordIndex(k) );" << endl;
  o << "            MY_ASSERT_EQUAL( set.data[j]->" << strDV << "[i], y[ set.getMeasurementIndex(l)] );" << endl;
  o << "            k++;" << endl;
  o << "         }" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << endl;
  o << "return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_mdvTest::testPredClass()
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
  o << "   const int nIndividuals = " << nIndividuals << ";" << endl;
  o << "   DataSet< CppAD::AD<double> > set;" << endl;
  o << "   const valarray<int> N = set.getN();" << endl;
  o << "   Pred< CppAD::AD<double> > pred( &set );" << endl;
  o << "   const int thetaLen    = " << thetaLen << ";" << endl;
  o << "   const int etaLen      = " << etaLen << ";" << endl;
  o << "   const int epsLen      = " << epsLen << ";" << endl;
  o << "   const int thetaOffset = 0;" << endl;
  o << "   const int etaOffset   = thetaLen;" << endl;
  o << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  o << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  o << endl;
  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nMDVs << ", y.size() );" << endl;
  o << "   for( int i=0, k=0, l=0; i<nIndividuals; i++ )" << endl;
  o << "   {" << endl;
  o << "      int nRecords = i+2;" << endl;
  o << "      int nObservs = i+1;" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, pred.getNRecords( i ) );" << endl;
  o << "      MY_ASSERT_EQUAL( nObservs, pred.getNObservs( i ) );" << endl;
  o << "      MY_ASSERT_EQUAL( nObservs, N[i] );" << endl;
  o << "      for( int j=0; j<nRecords; j++, l++ )" << endl;
  o << "      {" << endl;
  o << "         if( set.data[i]->" << strMDV << "[j] != 1 )" << endl;
  o << "         {" << endl;
  o << "            MY_ASSERT_EQUAL( k, pred.getMeasurementIndex( l ) );" << endl;
  o << "            MY_ASSERT_EQUAL( l, pred.getRecordIndex( k ) );" << endl;
  o << "            k++;" << endl;
  o << "         }" << endl;
  o << "      }" << endl;
  o << "   }" << endl;



  o << "   return !ok;" << endl;
  o << "}" << endl;
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fPredDriver_cpp, fPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 1024, "./%s", fPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  okToClean = true;
}

CppUnit::Test * pop_mdvTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_mdvTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_mdvTest>(
         "testIndDataClass", 
	 &pop_mdvTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_mdvTest>(
         "testDataSetClass", 
	 &pop_mdvTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_mdvTest>(
         "testPredClass", 
	 &pop_mdvTest::testPredClass ) );

  return suiteOfTests;
}

