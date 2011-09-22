#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_nonparamMethodRandomUniformTest.h"
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
#ifndef NDEBUG
  char CXXFLAGS[]   = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD -I/usr/local/include";
#else
  char CXXFLAGS[]   = "-O3 -Dspk_release -DNDEBUG -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD -I/usr/local/include";
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
  // Nonparametric method inputs
  //============================================
  const string approximation        = "nonparametric";
  const string auto_generate_method = "random_uniform";

  int number_of_points;
  int seed;
  int points_per_dimension;


  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = true;
  const int  sig_digits = 3;


  //============================================
  // <Data Set>
  //
  //   ID      TIME     CP=DV     (AMT)
  //   1       0.0       0.0       0.0
  //   2       0.0       0.0       0.0
  //   2       1.0      10.0       0.0
  //   3       0.0       0.0       0.0
  //   3       1.0      10.0       0.0
  //   3       2.0      20.0       0.0
  //   4       0.0       0.0       0.0
  //   4       1.0      10.0       0.0 
  //   4       2.0      20.0       0.0
  //   4       3.0      25.0       0.0
  //============================================
  map<const char*, const char*> label_alias;
  const char *strID         = "ID";
  const char *strTIME       = "TiMe";
  const char *strDV         = "DV";
  const char *strCP         = "CP";
  const char *strAMT        = "AMT";
  const char *strMDV        = "MDV";
  const char *strEVID       = "EVID";
  const char *label[]       = { strID, strTIME, strDV };
  const int    nLabels      = 3;
  const int    nIndividuals = 4;
  const int    nRecords     = 10;
  const int    nFixed       = 0;
  const int    nItems       = nLabels;
  valarray<int> N( nIndividuals );
  const double record0[] = { 1, 0.0,  0.0 };
  const double record1[] = { 2, 0.0,  0.0 };
  const double record2[] = { 2, 1.0, 10.0 };
  const double record3[] = { 3, 0.0,  0.0 };
  const double record4[] = { 3, 1.0, 10.0 };
  const double record5[] = { 3, 2.0, 20.0 };
  const double record6[] = { 4, 0.0,  0.0 };
  const double record7[] = { 4, 1.0, 10.0 };
  const double record8[] = { 4, 2.0, 20.0 };
  const double record9[] = { 4, 3.0, 25.0 };
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
  //
  // For the nonparametric method, 
  //
  //      nThetaPop  =  nThetaInd  .
  // 
  //============================================
  const int    thetaLen = 2;
  const double theta_in [ thetaLen ]   = {  1.0,  2.0 };
  const double theta_up [ thetaLen ]   = { 11.0, 12.0 };
  const double theta_low[ thetaLen ]   = { -9.0, -8.0 };
  const bool   theta_fix[ thetaLen ]   = { false, false };

  //============================================
  // The SPK Compiler determines the initial
  // values for eta, the variance of data
  // in the individual analysis case.
  //
  // For the nonparametric method, 
  //
  //      nEtaPop  =  nThetaInd  .
  // 
  //============================================
  const int etaLen = thetaLen;
  const double eta_in  [ etaLen ] = { 0.0, 0.0 };
  const bool   eta_fix [ etaLen ] = { false, false };

  const double i_eta_res [ etaLen * nIndividuals ] = { 1.1, 1.2,   /* for the 1st patient */
			                               2.1, 2.2,   /* 2nd patient */
                                                       3.1, 3.2,   /* 3rd patient */
                                                       4.1, 4.2 }; /* 4th patient */

  const double i_eta_wres[ etaLen * nIndividuals ] = { 1.1, 1.2,
                                                       2.1, 2.2,
                                                       3.1, 3.2,
                                                       4.1, 4.2 };

  const double p_eta_res [ etaLen * nIndividuals ] = { 11.1, 11.2,   /* for the 1st patient */
			                               12.1, 12.2,   /* 2nd patient */
                                                       13.1, 13.2,   /* 3rd patient */
                                                       14.1, 14.2 }; /* 4th patient */

  const double p_eta_wres[ etaLen * nIndividuals ] = { 11.1, 11.2,
                                                       12.1, 12.2,
                                                       13.1, 13.2,
                                                       14.1, 14.2 };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values as an identity matrix.
  //
  // For the nonparametric method,
  //
  //      OmegaPop  =  nThetaInd by nThetaInd .
  // 
  // and the population Omega must be full.
  //============================================
  const int    omegaDim                = etaLen;
  const Symbol::Structure omegaStruct  = Symbol::FULL;
  const int    omegaOrder              = (omegaStruct==Symbol::DIAGONAL? 
					  omegaDim : omegaDim * (omegaDim+1)/2 );
  const double omega_in [ omegaOrder ] = { 1.0, 0.0, 1.0 };
  const bool   omega_fix[ omegaOrder ] = { false, false, false };

  //============================================
  // For the nonparametric method,
  //
  //      nEpsPop  =  nEtaInd  .
  // 
  // Set the number of EPS's equal to the number of ETA's that appear
  // in the Pred block expression for this test.
  //============================================
  const int epsLen = 2;
   
  //============================================
  // The SPK Compiler decides the constraints
  // of Sigma matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal identity matrix.
  //
  // For the nonparametric method,
  //
  //      SigmaStructPop  =  OmegaStructInd  ,
  // 
  //      SigmaPop        =  nEtaInd by nEtaInd .
  //
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
  // PRED model
  //
  // Note that for the nonparametric method the Pred block expressions
  // use individual model notation.
  //
  // KA=THETA(1)
  // KE=THETA(2)
  // F=KE*KA
  // Y=F+ETA(1)+ETA(2)
  //============================================
  const char PREDEQN[]     = "ka = THETA(1)\nke = THETA(2)\nF = ke * ka\nY = F + ETA(1) + ETA(2)\n";
};

void pop_nonparamMethodRandomUniformTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,               testName );
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

  createDataML();
  createSourceML();
  parse();
}
void pop_nonparamMethodRandomUniformTest::tearDown()
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
      remove( fSavedReportML );
      remove( fTraceOut );
    }
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void pop_nonparamMethodRandomUniformTest::createDataML()
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
 
void pop_nonparamMethodRandomUniformTest::createSourceML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preparation for creating a sourceML document
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  // Set the inputs required for the different grid generation
  // methods.
  if ( auto_generate_method == "random_uniform" )
  {
    number_of_points = 2 * nIndividuals;
    seed             = 123;
  }
  else
  {
    points_per_dimension = 5;
  }

  //============================================
  // Create an sourceML based upon the
  // parameters set so far.
  //============================================
  ofstream oSource( fSourceML );
  CPPUNIT_ASSERT( oSource.good() );

  oSource << "<spksource>" << endl;
  oSource << "<nonmem>" << endl;
      
  oSource << "<constraint>" << endl;

  oSource << "<pop_analysis ";
  oSource << "mitr=\"" << mitr << "\" ";
  oSource << "is_estimation=\"" << (isEstimate? "yes" : "no") << "\" ";
  oSource << "approximation=\"" << approximation << "\" ";
  oSource << "pop_size=\"" << nIndividuals << "\"";
  oSource << ">" << endl;

  oSource << "<nonparametric_info>" << endl;
  oSource << "<measure_points_in ";
  oSource << "auto_generate_method=\"" << auto_generate_method << "\" ";

  if ( auto_generate_method == "random_uniform" )
  {
    oSource << "number_of_points=\"" << number_of_points << "\" ";
    oSource << "seed=\"" << seed << "\"";
  }
  else
  {
    oSource << "points_per_dimension=\"" << points_per_dimension << "\"";
  }
  oSource << "/>" << endl;
  oSource << "</nonparametric_info>" << endl;

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
void pop_nonparamMethodRandomUniformTest::parse()
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
void pop_nonparamMethodRandomUniformTest::testIndDataClass()
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
  // * PRED
  // * PPRED
  // * PWRES
  // * PRES
  // * PETARES
  // * PWETARES
  // * IPRED
  // * IWRES
  // * IRES
  // * IETARES
  // * IWETARES
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
    o << "   a_time[" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][2] << ";" << endl;
    o << "   a_amt [" << i << "] = "   << 0.0 << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << 0.0 << ";" << endl;
    o << "   a_evid[" << i << "] = "   << 0   << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_time, a_dv, a_amt, a_mdv, a_evid );" << endl;

  // { ID, DV=CP, TIME }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0 << ", A." << strAMT  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0 << ", A." << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << 0.0 << ", A." << strEVID << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to Pred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,   A." << strETA   << "[" << i << "].size() );" << endl;
      o << endl;
    }
  o << endl;  

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, A." << strIRES     << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIWRES    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIPRED    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIETARES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIWETARES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPRES     << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPWRES    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPPRED    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPETARES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strPWETARES << ".size() );" << endl;
  for( int i=0; i<nRecords; i++ )
    {
      o << "   MY_ASSERT_EQUAL( etaLen, A." << strIETARES  << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen, A." << strIWETARES << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen, A." << strPETARES  << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen, A." << strPWETARES << "[" << i << "].size() );" << endl;
    }
  o << "   MY_ASSERT_EQUAL( n, A." << strF       << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strY       << ".size() );" << endl;
  o << endl;

  o << "   valarray<double> iEtaRes   ( etaLen );" << endl;
  o << "   valarray<double> iEtaResWtd( etaLen );" << endl;
  o << "   valarray<double> pEtaRes   ( etaLen );" << endl;
  o << "   valarray<double> pEtaResWtd( etaLen );" << endl;
  for( int i=0; i<etaLen; i++ )
  {
     o << "   iEtaRes   [" << i << "] = " << i_eta_res [i] << ";" << endl;
     o << "   iEtaResWtd[" << i << "] = " << i_eta_wres[i] << ";" << endl;
     o << "   pEtaRes   [" << i << "] = " << p_eta_res [i] << ";" << endl;
     o << "   pEtaResWtd[" << i << "] = " << p_eta_wres[i] << ";" << endl;
  }
  o << "   A.replaceIEtaRes ( iEtaRes );" << endl;
  o << "   A.replaceIWEtaRes( iEtaResWtd );" << endl;
  o << "   A.replacePEtaRes ( pEtaRes );" << endl;
  o << "   A.replacePWEtaRes( pEtaResWtd );" << endl;

  for( int i=0; i<nRecords; i++ )
  {
     for( int j=0; j<etaLen; j++ )
     {
        o << "   MY_ASSERT_EQUAL( iEtaRes   [" << j << "]" << ", A." << strIETARES;
	o << "[" << i << "][" << j << "] );" << endl;
        o << "   MY_ASSERT_EQUAL( iEtaResWtd[" << j << "]" << ", A." << strIWETARES;
	o << "[" << i << "][" << j << "] );" << endl;
        o << "   MY_ASSERT_EQUAL( pEtaRes   [" << j << "]" << ", A." << strPETARES;
	o << "[" << i << "][" << j << "] );" << endl;
        o << "   MY_ASSERT_EQUAL( pEtaResWtd[" << j << "]" << ", A." << strPWETARES;
	o << "[" << i << "][" << j << "] );" << endl;
     }
  }

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nFixed << ", y.size() );" << endl;
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( A." << strDV << "[j], y[j] );" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CXXFLAGS );
cout << "command: " << command << endl;
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, MAXCHARS, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void pop_nonparamMethodRandomUniformTest::testDataSetClass()
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
  // { ID, DV=CP, TIME }
  for( int j=0, k=0; j<nIndividuals; j++ )
  {
     for( int i=0; i<N[j]; i++, k++ )
     {
       o << "   assert( strcmp( set.data[" << j << "]->" << strID << "[" << i << "], \"" << record[k][0] << "\" ) == 0 );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][1] << ", set.data[" << j << "]->" << strTIME;
       o << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][2] << ", set.data[" << j << "]->" << strCP;
       o << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][2] << ", set.data[" << j << "]->" << strDV;
       o << "[" << i << "] );" << endl;
     }
  }

  o << "for( int j=0; j<nIndividuals; j++ )" << endl;
  o << "{" << endl;
  o << "   for( int i=0; i<N[j]; i++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( thetaLen, set.data[j]->" << strTHETA    << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"   << strETA      << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"   << strIETARES  << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"   << strIWETARES << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"   << strPETARES  << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"   << strPWETARES << "[i].size() );" << endl;
  o << "   }" << endl;
  o << "}" << endl;

  // The current values of RES/WRES/PRED and ETARES/WETARES (for pop) should be always kept in memory
  // for displaying tables/scatterplots.
  for( int j=0; j<nIndividuals; j++ )
    {
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIRES     << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIWRES    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIPRED    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIETARES  << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIWETARES << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPRES     << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPWRES    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPPRED    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPETARES  << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPWETARES << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strF       << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strY       << ".size() );" << endl;
      o << endl;
    }

  o << "   valarray<double> iEtaRes   ( etaLen * nIndividuals );" << endl;
  o << "   valarray<double> iEtaResWtd( etaLen * nIndividuals );" << endl;
  o << "   valarray<double> pEtaRes   ( etaLen * nIndividuals );" << endl;
  o << "   valarray<double> pEtaResWtd( etaLen * nIndividuals );" << endl;
  for( int i=0; i<etaLen*nIndividuals; i++ )
  {
     o << "   iEtaRes   [" << i << "] = " << i_eta_res[i] << ";" << endl;
     o << "   iEtaResWtd[" << i << "] = " << i_eta_wres[i] << ";" << endl;
     o << "   pEtaRes   [" << i << "] = " << p_eta_res[i] << ";" << endl;
     o << "   pEtaResWtd[" << i << "] = " << p_eta_wres[i] << ";" << endl;
  }
  o << "   set.replaceIEtaRes ( iEtaRes );" << endl;
  o << "   set.replaceIWEtaRes( iEtaResWtd );" << endl;
  o << "   set.replacePEtaRes ( pEtaRes );" << endl;
  o << "   set.replacePWEtaRes( pEtaResWtd );" << endl;

  for( int k=0; k<nIndividuals; k++ )
  {
     for( int i=0; i<N[k]; i++ )
     {
        for( int j=0; j<etaLen; j++ )
        {
	  o << "   MY_ASSERT_EQUAL( iEtaRes   [" << j + k*etaLen << "]";
	  o << ", set.data[" << k << "]->" << strIETARES  << "[" << i << "][" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( iEtaResWtd[" << j + k*etaLen << "]";
	  o << ", set.data[" << k << "]->" << strIWETARES << "[" << i << "][" << j << "] );" << endl;
 	  o << "   MY_ASSERT_EQUAL( pEtaRes   [" << j + k*etaLen << "]";
	  o << ", set.data[" << k << "]->" << strPETARES  << "[" << i << "][" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( pEtaResWtd[" << j + k*etaLen << "]";
	  o << ", set.data[" << k << "]->" << strPWETARES << "[" << i << "][" << j << "] );" << endl;
        }
     }
  }
  o << "const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "for( int j=0, k=0 ; j<nIndividuals; j++ )" << endl;
  o << "{" << endl;
  o << "   for( int i=0; i<N[j]; i++, k++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( set.data[j]->" << strDV << "[i], y[k] );" << endl;
  o << "   }" << endl;
  o << "}" << endl;

  o << endl;
  o << "return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CXXFLAGS );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, MAXCHARS, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_nonparamMethodRandomUniformTest::testPredClass()
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
  o << "   const valarray<int> NObservs = set.getN();" << endl;
  o << "   const valarray<int> NRecords = set.getNRecords();" << endl;
  o << "   Pred< CppAD::AD<double> > pred( &set );" << endl;
  o << "   const double C1 = 1.0;" << endl;
  o << "   const double C2 = 2.0;" << endl;
  o << "   const double C3 = 3.0;" << endl;
  o << "   const int thetaLen    = " << thetaLen << ";" << endl;
  o << "   const int etaLen      = " << etaLen << ";" << endl;
  o << "   const int epsLen      = " << epsLen << ";" << endl;
  o << "   const int thetaOffset = 0;" << endl;
  o << "   const int etaOffset   = thetaLen;" << endl;
  o << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  o << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  o << "   double expectedF1[nIndividuals][NObservs.sum()];" << endl;
  o << "   double expectedY1[nIndividuals][NObservs.sum()];" << endl;
  o << "   int m;" << endl;
  o << endl;
  o << endl;

  o << "   for( int who=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<NRecords[who]; j++ )" << endl;
  o << "      {" << endl;
  o << "         const int n           = NObservs[who];" << endl;
  o << "         const int fOffset     = 0;" << endl;
  o << "         const int yOffset     = n;" << endl;
  o << "         vector< CppAD::AD<double> > depVar( n*2 );" << endl;
  o << "         fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  o << "         fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  //---------------------------------------------------------------------------------
  // A complete iteration over j
  //
  o << endl;
  o << "         indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  o << "         indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
  o << "         indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  o << "         indepVar[etaOffset  +1] = C1*j; // eta(2)" << endl;
  o << "         indepVar[epsOffset  +0] = C1*j; // eps(1)" << endl;
  o << "         indepVar[epsOffset  +1] = C1*j; // eps(2)" << endl;
  o << "         pred.eval( thetaOffset, thetaLen," << endl;
  o << "                    etaOffset,   etaLen," << endl;
  o << "                    epsOffset,   epsLen ," << endl;
  o << "                    fOffset,     n, " << endl;
  o << "                    yOffset,     n, " << endl;
  o << "                    who, j, m," << endl;
  o << "                    indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "         double actualF = CppAD::Value(depVar[ fOffset + m ]);" << endl;
  o << "         double KA = CppAD::Value( indepVar[thetaOffset+0] );" << endl;
  o << "         double KE = CppAD::Value( indepVar[thetaOffset+1] );" << endl;
  o << "         expectedF1[who][m]  = KE*KA;" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[who][m], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "         double actualY = CppAD::Value(depVar[ yOffset + m ]);" << endl;
  o << "         expectedY1[who][m]  = expectedF1[who][m] + CppAD::Value( indepVar[etaOffset+0] + indepVar[etaOffset+1] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY1[who][m], actualY );" << endl;
  o << "      }" << endl;
  o << "   } // End of the first complete iteration over j" << endl;

  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  o << "   for( int who=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0, m=0; j<NRecords[who]; j++ )" << endl;
  o << "      {" << endl;
  o << "         if( set.getMeasurementIndex(who, j) != -1.0 )" << endl;
  o << "         {" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][1] );" << endl;
  o << "            MY_ASSERT_EQUAL( expectedF1[who][m], set.data[who]->" << strF << "[j] );" << endl;
  o << "            MY_ASSERT_EQUAL( expectedY1[who][m], set.data[who]->" << strY<< "[j] );" << endl;
  o << "            m++;" << endl;
  o << "         }" << endl;
  o << "      }" << endl;
  o << "   }" << endl;

  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  //---------------------------------------------------------------------------------
  // Incomplete iteration over j
  //
  o << "   double expectedF2[nIndividuals][NObservs.sum()];" << endl;
  o << "   double expectedY2[nIndividuals][NObservs.sum()];" << endl;
  o << "   for( int who=1; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<1; j++ )" << endl;
  o << "      {" << endl;
  o << "         const int n           = NObservs[who];" << endl;
  o << "         assert( n>1 );" << endl;
  o << "         const int fOffset     = 0;" << endl;
  o << "         const int yOffset     = n;" << endl;
  o << "         vector< CppAD::AD<double> > depVar( n*2 );" << endl;
  o << "         fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  o << "         fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  o << "         indepVar[thetaOffset+0] = C2*j; // theta(0)" << endl;
  o << "         indepVar[thetaOffset+1] = C2*j; // theta(1)" << endl;
  o << "         indepVar[etaOffset  +0] = C2*j; // eta(0)" << endl;
  o << "         indepVar[etaOffset  +1] = C2*j; // eta(1)" << endl;
  o << "         indepVar[epsOffset  +0] = C2*j; // eps(0)" << endl;
  o << "         indepVar[epsOffset  +1] = C2*j; // eps(1)" << endl;
  o << endl;
  o << "         pred.eval( thetaOffset, thetaLen," << endl;
  o << "                    etaOffset,   etaLen," << endl;
  o << "                    epsOffset,   epsLen ," << endl;
  o << "                    fOffset,     n, " << endl;
  o << "                    yOffset,     n, " << endl;
  o << "                    who, j, m," << endl;
  o << "                    indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "         double actualF = CppAD::Value(depVar[ fOffset + m ]);" << endl;
  o << "         double KA = CppAD::Value( indepVar[thetaOffset+0] + indepVar[etaOffset+0] );" << endl;
  o << "         double KE = CppAD::Value( indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  o << "         expectedF2[who][m]  = KE*KA;" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF2[who][m], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "         double actualY = CppAD::Value(depVar[ yOffset + m ]);" << endl;
  o << "         expectedY2[who][m]  = expectedF2[who][m] + CppAD::Value( indepVar[epsOffset+0] + indepVar[epsOffset+1] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY2[who][m], actualY );" << endl;
  o << "      }" << endl;
  o << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  o << "   for( int who=0, k=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0, m=0; j<NRecords[who]; j++ )" << endl;
  o << "      {" << endl;
  o << "         if( set.getMeasurementIndex(who, j) != -1.0 )" << endl;
  o << "         {" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "            MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "            MY_ASSERT_EQUAL( expectedF1[who][m], set.data[who]->" << strF << "[j] );" << endl;
  o << "            MY_ASSERT_EQUAL( expectedY1[who][m], set.data[who]->" << strY << "[j] );" << endl;
  o << "            m++;" << endl;
  o << "         }" << endl;
  o << "      }" << endl;
  o << "   }" << endl;

  //
  //  End of an INcomplete iteration over j
  //---------------------------------------------------------------------------------
  o << "   return !ok;" << endl;
  o << "}" << endl;
  o.close();

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fPredDriver_cpp, fPredDriver, LDFLAG, CXXFLAGS );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, MAXCHARS, "./%s", fPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_nonparamMethodRandomUniformTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fFitDriver );
  int  exitcode      = 0;
  char command[1024];
  snprintf( command, MAXCHARS, "make -f %s debug", fMakefile );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fFitDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, MAXCHARS, "./%s > %s", fFitDriver, fTraceOut );

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
      snprintf( message, 
		MAXCHARS,
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
void pop_nonparamMethodRandomUniformTest::testReportML()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", "Verifying the results" );
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
  
  error_list = report->getElementsByTagName( XML.X_ERROR_LIST );
  CPPUNIT_ASSERT_EQUAL( 1, (int)error_list->getLength() );
  DOMElement* error = dynamic_cast<DOMElement*>( error_list->item(0) );
  const XMLCh* error_message = error->getFirstChild()->getNodeValue();
  CPPUNIT_ASSERT_MESSAGE( "<error_list> should have been empty.", XMLString::isAllWhiteSpace( error_message ) );
 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the objective value.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double obj_out = 0.0;
  DOMNodeList * objOut_list = report->getElementsByTagName( XML.X_POP_OBJ_OUT );
  if( objOut_list->getLength() > 0 )
    {
      DOMElement* objOut = dynamic_cast<DOMElement*>( objOut_list->item(0) );
      DOMNodeList* value_list = objOut->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( 1, n );
      obj_out = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );      
      //      CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale * nm_obj );
    }

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
	  //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], scale * nm_theta[i] );
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
	  //	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], scale * nm_omega[i] );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Get a pointer to the top of nonparametric results sub-tree.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *nonparametric_result = report->getElementsByTagName( XML.X_NONPARAMETRIC_RESULT );
  CPPUNIT_ASSERT( nonparametric_result->getLength() == 1 );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the presentation information
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *presentation_data = report->getElementsByTagName( XML.X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * pop_nonparamMethodRandomUniformTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_nonparamMethodRandomUniformTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_nonparamMethodRandomUniformTest>(
         "testIndDataClass", 
	 &pop_nonparamMethodRandomUniformTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_nonparamMethodRandomUniformTest>(
         "testDataSetClass", 
	 &pop_nonparamMethodRandomUniformTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_nonparamMethodRandomUniformTest>(
         "testPredClass", 
	 &pop_nonparamMethodRandomUniformTest::testPredClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_nonparamMethodRandomUniformTest>(
         "testDriver", 
	 &pop_nonparamMethodRandomUniformTest::testDriver ) );
  return suiteOfTests;
}

