#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_dataSetTest.h"
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

  char fPrefix                       [MAXCHARS];
  char fDataML                       [MAXCHARS];
  char fSourceML                     [MAXCHARS];
  char fGetNDriver                   [MAXCHARS];
  char fGetNDriver_cpp               [MAXCHARS];
  char fExpandDriver                 [MAXCHARS];
  char fExpandDriver_cpp             [MAXCHARS];
  char fGetMeasurementIndexDriver    [MAXCHARS];
  char fGetMeasurementIndexDriver_cpp[MAXCHARS];
  char fGetRecordIndexDriver         [MAXCHARS];
  char fGetRecordIndexDriver_cpp     [MAXCHARS];
  char fGetAllMeasurementsDriver     [MAXCHARS];
  char fGetAllMeasurementsDriver_cpp [MAXCHARS];

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
  //   ID      TIME     CP=DV      AMT
  //   1       0.0       0.0       1.0
  //   1       0.0       0.0       0.0
  //   2       0.0       0.0       1.0
  //   2       0.0       0.0       0.0
  //   2       1.0      10.0       0.0
  //   3       0.0       0.0       1.0
  //   3       0.0       0.0       0.0
  //   3       1.0      10.0       0.0
  //   3       2.0      20.0       0.0
  //   4       0.0       0.0       1.0
  //   4       0.0       0.0       0.0
  //   4       1.0      10.0       0.0 
  //   4       2.0      20.0       0.0
  //   4       3.0      25.0       0.0
  //============================================
  map<const char*, const char*> label_alias;
  const char * strID        = "ID";
  const char * strTIME      = "TiMe";
  const char * strDV        = "DV";
  const char * strCP        = "CP";
  const char * strAMT       = "AMT";
  const char * strMDV       = "MDV";
  const char * label[]      = { strID, strTIME, strDV, strAMT };
  const int    nLabels      = 4;
  const int    nIndividuals = 4;
  const int    nRecords     = 14;
  const int    nObservs     = 10;
  const int    nFixed       = 0;
  const int    nItems       = nLabels;
  valarray<int> NObservs( nIndividuals );
  valarray<int> NRecords( nIndividuals );
  const double record0[]  = { 1, 0.0,  0.0,  1.0 };
  const double record1[]  = { 1, 0.0,  0.0,  0.0 };
  const double record2[]  = { 2, 0.0,  0.0,  1.0 };
  const double record3[]  = { 2, 0.0,  0.0,  0.0 };
  const double record4[]  = { 2, 1.0, 10.0,  0.0 };
  const double record5[]  = { 3, 0.0,  0.0,  1.0 };
  const double record6[]  = { 3, 0.0,  0.0,  0.0 };
  const double record7[]  = { 3, 1.0, 10.0,  0.0 };
  const double record8[]  = { 3, 2.0, 20.0,  0.0 };
  const double record9[]  = { 4, 0.0,  0.0,  1.0 };
  const double record10[] = { 4, 0.0,  0.0,  0.0 };
  const double record11[] = { 4, 1.0, 10.0,  0.0 };
  const double record12[] = { 4, 2.0, 20.0,  0.0 };
  const double record13[] = { 4, 3.0, 25.0,  0.0 };
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

void pop_dataSetTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,                        testName );
  snprintf( fGetNDriver,                    MAXCHARS, "%s_getNDriver",                    fPrefix );
  snprintf( fGetNDriver_cpp,                MAXCHARS, "%s_getNDriver.cpp",                fPrefix );
  snprintf( fExpandDriver,                  MAXCHARS, "%s_expandDriver",                  fPrefix );
  snprintf( fExpandDriver_cpp,              MAXCHARS, "%s_expandDriver.cpp",              fPrefix );
  snprintf( fGetMeasurementIndexDriver,     MAXCHARS, "%s_getMeasurementIndexDriver",     fPrefix );
  snprintf( fGetMeasurementIndexDriver_cpp, MAXCHARS, "%s_getMeasurementIndexDriver.cpp", fPrefix );
  snprintf( fGetRecordIndexDriver,          MAXCHARS, "%s_getRecordDriver",               fPrefix );
  snprintf( fGetRecordIndexDriver_cpp,      MAXCHARS, "%s_getRecordDriver.cpp",           fPrefix );
  snprintf( fGetAllMeasurementsDriver,      MAXCHARS, "%s_getAllMeasurementsDriver",      fPrefix );
  snprintf( fGetAllMeasurementsDriver_cpp,  MAXCHARS, "%s_getAllMeasurementsDriver.cpp",  fPrefix );
  snprintf( fDataML,                        MAXCHARS, "%s_dataML.xml",                    fPrefix );
  snprintf( fSourceML,                      MAXCHARS, "%s_sourceML.xml",                  fPrefix );

  snprintf( LDFLAG, LDFLAG_MAXCHARS, "%s -l%s -l%s  -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
     LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB, CLNLIB, GINACLIB, BADLIB, BAPLIB, BAVLIB, BA0LIB, GSLLIB, GSLCBLASLIB );

  // ID doesn't have an alias
  label_alias[strID]   = NULL;

  // TIME doesn't have an alias
  label_alias[strTIME] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;

  // #of measurements for each individual
  NObservs[0] = 1;
  NObservs[1] = 2;
  NObservs[2] = 3;
  NObservs[3] = 4;

  NRecords[0] = 2;
  NRecords[1] = 3;
  NRecords[2] = 4;
  NRecords[3] = 5;

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
void pop_dataSetTest::tearDown()
{
  if( okToClean )
    {
      remove( fDataML );
      remove( fSourceML );
      remove( fReportML );
      remove( fGetNDriver );
      remove( fGetNDriver_cpp );
      remove( fExpandDriver );
      remove( fExpandDriver_cpp );
      remove( fGetMeasurementIndexDriver );
      remove( fGetMeasurementIndexDriver_cpp );
      remove( fGetRecordIndexDriver );
      remove( fGetRecordIndexDriver_cpp );
      remove( fGetAllMeasurementsDriver );
      remove( fGetAllMeasurementsDriver_cpp );
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
void pop_dataSetTest::createDataML()
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
 
void pop_dataSetTest::createSourceML()
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
void pop_dataSetTest::parse()
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
void pop_dataSetTest::getNTest()
{
  printf( "\n--- %s ---\n", fGetNDriver );
  ofstream o( fGetNDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <valarray>" << endl;
  o << "#include <iostream>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;

  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  for( int i=0; i<nIndividuals; i++ )
     o << "   MY_ASSERT_EQUAL( " << NObservs[i] << ", set.getNObservs(" << i << ") );" << endl;
  for( int i=0; i<nIndividuals; i++ )
     o << "   MY_ASSERT_EQUAL( " << NRecords[i] << ", set.getNRecords(" << i << ") );" << endl;
  o << "}" << endl;

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fGetNDriver_cpp, fGetNDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fGetNDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fGetNDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fGetNDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}

void pop_dataSetTest::getAllMeasurementsTest()
{
  printf( "\n--- %s ---\n", fGetAllMeasurementsDriver );
  ofstream o( fGetAllMeasurementsDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <spk/SpkValarray.h>" << endl;
  o << "#include <iostream>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;

  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   valarray<double> expected( " << nObservs << ");" << endl;
  o << "   valarray<double> actual  ( " << nObservs << ");" << endl;

  o << "   actual = set.getAllMeasurements();" << endl;
  for( int i=0, j=0; i<nRecords; i++ )
    {
      if( record[i][3] == 0.0 )
	{
	  o << "   MY_ASSERT_EQUAL( " << record[i][2] << ", actual[" << j << "] );" << endl;
	  j++;
	}
    }


  o << "}" << endl;

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fGetAllMeasurementsDriver_cpp, fGetAllMeasurementsDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fGetAllMeasurementsDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fGetAllMeasurementsDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fGetAllMeasurementsDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_dataSetTest::getMeasurementIndexTest()
{
  printf( "\n--- %s ---\n", fGetMeasurementIndexDriver );
  ofstream o( fGetMeasurementIndexDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <valarray>" << endl;
  o << "#include <iostream>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;

  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0, m=0; j<NRecords[i]; j++, k++ )
	{
	  if( record[k][3] == 0.0 ) // AMT == 0.0
	    {
	      o << "   MY_ASSERT_EQUAL( " << m << ", set.getMeasurementIndex( " << i << ", " << j << " ) );" << endl;
	      m++;
	    }
	  else
	    {
	      o << "   MY_ASSERT_EQUAL( -1.0, set.getMeasurementIndex( " << i << ", " << j << " ) );" << endl;
	    }
	}
      o << endl;
    }
  o << "}" << endl;

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fGetMeasurementIndexDriver_cpp, fGetMeasurementIndexDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fGetMeasurementIndexDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fGetMeasurementIndexDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fGetMeasurementIndexDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_dataSetTest::getRecordIndexTest()
{
  printf( "\n--- %s ---\n", fGetRecordIndexDriver );
  ofstream o( fGetRecordIndexDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <valarray>" << endl;
  o << "#include <iostream>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;

  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0, m=0; j<NRecords[i]; j++, k++ )
	{
	  if( record[k][3] == 0.0 ) // AMT == 0.0
	    {
	      o << "   MY_ASSERT_EQUAL( " << j << ", set.getRecordIndex( " << i << ", " << m << " ) );" << endl;
	      m++;
	    }
	}
      o << endl;
    }
  o << "}" << endl;

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fGetRecordIndexDriver_cpp, fGetRecordIndexDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fGetRecordIndexDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fGetRecordIndexDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fGetRecordIndexDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_dataSetTest::expandTest()
{
  printf( "\n--- %s ---\n", fExpandDriver );
  ofstream o( fExpandDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <valarray>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <spk/SpkValarray.h>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;

  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   valarray<double> records(" << nRecords << ");" << endl;
  o << "   valarray<double> observs(" << nObservs << ");" << endl;
  for( int i=0, j=0; i<nRecords; i++ )
    {
      if( record[i][3] == 0.0 )
	{
	  o << "   observs[" << j << "] = " << record[i][0] << ";" << endl;
	  j++;
	}
    }
  o << "   set.expand( observs, records );" << endl;
  for( int i=0; i<nRecords; i++ )
    {
      if( record[i][3] == 0.0 )
	{
	  o << "   MY_ASSERT_EQUAL( " << record[i][0] << ", records[" << i << "] );" << endl;
	}
      else
	{
	  // This element should be a NaN, which is not equal to itself.
	  o << "   MY_ASSERT_EQUAL( false, ( records[" << i << "] == records[" << i << "] ) );" << endl;
	}

    }
  o << "}" << endl;

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fExpandDriver_cpp, fExpandDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fExpandDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, MAXCHARS+1, "./%s", fExpandDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fExpandDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  okToClean = true;
}

CppUnit::Test * pop_dataSetTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_dataSetTest"  );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_dataSetTest>(
         "getNTest", 
	 &pop_dataSetTest::getNTest ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_dataSetTest>(
         "getAllMeasurementsTest", 
	 &pop_dataSetTest::getAllMeasurementsTest ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_dataSetTest>(
         "getMeasurementIndexTest", 
	 &pop_dataSetTest::getMeasurementIndexTest ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_dataSetTest>(
         "getRecordIndexTest", 
	 &pop_dataSetTest::getRecordIndexTest ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_dataSetTest>(
         "expandTest", 
	 &pop_dataSetTest::expandTest ) );

  return suiteOfTests;
}

