#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_dataSetTest.h"
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
  const unsigned int MAXCHARS = 64;

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
  char fNonmemParsDriver             [MAXCHARS];
  char fNonmemParsDriver_cpp         [MAXCHARS];
  char fMonteParsDriver              [MAXCHARS];
  char fMonteParsDriver_cpp          [MAXCHARS];
  char fIndDataDriver                [MAXCHARS];
  char fIndDataDriver_cpp            [MAXCHARS];
  char fExpandDriver                 [MAXCHARS];
  char fExpandDriver_cpp             [MAXCHARS];
  char fGetMeasurementIndexDriver    [MAXCHARS];
  char fGetMeasurementIndexDriver_cpp[MAXCHARS];

  char SPKLIB[]     = "spk";
  char SPKPREDLIB[] = "spkpred";
  char SPKOPTLIB[]  = "QN01Box";
  char ATLASLIB[]   = "lapack_atlas";
  char CBLASLIB[]   = "cblas";
  char CLAPACKLIB[] = "atlas";
  char PTHREADLIB[] = "pthread";
  char MLIB[]       = "m";
  char XERCESCLIB[] = "xerces-c";
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
  const char *strTIME = "TiMe";
  const char *strDV   = "DV";
  const char *strCP   = "CP";
  const char *strMDV  = "MDV";
  const char *label[] = { strID, strDV, strTIME, strMDV };
  map<const char*, const char*> label_alias;
  int nLabels         = 4;

  //============================================
  // data set
  //============================================
  const int    nRecords   =  8;
  const int    nFixed     =  0;
  const int    nItems     =  nLabels;
  const double record0[]  = { 1, 1, 0.1, 0 };
  const double record1[]  = { 1, 2, 0.2, 1 };  // 1
  const double record2[]  = { 1, 3, 0.3, 0 };
  const double record3[]  = { 1, 4, 0.4, 0 };
  const double record4[]  = { 1, 5, 0.5, 0 };
  const double record5[]  = { 1, 6, 0.6, 0 };
  const double record6[]  = { 1, 7, 0.7, 1 };  // 6
  const double record7[]  = { 1, 8, 0.8, 0 };

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
  const double theta_low[ thetaLen ]   = {  0.1,  0.1 };
  const double theta_in [ thetaLen ]   = {  1.0,  1.0 };
  const double theta_up [ thetaLen ]   = { 10.0, 10.0 };
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
  // PRED model based on Norris
  //
  // A = THETA(1)
  // B = THETA(2)
  // T = TIME
  // E = ETA(1)
  // 
  // F = A * EXP( - B * T  ) 
  // Y = F + E
  //============================================
  const char PREDEQN[] = "A = THETA(1)\n \
  B = THETA(2) \n \
  T = TiMe \n \
  E = ETA(1) \n \
  F = A * EXP( - B * T  ) \n \
  Y = F + E\n";
};

void ind_dataSetTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,                        testName );
  int lenPrefix = strlen( fPrefix );
  snprintf( fMonteParsDriver,               lenPrefix+128, "%s_MonteParsDriver",               fPrefix );
  snprintf( fMonteParsDriver_cpp,           lenPrefix+128, "%s_MonteParsDriver.cpp",           fPrefix );
  snprintf( fNonmemParsDriver,              lenPrefix+128, "%s_NonmemParsDriver",              fPrefix );
  snprintf( fNonmemParsDriver_cpp,          lenPrefix+128, "%s_NonmemParsDriver.cpp",          fPrefix );
  snprintf( fIndDataDriver,                 lenPrefix+128, "%s_IndDataDriver",                 fPrefix );
  snprintf( fIndDataDriver_cpp,             lenPrefix+128, "%s_IndDataDriver.cpp",             fPrefix );
  snprintf( fDataML,                        lenPrefix+128, "%s_dataML",                        fPrefix );
  snprintf( fSourceML,                      lenPrefix+128, "%s_sourceML.xml",                  fPrefix );
  snprintf( fExpandDriver,                  lenPrefix+128, "%s_expandDriver",                  fPrefix );
  snprintf( fExpandDriver_cpp,              lenPrefix+128, "%s_expandDriver.cpp",              fPrefix );
  snprintf( fGetMeasurementIndexDriver,     lenPrefix+128, "%s_getMeasurementIndexDriver",     fPrefix );
  snprintf( fGetMeasurementIndexDriver_cpp, lenPrefix+128, "%s_getMeasurementIndexDriver.cpp", fPrefix );

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
void ind_dataSetTest::tearDown()
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
      remove( fExpandDriver );
      remove( fExpandDriver_cpp );
      remove( fGetMeasurementIndexDriver );
      remove( fGetMeasurementIndexDriver_cpp );
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
void ind_dataSetTest::createDataML()
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
      snprintf( buf, MAXCHARS+1, "An error occurred during parsing %s.\n   Message: %s\n",
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
	  snprintf( buf, MAXCHARS+1, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
		   fDataML, e.code, XMLString::transcode(errText) );
	  
	  CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      snprintf( buf, MAXCHARS+1, "An unknown error occurred during parsing %s.\n", fDataML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
 
void ind_dataSetTest::createSourceML()
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
      snprintf( buf, MAXCHARS+1, "An error occurred during parsing %s.\n   Message: %s\n",
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
	  snprintf( buf, MAXCHARS+1, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
		   fSourceML, e.code, XMLString::transcode(errText) );
	  CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[MAXCHARS + 1];
      snprintf( buf, MAXCHARS+1, "An unknown error occurred during parsing %s.\n", fSourceML );
      
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Translation
// 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ind_dataSetTest::parse()
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
}

void ind_dataSetTest::testExpand()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fExpandDriver );
  ofstream o( fExpandDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <string>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <spk/SpkValarray.h>" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "using namespace std;" << endl;
  o << "using SPK_VA::valarray;" << endl;

  o << MY_ASSERT_EQUAL << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   const int n = set.getNObservs(0);" << endl;
  o << "   const int m = set.getNRecords(0);" << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   for( int j=0, k=0; j<m; j++ )" << endl;
  o << "   {" << endl;
  o << "       if( set.data[0]->" << strMDV << "[j] == 0 )" << endl;
  o << "       {" << endl;
  o << "          MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[j], y[k] );" << endl;
  o << "          k++;" << endl;
  o << "       }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   valarray<double> z;" << endl;
  o << "   set.expand( y, z ); " << endl;
  for( int i=0, k=0; i<nRecords; i++ )
    {
      if( record[i][3] == 1 )
	// This element should be a NaN, which is not equal to itself.
	o << "   MY_ASSERT_EQUAL( false, ( z[" << i << "] == z[" << i << "] ) );" << endl;
      else
	{
	  o << "   MY_ASSERT_EQUAL( y[" << k << "], z[" << i << "] );" << endl;
	  k++;
	}
    }
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fExpandDriver_cpp, fExpandDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      snprintf( message, 256, "Compilation of the generated %s failed!", fExpandDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 256, "./%s", fExpandDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      snprintf( message, 256, "A test driver, %s, failed!", fExpandDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

}
void ind_dataSetTest::testGetMeasurementIndex()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fGetMeasurementIndexDriver );
  ofstream o( fGetMeasurementIndexDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include <string>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <spk/SpkValarray.h>" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "using namespace std;" << endl;
  o << "using SPK_VA::valarray;" << endl;

  o << MY_ASSERT_EQUAL << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   const int n = set.getNObservs(0);" << endl;
  o << "   const int m = set.getNRecords(0);" << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   for( int j=0, k=0; j<m; j++ )" << endl;
  o << "   {" << endl;
  o << "       if( set.data[0]->" << strMDV << "[j] == 0 )" << endl;
  o << "       {" << endl;
  o << "          MY_ASSERT_EQUAL( k, set.getMeasurementIndex(0, j) );" << endl;
  o << "          k++;" << endl;
  o << "       }" << endl;
  o << "       else" << endl;
  o << "       {" << endl;
  o << "          MY_ASSERT_EQUAL( -1, set.getMeasurementIndex(0, j) );" << endl;
  o << "       }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fGetMeasurementIndexDriver_cpp, fGetMeasurementIndexDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      snprintf( message, 256, "Compilation of the generated %s failed!", fGetMeasurementIndexDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 256, "./%s", fGetMeasurementIndexDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      snprintf( message, 256, "A test driver, %s, failed!", fGetMeasurementIndexDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  okToClean = true;
}

CppUnit::Test * ind_dataSetTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_dataSetTest"  );

  suiteOfTests->addTest( new CppUnit::TestCaller<ind_dataSetTest>(
		 	 "testExpand", 
			 &ind_dataSetTest::testExpand ) );

  suiteOfTests->addTest( new CppUnit::TestCaller<ind_dataSetTest>(
		 	 "testGetMeasurementIndex", 
			 &ind_dataSetTest::testGetMeasurementIndex ) );

  return suiteOfTests;
}
