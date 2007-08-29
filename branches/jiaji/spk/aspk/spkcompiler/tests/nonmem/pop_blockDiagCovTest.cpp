#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_blockDiagCovTest.h"
#include "DOMPrint.h"
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

/*
 * NONMEM CONTROL FILE
 *
$PROBLEM OrangeTree
$DATA orangeData.txt

$INPUT ID AGE DV

$PRED
B1 = THETA(1) +ETA(1)
B2 = THETA(2) +ETA(2)
B3 = THETA(3) +ETA(3)
F = B1 / (  1 +EXP( -(AGE -B2)/B3 )  )
Y = F + EPS(1)

$THETA
(20.0,200,2000)
(70.0,700,7000)
(30.0,300,3000)
$OMEGA DIAGONAL(3) 800 1200 1000
$SIGMA DIAGONAL(1) 60

$ESTIMATION METHOD=0 SIGDIGITS=3 MAXEVALS=450 PRINT=5

$COVARIANCE MATRIX=R

$TABLE  ID AGE DV
*/

/*
 * NONMEM DATA FILE
 *

         1  118.00  30.000
         1  484.00  58.000
         1  664.00  87.000
         1  1004.0  115.00
         1  1231.0  120.00
         1  1372.0  142.00
         1  1582.0  145.00
         2  118.00  33.000
         2  484.00  69.000
         2  664.00  111.00
         2  1004.0  156.00
         2  1231.0  172.00
         2  1372.0  203.00
         2  1582.0  203.00
         3  118.00  30.000
         3  484.00  51.000
         3  664.00  75.000
         3  1004.0  108.00
         3  1231.0  115.00
         3  1372.0  139.00
         3  1582.0  140.00
         4  118.00  32.000
         4  484.00  62.000
         4  664.00  112.00
         4  1004.0  167.00
         4  1231.0  179.00
         4  1372.0  209.00
         4  1582.0  214.00
         5  118.00  30.000
         5  484.00  49.000
         5  664.00  81.000
         5  1004.0  125.00
         5  1231.0  142.00
         5  1372.0  174.00
         5  1582.0  177.00

 */

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

  //============================================
  map<const char*, const char*> label_alias;
  const char *strID         = "ID";
  const char *strAGE       = "AGE";
  const char *strDV         = "DV";
  const char *strCP         = "CP";
  const char *label[]       = { strID, strAGE, strDV };
  const int    nLabels      = 3;
  const int    nIndividuals = 5;
  const int    nRecords     = 35;
  const int    nItems       = nLabels;
  valarray<int> N( nIndividuals );
  const double record0[]  = { 1,  118.00,  30.000 };
  const double record1[]  = { 1,  484.00,  58.000 };
  const double record2[]  = { 1,  664.00,  87.000 };
  const double record3[]  = { 1,  1004.0 , 115.00 };
  const double record4[]  = { 1,  1231.0,  120.00 };
  const double record5[]  = { 1,  1372.0,  142.00 };
  const double record6[]  = { 1,  1582.0,  145.00 };
  const double record7[]  = { 2,  118.00,  33.000 };
  const double record8[]  = { 2,  484.00,  69.000 };
  const double record9[]  = { 2,  664.00,  111.00 };
  const double record10[] = { 2,  1004.0,  156.00 };
  const double record11[] = { 2,  1231.0,  172.00 };
  const double record12[] = { 2,  1372.0,  203.00 };
  const double record13[] = { 2,  1582.0,  203.00 };
  const double record14[] = { 3,  118.00,  30.000 };
  const double record15[] = { 3,  484.00,  51.000 };
  const double record16[] = { 3,  664.00,  75.000 };
  const double record17[] = { 3,  1004.0,  108.00 };
  const double record18[] = { 3,  1231.0,  115.00 };
  const double record19[] = { 3,  1372.0,  139.00 };
  const double record20[] = { 3,  1582.0,  140.00 };
  const double record21[] = { 4,  118.00,  32.000 };
  const double record22[] = { 4,  484.00,  62.000 };
  const double record23[] = { 4,  664.00,  112.00 };
  const double record24[] = { 4,  1004.0 , 167.00 };
  const double record25[] = { 4,  1231.0,  179.00 };
  const double record26[] = { 4,  1372.0,  209.00 };
  const double record27[] = { 4,  1582.0,  214.00 };
  const double record28[] = { 5,  118.00,  30.000 };
  const double record29[] = { 5,  484.00,  49.000 };
  const double record30[] = { 5,  664.00,  81.000 };
  const double record31[] = { 5,  1004.0,  125.00 };
  const double record32[] = { 5,  1231.0,  142.00 };
  const double record33[] = { 5,  1372.0,  174.00 };
  const double record34[] = { 5,  1582.0,  177.00 };

  double const * record[nRecords];

  //============================================
  // Define NONMEM keywords  (are these needed?)
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
  const char *strF        = "F";
  const char *strY        = "Y";

  //============================================
  // User defined words
  //============================================
  //const char * strKA    = "ka";
  //const char * strKE    = "ke";

  //============================================
  // The user is requested to feed in
  // the constraints and initial values for
  // theta.
  //============================================
  const int    thetaLen = 3;
  const double theta_in [ thetaLen ]   = {  200.0,  700.0,  300.0 };
  const double theta_up [ thetaLen ]   = { 2000.0, 7000.0, 3000.0};
  const double theta_low[ thetaLen ]   = {   20.0,   70.0,   30.0  };
  const bool   theta_fix[ thetaLen ]   = { false, false, false };

  //============================================
  // The SPK Compiler determines the initial
  // values for eta, the variance of data
  // in the individual analysis case.
  // The size of this vector is determined by
  // the order of Omega matrix.
  //============================================
  const int    etaLen            = 3;
  const double eta_in [ etaLen ] = { 0.0, 0.0, 0.0 };
  const bool   eta_fix[ etaLen ] = { false, false, false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal matrix.
  //============================================

  const int nBlock = 2;
  int dims[nBlock] = { 2, 1};
  const std::valarray<int>    omegaDim( dims, nBlock );
  Symbol::Structure omStruct[nBlock] = { Symbol::DIAGONAL, Symbol::DIAGONAL };
  const std::valarray<Symbol::Structure> omegaStruct( omStruct, nBlock );
  //std::valarray<int>   omegaOrder( 1, nBlock );
  //for( int i=0; i<nBlock; i++)
  //omegaOrder[i] = (omegaStruct[i]==Symbol::DIAGONAL? 
  //		     omegaDim[i] : omegaDim[i] * (omegaDim[i]+1)/2 );

  const std::valarray<int>   omegaOrder( dims, nBlock );
  const std::valarray<bool>   omegaSameAsPrev( false, nBlock);
  const double omega_in [ 3 ] = { 800, 1200, 1000 };
  const bool   omega_fix[ 3 ] = { false, false, false };


  //============================================
  // EPS is irrevalent in the individual 
  // analysis case.  It'll be ignored.
  //============================================
  const int epsLen = 1;
   
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
  const double sigma_in [ sigmaOrder ] = { 60.0 };
  const bool   sigma_fix[ sigmaOrder ] = { false };

  //============================================
  // Make requests for statistics.
  //============================================
  const string covForm          = "r";
  const bool pop_stderr         = true;
  const bool pop_coefficient    = true;
  const bool pop_confidence     = true;
  const bool pop_covariance     = true;
  const bool pop_inv_covariance = true;
  const bool pop_correlation    = true;

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
  // B1 = THETA(1) +ETA(1)
  // B2 = THETA(2) +ETA(2)
  // B3 = THETA(3) +ETA(3)
  // F = B1 / (  1 +EXP( -(AGE -B2)/B3 )  )
  // Y=F+EPS(1)
  //============================================
  const char PREDEQN[]     = "B1 = THETA(1)+ETA(1)\nB2 = THETA(2)+ETA(2)\nB3 = THETA(3)+ETA(3)\nF = B1 / (  1 +EXP( -(AGE -B2)/B3 )  )\nY = F + EPS(1)\n";

  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  const double nm_obj       =  131.434;
  const double nm_theta  [] = { 192.218, 728.908, 348.52 };
  const double nm_omega  [] = { 990.278, 495.74, 345.691 };
  const double nm_sigma  [] = { 57.1372 };
  const double nm_pred   [] = {  };

  const double nm_cov    [] = {   243.557,
				  213.552,  1288.1,
				  158.222,  721.108,  781.453,
				  -92.8964, 677.971,  61.3589,  476787,
				  -4932.09, -10207.2,  -11407.9,  534542,  6751820,
				  2074.63,  9080.98,  8943.99,  -334528,  -2253050,  2297250,
				  4.25272,  6.40273,  15.0946,  -641.712, -13415, -115.183,  281.149 }; 

 const double nm_inv_cov[] = {
   0.00493361,
   -0.000539269,
   0.00166963,
   -0.000489154,
   -0.0014222,
   0.00275258,
   3.80282e-07,
   -2.73258e-06,
   -2.36794e-06,
   2.40999e-06,
   3.01879e-06,
   -5.23221e-07,
   7.4919e-07,
   -1.09808e-07,
   2.64242e-07,
   2.60226e-06,
   -1.48619e-06,
   -4.26708e-06,
   2.62955e-07,
   2.40214e-07,
   7.29958e-07,
   0.000109892,
   1.46794e-05,
   -7.94011e-05,
   5.52574e-07,
   1.23821e-05,
   1.25846e-05,
   0.00415633};


  const double nm_stderr [] = { 
     15.6063,
   35.8901,
   27.9545,
   690.498,
   2598.43,
   1515.67,
   16.7675 };

  const double nm_cor    [] = {
   1,
   0.381268,
   1,
   0.362672,
   0.718744,
   1,
   -0.00862058,
   0.0273574,
   0.00317881,
   1,
   -0.121624,
   -0.109452,
   -0.157052,
   0.297926,
   1,
   0.0877076,
   0.166938,
   0.211094,
   -0.319644,
   -0.572081,
   1,
   0.0162517,
   0.0106395,
   0.0322034,
   -0.0554255,
   -0.307901,
   -0.00453226,
   1  };

  const double nm_cv     [] = { 
  8.11908,
   4.92381,
   8.02091,
   69.7276,
   524.151,
   438.446,
   29.346 };

  const double nm_ci     [] = { 
   160.256,
   655.405,
   291.269,
   -423.861,
   -4825.84,
   -2758.4,
   22.7974,
   224.18,
   802.411,
   405.771,
   2404.42,
   5817.32,
   3449.78,
   91.477  }; 


};

void pop_blockDiagCovTest::setUp()
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

  // AGE doesn't have an alias
  label_alias[strAGE] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;


  // #of records for each individual
  N[0] = 1;   //these N[] don't appear to be used
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
  record[10]  = record10;
  record[11]  = record11;
  record[12]  = record12;
  record[13]  = record13;
  record[14]  = record14;
  record[15]  = record15;
  record[16]  = record16;
  record[17]  = record17;
  record[18]  = record18;
  record[19]  = record19;
  record[20]  = record20;
  record[21]  = record21;
  record[22]  = record22;
  record[23]  = record23;
  record[24]  = record24;
  record[25]  = record25;
  record[26]  = record26;
  record[27]  = record27;
  record[28]  = record28;
  record[29]  = record29;
  record[30]  = record30;
  record[31]  = record31;
  record[32]  = record32;
  record[33]  = record33;
  record[34]  = record34;


  createDataML();
  createSourceML();
  parse();
}
void pop_blockDiagCovTest::tearDown()
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
void pop_blockDiagCovTest::createDataML()
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
 
void pop_blockDiagCovTest::createSourceML()
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

  int idx = 0;
  for(  int ii=0; ii<omegaStruct.size(); ii++) {
    oSource << "<omega struct=\"";
    oSource << (omegaStruct[ii]==Symbol::DIAGONAL? "diagonal" : "block");
    oSource << "\" same_as_previous=\"";
    oSource << ( omegaSameAsPrev[ii]? "yes" : "no");
    oSource << "\" dimension=\"";
    oSource << omegaDim[ii] << "\">" << endl;
    oSource << "<in>" << endl;

    for( int i=0; i<omegaOrder[ii]; i++ )
      {
	oSource << "<value";
	oSource << " fixed=\"" << (omega_fix[idx]? "yes" : "no") << "\"";
	oSource << ">" << omega_in[idx] << "</value>" << endl;
	idx++;
      }
    oSource << "</in>" << endl;
    oSource << "</omega>" << endl;
  }
    
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
void pop_blockDiagCovTest::parse()
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
void pop_blockDiagCovTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fFitDriver );
  int  exitcode      = 0;
  char command[1024];
  snprintf( command, 1024, "make -f %s debug", fMakefile );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s.cpp failed!", fFitDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 1024, "./%s > %s", fFitDriver, fTraceOut );

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
void pop_blockDiagCovTest::testReportML()
{
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
      CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale );
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
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], scale );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for Omega
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //double omega_out[omegaOrder];
  //DOMNodeList * omegaOut_list = report->getElementsByTagName( XML.X_OMEGA_OUT );
  //if( omegaOut_list->getLength() > 0 )
  //    {
  //      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
  //      DOMNodeList* value_list = omegaOut->getElementsByTagName( XML.X_VALUE );
  //      int n = value_list->getLength();
  //      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
  //      for( int i=0; i<+n; i++ )
  //	{
  //	  omega_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
  //	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], scale );
  //	}
  //    }
  double omega_out[3];
  DOMNodeList * omegaOut_list = report->getElementsByTagName( XML.X_OMEGA_OUT );
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut0 = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list0 = omegaOut0->getElementsByTagName( XML.X_VALUE );
      int n0 = value_list0->getLength();
      DOMElement* omegaOut1 = dynamic_cast<DOMElement*>( omegaOut_list->item(1) );
      DOMNodeList* value_list1 = omegaOut1->getElementsByTagName( XML.X_VALUE );
      int n1 = value_list1->getLength();

      double omega_scale;

      CPPUNIT_ASSERT_EQUAL( 3, n0+n1 );
      for( int i=0; i<+n0; i++ )
	{
	  omega_out[i] = atof( XMLString::transcode( value_list0->item(i)->getFirstChild()->getNodeValue() ) );
	  // Because the covariance elements are all greater than 1
	  // but of varying size, set the tolerance to be 1/100 of
	  // the known value.
	  omega_scale = fabs( nm_omega[i] ) / 100.0;
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], omega_scale );
	}
     for( int i=0; i<+n1; i++ )
	{
	  omega_out[i+n0] = atof( XMLString::transcode( value_list1->item(i)->getFirstChild()->getNodeValue() ) );

	  // Because the covariance elements are all greater than 1
	  // but of varying size, set the tolerance to be 1/100 of
	  // the known value.
	  omega_scale = fabs( nm_omega[i+n0] ) / 100.0;
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i+n0], omega_out[i+n0], omega_scale );
	}
    }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for Sigma
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double sigma_out[sigmaOrder];
  DOMNodeList * sigmaOut_list = report->getElementsByTagName( XML.X_SIGMA_OUT );
  if( sigmaOut_list->getLength() > 0 )
    {
      DOMElement* sigmaOut = dynamic_cast<DOMElement*>( sigmaOut_list->item(0) );
      DOMNodeList* value_list = sigmaOut->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( sigmaOrder, n );
      for( int i=0; i<+n; i++ )
	{
	  sigma_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_sigma[i], sigma_out[i], scale );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Grab a pointer to the top of "ind_stat_result" sub-tree.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *ind_analysis_result = report->getElementsByTagName( XML.X_POP_ANALYSIS_RESULT );
  CPPUNIT_ASSERT( ind_analysis_result->getLength() == 1 );
  DOMElement *ind_stat_result = dynamic_cast<DOMElement*>( ind_analysis_result->item( 0 ) );
  CPPUNIT_ASSERT( ind_stat_result != NULL );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the standard error of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> se_val;
  DOMNodeList * se_list = ind_stat_result->getElementsByTagName( XML.X_POP_STDERROR_OUT );
  if( se_list->getLength() == 1 )
    {
      DOMElement * se = dynamic_cast<DOMElement*>( se_list->item(0) );
      CPPUNIT_ASSERT( se != NULL );
      DOMNodeList * value_list = se->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      se_val.resize( n );

      double se_scale;

      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  se_val[i] = atof( XMLString::transcode( x_val ) );

	// Because the standard error elements are all greater than 1
	// but of varying size, set the tolerance to be 1/100 of the
	// known value.
	se_scale = fabs( nm_stderr[i] ) / 100.0;
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_stderr[i], se_val[i], se_scale );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the covariance of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cov_val;
  vector<double> inv_cov_val;
  //int covLen = series(1,1,omegaOrder+thetaLen);
  int covLen = series(1,1,3+thetaLen);
  DOMNodeList * cov_list =ind_stat_result->getElementsByTagName(  XML.X_POP_COVARIANCE_OUT ) ;
  if( cov_list->getLength() == 1 )
    {
      DOMElement * cov = dynamic_cast<DOMElement*>( cov_list->item(0) );
      CPPUNIT_ASSERT( cov != NULL );
      DOMNodeList * value_list = cov->getElementsByTagName( XML.X_VALUE );
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
  DOMNodeList * invcov_list =ind_stat_result->getElementsByTagName(  XML.X_POP_INVERSE_COVARIANCE_OUT ) ;
  if( invcov_list->getLength() == 1 )
    {
      DOMElement * invcov = dynamic_cast<DOMElement*>( invcov_list->item(0) );
      CPPUNIT_ASSERT( invcov != NULL );
      DOMNodeList * value_list = invcov->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      inv_cov_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  inv_cov_val[i] = atof( XMLString::transcode( x_val ) );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_inv_cov[i], inv_cov_val[i], scale );
      }
    }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the confidence interval for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> ci_val;
  DOMNodeList * ci_list =ind_stat_result->getElementsByTagName(  XML.X_POP_CONFIDENCE_OUT ) ;
  if( ci_list->getLength() == 1 )
    {
      DOMElement * ci = dynamic_cast<DOMElement*>( ci_list->item(0) );
      CPPUNIT_ASSERT( ci != NULL );
      DOMNodeList * value_list = ci->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      ci_val.resize( n );

      double ci_scale;

      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  ci_val[i] = atof( XMLString::transcode( x_val ) );

	// Because the confidence interval elements are all greater
	// than 1 but of varying size, set the tolerance to be 1/100
	// of the known value.
	ci_scale = fabs( nm_ci[i] ) / 100.0;
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_ci[i], ci_val[i], ci_scale );
      }
    }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the coefficient of variation for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cv_val;
  DOMNodeList * cv_list =ind_stat_result->getElementsByTagName(  XML.X_POP_COEFFICIENT_OUT ) ;
  if( cv_list->getLength() == 1 )
    {
      DOMElement * cv = dynamic_cast<DOMElement*>( cv_list->item(0) );
      CPPUNIT_ASSERT( cv != NULL );
      DOMNodeList * value_list = cv->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      cv_val.resize( n );

      double cv_scale;

      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cv_val[i] = atof( XMLString::transcode( x_val ) );

	// Because the coefficent of variation elements are all
	// greater than 1 but of varying size, set the tolerance to be
	// 1/100 of the known value.
	cv_scale = fabs( nm_cv[i] ) / 100.0;
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cv[i], cv_val[i], cv_scale );
      }
    }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the correlation matrix for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cor_val;
  DOMNodeList * cor_list =ind_stat_result->getElementsByTagName(  XML.X_POP_CORRELATION_OUT ) ;
  if( cor_list->getLength() == 1 )
    {
      DOMElement * cor = dynamic_cast<DOMElement*>( cor_list->item(0) );
      CPPUNIT_ASSERT( cor != NULL );
      DOMNodeList * value_list = cor->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      cor_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cor_val[i] = atof( XMLString::transcode( x_val ) );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cor[i], cor_val[i], scale );
      }
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( XML.X_PRESENTATION_DATA );

  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * pop_blockDiagCovTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_blockDiagCovTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_blockDiagCovTest>(
         "testDriver", 
	 &pop_blockDiagCovTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_blockDiagCovTest>(
         "testReportML", 
	 &pop_blockDiagCovTest::testReportML ) );
  return suiteOfTests;
}

