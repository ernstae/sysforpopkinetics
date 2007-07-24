#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_simNoEstTest.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "../../spkcompiler/nonmem/NonmemTranslator.h"
#include "../../spkcompiler/SymbolTable.h"
#include "../../spkcompiler/series.h"
#include "../../spkcompiler/SpkCompilerException.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
namespace{
  const unsigned int MAXCHARS = 256;

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
  char XERCESCLIB[]  = "xerces-c";
  char CLNLIB[]      = "cln";
  char GINACLIB[]    = "ginac";
  char BADLIB[]      = "bad";
  char BAPLIB[]      = "bap";
  char BAVLIB[]      = "bav";
  char BA0LIB[]      = "ba0";
  char GSLLIB[]      = "gsl";
  char GSLCBLASLIB[] = "gslcblas";

  char MLIB[]       = "m";
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
   std::cerr << __FILE__ << \"(\" << __LINE__ << \"): but was \" << actual << std::endl; \\\n \
   raise( SIGABRT ); \\\n \
} \\\n\n";

  
  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = false;

  //============================================
  // Setting up the array filled with data 
  // labels for internal (test) use.
  //============================================
  const char *strID    = "ID";
  const char *strTIME  = "TiMe";
  const char *strDV    = "DV";
  const char *strCP    = "CP";
  const char *strORGDV = "ORGDV";
  const char *label[]  = { strID, strDV, strTIME };
  map<const char*, const char*> label_alias;
  int nLabels          = 3;

  //============================================
  // <Data Set>
  //
  //   ID     DV=CP     SIMDV       TIME
  /*
        1       0.1     1.51227      0.2
        1     338.8   338.208      337.4
        1     118.1   117.688      118.2
        1     888.0   883.081      884.6
        1       9.2     9.89995     10.1
        1     228.1   224.428      226.5
        1     668.5   667.366      666.3
        1     998.5   955.467      996.3
        1     449.1   448.442      448.6
        1     778.9   776.086      777.0
        1     559.2   558.447      558.2
        1       0.3     1.44485      0.4
        1       0.1     3.02414      0.6 
        1     778.1   775.694      775.5
        1     668.8   666.35       666.9
        1     339.3   338.354      338.0
        1     448.9   447.472      447.5
        1      10.8    10.4716      11.6
        1     557.7   556.254      556.0
        1     228.3   227.238      228.1
        1     998.0   996.787      995.8
        1     888.0   887.73       887.6
        1     119.6   118.695      120.2
        1       0.3    -0.141019     0.3
        1       0.6     0.65588      0.3
        1     557.6   557.039      556.8
        1     339.3   339.194      339.1
        1     888.0   887.719      887.2
        1     998.5   998.309      999.0
        1     778.9   780.517      779.0
        1      10.2    11.8056      11.1
        1     117.6   118.341      118.3
        1     228.9   230.374      229.2
        1     668.4   669.058      669.1
        1     449.2   448.026      448.9
        1       0.2     1.2894       0.5
   */
  //============================================
  const int    nRecords   =  36;
  const int    nFixed     =  0;
  const int    nItems     =  3;
  const double record0[]  = { 1,   0.1,   0.2 };
  const double record1[]  = { 1, 338.8, 337.4 };
  const double record2[]  = { 1, 118.1, 118.2 };
  const double record3[]  = { 1, 888.0, 884.6 };
  const double record4[]  = { 1,   9.2,  10.1 };
  const double record5[]  = { 1, 228.1, 226.5 };
  const double record6[]  = { 1, 668.5, 666.3 };
  const double record7[]  = { 1, 998.5, 996.3 };
  const double record8[]  = { 1, 449.1, 448.6 };
  const double record9[]  = { 1, 778.9, 777.0 };
  const double record10[] = { 1, 559.2, 558.2 };
  const double record11[] = { 1,   0.3,   0.4 };
  const double record12[] = { 1,   0.1,   0.6 };
  const double record13[] = { 1, 778.1, 775.5 };
  const double record14[] = { 1, 668.8, 666.9 };
  const double record15[] = { 1, 339.3, 338.0 };
  const double record16[] = { 1, 448.9, 447.5 };
  const double record17[] = { 1,  10.8,  11.6 };
  const double record18[] = { 1, 557.7, 556.0 };
  const double record19[] = { 1, 228.3, 228.1 };
  const double record20[] = { 1, 998.0, 995.8 };
  const double record21[] = { 1, 888.8, 887.6 };
  const double record22[] = { 1, 119.6, 120.2 };
  const double record23[] = { 1,   0.3,   0.3 };
  const double record24[] = { 1,   0.6,   0.3 };
  const double record25[] = { 1, 557.6, 556.8 };
  const double record26[] = { 1, 339.3, 339.1 };
  const double record27[] = { 1, 888.0, 887.2 };
  const double record28[] = { 1, 998.5, 999.0 };
  const double record29[] = { 1, 778.9, 779.0 };
  const double record30[] = { 1,  10.2,  11.1 };
  const double record31[] = { 1, 117.6, 118.3 };
  const double record32[] = { 1, 228.9, 229.2 };
  const double record33[] = { 1, 668.4, 669.1 };
  const double record34[] = { 1, 449.2, 448.9 };
  const double record35[] = { 1,   0.2,   0.5 };

  double const * record[nRecords];

  const double simdv[nRecords] = {   1.51227, 
                                   338.208, 
                                   117.688, 
                                   883.081, 
                                     9.89995, 
                                   224.428, 
                                   667.366, 
                                   995.467, 
                                   448.442, 
                                   776.086, 
                                   558.447, 
                                     1.44485, 
                                     3.02414, 
                                   775.694, 
                                   666.35, 
                                   338.354, 
                                   447.472, 
                                    10.4716, 
                                   556.254, 
                                   227.238, 
                                   996.787, 
                                   887.73, 
                                   118.695,
                                    -0.141019, 
                                     0.655588, 
                                   557.039, 
                                   339.194, 
                                   887.719, 
                                   998.309, 
                                   780.517, 
                                    11.8056, 
                                   118.341, 
                                   230.374, 
                                   669.058, 
                                   448.026, 
                                     1.22894 };

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
  const double theta_in [ thetaLen ]   = { 0.2,  1.0 };
  const double theta_up [ thetaLen ]   = { 2.0, 10.0 };
  const double theta_low[ thetaLen ]   = { 0.02, 0.1 };
  const bool   theta_fix[ thetaLen ]   = { false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal --- 1 dimensional! --- matrix.
  //============================================
  const int    omegaDim                = 1;
  const Symbol::Structure omegaStruct  = Symbol::DIAGONAL;
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
  const int  subproblems        = 1;

  //============================================
  // PRED model based on Norris
  //
  // b0 = THETA(1)
  // b1 = THETA(2)
  // x = TIME
  // F = b0 + b1 * x = THETA(1) + THETA(2)*TIME
  // Y = F + ETA(1)
  //============================================
  const char PREDEQN[] = "b0 = THETA(1)\nb1 = THETA(2)\nx = TiMe\nF = b0 + b1 * x\nY = F + ETA(1)\n";


  //============================================
  // NONMEM's answers
  //
  // NOTE: NONMEM's matrices are placed
  // in the row-major order.
  //============================================
  const double nm_obj       =  46.4087;
  const double nm_theta[]   = { 0.02, 1.00171 };
  const double nm_omega[]   = { 0.771353 };
};

void ind_simNoEstTest::setUp()
{
  okToClean = false;

  // The string returned by returned by type_info.name() contains the 
  // class name preceeded by the number of charcters in the name.
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
  record[35]  = record35;
  //  record[36]  = record36;

  createDataML();
  createSourceML();
  parse();
}
void ind_simNoEstTest::tearDown()
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
void ind_simNoEstTest::createDataML()
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
 
void ind_simNoEstTest::createSourceML()
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
void ind_simNoEstTest::parse()
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
      CPPUNIT_ASSERT_MESSAGE( "Failed to compile." , false );
    }
}
void ind_simNoEstTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fFitDriver );
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
void ind_simNoEstTest::testReportML()
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
  
  error_list = report->getElementsByTagName( XML.X_ERROR_LIST );
  CPPUNIT_ASSERT_EQUAL( 1, (int)error_list->getLength() );
  DOMElement* error = dynamic_cast<DOMElement*>( error_list->item(0) );
  const XMLCh* error_message = error->getFirstChild()->getNodeValue();
  CPPUNIT_ASSERT_MESSAGE( "<error_list> should have been empty.", XMLString::isAllWhiteSpace( error_message ) );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the generated data
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *presentation_data_sets;
  
  presentation_data_sets = report->getElementsByTagName( XML.X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data_sets->getLength() == 1 );


  okToClean = true;
}

CppUnit::Test * ind_simNoEstTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_simNoEstTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_simNoEstTest>(
         "testDriver", 
	 &ind_simNoEstTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_simNoEstTest>(
         "testReportML", 
	 &ind_simNoEstTest::testReportML ) );
  return suiteOfTests;
}

