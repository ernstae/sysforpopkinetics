#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_withoutIDTest.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
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
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD";
  char LDFLAG[514];

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
  const char *strEVID = "EVID";
  const char *label[] = { strDV, strTIME, strMDV, strEVID };
  map<const char*, const char*> label_alias;
  int nLabels         = 4;

  //============================================
  // <Data Set>  --- SKIP ID
  //
  //   ID       DV=CP    TIME    MDV   EVID
  /*
        (1)       0.1     0.2      0      0
        (1)     338.8   337.4      0      0
        (1)     118.1   118.2      0      0
        (1)     888.0   884.6      0      0
        (1)       9.2    10.1      0      0
        (1)     228.1   226.5      0      0
        (1)     668.5   666.3      0      0
        (1)     998.5   996.3      0      0
        (1)     449.1   448.6      0      0
        (1)     778.9   777.0      0      0
        (1)     559.2   558.2      0      0
        (1)       0.3     0.4      0      0
        (1)       0.1     0.6      0      0
        (1)     778.1   775.5      0      0
        (1)     668.8   666.9      0      0
        (1)     339.3   338.0      0      0
        (1)     448.9   447.5      0      0
        (1)      10.8    11.6      0      0
        (1)     557.7   556.0      0      0
        (1)     228.3   228.1      0      0
        (1)     998.0   995.8      0      0
        (1)     888.0   887.6      0      0
        (1)     119.6   120.2      0      0
        (1)       0.3     0.3      0      0
        (1)       0.6     0.3      0      0
        (1)     557.6   556.8      0      0
        (1)     339.3   339.1      0      0
        (1)     888.0   887.2      0      0
        (1)     998.5   999.0      0      0
        (1)     778.9   779.0      0      0
        (1)      10.2    11.1      0      0
        (1)     117.6   118.3      0      0
        (1)     228.9   229.2      0      0
        (1)     668.4   669.1      0      0
        (1)     449.2   448.9      0      0
        (1)       0.2     0.5      0      0
   */
  //============================================
  const int    nRecords   =  36;
  const int    nFixed     =  0;
  const int    nItems     =  4;
  const double record0[]  = {   0.1,   0.2, 0, 0 };
  const double record1[]  = { 338.8, 337.4, 0, 0 };
  const double record2[]  = { 118.1, 118.2, 0, 0 };
  const double record3[]  = { 888.0, 884.6, 0, 0 };
  const double record4[]  = {   9.2,  10.1, 0, 0 };
  const double record5[]  = { 228.1, 226.5, 0, 0 };
  const double record6[]  = { 668.5, 666.3, 0, 0 };
  const double record7[]  = { 998.5, 996.3, 0, 0 };
  const double record8[]  = { 449.1, 448.6, 0, 0 };
  const double record9[]  = { 778.9, 777.0, 0, 0 };
  const double record10[] = { 559.2, 558.2, 0, 0 };
  const double record11[] = {   0.3,   0.4, 0, 0 };
  const double record12[] = {   0.1,   0.6, 0, 0 };
  const double record13[] = { 778.1, 775.5, 0, 0 };
  const double record14[] = { 668.8, 666.9, 0, 0 };
  const double record15[] = { 339.3, 338.0, 0, 0 };
  const double record16[] = { 448.9, 447.5, 0, 0 };
  const double record17[] = {  10.8, 11.6,  0 };
  const double record18[] = { 557.7, 556.0, 0, 0 };
  const double record19[] = { 228.3, 228.1, 0, 0 };
  const double record20[] = { 998.0, 995.8, 0, 0 };
  const double record21[] = { 888.8, 887.6, 0, 0 };
  const double record22[] = { 119.6, 120.2, 0, 0 };
  const double record23[] = {   0.3,   0.3, 0, 0 };
  const double record24[] = {   0.6,   0.3, 0, 0 };
  const double record25[] = { 557.6, 556.8, 0, 0 };
  const double record26[] = { 339.3, 339.1, 0, 0 };
  const double record27[] = { 888.0, 887.2, 0, 0 };
  const double record28[] = { 998.5, 999.0, 0, 0 };
  const double record29[] = { 778.9, 779.0, 0, 0 };
  const double record30[] = {  10.2 , 11.1, 0, 0 };
  const double record31[] = { 117.6, 118.3, 0, 0 };
  const double record32[] = { 228.9, 229.2, 0, 0 };
  const double record33[] = { 668.4, 669.1, 0, 0 };
  const double record34[] = { 449.2, 448.9, 0, 0 };
  const double record35[] = {  0.2,   0.5, 0, 0 };
  //  const double record36[] = { 0.0, 0.0, 1 };

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
  const double theta_in [ thetaLen ]   = { 0.2,  1.0 };
  const double theta_up [ thetaLen ]   = { 2.0, 10.0 };
  const double theta_low[ thetaLen ]   = { 0.02, 0.1 };
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
  const bool isSimulate         = false;
  const int  seed               = -1;
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

  const double nm_pred[]    = {  };
};

void ind_withoutIDTest::setUp()
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

  // TIME doesn't have an alias
  label_alias[strTIME] = NULL;

  // DV is aliased to CP
  label_alias[strDV]   = strCP;

  // MDV doesn't have an alias.
  label_alias[strMDV]  = NULL;

  X_ERROR_LIST                 = XMLString::transcode( C_ERROR_LIST );
  X_VALUE                      = XMLString::transcode( C_VALUE );
  X_IND_OBJ_OUT                = XMLString::transcode( C_IND_OBJ_OUT );
  X_THETA_OUT                  = XMLString::transcode( C_THETA_OUT );
  X_OMEGA_OUT                  = XMLString::transcode( C_OMEGA_OUT );
  X_IND_ANALYSIS_RESULT        = XMLString::transcode( C_IND_ANALYSIS_RESULT );
  X_IND_STDERROR_OUT           = XMLString::transcode( C_IND_STDERROR_OUT );
  X_IND_COVARIANCE_OUT         = XMLString::transcode( C_IND_COVARIANCE_OUT );
  X_IND_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_IND_INVERSE_COVARIANCE_OUT );
  X_IND_CONFIDENCE_OUT         = XMLString::transcode( C_IND_CONFIDENCE_OUT );
  X_IND_COEFFICIENT_OUT        = XMLString::transcode( C_IND_COEFFICIENT_OUT );
  X_IND_CORRELATION_OUT        = XMLString::transcode( C_IND_CORRELATION_OUT );
  X_PRESENTATION_DATA          = XMLString::transcode( C_PRESENTATION_DATA );

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
void ind_withoutIDTest::tearDown()
{  
  XMLString::release( &X_ERROR_LIST );
  XMLString::release( &X_VALUE );
  XMLString::release( &X_IND_OBJ_OUT );
  XMLString::release( &X_THETA_OUT );
  XMLString::release( &X_OMEGA_OUT );
  XMLString::release( &X_IND_ANALYSIS_RESULT );
  XMLString::release( &X_IND_STDERROR_OUT );
  XMLString::release( &X_IND_COVARIANCE_OUT );
  XMLString::release( &X_IND_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_IND_CONFIDENCE_OUT );
  XMLString::release( &X_IND_COEFFICIENT_OUT );
  XMLString::release( &X_IND_CORRELATION_OUT );
  XMLString::release( &X_PRESENTATION_DATA );

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
      remove( fMontePars_h );
      remove( fNonmemPars_h );
      remove( fIndData_h );
      remove( fDataSet_h );
      remove( fPred_h );
      remove( fPredEqn_cpp );
      remove( fMakefile );
      remove( fSavedReportML );
      remove( fTraceOut );
      remove( fCheckpoint_xml );
    }
  XMLPlatformUtils::Terminate();

}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void ind_withoutIDTest::createDataML()
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
  oData << "The data set (without ID) for the individual analysis test" << endl;
  oData << "</description>" << endl;

  // Labels
  oData << "<row position=\"1\">" << endl;
  for( int i=0; i<nItems; i++ )
    {
      oData << "<value type=\"string\">" << label[i] << "</value>" << endl;
    }
  oData << "</row>" << endl;

  // Values
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
 
void ind_withoutIDTest::createSourceML()
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
void ind_withoutIDTest::parse()
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

void ind_withoutIDTest::testIndDataClass()
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
  // * MDV (registered by the Compiler)
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
  o << "   vector<double> a_mdv(n);" << endl;
  o << "   vector<double> a_evid(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_id  [" << i << "] = \"" << 1 << "\";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][0] << ";" << endl;
    o << "   a_time[" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << record[i][2] << ";" << endl;
    o << "   a_evid[" << i << "] = "   << record[i][3] << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_dv, a_time, a_mdv, a_evid );" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << 1 << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", A." << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", A." << strEVID << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to Pred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen, A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,   A." << strETA   << "[" << i << "].size() );" << endl;
      o << endl;
    }
  o << endl;  

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, A." << strPRED  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strIPRED << ".size() );" << endl;

  o << "   MY_ASSERT_EQUAL( n, A." << strF     << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strY     << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nFixed << ", y.size() );" << endl;
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( A." << strMDV << "[j] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( A." << strDV << "[j], y[k] );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[256];
  sprintf( command, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void ind_withoutIDTest::testDataSetClass()
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
  o << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( set.data[0]->" << strID << "[" << i << "], \"" << 1 << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", set.data[0]->" << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][0] << ", set.data[0]->" << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", set.data[0]->" << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", set.data[0]->" << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", set.data[0]->" << strEVID << "[" << i << "] );" << endl;
    }

  o << "   for( int j=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( " << thetaLen << ", set.data[0]->" << strTHETA << "[j].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( " << etaLen   << ", set.data[0]->" << strETA   << "[j].size() );" << endl;
  o << "   }" << endl;

  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strPRED  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIRES  << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIWRES << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strIPRED << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strF    << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, set.data[0]->" << strY    << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   for( int j=0, k=0; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( set.data[0]->" << strMDV << "[j] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( set.data[0]->" << strDV << "[j], y[k] );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;

  o << endl;
  o << "  return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[256];
  sprintf( command, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_withoutIDTest::testDriver()
{
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fFitDriver );
  int  exitcode      = 0;
  char command[256];
  sprintf( command, "make -f %s test", fMakefile );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fFitDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s > %s", fFitDriver, fTraceOut );

  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for convergence problem <%d>!", fFitDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode == 2 )
    {
      char message[256];
      sprintf( message, "%s failed due to inproper file access permission <%d>!", fFitDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode > 2 )
    {
      char message[256];
      sprintf( message, 
               "%s failed for reasons other than convergence propblem or access permission <%d>!", 
               fFitDriver, exitcode );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
     }

  if( rename( fReportML, fSavedReportML ) != 0 )
    {
      char message[256];
      sprintf( message, 
               "Failed to rename %s to %s!",
               fReportML, fSavedReportML );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
   okToClean = true;
}


CppUnit::Test * ind_withoutIDTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_withoutIDTest"  );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testIndDataClass", 
	 &ind_withoutIDTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testDataSetClass", 
	 &ind_withoutIDTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_withoutIDTest>(
         "testDriver", 
	 &ind_withoutIDTest::testDriver ) );

  return suiteOfTests;
}

