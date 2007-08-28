#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_advan6Test.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "../../spkcompiler/nonmem/NonmemTranslator.h"
#include "../../spkcompiler/nonmem/CompModelInfo.h"
#include "../../spkcompiler/series.h"
#include "../../spkcompiler/SymbolTable.h"
#include "../../spkcompiler/SpkCompilerException.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

/*
===================================================================================
  Test POP033: INDOMETH1A
===================================================================================
*/
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
  char fODEPredDriver       [MAXCHARS+1];
  char fODEPredDriver_cpp   [MAXCHARS+1];

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
  const int  mitr       = 450;
  const bool isEstimate = true;
  const char method[]   = "foce";
  const int  sig_digits = 3;

  //============================================
  // <Data Set>
  //   C   ID   TIME   DV   DOSE=AMT
  //============================================
  map<const char*, const char*> label_alias;
  const char *strID         = "ID";
  const char *strDV         = "DV";
  const char *strTIME       = "TIME";
  const char *strDOSE       = "DOSE";
  const char *strAMT        = "AMT";
  const char *strJUNK       = "JUNK";
  const char *strMDV        = "MDV";
  const char *strEVID       = "EVID";
  const char *label[]       = { strID, strTIME, strDV, strDOSE, strMDV, strEVID };
  const int    nLabels      = 6;
  const int    nIndividuals = 6;
  const int    nRecords     = 12 + 12 + 11 + 12 + 12 + 12;
  const int    nFixed       = 0;
  const int    nMDV         = 6;
  const int    nItems       = nLabels;
  const int    c_N[]        = { 11, 11, 10, 11, 11, 11 };
  valarray<int> N( c_N, nIndividuals );
  const int    c_NRecords[] = { 12, 12, 11, 12, 12, 12 };
  valarray<int> NRecords( c_NRecords, nIndividuals );
  const double record0 [] = {1,0,0,1,1,1};
  const double record1 [] = {1,0.25,1.5,0,0,0};
  const double record2 [] = {1,0.5,0.94,0,0,0};
  const double record3 [] = {1,0.75,0.78,0,0,0};
  const double record4 [] = {1,1,0.48,0,0,0};
  const double record5 [] = {1,1.25,0.37,0,0,0};
  const double record6 [] = {1,2,0.19,0,0,0};
  const double record7 [] = {1,3,0.12,0,0,0};
  const double record8 [] = {1,4,0.11,0,0,0};
  const double record9 [] = {1,5,0.08,0,0,0};
  const double record10[] = {1,6,0.07,0,0,0};
  const double record11[] = {1,8,0.05,0,0,0};
  const double record12[] = {2,0,0,1,1,1};
  const double record13[] = {2,0.25,2.03,0,0,0};
  const double record14[] = {2,0.5,1.63,0,0,0};
  const double record15[] = {2,0.75,0.71,0,0,0};
  const double record16[] = {2,1,0.7,0,0,0};
  const double record17[] = {2,1.25,0.64,0,0,0};
  const double record18[] = {2,2,0.36,0,0,0};
  const double record19[] = {2,3,0.32,0,0,0};
  const double record20[] = {2,4,0.2,0,0,0};
  const double record21[] = {2,5,0.25,0,0,0};
  const double record22[] = {2,6,0.12,0,0,0};
  const double record23[] = {2,8,0.08,0,0,0};
  const double record24[] = {3,0,0,1,1,1};
  const double record25[] = {3,0.5,1.49,0,0,0};
  const double record26[] = {3,0.75,1.16,0,0,0};
  const double record27[] = {3,1,0.8,0,0,0};
  const double record28[] = {3,1.25,0.8,0,0,0};
  const double record29[] = {3,2,0.39,0,0,0};
  const double record30[] = {3,3,0.22,0,0,0};
  const double record31[] = {3,4,0.12,0,0,0};
  const double record32[] = {3,5,0.11,0,0,0};
  const double record33[] = {3,6,0.08,0,0,0};
  const double record34[] = {3,8,0.08,0,0,0};
  const double record35[] = {4,0,0,1,1,1};
  const double record36[] = {4,0.25,1.85,0,0,0};
  const double record37[] = {4,0.5,1.39,0,0,0};
  const double record38[] = {4,0.75,1.02,0,0,0};
  const double record39[] = {4,1,0.89,0,0,0};
  const double record40[] = {4,1.25,0.59,0,0,0};
  const double record41[] = {4,2,0.4,0,0,0};
  const double record42[] = {4,3,0.16,0,0,0};
  const double record43[] = {4,4,0.11,0,0,0};
  const double record44[] = {4,5,0.1,0,0,0};
  const double record45[] = {4,6,0.07,0,0,0};
  const double record46[] = {4,8,0.07,0,0,0};
  const double record47[] = {5,0,0,1,1,1};
  const double record48[] = {5,0.25,2.05,0,0,0};
  const double record49[] = {5,0.5,1.04,0,0,0};
  const double record50[] = {5,0.75,0.81,0,0,0};
  const double record51[] = {5,1,0.39,0,0,0};
  const double record52[] = {5,1.25,0.3,0,0,0};
  const double record53[] = {5,2,0.23,0,0,0};
  const double record54[] = {5,3,0.13,0,0,0};
  const double record55[] = {5,4,0.11,0,0,0};
  const double record56[] = {5,5,0.08,0,0,0};
  const double record57[] = {5,6,0.1,0,0,0};
  const double record58[] = {5,8,0.06,0,0,0};
  const double record59[] = {6,0,0,1,1,1};
  const double record60[] = {6,0.25,2.31,0,0,0};
  const double record61[] = {6,0.5,1.44,0,0,0};
  const double record62[] = {6,0.75,1.03,0,0,0};
  const double record63[] = {6,1,0.84,0,0,0};
  const double record64[] = {6,1.25,0.64,0,0,0};
  const double record65[] = {6,2,0.42,0,0,0};
  const double record66[] = {6,3,0.24,0,0,0};
  const double record67[] = {6,4,0.17,0,0,0};
  const double record68[] = {6,5,0.13,0,0,0};
  const double record69[] = {6,6,0.1,0,0,0};
  const double record70[] = {6,8,0.09,0,0,0};

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
  const char *strT        = "T";
  const char *strDADT     = "DADT";
  const char *strP        = "P";
  const char *strA        = "A";
  //  const char *strRATE     = "RATE";
  const char *strR        = "R";
  const char *strD        = "D";
  const char *strALAG     = "ALAG";
  const char *strS        = "S";
  const char *strFO       = "FO"; // ef-oh
  const char *strF0       = "F0"; // ef-zero
  const char *strS0       = "S0"; // es-zero

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
  const int    thetaLen = 4;
  const double theta_in [ thetaLen ]   = { 0.25,  0.75,  0.5,  0.5 };
  const double theta_up [ thetaLen ]   = { 2.5,   7.5,   5.0,  5.0 };
  const double theta_low[ thetaLen ]   = { 0.025, 0.075, 0.05, 0.05 };
  const bool   theta_fix[ thetaLen ]   = { false, false };

  //============================================
  // The SPK Compiler determines the initial
  // values for eta, the variance of data
  // in the individual analysis case.
  // The size of this vector is determined by
  // the order of Omega matrix.
  //============================================
  const int    etaLen = 1;
  const double eta_in  [ etaLen ] = { 0.0 };
  const bool   eta_fix [ etaLen ] = { false };

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
  const double omega_in [ omegaOrder ] = { 0.1 };
  const bool   omega_fix[ omegaOrder ] = { false };

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
  const double sigma_in [ sigmaOrder ] = { 0.03 };
  const bool   sigma_fix[ sigmaOrder ] = { false };

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
  // $MODEL - compartmental model definition
  //============================================
  // This definition is not based upon text.

  //============================================
  // $PK model 
  //
  // CL = THETA(1)*EXP(ETA(1))
  // V = THETA(2)
  // K = CL/V
  // S1 = V
  //============================================
  // Revisit Sachiko 08/05/2005
  //
  // What is basic PK parameters???
  // The length of P vector?
  const int nPkParams = 0;
  const char PK[] = "K-12=THETA(1)*EXP(ETA(1))\nK01=THETA(2)\nK21=THETA(3)\nVOL=THETA(4)\nS1=VOL\n";

  //============================================
  // $DES model
  //
  // DADT(1)=-K*A(1)
  //============================================
  const int  nComps       = 2;
  
  const int  nonmemNComps = nComps + 1;
  const int  nEquilibrims = 0;
  const char DIFFEQN[]    = "DADT(1) = - (K21 +K01)*A(1) +K12*A(2)\nDADT(2)= K21*A(1)- K12*A(2)\n";

  //============================================
  // $ERROR model
  //
  // Y = F*(1 + EPS(1) )
  //============================================
  const char ERROR[] = "Y = F*(1+EPS(1))\n";

  //============================================
  // Results
  //============================================
  double nm_theta    [] = { 2.49E-01, 7.81E-01, 5.07E-01, 4.12E-01 };
  double nm_omega    [] = { 5.48E-01 };
  double nm_sigma    [] = { 4.46E-02 };

  double nm_stderr   [] = { 0.0314398, 0.0627952, 0.0582593, 0.0331642,
                            0.373946,  0.00918988 };

  double nm_cov      [] = { 0.000988461,
                           -5.24604e-05,  0.00394323,
                            0.000544309,  0.00227364,  0.00339415,
                           -2.06129e-05, -0.0017772,  -0.00131102,  0.00109986,  
                           -0.00200745,   0.00483583, -0.00157015, -0.000278204,  0.139836,   
                           -6.08046e-05,  0.000112836, 1.04971e-05, 5.69998e-06,  0.000486753, 8.4454e-05 }; 
  double nm_corr     [] = { 1,
                           -0.0265721,    1,
                            0.297167,     0.621484,    1,
                           -0.0197693,   -0.853375,   -0.67854,     1,
                           -0.170748,     0.205938,   -0.0720722,  -0.0224329,    1,
                           -0.210449,     0.195529,    0.0196062,   0.0187023,    0.141641,    1 };
  double nm_invCov   [] = { 1280.7,
			       8.7812,  1286.42,
                            -362.532,   -134.114,    672.074,
                            -396.203,   1918.1,      581.218,     4697.75,      
                              10.0048,   -36.4285,      9.02958,   -45.7978,    8.53482,  
			     924.473,   -1615.25,     -256.632,  -2973.31,      8.65214,  14847.2  };
};
void pop_advan6Test::setUp()
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
  snprintf( fDataML,               MAXCHARS, "%s_data.xml",             fPrefix );
  snprintf( fSourceML,             MAXCHARS, "%s_source.xml",           fPrefix );
  snprintf( fDataSetDriver,        MAXCHARS, "%s_DataSetDriver",        fPrefix );
  snprintf( fDataSetDriver_cpp,    MAXCHARS, "%s_DataSetDriver.cpp",    fPrefix );
  snprintf( fODEPredDriver,        MAXCHARS, "%s_ODEPredDriver",        fPrefix );
  snprintf( fODEPredDriver_cpp,    MAXCHARS, "%s_ODEPredDriver.cpp",    fPrefix );

  snprintf( LDFLAG, LDFLAG_MAXCHARS, "%s -l%s -l%s  -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
     LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB, CLNLIB, GINACLIB, BADLIB, BAPLIB, BAVLIB, BA0LIB, GSLLIB, GSLCBLASLIB );

  // ID has no alias
  label_alias[strID]   = NULL;

  // TIME has no alias
  label_alias[strTIME] = NULL;

  // DV has no alias
  label_alias[strDV]   = NULL;

  // DOSE is aliased to AMT
  label_alias[strDOSE] = strAMT;

  record[0]    = record0;
  record[1]    = record1;
  record[2]    = record2;
  record[3]    = record3;
  record[4]    = record4;
  record[5]    = record5;
  record[6]    = record6;
  record[7]    = record7;
  record[8]    = record8;
  record[9]    = record9;
  record[10]   = record10;
  record[11]   = record11;
  record[12]   = record12;
  record[13]   = record13;
  record[14]   = record14;
  record[15]   = record15;
  record[16]   = record16;
  record[17]   = record17;
  record[18]   = record18;
  record[19]   = record19;
  record[20]   = record20;
  record[21]   = record21;
  record[22]   = record22;
  record[23]   = record23;
  record[24]   = record24;
  record[25]   = record25;
  record[26]   = record26;
  record[27]   = record27;
  record[28]   = record28;
  record[29]   = record29;
  record[30]   = record30;
  record[31]   = record31;
  record[32]   = record32;
  record[33]   = record33;
  record[34]   = record34;
  record[35]   = record35;
  record[36]   = record36;
  record[37]   = record37;
  record[38]   = record38;
  record[39]   = record39;
  record[40]   = record40;
  record[41]   = record41;
  record[42]   = record42;
  record[43]   = record43;
  record[44]   = record44;
  record[45]   = record45;
  record[46]   = record46;
  record[47]   = record47;
  record[48]   = record48;
  record[49]   = record49;
  record[50]   = record50;
  record[51]   = record51;
  record[52]   = record52;
  record[53]   = record53;
  record[54]   = record54;
  record[55]   = record55;
  record[56]   = record56;
  record[57]   = record57;
  record[58]   = record58;
  record[59]   = record59;
  record[60]   = record60;
  record[61]   = record61;
  record[62]   = record62;
  record[63]   = record63;
  record[64]   = record64;
  record[65]   = record65;
  record[66]   = record66;
  record[67]   = record67;
  record[68]   = record68;
  record[69]   = record69;
  record[70]   = record70;

  createDataML();
  createSourceML();
  try{
    parse();
  }
  catch( SpkCompilerException & e )
    {
       cerr << "parse() threw an SpkCompilerException." << endl;
       cerr << e << endl;
    }
  catch( ... )
    {
       fprintf( stderr, "parse() threw an unknown exception.\n" );
    }
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Translation
// 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void pop_advan6Test::parse()
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
      throw;
    }
  catch( ... )
    {
      cerr << "NonmemTranslator::translate() threw an unknown exception!" << endl;
      throw;
    }
  const CompModelInfo compmodel = xlator.getCompModel();

  if( nonmemNComps != compmodel.getNCompartments() )
  {
      fprintf( stderr, "nonmemComps (%d) != compmodel.getNCompartments() (%d)\n", 
               nonmemNComps, compmodel.getNCompartments() );
  }

  if( compmodel.getNParameters() != nPkParams )
  {
     fprintf( stderr, "nPkParams (%d) != compmodel.getNParameters() (%d)\n", 
              nPkParams, compmodel.getNParameters() );
  }

  if( nEquilibrims != compmodel.getNEquilibrims() )
  {
     fprintf( stderr, "nEquilibrims (%d) != compmodel.getNEquilibrims() (%d)",
              nEquilibrims, compmodel.getNEquilibrims() );
  }
  for( int i=0; i<nonmemNComps; i++ )
  {
    char name[56];
    snprintf( name, 56, "COMP%d", i+1 );
    switch( i )
    {
    case 0: 
      assert( strcmp( compmodel[i].getName().c_str(), "CENTRAL" ) == 0 );
      assert( compmodel[i].is_default_observation() );
      assert( compmodel[i].is_default_dose() );
      break;
    case 1: 
      assert( strcmp( compmodel[i].getName().c_str(), "PERIPH" ) == 0 );
      assert( !compmodel[i].is_default_observation() );
      assert( !compmodel[i].is_default_dose() );
      break;
    default: 
      assert( strcmp( compmodel[i].getName().c_str(), name ) == 0 );
      assert( !compmodel[i].is_default_observation() );
      assert( !compmodel[i].is_default_dose() );
      break;
    }
  }
  // Check general variables registered by the compiler
  SymbolTable* table = xlator.getSymbolTable();
  assert( table->find( strPRED ) );
  assert( table->find( strIPRED ) );
  assert( table->find( strIWRES ) );
  assert( table->find( strIRES ) );
  assert( table->find( strIETARES ) );
  assert( table->find( strIWETARES ) );
  assert( table->find( strPPRED ) );
  assert( table->find( strPWRES ) );
  assert( table->find( strPRES ) );
  assert( table->find( strPETARES ) );
  assert( table->find( strPWETARES ) );

  // Check variables that are required specifically by ADVAN6
  assert( table->find( strY ) );
  assert( table->find( strF ) );
  assert( table->find( strT ) );
  assert( table->find( strTIME ) );
  assert( table->find( strAMT ) );
  char Ri[ 24 ];
  char Di[ 24 ];
  char ALAGi[ 24 ];
  char Si[ 24 ];
  char Fi[ 24 ];
  for ( int i=0; i<nonmemNComps; i++ )
    {
      snprintf( Ri, 24, "R%d", i+1 );
      assert( table->find( Ri ) );

      snprintf( Di, 24, "D%d", i+1 );
      assert( table->find( Di ) );

      snprintf( ALAGi, 24, "ALAG%d", i+1 );
      assert( table->find( ALAGi ) );

      snprintf( Si, 24, "S%d", i+1 );
      assert( table->find( Si ) );

      if( i < nonmemNComps-1 )
	{
          // should be read-write
	  snprintf( Fi, 24, "F%d", i+1 );
	  assert( table->find( Fi ) );
	}
      else
        {
          // should be read only
	  snprintf( Fi, 24, "F%d", i+1 );
	  assert( table->find( Fi ) );
        }
    }
  assert( table->find( strFO ) );
  assert( table->find( strF0 ) );
  assert( table->find( strS0 ) );
}
void pop_advan6Test::tearDown()
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
      remove( fODEPredDriver );
      remove( fODEPredDriver_cpp );
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
#include "DOMPrint.h"
void pop_advan6Test::createDataML()
{
  /*
  ofstream o( fDataML );
  assert( o.good() );

  // create a data set
  
  o << "<?xml version=\"1.0\"?>" << endl;
  o << "<spkdata version=\"0.1\">" << endl;
  o << "<table columns=\"" << nItems << "\" rows=\"" << nRecords+1 << "\">" << endl;
  o << "<row position=\"1\">" << endl;
  for( int j=0; j<nItems; j++ )
    {
      o << "   <value type=\"string\">" << label[j] << "</value>" << endl;
    }
  o << "</row>" << endl;
  for( int i=0; i<nRecords; i++ )
    {
      o << "<row position=\"" << i+2 << "\">" << endl;
      for ( int j=0; j<nItems; j++ )
	o << "   <value>" << record[i][j] << "</value>" << endl;
      o << "</row>" << endl;
    }
  o << "</table>" << endl;
  o << "</spkdataml>" << endl;
  o.close();
  */

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Parse a data set 
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  xercesc::XercesDOMParser *dataParser = new xercesc::XercesDOMParser;
  dataParser->setValidationScheme( XercesDOMParser::Val_Auto );
  dataParser->setDoNamespaces( true );
  dataParser->setDoSchema( true );
  dataParser->setValidationSchemaFullChecking( true );
  dataParser->setCreateEntityReferenceNodes( true );
  
  try{
    //ifstream junk( fDataML );
    ifstream junk( "pop_advan6Test.data.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open a data xml.", junk.good() );
    junk.close();
    dataParser->parse( "pop_advan6Test.data.xml" );
    //dataParser->parse( fDataML );
    data = dataParser->getDocument();
    //DOMPrint( data );
    CPPUNIT_ASSERT_MESSAGE( "Failed to obtain a pointer to DOM parse tree for the data set.", 
                            data != NULL);
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
 
void pop_advan6Test::createSourceML()
{
  /*
  ofstream o( fSourceML );
  CPPUNIT_ASSERT_MESSAGE( "Failed to create a source xml", o.good() );
  
  o << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
  o << "<spksource>" << endl;
  o << "  <nonmem version=\"0.1\">" << endl;
  o << "    <constraint>" << endl;
  o << "      <pop_analysis is_estimation=\"yes\" abort=\"yes\" approximation=\"" << method << "\"";
  o << " interation=\"yes\" mitr=\"" << mitr << "\" pop_size=\"" << nIndividuals << "\"";
  o << " sig_digits=\"" << sig_digits << "\">" << endl;
  o << "        <data_labels>" << endl;
  o << "          <label name=\"ID\"/>" << endl;
  o << "          <label name=\"TIME\"/>" << endl;
  o << "          <label name=\"DV\"/>" << endl;
  o << "          <label name=\"DOSE\" synonym=\"AMT\"/>" << endl;
  o << "        </data_labels>" << endl;
  o << "        <theta length=\"" << thetaLen << "\">" << endl;
  o << "          <low>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      o << "            <value fixed=\"" << theta_fix[i] << "\">" << theta_low[i] << "</value>" << endl;
    }
  o << "          </low>" << endl;
  o << "          <in>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      o << "            <value fixed=\"" << theta_fix[i] << "\">" << theta_in[i] << "</value>" << endl;
    }
  o << "          </in>" << endl;
  o << "          <up>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      o << "            <value fixed=\"" << theta_fix[i] << "\">" << theta_up[i] << "</value>" << endl;
    }
  o << "          </up>" << endl;
  o << "        </theta>" << endl;
  o << "        <omega dimension=\"" << omegaDim << "\" struct=\"" << (omegaStruct==Symbol::DIAGONAL? "diagonal":"full")       << "\">" << endl;
  o << "          <in>" << endl;
  for( int i=0; i<omegaOrder; i++ )
    {
      o << "            <value>" << omega_in[i] << "</value>" << endl;
    }
  o << "          </in>" << endl;
  o << "        </omega>" << endl;
  o << "        <sigma dimension=\"" << sigmaDim << "\" struct=\"" << (sigmaStruct==Symbol::DIAGONAL? "diagonal":"full") << "\">" << endl;
  o << "          <in>" << endl;
  for( int i=0; i<sigmaOrder; i++ )
    {
      o << "            <value>" << sigma_in[i] << "</value>" << endl;
    }
  o << "          </in>" << endl;
  o << "        </sigma>" << endl;
  o << "      </pop_analysis>" << endl;
  o << "    </constraint>" << endl;
  o << "    <model advan=\"6\" tolerance=\"4\">" << endl;
  o << "      <comp_model ncompartments=\"" << nComps << "\"";
  o << " nequilibriums=\"" << nEquilibrims << "\" nparameters=\"" << nPkParams << "\">" << endl;
  o << "         <compartment default_dose=\"yes\" default_observation=\"yes\" initial_off=\"no\" name=\"CENTRAL\"";
  o << " no_dose=\"no\" no_off=\"no\"/>" << endl;
  o << "      </comp_model>" << endl;
  o << "      <diffeqn>" << endl;
  o << DIFFEQN << endl;
  o << "      </diffeqn>" << endl;
  o << "      <pk>" << endl;
  o << PK << endl;
  o << "      </pk>" << endl;
  o << "      <error>" << endl;
  o << ERROR << endl;
  o << "      </error>" << endl;
  o << "    </model>" << endl;
  o << "  </nonmem>" << endl;
  o << "</spksource>" << endl;

  o.close();
  */

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
    //ifstream junk( fSourceML );
    ifstream junk( "pop_advan6Test.source.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open CAD1996A1.source.xml", junk.good() );
    junk.close();
    sourceParser->parse( "pop_advan6Test.source.xml" );
    //sourceParser->parse( fSourceML );
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
void pop_advan6Test::testIndDataClass()
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
  // * DV
  // * DOSE=AMT
  //
  // Check PK Parameters
  // * theta
  // * Omega
  // * eta (registered by the Compiler)
  //
  // Check the variables appeared on the left hand side 
  // of equations in the PRED definition.
  // * F
  // * Y
  // * T
  // * DADT(n)
  // * A(n)
  // * P(n)
  //
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
  o << "   const int n             = " << nRecords     << ";" << endl;
  o << "   const int thetaLen      = " << thetaLen     << ";" << endl;
  o << "   const int etaLen        = " << etaLen       << ";" << endl;
  o << "   const int epsLen        = " << epsLen       << ";" << endl;
  o << "   const int nonmemComps   = " << nonmemNComps << ";" << endl;
  o << "   const int nParams       = " << nPkParams    << ";" << endl;

  o << "   vector<char*>  a_id(n);" << endl;
  o << "   vector<double> a_time(n);" << endl;
  o << "   vector<double> a_dv(n);" << endl;
  o << "   vector<double> a_dose(n);" << endl;
  o << "   vector<double> a_amt(n);" << endl;
  o << "   vector<double> a_mdv(n);" << endl;
  o << "   vector<int> a_evid(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_id  [" << i << "] = \"" << record[i][0] << "\";" << endl;
    o << "   a_time[" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][2] << ";" << endl;
    o << "   a_dose[" << i << "] = "   << record[i][3] << ";" << endl;
    o << "   a_amt [" << i << "] = "   << record[i][3] << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << record[i][4] << ";" << endl;
    o << "   a_evid[" << i << "] = "   << record[i][5] << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_time, a_dv, a_dose, a_mdv, a_evid );" << endl;

  // { ID, DV=CP, TIME }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][1] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][2] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][3] << ", A." << strDOSE << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][3] << ", A." << strAMT  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][4] << ", A." << strMDV  << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][5] << ", A." << strEVID << "[" << i << "] );" << endl;

      // There have to be placeholders for the current values of theta/eta for
      // each call to ODEPred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen,    A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,      A." << strETA   << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( epsLen,      A." << strEPS   << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nonmemComps, A." << strDADT  << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nonmemComps, A." << strA     << "[" << i << "].size() );" << endl;

      // REVISIT Sachiko 08/11/2005
      // What is "#of basic PK parameters?  What is NPARAMETERS good for?
      // Until we figure that out, ignore it.
      //
      //o << "   MY_ASSERT_EQUAL( nParams, A." << strP      << "[" << i << "].size() );" << endl;

      o << endl;
    }
  // The current values of RES/WRES/PRED should be always kept in memory
  // for displaying tables/scatterplots.
  o << "   MY_ASSERT_EQUAL( n, A." << strPRED     << ".size() );" << endl;
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
  o << "   MY_ASSERT_EQUAL( n, A." << strT       << ".size() );" << endl;
  char Ri[ 24 ],  Di[ 24 ], ALAGi[ 24 ], Si[ 24 ], Fi[ 24 ];
  for ( int i=0; i<nonmemNComps; i++ )
    {
      snprintf( Ri, 24, "R%d", i+1 );
      o << "   MY_ASSERT_EQUAL( n, A." << Ri << ".size() );" << endl;
      
      snprintf( Di, 24, "D%d", i+1 );
      o << "   MY_ASSERT_EQUAL( n, A." << Di <<".size() );" << endl;
      
      snprintf( ALAGi, 24, "ALAG%d", i+1 );
      o << "   MY_ASSERT_EQUAL( n, A." << ALAGi << ".size() );" << endl;
      
      snprintf( Si, 24, "S%d", i+1 );
      o << "   MY_ASSERT_EQUAL( n, A." << Si << ".size() );" << endl;
      
      if( i < nonmemNComps-1 )
	{
	  snprintf( Fi, 24, "F%d", i+1 );
	  o << "   MY_ASSERT_EQUAL( n, A." << Fi << ".size() );" << endl;
	}
    }
  o << "   MY_ASSERT_EQUAL( n, A." << strFO << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strF0 << ".size() );" << endl;
  o << "   MY_ASSERT_EQUAL( n, A." << strS0 << ".size() );" << endl;
  o << endl;

  o << "   const valarray<double> y = A.getMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords-nFixed-nMDV << ", y.size() );" << endl;
  o << "   for( int j=0, k=0 ; j<n; j++ )" << endl;
  o << "   {" << endl;
  o << "      if( A." << strAMT << "[j] == 0 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( A." << strDV << "[j], y[k] );" << endl;
  o << "         k++;" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << endl;

  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[512];
  snprintf( command,
	    512,  
	   "g++ %s -o %s %s %s", 
	   fIndDataDriver_cpp, 
	   fIndDataDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  
	       "Compilation of the generated %s failed!", 
	       fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  
	       "A test driver, %s, failed!",
	       fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void pop_advan6Test::testDataSetClass()
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
  o << "   const int n            = " << nRecords     << ";" << endl;
  o << "   const int thetaLen     = " << thetaLen     << ";" << endl;
  o << "   const int etaLen       = " << etaLen       << ";" << endl;
  o << "   const int epsLen       = " << epsLen       << ";" << endl;
  o << "   const int nIndividuals = " << nIndividuals << ";" << endl;
  o << "   const int nonmemComps  = " << nonmemNComps << ";" << endl;
  o << "   const int nParams      = " << nPkParams    << ";" << endl;
  
  //
  // REVISIT Sachiko 08/05/2005
  // What is #of basic PK parameters?  Until we figure that out,
  // ingnore the value.
  //
  //  o << "   const int nPkParams = " << nPkParams << ";" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   valarray<int> N = set.getN();" << endl;
  o << "   int nRecords;" << endl;
  char Ri[ 24 ],  Di[ 24 ], ALAGi[ 24 ], Si[ 24 ], Fi[ 24 ];

  // { ID, DV=CP, TIME }
  for( int j=0, k=0; j<nIndividuals; j++ )
  {
    o << "   nRecords = set.getNRecords( " << j << " );" << endl;
     for( int i=0; i<NRecords[j]; i++, k++ )
     {
       o << "   assert( strcmp( set.data[" << j << "]->" << strID << "[" << i << "], \"" << record[k][0] << "\" ) == 0 );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][1] << ", set.data[" << j << "]->" << strTIME << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][2] << ", set.data[" << j << "]->" << strDV   << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][3] << ", set.data[" << j << "]->" << strDOSE << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][3] << ", set.data[" << j << "]->" << strAMT  << "[" << i << "] );" << endl;

       for ( int k=0; k<nonmemNComps; k++ )
	 {
	   snprintf( Ri,    24, "R%d", k+1 );
	   snprintf( Di,    24, "D%d", k+1 );;
	   snprintf( ALAGi, 24, "ALAG%d", k+1 );
	   snprintf( Si,    24, "S%d", k+1 );
	   o << "   MY_ASSERT_EQUAL( nRecords, set.data[" << j << "]->" << Ri << ".size() );" << endl;
	   o << "   MY_ASSERT_EQUAL( nRecords, set.data[" << j << "]->" << Di << ".size() );" << endl;    
	   o << "   MY_ASSERT_EQUAL( nRecords, set.data[" << j << "]->" << ALAGi << ".size() );" << endl;
	   o << "   MY_ASSERT_EQUAL( nRecords, set.data[" << j << "]->" << Si << ".size() );" << endl;
	 }
       for( int k=0; k<nonmemNComps-1; k++ )
	 {
	   snprintf( Fi, 24, "F%d", k+1 );
	   o << "   MY_ASSERT_EQUAL( nRecords, set.data[" << j << "]->" << Fi << ".size() );" << endl;
	 }
     }
     o << endl;
  }
  o << "   for( int j=0; j<nIndividuals; j++ )" << endl;
  o << "   {" << endl;
  o << "      int nRecords = set.getNRecords( j );" << endl;
  o << "      for( int i=0; i<nRecords; i++ )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( thetaLen,    set.data[j]->"  << strTHETA   << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( etaLen,      set.data[j]->"  << strETA     << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( epsLen,      set.data[j]->"  << strEPS     << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( etaLen,      set.data[j]->"  << strIETARES  << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( etaLen,      set.data[j]->"  << strIWETARES << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( etaLen,      set.data[j]->"  << strPETARES  << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( etaLen,      set.data[j]->"  << strPWETARES << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( nonmemComps, set.data[j]->"  << strDADT    << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( nonmemComps, set.data[j]->"  << strA       << "[i].size() );" << endl;
  //  o << "         MY_ASSERT_EQUAL( nPkParams, set.data[j]->" << strP      << "[i].size() );" << endl;
  o << "      }" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strPRES     << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strPWRES    << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strPPRED    << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strPETARES  << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strPWETARES << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strIRES     << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strIWRES    << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strIPRED    << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strIETARES  << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strIWETARES << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strF       << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strY       << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strT       << ".size() );" << endl;
  o << "   }" << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << N.sum() << ", y.size() );" << endl;
  o << "   for( int j=0, k=0 ; j<nIndividuals; j++ )" << endl;
  o << "   {" << endl;
  o << "      int nRecords = set.getNRecords( j );" << endl;
  o << "      for( int i=0; i<nRecords; i++ )" << endl;
  o << "      {" << endl;
  o << "         if( set.data[j]->" << strAMT << "[i] == 0 )" << endl;
  o << "         {" << endl;
  o << "            MY_ASSERT_EQUAL( set.data[j]->" << strDV << "[i], y[k] );" << endl;
  o << "            k++;" << endl;
  o << "         }" << endl;
  o << "      }" << endl;
  o << "   }" << endl;

  o << endl;
  o << "return 0;" << endl;
  o << "}" << endl;
  
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fDataSetDriver_cpp, fDataSetDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "Compilation of the generated %s failed!", fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_advan6Test::testNonmemPars()
{
  printf( "\n--- %s ---\n", fNonmemParsDriver );
  ofstream o( fNonmemParsDriver_cpp );
  CPPUNIT_ASSERT( o.good() );
  
  o << "#include \"NonmemPars.h\"" << endl;
  o << "#include <valarray>" << endl;
  o << "#include <vector>" << endl;
  o << "#include <string>" << endl;
  o << "#include <iostream>" << endl;
  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   MY_ASSERT_EQUAL( 0, NonmemPars::isPkFunctionOfT );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nonmemNComps << ", NonmemPars::nCompartments );" << endl;
  o << "   MY_ASSERT_EQUAL( 0, NonmemPars::nParameters );" << endl;
  o << "   MY_ASSERT_EQUAL( 1, NonmemPars::defaultDoseComp );" << endl;
  o << "   MY_ASSERT_EQUAL( 1, NonmemPars::defaultObservationComp );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nonmemNComps << ", NonmemPars::initialOff.size() );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nonmemNComps << ", NonmemPars::noOff.size() );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nonmemNComps << ", NonmemPars::noDose.size() );" << endl;
  o << "   for( int i=0; i< NonmemPars::nCompartments; i++ )" << endl;
  o << "   {" << endl;
  o << "      if( i < NonmemPars::nCompartments - 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( 0, NonmemPars::initialOff[i] );" << endl;
  o << "      }" << endl;
  o << "      else" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( 1, NonmemPars::initialOff[i] );" << endl;
  o << "      }" << endl;
  o << "      MY_ASSERT_EQUAL( 0, NonmemPars::noOff[i] );" << endl;
  o << "      MY_ASSERT_EQUAL( 0, NonmemPars::noDose[i] );" << endl;
  o << "   }" << endl;
  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();
  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fNonmemParsDriver_cpp, fNonmemParsDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fNonmemParsDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fNonmemParsDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "A test driver, %s, failed!", fNonmemParsDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_advan6Test::testODEPredClass()
{ 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test ODEPred class to see if it has defined
  // ODEPred::evalODE(), ODEPred::evalError(), ODEPred::evalPK() properly.
  // Especially, the proper elements of the dependent variable-
  // vector given as an argument are replaced by the computed 
  // value of Y(j) and F(j).
  // Also, make sure the currently computed values, all of them,
  // are stored in memory for potential retrieval from the 
  // outside.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fODEPredDriver );
  ofstream o( fODEPredDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"OdePred.h\"" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "#include \"NonmemPars.h\"" << endl;
  o << "#include <CppAD/CppAD.h>" << endl;
  o << "#include <vector>" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;
  o << MY_ASSERT_EQUAL << endl;
  o << "using namespace std;" << endl;
  o << "int main()" << endl;
  o << "{" << endl;
  o << "   int  nIndividuals           = " << nIndividuals << ";" << endl;
  o << "   bool isPkFunctionOfT        = NonmemPars::isPkFunctionOfT;" << endl;
  o << "   int  nCompartments          = NonmemPars::nCompartments;" << endl;
  o << "   int  nParameters            = NonmemPars::nParameters;" << endl;
  o << "   int  defaultDoseComp        = NonmemPars::defaultDoseComp;" << endl;
  o << "   int  defaultObservationComp = NonmemPars::defaultObservationComp;" << endl;
  o << "   valarray<bool> initialOff   ( NonmemPars::nCompartments );" << endl;
  o << "   valarray<bool> noOff        ( NonmemPars::nCompartments );" << endl;
  o << "   valarray<bool> noDose       ( NonmemPars::nCompartments );" << endl;
  o << "   double tolRel               = NonmemPars::relTol;" << endl;
  o << "   DataSet< CppAD::AD<double> > set;" << endl;
  o << "   OdePred< CppAD::AD<double> > odePred( &set," << endl; 
  o << "                                nIndividuals," << endl;
  o << "                                isPkFunctionOfT," << endl;
  o << "                                nCompartments," << endl;
  o << "                                nParameters," << endl;
  o << "                                defaultDoseComp," << endl;
  o << "                                defaultObservationComp," << endl;
  o << "                                initialOff," << endl;
  o << "                                noOff," << endl;
  o << "                                noDose," << endl;
  o << "                                tolRel );" << endl;
  o << "   valarray<int> N            = set.getN();" << endl;
  for( int i=0; i<nIndividuals; i++ )
    {
      o << "      MY_ASSERT_EQUAL( " << N[i]        << ", N[" << i << "] );" << endl;
      o << "      MY_ASSERT_EQUAL( " << N[i]        << ", odePred.getNObservs(" << i << ") );" << endl;
      o << "      MY_ASSERT_EQUAL( " << NRecords[i] << ", odePred.getNRecords(" << i << ") );" << endl;
    }
  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fODEPredDriver_cpp, fODEPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "Compilation of the generated %s failed!", fODEPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fODEPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "A test driver, %s, failed!", fODEPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_advan6Test::testDriver()
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
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fFitDriver );
      
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
void pop_advan6Test::testReportML()
{
  const double tol = 0.01;

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

  // NONMEM objective and SPK objective never match.

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
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_theta[i], theta_out[i], tol );
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
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], tol );
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
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  se_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "se[%d] = %f\n", i, se_val[i] );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_stderr[i], se_val[i], tol );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the covariance of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cov_val;
  vector<double> inv_cov_val;
  int covLen = series(1,1,omegaOrder+thetaLen);
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

	//printf( "cov[%d] = %f\n", i, cov_val[i] );

	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cov[i], cov_val[i], tol );
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

      // Because the inverse covariance elements are all greater than
      // 1 but of varying size, set the tolerance to be 1/1000 of
      // the first known diagonal value.
      double inv_cov_tol = fabs( nm_invCov[0] ) / 1000.0;

      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  inv_cov_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "inv_cov[%d] = %f\n", i, inv_cov_val[i] );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_invCov[i], inv_cov_val[i], inv_cov_tol );
      }
    }


  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the correlation matrix for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> corr_val;
  DOMNodeList * corr_list =ind_stat_result->getElementsByTagName(  XML.X_POP_CORRELATION_OUT ) ;
  if( corr_list->getLength() == 1 )
    {
      DOMElement * corr = dynamic_cast<DOMElement*>( corr_list->item(0) );
      CPPUNIT_ASSERT( corr != NULL );
      DOMNodeList * value_list = corr->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      corr_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  corr_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "cor[%d] = %f\n", i, cor_val[i] );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_corr[i], corr_val[i], tol );
      }
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( XML.X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * pop_advan6Test::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_advan6Test" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testIndDataClass", 
	 &pop_advan6Test::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testDataSetClass", 
	 &pop_advan6Test::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testNonmemPars", 
	 &pop_advan6Test::testNonmemPars ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testODEPredClass", 
	 &pop_advan6Test::testODEPredClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testDriver", 
	 &pop_advan6Test::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_advan6Test>(
         "testReportML", 
	 &pop_advan6Test::testReportML ) );

  return suiteOfTests;
}

