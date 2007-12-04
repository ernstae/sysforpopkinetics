#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_diffeqnTest.h"
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

namespace{ 
  const unsigned int MAXCHARS = 256;

  const char * testName;
  char fSavedReportML[]   = "saved_result.xml";
  char fTraceOut[]        = "trace_output";
  char fFitDriver[]       = "driver";
  char fReportML[]        = "result.xml";

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
  char SPKOPTLIB[]  = "spkopt";
  char ATLASLIB[]   = "atlas_lapack";
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

  char LDPATH[]     = "../../spkcompiler/libcommon.a ../../spkcompiler/nonmem/libnonmem.a -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest";
#ifndef SPK_RELEASE
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest";
#else
  char CPPFLAG[]    = "-O3 -Dspk_release -DNDEBUG -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest";
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
  //      ID  DOSE=AMT  TIME  CP=DV   WT
  //============================================
  map<const char*, const char*> label_alias;
  const char *strID         = "ID";
  const char *strDOSE       = "DOSE";
  const char *strAMT        = "AMT";
  const char *strTIME       = "TIME";
  const char *strCP         = "CP";
  const char *strDV         = "DV";
  const char *strWT         = "WT";
  const char *strMDV        = "MDV";
  const char *label[]       = { strID, strDOSE, strTIME, strCP, strMDV };
  const int    nLabels      = 5;
  const int    nIndividuals = 12;
  const int    nRecords     = 12 * nIndividuals;
  const int    nFixed       = 0;
  const int    nItems       = nLabels;
  valarray<int> N( nIndividuals );
  const double record0  [] = {  1, 4.02,  0.00,  0.00, 79.60 };
  const double record1  [] = {  1, 0.00,  0.00,  0.74,  0.00 };
  const double record2  [] = {  1, 0.00,  0.25,  2.84,  0.00 };
  const double record3  [] = {  1, 0.00,  0.57,  6.57,  0.00 };
  const double record4  [] = {  1, 0.00,  1.12, 10.50,  0.00 };
  const double record5  [] = {  1, 0.00,  2.02,  9.66,  0.00 };
  const double record6  [] = {  1, 0.00,  3.82,  8.58,  0.00 };
  const double record7  [] = {  1, 0.00,  5.10,  8.36,  0.00 };
  const double record8  [] = {  1, 0.00,  7.03,  7.47,  0.00 };
  const double record9  [] = {  1, 0.00,  9.05,  6.89,  0.00 };
  const double record10 [] = {  1, 0.00, 12.12,  5.94,  0.00 };
  const double record11 [] = {  1, 0.00, 24.37,  3.28,  0.00 };
  const double record12 [] = {  2, 4.40,  0.00,  0.00, 72.40 };
  const double record13 [] = {  2, 0.00,  0.00,  0.00,  0.00 };
  const double record14 [] = {  2, 0.00,  0.27,  1.72,  0.00 };
  const double record15 [] = {  2, 0.00,  0.52,  7.91,  0.00 };
  const double record16 [] = {  2, 0.00,  1.00,  8.31,  0.00 };
  const double record17 [] = {  2, 0.00,  1.92,  8.33,  0.00 };
  const double record18 [] = {  2, 0.00,  3.50,  6.85,  0.00 };
  const double record19 [] = {  2, 0.00,  5.02,  6.08,  0.00 };
  const double record20 [] = {  2, 0.00,  7.03,  5.40,  0.00 };
  const double record21 [] = {  2, 0.00,  9.00,  4.55,  0.00 };
  const double record22 [] = {  2, 0.00, 12.00,  3.01,  0.00 };
  const double record23 [] = {  2, 0.00, 24.30,  0.90,  0.00 };
  const double record24 [] = {  3, 4.53,  0.00,  0.00, 70.50 };
  const double record25 [] = {  3, 0.00,  0.00,  0.00,  0.00 };
  const double record26 [] = {  3, 0.00,  0.27,  4.40,  0.00 };
  const double record27 [] = {  3, 0.00,  0.58,  6.90,  0.00 };
  const double record28 [] = {  3, 0.00,  1.02,  8.20,  0.00 };
  const double record29 [] = {  3, 0.00,  2.02,  7.80,  0.00 };
  const double record30 [] = {  3, 0.00,  3.62,  7.50,  0.00 };
  const double record31 [] = {  3, 0.00,  5.08,  6.20,  0.00 };
  const double record32 [] = {  3, 0.00,  7.07,  5.30,  0.00 };
  const double record33 [] = {  3, 0.00,  9.00,  4.90,  0.00 };
  const double record34 [] = {  3, 0.00, 12.15,  3.70,  0.00 };
  const double record35 [] = {  3, 0.00, 24.17,  1.05,  0.00 };
  const double record36 [] = {  4, 4.40,  0.00,  0.00, 72.70 };
  const double record37 [] = {  4, 0.00,  0.00,  0.00,  0.00 };
  const double record38 [] = {  4, 0.00,  0.35,  1.89,  0.00 };
  const double record39 [] = {  4, 0.00,  0.60,  4.60,  0.00 };
  const double record40 [] = {  4, 0.00,  1.07,  8.60,  0.00 };
  const double record41 [] = {  4, 0.00,  2.13,  8.38,  0.00 };
  const double record42 [] = {  4, 0.00,  3.50,  7.54,  0.00 };
  const double record43 [] = {  4, 0.00,  5.02,  6.88,  0.00 };
  const double record44 [] = {  4, 0.00,  7.02,  5.78,  0.00 };
  const double record45 [] = {  4, 0.00,  9.02,  5.33,  0.00 };
  const double record46 [] = {  4, 0.00, 11.98,  4.19,  0.00 };
  const double record47 [] = {  4, 0.00, 24.65,  1.15,  0.00 };
  const double record48 [] = {  5, 5.86,  0.00,  0.00, 54.60 };
  const double record49 [] = {  5, 0.00,  0.00,  0.00,  0.00 };
  const double record50 [] = {  5, 0.00,  0.30,  2.02,  0.00 };
  const double record51 [] = {  5, 0.00,  0.52,  5.63,  0.00 };
  const double record52 [] = {  5, 0.00,  1.00, 11.40,  0.00 };
  const double record53 [] = {  5, 0.00,  2.02,  9.33,  0.00 };
  const double record54 [] = {  5, 0.00,  3.50,  8.74,  0.00 };
  const double record55 [] = {  5, 0.00,  5.02,  7.56,  0.00 };
  const double record56 [] = {  5, 0.00,  7.02,  7.09,  0.00 };
  const double record57 [] = {  5, 0.00,  9.10,  5.90,  0.00 };
  const double record58 [] = {  5, 0.00, 12.00,  4.37,  0.00 };
  const double record59 [] = {  5, 0.00, 24.35,  1.57,  0.00 };
  const double record60 [] = {  6, 4.00,  0.00,  0.00, 80.00 };
  const double record61 [] = {  6, 0.00,  0.00,  0.00,  0.00 };
  const double record62 [] = {  6, 0.00,  0.27,  1.29,  0.00 };
  const double record63 [] = {  6, 0.00,  0.58,  3.08,  0.00 };
  const double record64 [] = {  6, 0.00,  1.15,  6.44,  0.00 };
  const double record65 [] = {  6, 0.00,  2.03,  6.32,  0.00 };
  const double record66 [] = {  6, 0.00,  3.57,  5.53,  0.00 };
  const double record67 [] = {  6, 0.00,  5.00,  4.94,  0.00 };
  const double record68 [] = {  6, 0.00,  7.00,  4.02,  0.00 };
  const double record69 [] = {  6, 0.00,  9.22,  3.46,  0.00 };
  const double record70 [] = {  6, 0.00, 12.10,  2.78,  0.00 };
  const double record71 [] = {  6, 0.00, 23.85,  0.92,  0.00 };
  const double record72 [] = {  7, 4.95,  0.00,  0.00, 64.60 };
  const double record73 [] = {  7, 0.00,  0.00,  0.15,  0.00 };
  const double record74 [] = {  7, 0.00,  0.25,  0.85,  0.00 };
  const double record75 [] = {  7, 0.00,  0.50,  2.35,  0.00 };
  const double record76 [] = {  7, 0.00,  1.02,  5.02,  0.00 };
  const double record77 [] = {  7, 0.00,  2.02,  6.58,  0.00 };
  const double record78 [] = {  7, 0.00,  3.48,  7.09,  0.00 };
  const double record79 [] = {  7, 0.00,  5.00,  6.66,  0.00 };
  const double record80 [] = {  7, 0.00,  6.98,  5.25,  0.00 };
  const double record81 [] = {  7, 0.00,  9.00,  4.39,  0.00 };
  const double record82 [] = {  7, 0.00, 12.05,  3.53,  0.00 };
  const double record83 [] = {  7, 0.00, 24.22,  1.15,  0.00 };
  const double record84 [] = {  8, 4.53,  0.00,  0.00, 70.50 };
  const double record85 [] = {  8, 0.00,  0.00,  0.00,  0.00 };
  const double record86 [] = {  8, 0.00,  0.25,  3.05,  0.00 };
  const double record87 [] = {  8, 0.00,  0.52,  3.05,  0.00 };
  const double record88 [] = {  8, 0.00,  0.98,  7.31,  0.00 };
  const double record89 [] = {  8, 0.00,  2.02,  7.56,  0.00 };
  const double record90 [] = {  8, 0.00,  3.53,  6.59,  0.00 };
  const double record91 [] = {  8, 0.00,  5.05,  5.88,  0.00 };
  const double record92 [] = {  8, 0.00,  7.15,  4.73,  0.00 };
  const double record93 [] = {  8, 0.00,  9.07,  4.57,  0.00 };
  const double record94 [] = {  8, 0.00, 12.10,  3.00,  0.00 };
  const double record95 [] = {  8, 0.00, 24.12,  1.25,  0.00 };
  const double record96 [] = {  9, 3.10,  0.00,  0.00, 86.40 };
  const double record97 [] = {  9, 0.00,  0.00,  0.00,  0.00 };
  const double record98 [] = {  9, 0.00,  0.30,  7.37,  0.00 };
  const double record99 [] = {  9, 0.00,  0.63,  9.03,  0.00 };
  const double record100[] = {  9, 0.00,  1.05,  7.14,  0.00 };
  const double record101[] = {  9, 0.00,  2.02,  6.33,  0.00 };
  const double record102[] = {  9, 0.00,  3.53,  5.66,  0.00 };
  const double record103[] = {  9, 0.00,  5.02,  5.67,  0.00 };
  const double record104[] = {  9, 0.00,  7.17,  4.24,  0.00 };
  const double record105[] = {  9, 0.00,  8.80,  4.11,  0.00 };
  const double record106[] = {  9, 0.00, 11.60,  3.16,  0.00 };
  const double record107[] = {  9, 0.00, 24.43,  1.12,  0.00 };
  const double record108[] = { 10, 5.50,  0.00,  0.00, 58.20 };
  const double record109[] = { 10, 0.00,  0.00,  0.24,  0.00 };
  const double record110[] = { 10, 0.00,  0.37,  2.89,  0.00 };
  const double record111[] = { 10, 0.00,  0.77,  5.22,  0.00 };
  const double record112[] = { 10, 0.00,  1.02,  6.41,  0.00 };
  const double record113[] = { 10, 0.00,  2.05,  7.83,  0.00 };
  const double record114[] = { 10, 0.00,  3.55, 10.21,  0.00 };
  const double record115[] = { 10, 0.00,  5.05,  9.18,  0.00 };
  const double record116[] = { 10, 0.00,  7.08,  8.02,  0.00 };
  const double record117[] = { 10, 0.00,  9.38,  7.14,  0.00 };
  const double record118[] = { 10, 0.00, 12.10,  5.68,  0.00 };
  const double record119[] = { 10, 0.00, 23.70,  2.42,  0.00 };
  const double record120[] = { 11, 4.92,  0.00,  0.00, 65.00 };
  const double record121[] = { 11, 0.00,  0.00,  0.00,  0.00 };
  const double record122[] = { 11, 0.00,  0.25,  4.86,  0.00 };
  const double record123[] = { 11, 0.00,  0.50,  7.24,  0.00 };
  const double record124[] = { 11, 0.00,  0.98,  8.00,  0.00 };
  const double record125[] = { 11, 0.00,  1.98,  6.81,  0.00 };
  const double record126[] = { 11, 0.00,  3.60,  5.87,  0.00 };
  const double record127[] = { 11, 0.00,  5.02,  5.22,  0.00 };
  const double record128[] = { 11, 0.00,  7.03,  4.45,  0.00 };
  const double record129[] = { 11, 0.00,  9.03,  3.62,  0.00 };
  const double record130[] = { 11, 0.00, 12.12,  2.69,  0.00 };
  const double record131[] = { 11, 0.00, 24.08,  0.86,  0.00 };
  const double record132[] = { 12, 5.30,  0.00,  0.00, 60.50 };
  const double record133[] = { 12, 0.00,  0.00,  0.00,  0.00 };
  const double record134[] = { 12, 0.00,  0.25,  1.25,  0.00 };
  const double record135[] = { 12, 0.00,  0.50,  3.96,  0.00 };
  const double record136[] = { 12, 0.00,  1.00,  7.82,  0.00 };
  const double record137[] = { 12, 0.00,  2.00,  9.72,  0.00 };
  const double record138[] = { 12, 0.00,  3.52,  9.75,  0.00 };
  const double record139[] = { 12, 0.00,  5.07,  8.57,  0.00 };
  const double record140[] = { 12, 0.00,  7.07,  6.59,  0.00 };
  const double record141[] = { 12, 0.00,  9.03,  6.11,  0.00 };
  const double record142[] = { 12, 0.00, 12.05,  4.57,  0.00 };
  const double record143[] = { 12, 0.00, 24.15,  1.17,  0.00 };
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
  const char *strDADT     = "DADT";
  const char *strP        = "P";
  const char *strA        = "A";

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

  const double p_eta_res [ etaLen * nIndividuals ] = { 1.1, 1.2,   /* for the 1st patient */
			                               2.1, 2.2,   /* 2nd patient */
                                                       3.1, 3.2,   /* 3rd patient */
                                                       4.1, 4.2 }; /* 4th patient */

  const double p_eta_wres[ etaLen * nIndividuals ] = { 1.1, 1.2,
                                                       2.1, 2.2,
                                                       3.1, 3.2,
                                                       4.1, 4.2 };

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
  // $MODEL - compartmental model definition
  //============================================
  // This definition is not based upon text.

  //============================================
  // $PK model
  //
  // KA = THETA(1) + ETA(1)
  // KE = THETA(2) + ETA(2)
  // CL = THETA(3) * WT + ETA(3)
  // S2 = CL / KE / WT
  //============================================
  const int nPkParams = 3;
  const char PK[] = "KA=THETA(1)+ETA(1)\nKE=THETA(2)+ETA(2)\nCL=THETA(3)*WT+ETA(3)\nS2=CL/KE/WT\n";

  //============================================
  // $DES model
  //
  // DADT(1) = -KA * A(1)
  // DADT(2) =  KA * A(1) - KE * A(2)
  //============================================
  const int nComps = 2;
  const char DIFFEQN[] = "DADT(1) = -KA*A(1)\nDADT(2) = KA*A(1) - KE*A(2)\n";

  //============================================
  // $ERROR model
  // 
  // Y = F + EPS(1)
  //============================================
  const char ERROR[] = "Y = F + EPS(1)\n";
};
void pop_diffeqnTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,               testName );
  snprintf( fMonteParsDriver,      "%s_MonteParsDriver",      fPrefix );
  snprintf( fMonteParsDriver_cpp,  "%s_MonteParsDriver.cpp",  fPrefix );
  snprintf( fNonmemParsDriver,     "%s_NonmemParsDriver",     fPrefix );
  snprintf( fNonmemParsDriver_cpp, "%s_NonmemParsDriver.cpp", fPrefix );
  snprintf( fIndDataDriver,        "%s_IndDataDriver",        fPrefix );
  snprintf( fIndDataDriver_cpp,    "%s_IndDataDriver.cpp",    fPrefix );
  snprintf( fDataML,               "%s_dataML.xml",           fPrefix );
  snprintf( fSourceML,             "%s_sourceML.xml",         fPrefix );
  snprintf( fDataSetDriver,        "%s_DataSetDriver",        fPrefix );
  snprintf( fDataSetDriver_cpp,    "%s_DataSetDriver.cpp",    fPrefix );
  snprintf( fODEPredDriver,        "%s_ODEPredDriver",        fPrefix );
  snprintf( fODEPredDriver_cpp,    "%s_ODEPredDriver.cpp",    fPrefix );

  X_ERROR_LIST                 = XMLString::transcode( C_ERROR_LIST );
  X_VALUE                      = XMLString::transcode( C_VALUE );
  X_POP_OBJ_OUT                = XMLString::transcode( C_POP_OBJ_OUT );
  X_THETA_OUT                  = XMLString::transcode( C_THETA_OUT );
  X_OMEGA_OUT                  = XMLString::transcode( C_OMEGA_OUT );
  X_POP_ANALYSIS_RESULT        = XMLString::transcode( C_POP_ANALYSIS_RESULT );
  X_POP_STDERROR_OUT           = XMLString::transcode( C_POP_STDERROR_OUT );
  X_POP_COVARIANCE_OUT         = XMLString::transcode( C_POP_COVARIANCE_OUT );
  X_POP_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_POP_INVERSE_COVARIANCE_OUT );
  X_POP_CONFIDENCE_OUT         = XMLString::transcode( C_POP_CONFIDENCE_OUT );
  X_POP_COEFFICIENT_OUT        = XMLString::transcode( C_POP_COEFFICIENT_OUT );
  X_POP_CORRELATION_OUT        = XMLString::transcode( C_POP_CORRELATION_OUT );
  X_PRESENTATION_DATA          = XMLString::transcode( C_PRESENTATION_DATA );

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
  N = 12;

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
  record[71]   = record71;
  record[72]   = record72;
  record[73]   = record73;
  record[74]   = record74;
  record[75]   = record75;
  record[76]   = record76;
  record[77]   = record77;
  record[78]   = record78;
  record[79]   = record79;
  record[80]   = record80;
  record[81]   = record81;
  record[82]   = record82;
  record[83]   = record83;
  record[84]   = record84;
  record[85]   = record85;
  record[86]   = record86;
  record[87]   = record87;
  record[88]   = record88;
  record[89]   = record89;
  record[90]   = record90;
  record[91]   = record91;
  record[92]   = record92;
  record[93]   = record93;
  record[94]   = record94;
  record[95]   = record95;
  record[96]   = record96;
  record[97]   = record97;
  record[98]   = record98;
  record[99]   = record99;
  record[100]   = record100;
  record[101]   = record101;
  record[102]   = record102;
  record[103]   = record103;
  record[104]   = record104;
  record[105]   = record105;
  record[106]   = record106;
  record[107]   = record107;
  record[108]   = record108;
  record[109]   = record109;
  record[110]   = record110;
  record[110]   = record110;
  record[111]   = record111;
  record[112]   = record112;
  record[113]   = record113;
  record[114]   = record114;
  record[115]   = record115;
  record[116]   = record116;
  record[117]   = record117;
  record[118]   = record118;
  record[119]   = record119;
  record[120]   = record120;
  record[121]   = record121;
  record[122]   = record122;
  record[123]   = record123;
  record[124]   = record124;
  record[125]   = record125;
  record[126]   = record126;
  record[127]   = record127;
  record[128]   = record128;
  record[129]   = record129;
  record[130]   = record130;
  record[131]   = record131;
  record[132]   = record132;
  record[133]   = record133;
  record[134]   = record134;
  record[135]   = record135;
  record[136]   = record136;
  record[137]   = record137;
  record[138]   = record138;
  record[139]   = record139;
  record[140]   = record140;
  record[141]   = record141;
  record[142]   = record142;
  record[143]   = record143;

  createDataML();
  createSourceML();
  try{
    parse();
  }
  catch( SpkCompilerException & e )
    {
      cerr << e << endl;
    }
  catch( ... )
    {
      cerr << "Parse failure!" << endl;
    }
}
void pop_diffeqnTest::tearDown()
{
  XMLString::release( &X_ERROR_LIST );
  XMLString::release( &X_VALUE );
  XMLString::release( &X_POP_OBJ_OUT );
  XMLString::release( &X_THETA_OUT );
  XMLString::release( &X_OMEGA_OUT );
  XMLString::release( &X_POP_ANALYSIS_RESULT );
  XMLString::release( &X_POP_STDERROR_OUT );
  XMLString::release( &X_POP_COVARIANCE_OUT );
  XMLString::release( &X_POP_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_POP_CONFIDENCE_OUT );
  XMLString::release( &X_POP_COEFFICIENT_OUT );
  XMLString::release( &X_POP_CORRELATION_OUT );
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
      remove( fODEPredDriver );
      remove( fODEPredDriver_cpp );
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
}
//******************************************************************************
//
// Test a problem that takes a data set with the ID field filled in.
//
//******************************************************************************
void pop_diffeqnTest::createDataML()
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
 
void pop_diffeqnTest::createSourceML()
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

  oSource << "<model advan=\"6\" tol=\"3\">" << endl;
  oSource << "<diffeqn>" << endl;
  oSource << "   " << DIFFEQN << endl;
  oSource << "</diffeqn>" << endl;
  oSource << "<pk>" << endl;
  oSource << "   " << PK << endl;
  oSource << "</pk>" << endl;
  oSource << "<error>" << endl;
  oSource << "   " << ERROR << endl;
  oSource << "</error>" << endl;
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
void pop_diffeqnTest::parse()
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
      CPPUNIT_ASSERT_MESSAGE( "Failed to translate.", false );
    }
}
void pop_diffeqnTest::testIndDataClass()
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
  // * IETARES
  // * IWETARES
  // * PPRED
  // * PWRES
  // * PRES
  // * PETARES
  // * PWETARES
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
  o << "   const int nComps = " << nComps << ";" << endl;
  o << "   const int nPkParams = " << nPkParams << ";" << endl;
  o << "   vector<char*>  a_id(n);" << endl;
  o << "   vector<double> a_time(n);" << endl;
  o << "   vector<double> a_dv(n);" << endl;
  o << "   vector<double> a_mdv(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_id  [" << i << "] = \"" << record[i][0] << "\";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_time[" << i << "] = "   << record[i][2] << ";" << endl;
    o << "   a_mdv [" << i << "] = "   << record[i][3] << ";" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_dv, a_time, a_mdv );" << endl;

  // { ID, DV=CP, TIME, MDV }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   assert( strcmp( A." << strID << "[" << i << "], \"" << record[i][0] << "\" ) == 0 );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strCP   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][1] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][2] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL(  " << record[i][3] << ", A." << strMDV  << "[" << i << "] );" << endl;
      // There have to be placeholders for the current values of theta/eta for
      // each call to ODEPred::eval().
      o << "   MY_ASSERT_EQUAL( thetaLen,  A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,    A." << strETA   << "[" << i << "].size() );" << endl;

      o << "   MY_ASSERT_EQUAL( nComps,    A." << strDADT   << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nComps,    A." << strA      << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nPkParams, A." << strP      << "[" << i << "].size() );" << endl;

      o << endl;
    }
  o << endl;  

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
  o << endl;

  o << "   valarray<double> iEtaRes   ( etaLen );" << endl;
  o << "   valarray<double> iEtaResWtd( etaLen );" << endl;
  o << "   valarray<double> pEtaRes   ( etaLen );" << endl;
  o << "   valarray<double> pEtaResWtd( etaLen );" << endl;
  for( int i=0; i<etaLen; i++ )
  {
     o << "   iEtaRes   [" << i << "] = " << i_eta_res[i] << ";" << endl;
     o << "   iEtaResWtd[" << i << "] = " << i_eta_wres[i] << ";" << endl;
     o << "   pEtaRes   [" << i << "] = " << p_eta_res[i] << ";" << endl;
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

  char command[1024];
  snprintf( command, 1024, "g++ %s -o %s %s %s", fIndDataDriver_cpp, fIndDataDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fIndDataDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, "./%s", fIndDataDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fIndDataDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
}
void pop_diffeqnTest::testDataSetClass()
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
  o << "   const int nComps = " << nComps << ";" << endl;
  o << "   const int nPkParams = " << nPkParams << ";" << endl;
  o << "   DataSet<double> set;" << endl;
  o << "   valarray<int> N = set.getN();" << endl;
  // { ID, DV=CP, TIME, MDV }
  for( int j=0, k=0; j<nIndividuals; j++ )
  {
     for( int i=0; i<N[j]; i++, k++ )
     {
       o << "   assert( strcmp( set.data[" << j << "]->" << strID << "[" << i << "], \"" << record[k][0] << "\" ) == 0 );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][1] << ", set.data[" << j << "]->" << strCP   << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][1] << ", set.data[" << j << "]->" << strDV   << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][2] << ", set.data[" << j << "]->" << strTIME << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][3] << ", set.data[" << j << "]->" << strMDV  << "[" << i << "] );" << endl;
     }
  }

  o << "for( int j=0; j<nIndividuals; j++ )" << endl;
  o << "{" << endl;
  o << "   for( int i=0; i<N[j]; i++ )" << endl;
  o << "   {" << endl;
  o << "      MY_ASSERT_EQUAL( thetaLen, set.data[j]->"  << strTHETA   << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"    << strETA     << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"    << strIETARES  << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"    << strIWETARES << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"    << strPETARES  << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( etaLen, set.data[j]->"    << strPWETARES << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nComps, set.data[j]->"    << strDADT    << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nComps, set.data[j]->"    << strA       << "[i].size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nPkParams, set.data[j]->" << strP      << "[i].size() );" << endl;
  o << "   }" << endl;
  o << "}" << endl;

  // The current values of RES/WRES/PRED and ETARES/WETARES (for pop) should be always kept in memory
  // for displaying tables/scatterplots.
  for( int j=0; j<nIndividuals; j++ )
    {
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPRES     << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPWRES    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPPRED    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPETARES  << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strPWETARES << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIRES     << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIWRES    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIPRED    << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIETARES  << ".size() );" << endl;
      o << "MY_ASSERT_EQUAL( N[" << j << "], set.data[" << j << "]->" << strIWETARES << ".size() );" << endl;
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
  o << "   set.replaceAllEtaRes ( etaRes );" << endl;
  o << "   set.replaceAllWEtaRes( etaResWtd );" << endl;

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
  o << "      if( set.data[j]->" << strMDV << "[i] != 1 )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( set.data[j]->" << strDV << "[i], y[k] );" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  o << "}" << endl;

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
  snprintf( command, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_diffeqnTest::testODEPredClass()
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

  o << "#include \"Pred.h\"" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "#include <CppAD/CppAD.h>" << endl;
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
  o << "   ODEPred< CppAD::AD<double> > odepred( &set );" << endl;
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
  o << "   double expectedF1[N.sum()];" << endl;
  o << "   double expectedY1[N.sum()];" << endl;
  o << endl;
  o << endl;

  o << "   for( int who=0, k=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<N[who]; j++, k++ )" << endl;
  o << "      {" << endl;
  o << "         const int n           = N[who];" << endl;
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
  o << "         indepVar[thetaOffset+2] = C1*j; // theta(3)" << endl;
  o << "         indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  o << "         indepVar[etaOffset  +1] = C1*j; // eta(2)" << endl;
  o << "         indepVar[epsOffset  +0] = C1*j; // eps(1)" << endl;
  o << "         indepVar[epsOffset  +1] = C1*j; // eps(2)" << endl;
  o << "         odepred.eval( thetaOffset, thetaLen," << endl;
  o << "                       etaOffset,   etaLen," << endl;
  o << "                       epsOffset,   epsLen ," << endl;
  o << "                       fOffset,     n, " << endl;
  o << "                       yOffset,     n, " << endl;
  o << "                       who,         j, " << endl;
  o << "                       indepVar,    depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "         double actualF = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  o << "         double KA = CppAD::Value( indepVar[thetaOffset+0] + indepVar[etaOffset+0] );" << endl;
  o << "         double KE = CppAD::Value( indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  o << "         expectedF1[k]  = KE*KA;" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[k], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "         double actualY = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  o << "         expectedY1[k]  = expectedF1[k] + CppAD::Value( indepVar[epsOffset+0] + indepVar[epsOffset+1] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY1[k], actualY );" << endl;
  o << "      }" << endl;
  o << "   } // End of the first complete iteration over j" << endl;

  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  o << "   for( int who=0, k=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<N[who]; j++, k++ )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][2] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][1] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[k], set.data[who]->" << strPRED << "[j] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[k], set.data[who]->" << strF << "[j] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY1[k], set.data[who]->" << strY<< "[j] );" << endl;
  o << "      }" << endl;
  o << "   }" << endl;


  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  //---------------------------------------------------------------------------------
  // Incomplete iteration over j
  //
  o << "   double expectedF2[N.sum()];" << endl;
  o << "   double expectedY2[N.sum()];" << endl;
  o << "   for( int who=1, k=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<1; j++, k++ )" << endl;
  o << "      {" << endl;
  o << "         const int n           = N[who];" << endl;
  o << "         assert( n>1 );" << endl;
  o << "         const int fOffset     = 0;" << endl;
  o << "         const int yOffset     = n;" << endl;
  o << "         vector< CppAD::AD<double> > depVar( n*2 );" << endl;
  o << "         fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  o << "         fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  o << "         indepVar[thetaOffset+0] = C2*j; // theta(0)" << endl;
  o << "         indepVar[thetaOffset+1] = C2*j; // theta(1)" << endl;
  o << "         indepVar[thetaOffset+2] = C2*j; // theta(2)" << endl;
  o << "         indepVar[etaOffset  +0] = C2*j; // eta(0)" << endl;
  o << "         indepVar[etaOffset  +1] = C2*j; // eta(1)" << endl;
  o << "         indepVar[epsOffset  +0] = C2*j; // eps(0)" << endl;
  o << "         indepVar[epsOffset  +1] = C2*j; // eps(1)" << endl;
  o << endl;
  o << "         odepred.eval( thetaOffset, thetaLen," << endl;
  o << "                    etaOffset,   etaLen," << endl;
  o << "                    epsOffset,   epsLen ," << endl;
  o << "                    fOffset,     n, " << endl;
  o << "                    yOffset,     n, " << endl;
  o << "                    who, j, " << endl;
  o << "                    indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  o << "         double actualF = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  o << "         double KA = CppAD::Value( indepVar[thetaOffset+0] + indepVar[etaOffset+0] );" << endl;
  o << "         double KE = CppAD::Value( indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  o << "         expectedF2[k]  = KE*KA;" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF2[k], actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  o << "         double actualY = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  o << "         expectedY2[k]  = expectedF2[k] + CppAD::Value( indepVar[epsOffset+0] + indepVar[epsOffset+1] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY2[k], actualY );" << endl;
  o << "      }" << endl;
  o << "   } // End of the first complete iteration over j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the most recent complete iteration.
  o << "   for( int who=0, k=0; who<nIndividuals; who++ )" << endl;
  o << "   {" << endl;
  o << "      for( int j=0; j<N[who]; j++, k++ )" << endl;
  o << "      {" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][0] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strTHETA << "[j][1] );" << endl;
  o << "         MY_ASSERT_EQUAL( C1*j, set.data[who]->" << strETA   << "[j][0] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[k], set.data[who]->" << strPRED << "[j] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedF1[k], set.data[who]->" << strF << "[j] );" << endl;
  o << "         MY_ASSERT_EQUAL( expectedY1[k], set.data[who]->" << strY << "[j] );" << endl;
  o << "      }" << endl;
  o << "   }" << endl;
  //
  //  End of an incomplete iteration over j
  //---------------------------------------------------------------------------------
  o << "   return !ok;" << endl;
  o << "}" << endl;
  o.close();

  char command[MAXCHARS+1];
  snprintf( command, "g++ %s -o %s %s %s", fODEPredDriver_cpp, fODEPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fODEPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, "./%s", fODEPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS, "A test driver, %s, failed!", fODEPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void pop_diffeqnTest::testDriver()
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
      snprintf( message, MAXCHARS, "Compilation of the generated %s failed!", fFitDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, "./%s > %s", fFitDriver, fTraceOut );

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
void pop_diffeqnTest::testReportML()
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
  
  error_list = report->getElementsByTagName( X_ERROR_LIST );
  CPPUNIT_ASSERT_EQUAL( 1, (int)error_list->getLength() );
  DOMElement* error = dynamic_cast<DOMElement*>( error_list->item(0) );
  const XMLCh* error_message = error->getFirstChild()->getNodeValue();
  CPPUNIT_ASSERT_MESSAGE( "<error_list> should have been empty.", XMLString::isAllWhiteSpace( error_message ) );
   
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the objective value.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double obj_out = 0.0;
  DOMNodeList * objOut_list = report->getElementsByTagName( X_POP_OBJ_OUT );
  if( objOut_list->getLength() > 0 )
    {
      DOMElement* objOut = dynamic_cast<DOMElement*>( objOut_list->item(0) );
      DOMNodeList* value_list = objOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( 1, n );
      obj_out = atof( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );      
      // CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_obj, obj_out, scale * nm_obj );
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the final estimate for theta
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  double theta_out[thetaLen];
  DOMNodeList * thetaOut_list = report->getElementsByTagName( X_THETA_OUT );
  if( thetaOut_list->getLength() > 0 )
    {
      DOMElement* thetaOut = dynamic_cast<DOMElement*>( thetaOut_list->item(0) );
      DOMNodeList* value_list = thetaOut->getElementsByTagName( X_VALUE );
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
  DOMNodeList * omegaOut_list = report->getElementsByTagName( X_OMEGA_OUT );
  if( omegaOut_list->getLength() > 0 )
    {
      DOMElement* omegaOut = dynamic_cast<DOMElement*>( omegaOut_list->item(0) );
      DOMNodeList* value_list = omegaOut->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( omegaOrder, n );
      for( int i=0; i<+n; i++ )
	{
	  omega_out[i] = atof( XMLString::transcode( value_list->item(i)->getFirstChild()->getNodeValue() ) );
	  //CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_omega[i], omega_out[i], scale * nm_omega[i] );
	}
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Grab a pointer to the top of "ind_stat_result" sub-tree.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  DOMNodeList *ind_analysis_result = report->getElementsByTagName( X_POP_ANALYSIS_RESULT );
  CPPUNIT_ASSERT( ind_analysis_result->getLength() == 1 );
  DOMElement *ind_stat_result = dynamic_cast<DOMElement*>( ind_analysis_result->item( 0 ) );
  CPPUNIT_ASSERT( ind_stat_result != NULL );

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the standard error of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> se_val;
  DOMNodeList * se_list = ind_stat_result->getElementsByTagName( X_POP_STDERROR_OUT );
  if( se_list->getLength() == 1 )
    {
      DOMElement * se = dynamic_cast<DOMElement*>( se_list->item(0) );
      CPPUNIT_ASSERT( se != NULL );
      DOMNodeList * value_list = se->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      se_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  se_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "se[%d] = %f\n", i, se_val[i] );
	//	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_stderr[i], se_val[i], scale * nm_stderr[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the covariance of the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cov_val;
  vector<double> inv_cov_val;
  int covLen = series(1,1,omegaOrder+thetaLen);
  DOMNodeList * cov_list =ind_stat_result->getElementsByTagName(  X_POP_COVARIANCE_OUT ) ;
  if( cov_list->getLength() == 1 )
    {
      DOMElement * cov = dynamic_cast<DOMElement*>( cov_list->item(0) );
      CPPUNIT_ASSERT( cov != NULL );
      DOMNodeList * value_list = cov->getElementsByTagName( X_VALUE );
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

//CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cov[i], cov_val[i], scale * nm_cov[i] );
      }
    }
  DOMNodeList * invcov_list =ind_stat_result->getElementsByTagName(  X_POP_INVERSE_COVARIANCE_OUT ) ;
  if( invcov_list->getLength() == 1 )
    {
      DOMElement * invcov = dynamic_cast<DOMElement*>( invcov_list->item(0) );
      CPPUNIT_ASSERT( invcov != NULL );
      DOMNodeList * value_list = invcov->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      inv_cov_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  inv_cov_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "inv_cov[%d] = %f\n", i, inv_cov_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the confidence interval for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> ci_val;
  DOMNodeList * ci_list =ind_stat_result->getElementsByTagName(  X_POP_CONFIDENCE_OUT ) ;
  if( ci_list->getLength() == 1 )
    {
      DOMElement * ci = dynamic_cast<DOMElement*>( ci_list->item(0) );
      CPPUNIT_ASSERT( ci != NULL );
      DOMNodeList * value_list = ci->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      ci_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  ci_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "ci[%d] = %f\n", i, ci_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the coefficient of variation for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cv_val;
  DOMNodeList * cv_list =ind_stat_result->getElementsByTagName(  X_POP_COEFFICIENT_OUT ) ;
  if( cv_list->getLength() == 1 )
    {
      DOMElement * cv = dynamic_cast<DOMElement*>( cv_list->item(0) );
      CPPUNIT_ASSERT( cv != NULL );
      DOMNodeList * value_list = cv->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      cv_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cv_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "cv[%d] = %f\n", i, cv_val[i] );
      }
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the correlation matrix for the final estimate of parameters
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  vector<double> cor_val;
  DOMNodeList * cor_list =ind_stat_result->getElementsByTagName(  X_POP_CORRELATION_OUT ) ;
  if( cor_list->getLength() == 1 )
    {
      DOMElement * cor = dynamic_cast<DOMElement*>( cor_list->item(0) );
      CPPUNIT_ASSERT( cor != NULL );
      DOMNodeList * value_list = cor->getElementsByTagName( X_VALUE );
      int n = value_list->getLength();
      cor_val.resize( n );
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  cor_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "cor[%d] = %f\n", i, cor_val[i] );
      }
    }

  DOMNodeList *presentation_data = report->getElementsByTagName( X_PRESENTATION_DATA );
  CPPUNIT_ASSERT( presentation_data->getLength() == 1 );

  okToClean = true;
}

CppUnit::Test * pop_diffeqnTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_diffeqnTest" );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_diffeqnTest>(
         "testIndDataClass", 
	 &pop_diffeqnTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_diffeqnTest>(
         "testDataSetClass", 
	 &pop_diffeqnTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_diffeqnTest>(
         "testODEPredClass", 
	 &pop_diffeqnTest::testODEPredClass ) );
  /*
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_diffeqnTest>(
         "testDriver", 
	 &pop_diffeqnTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_diffeqnTest>(
         "testReportML", 
	 &pop_diffeqnTest::testReportML ) );
  */
  return suiteOfTests;
}

