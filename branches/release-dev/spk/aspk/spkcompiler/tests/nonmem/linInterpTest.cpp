#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "linInterpTest.h"
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
   NONMEM Control File
===================================================================================
$DATA Cadralazine.dat
$INPUT ID TIME DV AMT=DOSE
$SUBROUTINES ADVAN6 TOL=4
$MODEL NCOMPARTMENTS=1 NEQUILIBRIUM=0 NPARAMETERS=0
COMP=(CENTRAL)
$PK
CL = THETA(1)*EXP(ETA(1))
V = THETA(2)
K = CL/V
S1 = V
$THETA
(0.3,3,30)
(0.5,5,50.0)
$OMEGA DIAGONAL(1)
0.09
$DES
DADT(1)=-K*A(1)
Z = LININTERP( DV )
$ERROR
Y = F*(1 + EPS(1))
$SIGMA DIAGONAL(1)
0.03
$ESTIMATION METHOD=1 INTERACTION SIGDIGITS=3 MAXEVALS=450 PRINT=5
$COVARIANCE
====================================================================================
*/
/*
====================================================================================
   NONMEM DATA file
====================================================================================
1,0,0,30
1,2.00E+00,1.09E+00,0
1,4.00E+00,7.50E-01,0
1,6.00E+00,5.30E-01,0
1,8.00E+00,3.40E-01,0
1,1.00E+01,2.30E-01,0
1,2.40E+01,2.00E-02,0
2,0,0,30
2,2.00E+00,2.03E+00,0
2,4.00E+00,1.28E+00,0
2,6.00E+00,1.20E+00,0
2,8.00E+00,1.02E+00,0
2,1.00E+01,8.30E-01,0
2,2.40E+01,2.80E-01,0
3,0,0,30
3,2.00E+00,1.44E+00,0
3,4.00E+00,1.30E+00,0
3,6.00E+00,9.50E-01,0
3,8.00E+00,6.80E-01,0
3,1.00E+01,5.20E-01,0
3,2.40E+01,6.00E-02,0
4,0,0,30
4,2.00E+00,1.55E+00,0
4,4.00E+00,9.60E-01,0
4,6.00E+00,8.00E-01,0
4,8.00E+00,6.20E-01,0
4,1.00E+01,4.60E-01,0
4,2.40E+01,8.00E-02,0
5,0,0,30
5,2.00E+00,1.35E+00,0
5,4.00E+00,7.80E-01,0
5,6.00E+00,5.00E-01,0
5,8.00E+00,3.30E-01,0
5,1.00E+01,1.80E-01,0
5,2.40E+01,2.00E-02,0
6,0,0,30
6,2.00E+00,1.08E+00,0
6,4.00E+00,5.90E-01,0
6,6.00E+00,3.70E-01,0
6,8.00E+00,2.30E-01,0
6,1.00E+01,1.70E-01,0
6,2.40E+01,0.00E+00,0
7,0,0,30
7,2.00E+00,1.32E+00,0
7,4.00E+00,7.40E-01,0
7,6.00E+00,4.60E-01,0
7,8.00E+00,2.80E-01,0
7,1.00E+01,2.70E-01,0
7,2.40E+01,3.00E-02,0
7,2.80E+01,2.00E-02,0
7,3.20E+01,0.00E+00,0
8,0,0,30
8,2.00E+00,1.63E+00,0
8,4.00E+00,1.01E+00,0
8,6.00E+00,7.30E-01,0
8,8.00E+00,5.50E-01,0
8,1.00E+01,4.10E-01,0
8,2.40E+01,1.00E-02,0
8,2.80E+01,6.00E-02,0
8,3.20E+01,2.00E-02,0
9,0,0,30
9,2.00E+00,1.26E+00,0
9,4.00E+00,7.30E-01,0
9,6.00E+00,4.00E-01,0
9,8.00E+00,3.00E-01,0
9,1.00E+01,2.10E-01,0
9,2.40E+01,0.00E+00,0
10,0,0,30
10,2.00E+00,1.30E+00,0
10,4.00E+00,7.00E-01,0
10,6.00E+00,4.00E-01,0
10,8.00E+00,2.50E-01,0
10,1.00E+01,1.40E-01,0
10,2.40E+01,0.00E+00,0

====================================================================================
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
  char CPPFLAG[]    = "-g -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD";
#else
  char CPPFLAG[]    = "-O3 -Dspk_release -DNDEBUG -I./ -I../ -I../../spkcompiler -I/usr/local/include/spktest -I/usr/local/include/spktest/CppAD";
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
  const char *label[]       = { strID, strTIME, strDV, strDOSE };
  const int    nLabels      = 4;
  const int    nIndividuals = 10;
  const int    nRecords     = 7+7+7+7+7+7+9+9+7+7;
  const int    nFixed       = 0;
  const int    nMDV         = 10;
  const int    nItems       = nLabels;
  const int    c_N[]        = { 6, 6, 6, 6, 6, 6, 8, 8, 6, 6 };
  valarray<int> N( c_N, nIndividuals );
  const int    c_NRecords[] = { 7, 7, 7, 7, 7, 7, 9, 9, 7, 7 };
  valarray<int> NRecords( c_NRecords, nIndividuals );
  const double record0 [] = { 1,  0,        0,        30,  1.0 };
  const double record1 [] = { 1,  2.00E+00, 1.09E+00,  0,  1.1 };
  const double record2 [] = { 1,  4.00E+00, 7.50E-01,  0,  1.2 };
  const double record3 [] = { 1,  6.00E+00, 5.30E-01,  0,  1.3 };
  const double record4 [] = { 1,  8.00E+00, 3.40E-01,  0,  1.4 };
  const double record5 [] = { 1,  1.00E+01, 2.30E-01,  0,  1.5 };
  const double record6 [] = { 1,  2.40E+01, 2.00E-02,  0,  1.6 };
  const double record7 [] = { 2,  0,        0,        30,  2.0 };
  const double record8 [] = { 2,  2.00E+00, 2.03E+00,  0,  2.1 };
  const double record9 [] = { 2,  4.00E+00, 1.28E+00,  0,  2.2 };
  const double record10[] = { 2,  6.00E+00, 1.20E+00,  0,  2.3 };
  const double record11[] = { 2,  8.00E+00, 1.02E+00,  0,  2.4 };
  const double record12[] = { 2,  1.00E+01, 8.30E-01,  0,  2.5 };
  const double record13[] = { 2,  2.40E+01, 2.80E-01,  0,  2.6 };
  const double record14[] = { 3,  0,        0,        30,  3.0 };
  const double record15[] = { 3,  2.00E+00, 1.44E+00,  0,  3.1 };
  const double record16[] = { 3,  4.00E+00, 1.30E+00,  0,  3.2 };
  const double record17[] = { 3,  6.00E+00, 9.50E-01,  0,  3.3 };
  const double record18[] = { 3,  8.00E+00, 6.80E-01,  0,  3.4 };
  const double record19[] = { 3,  1.00E+01, 5.20E-01,  0,  3.5 };
  const double record20[] = { 3,  2.40E+01, 6.00E-02,  0,  3.6 };
  const double record21[] = { 4,  0,        0,        30,  4.0 };
  const double record22[] = { 4,  2.00E+00, 1.55E+00,  0,  4.1 };
  const double record23[] = { 4,  4.00E+00, 9.60E-01,  0,  4.2 };
  const double record24[] = { 4,  6.00E+00, 8.00E-01,  0,  4.3 };
  const double record25[] = { 4,  8.00E+00, 6.20E-01,  0,  4.4 };
  const double record26[] = { 4,  1.00E+01, 4.60E-01,  0,  4.5 };
  const double record27[] = { 4,  2.40E+01, 8.00E-02,  0,  4.6 };
  const double record28[] = { 5,  0,        0,        30,  5.0 };
  const double record29[] = { 5,  2.00E+00, 1.35E+00,  0,  5.1 };
  const double record30[] = { 5,  4.00E+00, 7.80E-01,  0,  5.2 };
  const double record31[] = { 5,  6.00E+00, 5.00E-01,  0,  5.3 };
  const double record32[] = { 5,  8.00E+00, 3.30E-01,  0,  5.4 };
  const double record33[] = { 5,  1.00E+01, 1.80E-01,  0,  5.5 };
  const double record34[] = { 5,  2.40E+01, 2.00E-02,  0,  5.6 };
  const double record35[] = { 6,  0,        0,        30,  6.0 };
  const double record36[] = { 6,  2.00E+00, 1.08E+00,  0,  6.1 };
  const double record37[] = { 6,  4.00E+00, 5.90E-01,  0,  6.1 };
  const double record38[] = { 6,  6.00E+00, 3.70E-01,  0,  6.2 };
  const double record39[] = { 6,  8.00E+00, 2.30E-01,  0,  6.3 };
  const double record40[] = { 6,  1.00E+01, 1.70E-01,  0,  6.4 };
  const double record41[] = { 6,  2.40E+01, 0.00E+00,  0,  6.5 };
  const double record42[] = { 7,  0,        0,        30,  7.0 };
  const double record43[] = { 7,  2.00E+00, 1.32E+00,  0,  7.1 };
  const double record44[] = { 7,  4.00E+00, 7.40E-01,  0,  7.2 };
  const double record45[] = { 7,  6.00E+00, 4.60E-01,  0,  7.3 };
  const double record46[] = { 7,  8.00E+00, 2.80E-01,  0,  7.4 };
  const double record47[] = { 7,  1.00E+01, 2.70E-01,  0,  7.5 };
  const double record48[] = { 7,  2.40E+01, 3.00E-02,  0,  7.6 };
  const double record49[] = { 7,  2.80E+01, 2.00E-02,  0,  7.7 };
  const double record50[] = { 7,  3.20E+01, 0.00E+00,  0,  7.8 };
  const double record51[] = { 8,  0,        0,        30,  8.0 };
  const double record52[] = { 8,  2.00E+00, 1.63E+00,  0,  8.1 };
  const double record53[] = { 8,  4.00E+00, 1.01E+00,  0,  8.2 };
  const double record54[] = { 8,  6.00E+00, 7.30E-01,  0,  8.3 };
  const double record55[] = { 8,  8.00E+00, 5.50E-01,  0,  8.4 };
  const double record56[] = { 8,  1.00E+01, 4.10E-01,  0,  8.5 };
  const double record57[] = { 8,  2.40E+01, 1.00E-02,  0,  8.6 };
  const double record58[] = { 8,  2.80E+01, 6.00E-02,  0,  8.7 };
  const double record59[] = { 8,  3.20E+01, 2.00E-02,  0,  8.8 };
  const double record60[] = { 9,  0,        0,        30,  9.0 };
  const double record61[] = { 9,  2.00E+00, 1.26E+00,  0,  9.1 };
  const double record62[] = { 9,  4.00E+00, 7.30E-01,  0,  9.2 };
  const double record63[] = { 9,  6.00E+00, 4.00E-01,  0,  9.3 };
  const double record64[] = { 9,  8.00E+00, 3.00E-01,  0,  9.4 };
  const double record65[] = { 9,  1.00E+01, 2.10E-01,  0,  9.5 };
  const double record66[] = { 9,  2.40E+01, 0.00E+00,  0,  9.6 };
  const double record67[] = { 10, 0,        0,        30, 10.0 };
  const double record68[] = { 10, 2.00E+00, 1.30E+00  ,0, 10.1 };
  const double record69[] = { 10, 4.00E+00, 7.00E-01,  0, 10.2 };
  const double record70[] = { 10, 6.00E+00, 4.00E-01,  0, 10.3 };
  const double record71[] = { 10, 8.00E+00, 2.50E-01,  0, 10.4 };
  const double record72[] = { 10, 1.00E+01, 1.40E-01,  0, 10.5 };
  const double record73[] = { 10, 2.40E+01, 0.00E+00,  0, 10.6 };

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
  const int    thetaLen = 2;
  const double theta_in [ thetaLen ]   = {  3.0,  5.0 };
  const double theta_up [ thetaLen ]   = { 30.0, 50.0 };
  const double theta_low[ thetaLen ]   = {  0.3,  0.5 };
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
  const double omega_in [ omegaOrder ] = { 0.09 };
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
//  const char PK[] = "CL=THETA(1)*EXP(ETA(1))\nV=THETA(2)\nK=CL/V\nS1=V\nIF( AMT.EQ.0 ) THEN\nZ = LININTERP(DV)\nENDIF\n";
  const char PK[] = "CL=THETA(1)*EXP(ETA(1))\nV=THETA(2)\nK=CL/V\nS1=V\n";

  //============================================
  // $DES model
  //
  // DADT(1)=-K*A(1)
  //============================================
  const int  nComps       = 1;
  
  // user defined comps + 1 (NONMEM adds the output comp)
  const int  nonmemNComps = nComps + 1;
  const int  nEquilibrims = 0;
//  const char DIFFEQN[]    = "DADT(1) = -K*A(1)\nIF( AMT.EQ.0 ) THEN\nZ = LININTERP(DV)\nENDIF\n";
  const char DIFFEQN[]    = "DADT(1) = -K*A(1)\n";

  //============================================
  // $ERROR model
  //
  // Y = F*(1 + EPS(1) )
  //============================================
//  const char ERROR[] = "Y = F*(1+EPS(1))\n";
  const char ERROR[] = "Y = F*(1+EPS(1))\nIF( AMT.EQ.0 ) THEN\nZ = LININTERP(DV)\nENDIF\n";

  //============================================
  // Results
  //============================================
  double nm_theta    [] = { 3.07, 18.37 };
  double nm_omega    [] = { 1.16E-01 };
  double nm_sigma    [] = { 1.23E-01 };

  double nm_stderr   [] = { 4.24E-01, 1.08E+00, 5.20E-02, 3.33E-02 };
  double nm_cov      [] = { 1.79E-01, 
                            2.94E-01, 1.17E+00,
		  	   -1.25E-02,-3.28E-02, 2.70E-03,
                            9.69E-03, 1.81E-02,-2.48E-04, 1.11E-03 }; 
  double nm_corr     [] = { 1.00E+00,
                            6.42E-01, 1.00E-00,
		  	   -5.55E-01,-5.83E-01, 1.00E+00,
                            6.87E-01, 5.01E-01,-1.43E-01, 1.00E+00 };
  double nm_invCov   [] = { 1.86E+01,
                           -8.13E-01, 1.81E+00,
                            6.36E+01, 1.65E+01, 8.04E+02,
		  	   -1.35E+02,-1.87E+01,-6.44E+02, 2.24E+03 };
};
void linInterpTest::setUp()
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
  record[71]   = record71;
  record[72]   = record72;
  record[73]   = record73;

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
void linInterpTest::parse()
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

  // Check variables that are required specifically by LININTERP
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
void linInterpTest::tearDown()
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
void linInterpTest::createDataML()
{
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
    ifstream junk( fDataML );
    //ifstream junk( "linInterpTest.data.xml" );
     CPPUNIT_ASSERT_MESSAGE( "Failed to open a data xml.", junk.good() );
    junk.close();
    //dataParser->parse( "linInterpTest.data.xml" );
    dataParser->parse( fDataML );
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
 
void linInterpTest::createSourceML()
{
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
    ifstream junk( fSourceML );
    //ifstream junk( "linInterpTest.source.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open CAD1996A1.source.xml", junk.good() );
    junk.close();
    //sourceParser->parse( "linInterpTest.source.xml" );
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
void linInterpTest::testDriver()
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
void linInterpTest::testReportML()
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
      for( int i=0; i<n; i++ )
      {
	DOMElement * value =  dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh * x_val = value->getFirstChild()->getNodeValue();
	if( x_val != NULL )
	  inv_cov_val[i] = atof( XMLString::transcode( x_val ) );
	//printf( "inv_cov[%d] = %f\n", i, inv_cov_val[i] );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( nm_cov[i], inv_cov_val[i], tol );
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

CppUnit::Test * linInterpTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "linInterpTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<linInterpTest>(
         "testDriver", 
	 &linInterpTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<linInterpTest>(
         "testReportML", 
	 &linInterpTest::testReportML ) );

  return suiteOfTests;
}

