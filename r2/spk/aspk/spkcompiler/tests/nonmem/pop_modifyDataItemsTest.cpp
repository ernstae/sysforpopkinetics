#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "pop_modifyDataItemsTest.h"
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
0.09 ;[P]
$DES
DADT(1)=-K*A(1)
$ERROR
Y = F*(1 + EPS(1))
$SIGMA DIAGONAL(1)
0.03 ;[P]
$ESTIMATION METHOD=1 INTERACTION SIGDIGITS=3 MAXEVALS=450 PRINT=5
$COVARIANCE
====================================================================================
*/
namespace{ 
  const unsigned int MAXCHARS = 512;

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
  // Possible data labels
  //============================================  
  const char *strID         = "ID";
  const char *strDV         = "DV";
  const char *strTIME       = "TIME";
  const char *strAMT        = "AMT";
  const char *strMDV        = "MDV";
  const char *strEVID       = "EVID";
  const char *strDROP       = "DROP";
  const char *strSKIP       = "SKIP";

  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 450;
  const bool isEstimate = true;
  const char method[]   = "foce";
  const int  sig_digits = 3;

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
  const int    etaLen = 2;
  const double eta_in  [ etaLen ] = { 0.0, 0.0 };
  const bool   eta_fix [ etaLen ] = { false };

  //============================================
  // The SPK Compiler decides the constraints
  // of Omega matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal matrix.
  //============================================
  const int    omegaDim                = etaLen;
  const Symbol::Structure omegaStruct  = Symbol::FULL;
  const int    omegaOrder              = (omegaStruct==Symbol::DIAGONAL? 
					  omegaDim : (omegaDim * (omegaDim+1))/2 );
  const double omega_in [ omegaOrder ] = { 0.09, 0.02, 0.01 };
  const bool   omega_fix[ omegaOrder ] = { false, false, false };

  //============================================
  // EPS is irrevalent in the individual 
  // analysis case.  It'll be ignored.
  //============================================
  const int epsLen = 3;
   
  //============================================
  // The SPK Compiler decides the constraints
  // of Sigma matrix. Just feed the initial
  // values.  Here, test with a simple
  // diagonal matrix.
  //============================================
  const int    sigmaDim                = epsLen;
  const Symbol::Structure sigmaStruct  = Symbol::FULL;
  const int    sigmaOrder              = (sigmaStruct==Symbol::DIAGONAL? 
					  sigmaDim : (sigmaDim * (sigmaDim+1))/2 );
  const double sigma_in [ sigmaOrder ] = { 0.01, 0.02, 0.03, 0.04, 0.05, 0.06 };
  const bool   sigma_fix[ sigmaOrder ] = { false, false, false, false, false, false };

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
  const char DIFFEQN[]    = "DADT(1) = -K*A(1)\n";

  //============================================
  // $ERROR model
  //
  // Y = F*(1 + EPS(1) )
  //============================================
  const char ERROR[] = "Y = F*(1+EPS(1))\n";

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

/*
====================================================================================
   Data set 1 [ AMT=no, MDV=no, EVID=no ]
====================================================================================

1,2.00E+00,1.09E+00
1,4.00E+00,7.50E-01
1,6.00E+00,5.30E-01
1,8.00E+00,3.40E-01
1,1.00E+01,2.30E-01
1,2.40E+01,2.00E-02

2,2.00E+00,2.03E+00
2,4.00E+00,1.28E+00
2,6.00E+00,1.20E+00
2,8.00E+00,1.02E+00
2,1.00E+01,8.30E-01
2,2.40E+01,2.80E-01

3,2.00E+00,1.44E+00
3,4.00E+00,1.30E+00
3,6.00E+00,9.50E-01
3,8.00E+00,6.80E-01
3,1.00E+01,5.20E-01
3,2.40E+01,6.00E-02

====================================================================================
*/
void pop_modifyDataItemsTest::noAMT_noMDV_noEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 3;
  const char * label[] = { strID, strTIME, strDV }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][1] = 1.44E+00;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][1] = 1.30E+00;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][1] = 9.50E-01;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][1] = 6.80E-01;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][1] = 5.20E-01;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][1] = 6.00E-02;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << 0.0 << ", set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << 0   << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
                MAXCHARS,
	       "Compilation of the generated %s failed!", 
	       fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver_cpp );
  remove( fDataSetDriver );
}
/*
====================================================================================
   Data set 2 [ AMT=no, MDV=no, EVID=yes ]
====================================================================================
                    EVID
1,2.00E+00,1.09E+00, 1
1,4.00E+00,7.50E-01, 0
1,6.00E+00,5.30E-01, 0
1,8.00E+00,3.40E-01, 0
1,1.00E+01,2.30E-01, 0
1,2.40E+01,2.00E-02, 0

2,2.00E+00,2.03E+00, 0
2,4.00E+00,1.28E+00, 1
2,6.00E+00,1.20E+00, 0
2,8.00E+00,1.02E+00, 0
2,1.00E+01,8.30E-01, 0
2,2.40E+01,2.80E-01, 0

3,2.00E+00,1.44E+00, 0
3,4.00E+00,1.30E+00, 0
3,6.00E+00,9.50E-01, 1
3,8.00E+00,6.80E-01, 0
3,1.00E+01,5.20E-01, 0
3,2.40E+01,6.00E-02, 0

====================================================================================
*/
void pop_modifyDataItemsTest::noAMT_noMDV_yesEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 4;
  const char * label[] = { strID, strTIME, strDV, "EVID" }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 0;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 1;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 0;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-00; set[14][3] = 1;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 0;
  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << 0.0 << ", set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	  if( set[k][3] == 0 )
	    o << "   MY_ASSERT_EQUAL( " << 0 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	  else
	    o << "   MY_ASSERT_EQUAL( " << 1 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	}
    }
  o << "}" << endl;
  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
	        MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  //remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 3 [ AMT=no, MDV=yes, EVID=no ]
====================================================================================
                    MDV
1,2.00E+00,1.09E+00, 1
1,4.00E+00,7.50E-01, 0
1,6.00E+00,5.30E-01, 0
1,8.00E+00,3.40E-01, 0
1,1.00E+01,2.30E-01, 0
1,2.40E+01,2.00E-02, 0

2,2.00E+00,2.03E+00, 0
2,4.00E+00,1.28E+00, 1
2,6.00E+00,1.20E+00, 0
2,8.00E+00,1.02E+00, 0
2,1.00E+01,8.30E-01, 0
2,2.40E+01,2.80E-01, 0

3,2.00E+00,1.44E+00, 0
3,4.00E+00,1.30E+00, 0
3,6.00E+00,9.50E-01, 1
3,8.00E+00,6.80E-01, 0
3,1.00E+01,5.20E-01, 0
3,2.40E+01,6.00E-02, 0

====================================================================================
*/
void pop_modifyDataItemsTest::noAMT_yesMDV_noEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 4;
  const char * label[] = { strID, strTIME, strDV, strMDV }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 0;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 1;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 0;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] = 1;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( 0.0, set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->MDV[" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
	        MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 4 [ AMT=no, MDV=yes, EVID=yes ]
====================================================================================

                    MDV  EVID
1,2.00E+00,1.09E+00, 1,   1
1,4.00E+00,7.50E-01, 0,   0
1,6.00E+00,5.30E-01, 0,   0
1,8.00E+00,3.40E-01, 0,   0
1,1.00E+01,2.30E-01, 0,   0
1,2.40E+01,2.00E-02, 0,   0

2,2.00E+00,2.03E+00, 0,   1
2,4.00E+00,1.28E+00, 1,   0
2,6.00E+00,1.20E+00, 0,   0
2,8.00E+00,1.02E+00, 0,   0
2,1.00E+01,8.30E-01, 0,   0
2,2.40E+01,2.80E-01, 0,   0

3,2.00E+00,1.44E+00, 0,   0
3,4.00E+00,1.30E+00, 0,   0
3,6.00E+00,9.50E-01, 1,   0
3,8.00E+00,6.80E-01, 0,   0
3,1.00E+01,5.20E-01, 0,   0
3,2.40E+01,6.00E-02, 0,   0

====================================================================================
*/
void pop_modifyDataItemsTest::noAMT_yesMDV_yesEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 5;

  const char * label[] = { strID, strTIME, strDV, strMDV, strEVID }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 1; set[0] [4] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 0; set[0] [4] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 0; set[0] [4] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 0; set[0] [4] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 0; set[0] [4] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 0; set[0] [4] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 0; set[0] [4] = 1;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 1; set[0] [4] = 0;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 0; set[0] [4] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 0; set[0] [4] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 0; set[0] [4] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 0; set[0] [4] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 0; set[0] [4] = 1;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 0; set[0] [4] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] = 1; set[0] [4] = 0;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 0; set[0] [4] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 0; set[0] [4] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 0; set[0] [4] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( 0.0, set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->MDV[" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][4] << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
                MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 5 [ AMT=yes, MDV=no, EVID=no ]
====================================================================================
                     AMT
1,2.00E+00,1.09E+00, 30
1,4.00E+00,7.50E-01,  0
1,6.00E+00,5.30E-01,  0
1,8.00E+00,3.40E-01,  0
1,1.00E+01,2.30E-01,  0
1,2.40E+01,2.00E-02,  0

2,2.00E+00,2.03E+00, 30
2,4.00E+00,1.28E+00,  0
2,6.00E+00,1.20E+00,  0
2,8.00E+00,1.02E+00,  0
2,1.00E+01,8.30E-01,  0
2,2.40E+01,2.80E-01,  0

3,2.00E+00,1.44E+00, 30
3,4.00E+00,1.30E+00,  0
3,6.00E+00,9.50E-01,  0
3,8.00E+00,6.80E-01,  0
3,1.00E+01,5.20E-01,  0
3,2.40E+01,6.00E-02,  0

====================================================================================
*/
void pop_modifyDataItemsTest::yesAMT_noMDV_noEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 4;

  const char * label[] = { strID, strTIME, strDV, strAMT }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }

  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 30;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 30;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 0;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 30;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] = 0;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  if( set[k][3] == 0 )
	    o << "   MY_ASSERT_EQUAL( " << 0 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	  else
	    o << "   MY_ASSERT_EQUAL( " << 1 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message,
                MAXCHARS, 
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 6 [ AMT=yes, MDV=no, EVID=yes ]
====================================================================================
                    AMT  EVID
1,2.00E+00,1.09E+00,30,   1
1,4.00E+00,7.50E-01, 0,   0
1,6.00E+00,5.30E-01, 0,   0
1,8.00E+00,3.40E-01, 0,   0
1,1.00E+01,2.30E-01, 0,   0
1,2.40E+01,2.00E-02, 0,   0

2,2.00E+00,2.03E+00,30,   0
2,4.00E+00,1.28E+00, 0,   1
2,6.00E+00,1.20E+00, 0,   0
2,8.00E+00,1.02E+00, 0,   0
2,1.00E+01,8.30E-01, 0,   0
2,2.40E+01,2.80E-01, 0,   0

3,2.00E+00,1.44E+00,30,   0
3,4.00E+00,1.30E+00, 0,   0
3,6.00E+00,9.50E-01, 0,   1
3,8.00E+00,6.80E-01, 0,   0
3,1.00E+01,5.20E-01, 0,   0
3,2.40E+01,6.00E-02, 0,   0

====================================================================================
*/
void pop_modifyDataItemsTest::yesAMT_noMDV_yesEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 5;

  const char * label[] = { strID, strTIME, strDV, strAMT, strEVID }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }

  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 30; set[0] [4] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] =  0; set[1] [4] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] =  0; set[2] [4] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] =  0; set[3] [4] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] =  0; set[4] [4] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] =  0; set[5] [4] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 30; set[6] [4] = 0;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] =  0; set[7] [4] = 1;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] =  0; set[8] [4] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] =  0; set[9] [4] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] =  0; set[10][4] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] =  0; set[11][4] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 30; set[12][4] = 0;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] =  0; set[13][4] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] =  0; set[14][4] = 1;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] =  0; set[15][4] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] =  0; set[16][4] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] =  0; set[17][4] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][4] << ", set.data[" << i << "]->EVID [" << j << "] );" << endl;
	  if( set[k][4] == 0 )
	    o << "   MY_ASSERT_EQUAL( " << 0 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	  else
	    o << "   MY_ASSERT_EQUAL( " << 1 << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
                MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );

}

/*
====================================================================================
   Data set 7 [ AMT=yes, MDV=yes, EVID=no ]
====================================================================================
                    AMT  MDV
1,2.00E+00,1.09E+00,30,   1       (1)
1,4.00E+00,7.50E-01, 0,   0       (0)
1,6.00E+00,5.30E-01, 0,   0       (0)
1,8.00E+00,3.40E-01, 0,   0       (0)
1,1.00E+01,2.30E-01, 0,   0       (0)
1,2.40E+01,2.00E-02, 0,   0       (0)

2,2.00E+00,2.03E+00,30,   0       (0)
2,4.00E+00,1.28E+00, 0,   1       (2)
2,6.00E+00,1.20E+00, 0,   0       (0)
2,8.00E+00,1.02E+00, 0,   0       (0)
2,1.00E+01,8.30E-01, 0,   0       (0)
2,2.40E+01,2.80E-01, 0,   0       (0)

3,2.00E+00,1.44E+00,30,   0       (0)
3,4.00E+00,1.30E+00, 0,   0       (0)
3,6.00E+00,9.50E-01, 0,   1       (2)
3,8.00E+00,6.80E-01, 0,   0       (0)
3,1.00E+01,5.20E-01, 0,   0       (0)
3,2.40E+01,6.00E-02, 0,   0       (0)

====================================================================================
*/
void pop_modifyDataItemsTest::yesAMT_yesMDV_noEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 5;

  const char * label[] = { strID, strTIME, strDV, strAMT, strMDV }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }

  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 30; set[0] [4] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] =  0; set[1] [4] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] =  0; set[2] [4] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] =  0; set[3] [4] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] =  0; set[4] [4] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] =  0; set[5] [4] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 30; set[6] [4] = 0;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] =  0; set[7] [4] = 1;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] =  0; set[8] [4] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] =  0; set[9] [4] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] =  0; set[10][4] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] =  0; set[11][4] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 30; set[12][4] = 0;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] =  0; set[13][4] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] =  0; set[14][4] = 1;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] =  0; set[15][4] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] =  0; set[16][4] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] =  0; set[17][4] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->AMT[" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][4] << ", set.data[" << i << "]->MDV[" << j << "] );" << endl;
	  if( set[k][4] == 1 )
	    {
	      if( set[k][3] == 0.0 )
		o << "   MY_ASSERT_EQUAL( " << 2 << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	      else
		o << "   MY_ASSERT_EQUAL( " << 1 << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	    }
	  else
	    o << "   MY_ASSERT_EQUAL( " << 0 << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
                MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 8 [ AMT=yes, MDV=yes, EVID=yes ]
====================================================================================
                     AMT  MDV  EVID
1,2.00E+00,1.09E+00, 30,   1,   1
1,4.00E+00,7.50E-01,  0,   0,   0
1,6.00E+00,5.30E-01,  0,   0,   0
1,8.00E+00,3.40E-01,  0,   0,   0
1,1.00E+01,2.30E-01,  0,   0,   0
1,2.40E+01,2.00E-02,  0,   0,   0

2,2.00E+00,2.03E+00, 30,   0,   1
2,4.00E+00,1.28E+00,  0,   1,   0
2,6.00E+00,1.20E+00,  0,   0,   0
2,8.00E+00,1.02E+00,  0,   0,   0
2,1.00E+01,8.30E-01,  0,   0,   0
2,2.40E+01,2.80E-01,  0,   0,   0

3,2.00E+00,1.44E+00, 30,   0,   0
3,4.00E+00,1.30E+00,  0,   0,   0
3,6.00E+00,9.50E-01,  0,   1,   0
3,8.00E+00,6.80E-01,  0,   0,   0
3,1.00E+01,5.20E-01,  0,   0,   0
3,2.40E+01,6.00E-02,  0,   0,   0

====================================================================================
*/
void pop_modifyDataItemsTest::yesAMT_yesMDV_yesEVID()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 6;

  const char * label[] = { strID, strTIME, strDV, strAMT, strMDV, strEVID }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 30; set[0] [4] = 1; set[0] [5] = 1;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] =  0; set[1] [4] = 0; set[1] [5] = 0;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] =  0; set[2] [4] = 0; set[2] [5] = 0;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] =  0; set[3] [4] = 0; set[3] [5] = 0;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] =  0; set[4] [4] = 0; set[4] [5] = 0;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] =  0; set[5] [4] = 0; set[5] [5] = 0;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 30; set[6] [4] = 0; set[6] [5] = 1;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] =  0; set[7] [4] = 1; set[7] [5] = 0;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] =  0; set[8] [4] = 0; set[8] [5] = 0;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] =  0; set[9] [4] = 0; set[9] [5] = 0;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] =  0; set[10][4] = 0; set[10][5] = 0;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] =  0; set[11][4] = 0; set[11][5] = 0;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 30; set[12][4] = 0; set[12][5] = 1;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] =  0; set[13][4] = 0; set[13][5] = 0;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] =  0; set[14][4] = 1; set[14][5] = 0;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] =  0; set[15][4] = 0; set[15][5] = 0;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] =  0; set[16][4] = 0; set[16][5] = 0;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] =  0; set[17][4] = 0; set[17][5] = 0;

  createDataML( "noAMT_noMDV_noEVID.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "noAMT_noMDV_noEVID.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "noAMT_noMDV_noEVID.data.xml", nIndividuals );
  parseSourceML( sourceParser, "noAMT_noMDV_noEVID.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "AMT not found", table->find( strAMT )!=Symbol::empty() );

  ofstream o ( fDataSetDriver_cpp );
  assert( o.good() );

  o << "#include \"DataSet.h\"" << endl;
  o << "#include <iostream>" << endl;
  o << "#include <valarray>" << endl;

  o << "using namespace std;" << endl;
  o << MY_ASSERT_EQUAL << endl;
  
  o << "int main()" << endl;
  o << "{" << endl;
  o << "  int nIndividuals = " << nIndividuals << ";" << endl;
  o << "  valarray<int> N(nIndividuals);" << endl;
  for( int i=0; i<nIndividuals; i++ )
    o << "  N[" << i << "] = " << N[i] << ";" << endl;
  o << "  DataSet<double> set;" << endl;
  for( int i=0, k=0; i<nIndividuals; i++ )
    {
      for( int j=0; j<N[i]; j++, k++ )
	{
	  o << "   MY_ASSERT_EQUAL( " << set[k][3] << ", set.data[" << i << "]->AMT [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][4] << ", set.data[" << i << "]->MDV [" << j << "] );" << endl;
	  o << "   MY_ASSERT_EQUAL( " << set[k][5] << ", set.data[" << i << "]->EVID[" << j << "] );" << endl;
	}
    }
  o << "}" << endl;

  o.close();

  char command[1024];
  snprintf( command,
	    1024,
	   "g++ %s -o %s %s %s", 
	   fDataSetDriver_cpp, 
	   fDataSetDriver, 
	   LDFLAG, 
	   CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS];
      snprintf( message, 
                MAXCHARS,
	        "Compilation of the generated %s failed!", 
	        fDataSetDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  snprintf( command, 1024, "./%s", fDataSetDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, 
		MAXCHARS,
	       "A test driver, %s, failed!",
	       fDataSetDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
   }
  remove( fDataSetDriver );
  remove( fDataSetDriver_cpp );
}
/*
====================================================================================
   Data set 9 [ ID, TIME, DV, DROP ]
====================================================================================
                     DROP
1,2.00E+00,1.09E+00,  999
1,4.00E+00,7.50E-01,  999
1,6.00E+00,5.30E-01,  999
1,8.00E+00,3.40E-01,  999
1,1.00E+01,2.30E-01,  999
+01,2.00E-02,  999

2,2.00E+00,2.03E+00,  999
2,4.00E+00,1.28E+00,  999 
2,6.00E+00,1.20E+00,  999
2,8.00E+00,1.02E+00,  999
2,1.00E+01,8.30E-01,  999
2,2.40E+01,2.80E-01,  999

3,2.00E+00,1.44E+00,  999
3,4.00E+00,1.30E+00,  999
3,6.00E+00,9.50E-01,  999
3,8.00E+00,6.80E-01,  999
3,1.00E+01,5.20E-01,  999
3,2.40E+01,6.00E-02,  999

====================================================================================
*/
void pop_modifyDataItemsTest::drop()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 5;

  const char * label[] = { strID, strTIME, strDV, strDROP, strDROP }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 999; set[0] [4] = 999;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 999; set[1] [4] = 999;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 999; set[2] [4] = 999;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 999; set[3] [4] = 999;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 999; set[4] [4] = 999;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 999; set[5] [4] = 999;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 999; set[6] [4] = 999;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 999; set[7] [4] = 999;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 999; set[8] [4] = 999;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 999; set[9] [4] = 999;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 999; set[10][4] = 999;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 999; set[11][4] = 999;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 999; set[12][4] = 999;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 999; set[13][4] = 999;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] = 999; set[14][4] = 999;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 999; set[15][4] = 999;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 999; set[16][4] = 999;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 999; set[17][4] = 999;

  createDataML( "drop.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "drop.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "drop.data.xml", nIndividuals );
  parseSourceML( sourceParser, "drop.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "DROP hasn't been removed", table->find( strDROP ) == Symbol::empty() );
}

/*
====================================================================================
   Data set 9 [ ID, TIME, DV, SKIP ]
====================================================================================
                     SKIP
1,2.00E+00,1.09E+00,  999
1,4.00E+00,7.50E-01,  999
1,6.00E+00,5.30E-01,  999
1,8.00E+00,3.40E-01,  999
1,1.00E+01,2.30E-01,  999
+01,2.00E-02,  999

2,2.00E+00,2.03E+00,  999
2,4.00E+00,1.28E+00,  999 
2,6.00E+00,1.20E+00,  999
2,8.00E+00,1.02E+00,  999
2,1.00E+01,8.30E-01,  999
2,2.40E+01,2.80E-01,  999

3,2.00E+00,1.44E+00,  999
3,4.00E+00,1.30E+00,  999
3,6.00E+00,9.50E-01,  999
3,8.00E+00,6.80E-01,  999
3,1.00E+01,5.20E-01,  999
3,2.40E+01,6.00E-02,  999

====================================================================================
*/
void pop_modifyDataItemsTest::skip()
{
  const int    nIndividuals = 3;
  valarray<int> N(nIndividuals);
  N[0] = 6;
  N[1] = 6;
  N[2] = 6;
  const int    nRecords     = N.sum();
  const int    nItems       = 5;

  const char * label[] = { strID, strTIME, strDV, strSKIP, strSKIP }; 
  vector< vector<double> > set(nRecords);
  for( int i=0; i<nRecords; i++ )
    {
      set[i].resize( nItems );
    }
  set[0] [0] = 1; set[0] [1] = 2.00E+00; set[0] [2] = 1.09E+00; set[0] [3] = 999; set[0] [4] = 999;
  set[1] [0] = 1; set[1] [1] = 4.00E+00; set[1] [2] = 7.50E-01; set[1] [3] = 999; set[1] [4] = 999;
  set[2] [0] = 1; set[2] [1] = 6.00E+00; set[2] [2] = 5.30E-01; set[2] [3] = 999; set[2] [4] = 999;
  set[3] [0] = 1; set[3] [1] = 8.00E+00; set[3] [2] = 3.40E-01; set[3] [3] = 999; set[3] [4] = 999;
  set[4] [0] = 1; set[4] [1] = 1.00E+01; set[4] [2] = 2.30E-01; set[4] [3] = 999; set[4] [4] = 999;
  set[5] [0] = 1; set[5] [1] = 2.40E+01; set[5] [2] = 2.00E-02; set[5] [3] = 999; set[5] [4] = 999;
  set[6] [0] = 2; set[6] [1] = 2.00E+00; set[6] [2] = 2.03E+00; set[6] [3] = 999; set[6] [4] = 999;
  set[7] [0] = 2; set[7] [1] = 4.00E+00; set[7] [2] = 1.28E+00; set[7] [3] = 999; set[7] [4] = 999;
  set[8] [0] = 2; set[8] [1] = 6.00E+00; set[8] [2] = 1.20E+00; set[8] [3] = 999; set[8] [4] = 999;
  set[9] [0] = 2; set[9] [1] = 8.00E+00; set[9] [2] = 1.02E+00; set[9] [3] = 999; set[9] [4] = 999;
  set[10][0] = 2; set[10][1] = 1.00E+01; set[10][2] = 8.30E-01; set[10][3] = 999; set[10][4] = 999;
  set[11][0] = 2; set[11][1] = 2.40E+01; set[11][2] = 2.80E-01; set[11][3] = 999; set[11][4] = 999;
  set[12][0] = 3; set[12][1] = 2.00E+00; set[12][2] = 1.44E+00; set[12][3] = 999; set[12][4] = 999;
  set[13][0] = 3; set[13][1] = 4.00E+00; set[13][2] = 1.30E+00; set[13][3] = 999; set[13][4] = 999;
  set[14][0] = 3; set[14][1] = 6.00E+00; set[14][2] = 9.50E-01; set[14][3] = 999; set[14][4] = 999;
  set[15][0] = 3; set[15][1] = 8.00E+00; set[15][2] = 6.80E-01; set[15][3] = 999; set[15][4] = 999;
  set[16][0] = 3; set[16][1] = 1.00E+01; set[16][2] = 5.20E-01; set[16][3] = 999; set[16][4] = 999;
  set[17][0] = 3; set[17][1] = 2.40E+01; set[17][2] = 6.00E-02; set[17][3] = 999; set[17][4] = 999;

  createDataML( "skip.data.xml", nIndividuals, nItems, label, nRecords, set );
  createSourceML( "skip.source.xml", nIndividuals, nItems, label );
  parseDataML( dataParser, "skip.data.xml", nIndividuals );
  parseSourceML( sourceParser, "skip.source.xml", nIndividuals );

  NonmemTranslator nm( source, data );
  try{
    nm.translate();
  }
  catch ( const SpkCompilerException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT( false );
    }
  catch ( ... )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation failed due to a unknown error", false );
    }

  const SymbolTable * table = nm.getSymbolTable();
  CPPUNIT_ASSERT_MESSAGE( "SKIP hasn't been removed", table->find( strSKIP ) == Symbol::empty() );
}
void pop_modifyDataItemsTest::setUp()
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
  snprintf( fDataML,               MAXCHARS, "%s_data.xml",             fPrefix );
  snprintf( fSourceML,             MAXCHARS, "%s_source.xml",           fPrefix );
  snprintf( fDataSetDriver,        MAXCHARS, "%s_DataSetDriver",        fPrefix );
  snprintf( fDataSetDriver_cpp,    MAXCHARS, "%s_DataSetDriver.cpp",    fPrefix );
  snprintf( fODEPredDriver,        MAXCHARS, "%s_ODEPredDriver",        fPrefix );
  snprintf( fODEPredDriver_cpp,    MAXCHARS, "%s_ODEPredDriver.cpp",    fPrefix );

  snprintf( LDFLAG, LDFLAG_MAXCHARS, "%s -l%s -l%s  -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
     LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB, CLNLIB, GINACLIB, BADLIB, BAPLIB, BAVLIB, BA0LIB, GSLLIB, GSLCBLASLIB );

  dataParser = new xercesc::XercesDOMParser;
  dataParser->setValidationScheme( XercesDOMParser::Val_Auto );
  dataParser->setDoNamespaces( true );
  dataParser->setDoSchema( true );
  dataParser->setValidationSchemaFullChecking( true );
  dataParser->setCreateEntityReferenceNodes( true );

  sourceParser = new xercesc::XercesDOMParser;
  sourceParser->setValidationScheme( XercesDOMParser::Val_Auto );
  sourceParser->setDoNamespaces( true );
  sourceParser->setDoSchema( true );
  sourceParser->setValidationSchemaFullChecking( true );
  sourceParser->setCreateEntityReferenceNodes( true );

}

void pop_modifyDataItemsTest::tearDown()
{
  delete data;
  delete source;


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
void pop_modifyDataItemsTest::createDataML( const char * fDataML, 
					    int nIndividuals,
					    int nItems, 
					    const char* label[],
					    int nRecords, 
					    const vector< vector<double> > & set )
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
	o << "   <value>" << set[i][j] << "</value>" << endl;
      o << "</row>" << endl;
    }
  o << "</table>" << endl;
  o << "</spkdataml>" << endl;
  o.close();
}
void pop_modifyDataItemsTest::parseDataML( xercesc::XercesDOMParser *dataParser, 
					   const char * fDataML,
					   int nIndividuals )
{
  try{
    ifstream dataML( fDataML );
     CPPUNIT_ASSERT_MESSAGE( "Failed to open a data xml.", dataML.good() );
    dataML.close();
    dataParser->reset();
    dataParser->parse( fDataML );
    data = dataParser->getDocument();
    //    DOMPrint( data );
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
 
void pop_modifyDataItemsTest::createSourceML( const char* fSourceML, 
					      int nIndividuals,
					      int nLabels, 
					      const char* label[] )
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
  for( int i=0; i<nLabels; i++ )
    
    o << "          <label name=\"" << label[i] << "\"/>" << endl;
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
  o << "        <omega dimension=\"" << omegaDim << "\" struct=\"" << (omegaStruct==Symbol::DIAGONAL? "diagonal":"block")       << "\">" << endl;
  o << "          <in>" << endl;
  for( int i=0; i<omegaOrder; i++ )
    {
      o << "            <value>" << omega_in[i] << "</value>" << endl;
    }
  o << "          </in>" << endl;
  o << "        </omega>" << endl;
  o << "        <sigma dimension=\"" << sigmaDim << "\" struct=\"" << (sigmaStruct==Symbol::DIAGONAL? "diagonal":"block") << "\">" << endl;
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
}
void pop_modifyDataItemsTest::parseSourceML( xercesc::XercesDOMParser *sourceParser, 
					     const char* fSourceML,
					     int nIndividuals )
{
  //============================================
  // Build a parse tree from the sourceML
  // document.
  //============================================
  try{
    ifstream sourceML( fSourceML );
    //ifstream sourceML( "pop_modifyDataItemsTest.source.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open CAD1996A1.source.xml", sourceML.good() );
    sourceML.close();
    //sourceParser->parse( "pop_modifyDataItemsTest.source.xml" );
    sourceParser->reset();
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


CppUnit::Test * pop_modifyDataItemsTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "pop_modifyDataItemsTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "noAMT_noMDV_noEVID", 
	 &pop_modifyDataItemsTest::noAMT_noMDV_noEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "noAMT_noMDV_yesEVID", 
	 &pop_modifyDataItemsTest::noAMT_noMDV_yesEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "noAMT_yesMDV_noEVID", 
	 &pop_modifyDataItemsTest::noAMT_yesMDV_noEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "noAMT_yesMDV_yesEVID", 
	 &pop_modifyDataItemsTest::noAMT_yesMDV_yesEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "yesAMT_noMDV_noEVID", 
	 &pop_modifyDataItemsTest::yesAMT_noMDV_noEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "yesAMT_noMDV_yesEVID", 
	 &pop_modifyDataItemsTest::yesAMT_noMDV_yesEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "yesAMT_yesMDV_noEVID", 
	 &pop_modifyDataItemsTest::yesAMT_yesMDV_noEVID ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "yesAMT_yesMDV_yesEVID", 
	 &pop_modifyDataItemsTest::yesAMT_yesMDV_yesEVID ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<pop_modifyDataItemsTest>(
         "drop", 
	 &pop_modifyDataItemsTest::drop ) );

  return suiteOfTests;
}

