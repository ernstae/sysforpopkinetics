#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "ind_identTest.h"
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
=============================================================================================
  Individual identifiability test using the "Cadralazine for Workshop" model and data
=============================================================================================
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
  char fIndDataDriver       [MAXCHARS+1];
  char fIndDataDriver_cpp   [MAXCHARS+1];
  char fDataSetDriver       [MAXCHARS+1];
  char fDataSetDriver_cpp   [MAXCHARS+1];
  char fIdentPredDriver     [MAXCHARS+1];
  char fIdentPredDriver_cpp [MAXCHARS+1];

  char SPKLIB[]      = "spk";
  char SPKPREDLIB[]  = "spkpred";
  char SPKOPTLIB[]   = "QN01Box";
  char ATLASLIB[]    = "lapack_atlas";
  char CBLASLIB[]    = "cblas";
  char CLAPACKLIB[]  = "atlas";
  char PTHREADLIB[]  = "pthread";
  char MLIB[]        = "m";
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
  // <Data Set>
  //
  //     TIME   DV   AMT
  //============================================

  const char *strDV         = "DV";
  const char *strTIME       = "TIME";
  const char *strAMT        = "AMT";
  const char *label[]       = { strTIME, strDV, strAMT };
  const int    nLabels      = 1;
  const int    nIndividuals = 1;
  const int    nRecords     = 7;
  const int    nFixed       = 0;
  const int    nMDV         = 1;
  const int    nItems       = nLabels;
  const int    c_N[]        = { 7 };
  valarray<int> N( c_N, nIndividuals );
  const int    c_NRecords[] = { 6 };
  valarray<int> NRecords( c_NRecords, nIndividuals );
  const double record0 [] = { 0, 0, 30 };
  const double record1 [] = { 2.00E+00, 1.09E+00, 0 };
  const double record2 [] = { 4.00E+00, 7.50E-01, 0 };
  const double record3 [] = { 6.00E+00, 5.30E-01, 0 };
  const double record4 [] = { 8.00E+00, 3.40E-01, 0 };
  const double record5 [] = { 1.00E+01, 2.30E-01, 0 }; 
  const double record6 [] = { 2.40E+01, 2.00E-02, 0 };

  double const * record[nRecords];

  //============================================
  // Define NONMEM keywords
  //============================================

  const char *strTHETA    = "THETA";
  const char *strOMEGA    = "OMEGA";
  const char *strETA      = "ETA";
  const char *strF        = "F";
  const char *strY        = "Y";
  const char *strT        = "T";
  const char *strDADT     = "DADT";
  const char *strA        = "A";

  //============================================
  // User defined words
  //============================================

  const char * strK10    = "K10";

  //============================================
  // Set the length for theta.
  //============================================

  const int    thetaLen = 1;

  //============================================
  // Set the dimension for Omega, assuming it's diagonal.
  //============================================

  const int    omegaDim = 1;

  //============================================
  // Set the length for eta.
  //============================================

  const int    etaLen   = omegaDim;

  //============================================
  // Set the seed for the theta value simulation.
  //============================================

  const int  seed       = 1;

  //============================================
  // $MODEL - compartmental model definition
  //============================================
  // This definition is not based upon text.

  //============================================
  // $PK model 
  //
  //     K10=THETA(1)
  //
  //============================================

  const int nPkParams = 0;
  const char PK[] = "K10=THETA(1)\n";

  //============================================
  // $DES model
  //
  //     DADT(1)=-K10*A(1)
  //
  //============================================

  const int  nComps       = 1;
  
  const int  nonmemNComps = nComps + 1;
  const int  nEquilibrims = 0;
  const char DIFFEQN[]    = "DADT(1)=-K10*A(1)\n";

  //============================================
  // $ERROR model
  //
  //     Y=F+ETA(1)
  //
  //============================================

  const char ERROR[] = "Y=F+ETA(1)\n";

  //============================================
  // Known results
  //============================================

  int nGroebnerBasisSolnKnown = 1;
  string identStatusKnown = "Globally Identifiable";;

};
void ind_identTest::setUp()
{
  okToClean = false;

  // The first element of the char array returned by type_info.name() is the number of characters that follows.
  testName = typeid( *this ).name();

  strcpy ( fPrefix,               testName );
  snprintf( fNonmemParsDriver,     MAXCHARS, "%s_NonmemParsDriver",     fPrefix );
  snprintf( fNonmemParsDriver_cpp, MAXCHARS, "%s_NonmemParsDriver.cpp", fPrefix );
  snprintf( fIndDataDriver,        MAXCHARS, "%s_IndDataDriver",        fPrefix );
  snprintf( fIndDataDriver_cpp,    MAXCHARS, "%s_IndDataDriver.cpp",    fPrefix );
  snprintf( fDataML,               MAXCHARS, "%s_data.xml",             fPrefix );
  snprintf( fSourceML,             MAXCHARS, "%s_source.xml",           fPrefix );
  snprintf( fDataSetDriver,        MAXCHARS, "%s_DataSetDriver",        fPrefix );
  snprintf( fDataSetDriver_cpp,    MAXCHARS, "%s_DataSetDriver.cpp",    fPrefix );
  snprintf( fIdentPredDriver,      MAXCHARS, "%s_IdentPredDriver",      fPrefix );
  snprintf( fIdentPredDriver_cpp,  MAXCHARS, "%s_IdentPredDriver.cpp",  fPrefix );

  snprintf( LDFLAG, LDFLAG_MAXCHARS, "%s -l%s -l%s  -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s -l%s",
    LDPATH, SPKLIB, SPKPREDLIB, SPKOPTLIB, ATLASLIB, CBLASLIB, CLAPACKLIB, PTHREADLIB, MLIB, XERCESCLIB, CLNLIB, GINACLIB, BADLIB, BAPLIB, BAVLIB, BA0LIB, GSLLIB, GSLCBLASLIB );

  record[0]    = record0;
  record[1]    = record1;
  record[2]    = record2;
  record[3]    = record3;
  record[4]    = record4;
  record[5]    = record5;
  record[6]    = record6;

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
void ind_identTest::parse()
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
      assert( strcmp( compmodel[i].getName().c_str(), "A1" ) == 0 );
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
}
void ind_identTest::tearDown()
{
  if( okToClean )
    {
      remove( fDataML );
      remove( fSourceML );
      remove( fReportML );
      remove( fFitDriver );
      remove( fFitDriver_cpp );
      remove( fNonmemParsDriver );
      remove( fNonmemParsDriver_cpp );
      remove( fIndDataDriver );
      remove( fIndDataDriver_cpp );
      remove( fDataSetDriver );
      remove( fDataSetDriver_cpp );
      remove( fIdentPredDriver );
      remove( fIdentPredDriver_cpp );
      remove( fMakefile );
      remove( fSavedReportML );
      remove( fTraceOut );
    }
}
//******************************************************************************
//
// Read in the data XML file.
//
//******************************************************************************
#include "DOMPrint.h"
void ind_identTest::createDataML()
{
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
    ifstream junk( "ind_identTest.data.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open a data xml file.", junk.good() );
    junk.close();
    dataParser->parse( "ind_identTest.data.xml" );
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
//******************************************************************************
//
// Read in the source XML file.
//
//******************************************************************************
void ind_identTest::createSourceML()
{
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
    ifstream junk( "ind_identTest.source.xml" );
    CPPUNIT_ASSERT_MESSAGE( "Failed to open a source xml file.", junk.good() );
    junk.close();
    sourceParser->parse( "ind_identTest.source.xml" );
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
void ind_identTest::testIndDataClass()
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
  // * AMT
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
  o << "   const int nonmemComps   = " << nonmemNComps << ";" << endl;
  o << "   const int nParams       = " << nPkParams    << ";" << endl;

  o << "   vector<char*>  a_id  (n);" << endl;
  o << "   vector<double> a_time(n);" << endl;
  o << "   vector<double> a_dv  (n);" << endl;
  o << "   vector<double> a_dose(n);" << endl;
  o << "   vector<double> a_amt (n);" << endl;
  o << "   vector<double> a_mdv (n);" << endl;
  o << "   vector<int>    a_evid(n);" << endl;

  for( int i=0; i<nRecords; i++ )
  {
    o << "   a_time[" << i << "] = "   << record[i][0] << ";" << endl;
    o << "   a_dv  [" << i << "] = "   << record[i][1] << ";" << endl;
    o << "   a_amt [" << i << "] = "   << record[i][2] << ";" << endl;

    o << "   a_id  [" << i << "] = \"1\";" << endl;
    o << "   a_dose[" << i << "] = 0;" << endl;

    o << "   if( a_amt[" << i << "] != 0 )" << endl;
    o << "     {" << endl;
    o << "       a_mdv [" << i << "] = 1;" << endl;
    o << "     }" << endl;
    o << "   else" << endl;
    o << "     {" << endl;
    o << "       a_mdv [" << i << "] = 0;" << endl;
    o << "     }" << endl;

    o << "   a_evid[" << i << "] = 0;" << endl;
  }

  o << "   IndData<double> A( n, a_id, a_time, a_dv, a_amt, a_mdv, a_evid );" << endl;

  // { ID, DV=CP, TIME }
  for( int i=0; i<nRecords; i++ )
    {
      o << "   MY_ASSERT_EQUAL( " << record[i][0] << ", A." << strTIME << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][1] << ", A." << strDV   << "[" << i << "] );" << endl;
      o << "   MY_ASSERT_EQUAL( " << record[i][2] << ", A." << strAMT  << "[" << i << "] );" << endl;

      o << "   MY_ASSERT_EQUAL( thetaLen,    A." << strTHETA << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( etaLen,      A." << strETA   << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nonmemComps, A." << strDADT  << "[" << i << "].size() );" << endl;
      o << "   MY_ASSERT_EQUAL( nonmemComps, A." << strA     << "[" << i << "].size() );" << endl;

      // REVISIT Sachiko 08/11/2005
      // What is "#of basic PK parameters?  What is NPARAMETERS good for?
      // Until we figure that out, ignore it.
      //
      //o << "   MY_ASSERT_EQUAL( nParams, A." << strP      << "[" << i << "].size() );" << endl;

      o << endl;
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
void ind_identTest::testDataSetClass()
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
       o << "   MY_ASSERT_EQUAL(  " << record[k][0] << ", set.data[" << j << "]->" << strTIME << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][1] << ", set.data[" << j << "]->" << strDV   << "[" << i << "] );" << endl;
       o << "   MY_ASSERT_EQUAL(  " << record[k][2] << ", set.data[" << j << "]->" << strAMT  << "[" << i << "] );" << endl;

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
  o << "         MY_ASSERT_EQUAL( nonmemComps, set.data[j]->"  << strDADT    << "[i].size() );" << endl;
  o << "         MY_ASSERT_EQUAL( nonmemComps, set.data[j]->"  << strA       << "[i].size() );" << endl;
  //  o << "         MY_ASSERT_EQUAL( nPkParams, set.data[j]->" << strP      << "[i].size() );" << endl;
  o << "      }" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strF       << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strY       << ".size() );" << endl;
  o << "      MY_ASSERT_EQUAL( nRecords, set.data[j]->" << strT       << ".size() );" << endl;
  o << "   }" << endl;

  o << "   const valarray<double> y = set.getAllMeasurements();" << endl;
  o << "   MY_ASSERT_EQUAL( " << N.sum() - nMDV << ", y.size() );" << endl;
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
void ind_identTest::testNonmemPars()
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
void ind_identTest::testIdentPredClass()
{ 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test IdentPred class to see if it has defined
  // IdentPred::evalDes(), IdentPred::evalError(), IdentPred::evalPK() properly.
  // Especially, the proper elements of the dependent variable-
  // vector given as an argument are replaced by the computed 
  // value of Y(j) and F(j).
  // Also, make sure the currently computed values, all of them,
  // are stored in memory for potential retrieval from the 
  // outside.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  printf( "\n--- %s ---\n", fIdentPredDriver );
  ofstream o( fIdentPredDriver_cpp );
  CPPUNIT_ASSERT( o.good() );

  o << "#include \"IdentPred.h\"" << endl;
  o << "#include \"DataSet.h\"" << endl;
  o << "#include \"NonmemPars.h\"" << endl;
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
  o << "   DataSet< GiNaC::ex > set;" << endl;
  o << "   IdentPred< GiNaC::ex > identPred( NonmemPars::nTheta," << endl; 
  o << "                                     NonmemPars::nEta," << endl;
  o << "                                     &set," << endl; 
  o << "                                     nIndividuals," << endl;
  o << "                                     isPkFunctionOfT," << endl;
  o << "                                     nCompartments," << endl;
  o << "                                     nParameters," << endl;
  o << "                                     defaultDoseComp," << endl;
  o << "                                     defaultObservationComp," << endl;
  o << "                                     initialOff," << endl;
  o << "                                     noOff," << endl;
  o << "                                     noDose );" << endl;
  o << "   valarray<int> N            = set.getN();" << endl;
  int i=0;
  o << "   MY_ASSERT_EQUAL( " << nRecords - nMDV << ", N[" << i << "] );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords - nMDV << ", identPred.getNObservs(" << i << ") );" << endl;
  o << "   MY_ASSERT_EQUAL( " << nRecords        << ", identPred.getNRecords(" << i << ") );" << endl;
  o << "   return 0;" << endl;
  o << "}" << endl;
  o.close();

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - BLAD 64 Bit Flag is Missing - Mitch]
  //
  // If this test is run on a 64 bit machine, it may not give the
  // correct value because the following flag is missing
  //
  //     -DBA0_64BITS
  //
  // This flag indicates to BLAD that it is being compiled on a 64 bit
  // machine.
  //
  // To work on 64 bit machines, this unit test will need to be
  // modified to check for the number of bits where it is being
  // compiled.  See the file Makefile.SPK that is currently being
  // generated by NM_generateMakefile().
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  char command[512];
  snprintf( command, 512, "g++ %s -o %s %s %s", fIdentPredDriver_cpp, fIdentPredDriver, LDFLAG, CPPFLAG );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "Compilation of the generated %s failed!", fIdentPredDriver_cpp );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  snprintf( command, 512, "./%s", fIdentPredDriver );
  if( system( command ) != 0 )
    {
      char message[MAXCHARS+1];
      snprintf( message, MAXCHARS,  "A test driver, %s, failed!", fIdentPredDriver );
      
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
}
void ind_identTest::testDriver()
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
void ind_identTest::testReportML()
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
  // Verify that the identifiability seed was returned. 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  DOMNodeList * seed_list = report->getElementsByTagName( XML.X_SEED );
  CPPUNIT_ASSERT( seed_list->getLength() != 0 );


  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify the number of Groebner Basis solutions.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  int nGroebnerBasisSoln;

  DOMNodeList * ind_ident_number_of_solutions_list = report->getElementsByTagName( XML.X_IND_IDENT_NUMBER_OF_SOLUTIONS );
  if( ind_ident_number_of_solutions_list->getLength() > 0 )
    {
      DOMElement* ind_ident_number_of_solutions = dynamic_cast<DOMElement*>( ind_ident_number_of_solutions_list->item(0) );
      DOMNodeList* value_list = ind_ident_number_of_solutions->getElementsByTagName( XML.X_VALUE );
      int n = value_list->getLength();
      CPPUNIT_ASSERT_EQUAL( 1, n );
      nGroebnerBasisSoln = atoi( XMLString::transcode( value_list->item(0)->getFirstChild()->getNodeValue() ) );
    }

  CPPUNIT_ASSERT_EQUAL( nGroebnerBasisSolnKnown, nGroebnerBasisSoln );


  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Verify that the identifiability status was returned. 
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  DOMNodeList * ind_ident_status_list = report->getElementsByTagName( XML.X_IND_IDENT_STATUS );
  CPPUNIT_ASSERT( ind_ident_status_list->getLength() != 0 );


  okToClean = true;
}

CppUnit::Test * ind_identTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ind_identTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testIndDataClass", 
         &ind_identTest::testIndDataClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testDataSetClass", 
         &ind_identTest::testDataSetClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testNonmemPars", 
         &ind_identTest::testNonmemPars ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testIdentPredClass", 
         &ind_identTest::testIdentPredClass ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testDriver", 
         &ind_identTest::testDriver ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<ind_identTest>(
         "testReportML", 
         &ind_identTest::testReportML ) );

  return suiteOfTests;
}

