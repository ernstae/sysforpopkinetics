#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "NonmemTranslatorPopTest.h"
#include <spkcompiler/series.h>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <spkcompiler/nonmem/NonmemTranslator.h>
#include <spkcompiler/SymbolTable.h>

using namespace std;
using namespace CppUnit;
using namespace xercesc;

namespace{
  const unsigned int maxChars = 2047;

  bool okToClean;

  char fPrefix[128];
  char fDataML[128];
  char fSourceML[128];
  char fIndDataDriver[128];
  char fIndDataDriver_cpp[128];
  char fDataSetDriver[128];
  char fDataSetDriver_cpp[128];
  char fPredDriver[128];
  char fPredDriver_cpp[128];
  char fSpkMakefile[128];
  char fSpkDriver[128];
  char fSpkDriver_cpp[128];
  char fReportML[128];

  //============================================
  // Optimizer controls
  //============================================
  const int  mitr       = 100;
  const bool isEstimate = true;
  const char approx[]   = "fo";

  const char * strID    = "ID";
  const char * strTIME  = "TIME";
  const char * strCP    = "CP";
  const char * strDV    = "DV";

  const char * strKA    = "ka";
  const char * strKE    = "ke";
  const char * strF     = "F";
  const char * strY     = "Y";
  const char * strTHETA = "THETA";
  const char * strOMEGA = "OMEGA";
  const char * strSIGMA = "SIGMA";
  const char * strETA   = "ETA";
  const char * strEPS   = "EPS";
  const char * strPRED  = "PRED";
  const char * strRES   = "RES";
  const char * strWRES  = "WRES";
  const char * strMDV   = "MDV";
  const char * strSIMDV = "SIMDV";

  //=====================================================
  // Instanciate the NONMEM translator.
  // It initializes the symbol table and allows us to
  // handle it.
  // Fill the symbol table with data records as if
  // an spkdataML document has been parsed.
  //
  // Population size:          3
  // Data labels/items:        [ ID, TIME CP/DV ]
  //
  // Data Set2
  // --------
  //  
  //   ID      TIME     CP=DV    (MDV)
  //   1       0.0       0.0      0
  //   2       0.0       0.0      0
  //   2       1.0      10.0      0
  //   3       0.0       0.0      0
  //   3       1.0      10.0      0
  //   3       2.0      20.0      0
  //   4       0.0       0.0      0
  //   4       1.0      10.0      0
  //   4       2.0      20.0      0
  //   4       3.0      25.0      0
  //=====================================================
  const int pop_size   = 4;
  const int nLabels    = 4;
  const char * label[] = { strID, strTIME, strCP, strMDV };
  map<const char*, const char*> label_alias;

  const int    nRecords  = 10;
  const int    nItems    = nLabels;
  valarray<int> N( pop_size );
  const double record0[] = { 1, 0.0,  0.0, 0 };
  const double record1[] = { 2, 0.0,  0.0, 0 };
  const double record2[] = { 2, 1.0, 10.0, 0 };
  const double record3[] = { 3, 0.0,  0.0, 0 };
  const double record4[] = { 3, 1.0, 10.0, 0 };
  const double record5[] = { 3, 2.0, 20.0, 0 };
  const double record6[] = { 4, 0.0,  0.0, 0 };
  const double record7[] = { 4, 1.0, 10.0, 0 };
  const double record8[] = { 4, 2.0, 20.0, 0 };
  const double record9[] = { 4, 3.0, 25.0, 0 };
  double const * record[nRecords];

  //=====================================================
  // Set up the test parameters
  // --------------------------
  //
  // theta (initial):          [ 1, 2, 3 ]
  // theta (upper):            [ 11, 12, 13 ]
  // theta (lower):            [ -9, -8, -7 ]
  // theta (fixed?):           [ F, T, F ]
  // Omega (initial):          /      \
  //                           | 1  0 |
  //                           | 0  2 |
  //                           \      /
  // Omega (fixed?):           [ F, T ]
  // Sigma (initial):          /      \
  //                           | 1  0 |
  //                           | 0  1 |
  //                           \      /
  // eta   (initial):          [ 0.0, 0.0 ]
  // eta   (fixed?):           [ F, F ]
  // eps   (initial):          [ 0.0, 0.0 ]
  // eps   (fixed?):           [ F, F ]
  //
  // Covariance form:          R
  // Standard error out?:      yes
  // Coefficient of variation? yes
  // Confidence interval?      yes
  // Covariance?               yes
  // Inverse of covariance     no
  // Correlation?              yes
  //
  // Data simulation?          yes
  // Seed                      1
  //
  // PRED model:               KA=THETA(1) + ETA(1)
  //                           KE=THETA(2) + ETA(2)
  //                           F=KE*KA
  //                           Y=F+EPS(1)+EPS(2)
  //
  //=====================================================

  const int  sig_digits = 3;
  const char PRED[]     = "ka = THETA(1) + ETA(1)\nke = THETA(2) + ETA(2)\nF = ke * ka\nY = F + EPS(1) + EPS(2)\n";

  const int thetaLen    = 2;
  vector<double> theta_in (thetaLen);
  vector<double> theta_up (thetaLen);
  vector<double> theta_low(thetaLen);
  vector<bool>   theta_fix(thetaLen);

  const int omegaDim = 2;
  Symbol::Structure omegaStruct;
  int omegaOrder;
  vector<double> omega_in;
  vector<bool>   omega_fix;

  const int sigmaDim = 2;
  Symbol::Structure sigmaStruct;
  int sigmaOrder;
  vector<double> sigma_in;
  vector<bool>   sigma_fix;

  int etaLen;
  vector<double> eta_in;
  vector<bool>   eta_fix;

  int epsLen;
  vector<double> eps_in;
  vector<bool>   eps_fix;

  bool pop_stderr         = false;
  bool pop_coefficient    = false;
  bool pop_confidence     = false;
  bool pop_covariance     = false;
  bool pop_inv_covariance = false;
  bool pop_correlation    = false;

  string pop_cov_form     = "r";
  bool isSimulate         = false;
  const int seed          = 1;
};

void NonmemTranslatorPopTest::setUp()
{
  okToClean = false;

  sprintf( fPrefix,            "pop" );
  sprintf( fDataML,            "%s_dataML.xml", fPrefix );
  sprintf( fSourceML,          "%s_sourceML.xml", fPrefix );
  sprintf( fIndDataDriver,     "%s_IndDataDriver", fPrefix );
  sprintf( fIndDataDriver_cpp, "%s_IndDataDriver.cpp", fPrefix );
  sprintf( fDataSetDriver,     "%s_DataSetDriver", fPrefix );
  sprintf( fDataSetDriver_cpp, "%s_DataSetDriver.cpp", fPrefix );
  sprintf( fPredDriver,        "%s_PredDriver", fPrefix );
  sprintf( fPredDriver_cpp,    "%s_PredDriver.cpp", fPrefix );

  sprintf( fSpkDriver,         "spkDriver" );
  sprintf( fSpkDriver_cpp,     "spkDriver.cpp" );
  sprintf( fSpkMakefile,       "Makefile.SPK" );

  label_alias[strID]   = NULL;
  label_alias[strTIME] = NULL;
  label_alias[strCP]   = strDV;
  for( int i=0; i<pop_size; i++ )
     N[i] = i+1;
  
  record[0] = record0;
  record[1] = record1;
  record[2] = record2;
  record[3] = record3;
  record[4] = record4;
  record[5] = record5;
  record[6] = record6;
  record[7] = record6;
  record[8] = record6;
  record[9] = record6;

  for( int i=0; i<thetaLen; i++ )
    {
      theta_in[i]  =  i+1;
      theta_up[i]  = +10.0 * theta_in[i];
      theta_low[i] = -10.0 * theta_in[i];
      theta_fix[i] = ( i%2==0? true : false );
    }

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
      char buf[maxChars + 1];
      sprintf( buf, "Error during Xerces-c initialization.\nException message: %s.\n", 
               XMLString::transcode( toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

}
void NonmemTranslatorPopTest::tearDown()
{
  if( okToClean )
    {

      remove( fDataML );
      remove( fSourceML );
      remove( fIndDataDriver );
      remove( fIndDataDriver_cpp );
      remove( fDataSetDriver );
      remove( fDataSetDriver_cpp );
      remove( fPredDriver );
      remove( fPredDriver_cpp );
      remove( fSpkDriver );
      remove( "driver.cpp" );
      remove( "IndData.h" );
      remove( "DataSet.h" );
      remove( "Pred.h" );
      remove( "predEqn.cpp" );
      remove( fSpkMakefile );
      remove( fReportML );
    } 
                                                                          
  XMLPlatformUtils::Terminate();

}
void NonmemTranslatorPopTest::diagOmegaDiagSigma()
{
  omegaStruct = Symbol::DIAGONAL;
  omegaOrder  = ( omegaStruct == Symbol::DIAGONAL? omegaDim : series(1,1,omegaDim) );
  omega_in    .resize( omegaOrder );
  omega_fix   .resize( omegaOrder );

  sigmaStruct = Symbol::DIAGONAL;
  sigmaOrder  = ( sigmaStruct == Symbol::DIAGONAL? sigmaDim : series(1,1,sigmaDim) );
  sigma_in    .resize( sigmaOrder );
  sigma_fix   .resize( sigmaOrder );

  etaLen = omegaOrder;
  eta_in  .resize( etaLen );
  eta_fix .resize( etaLen );

  epsLen = sigmaOrder;
  eps_in  .resize( epsLen );
  eps_fix .resize( epsLen );

  for( int i=0; i<omegaOrder; i++ )
    {
      omega_in[i]  = i+1;
      omega_fix[i] = ( i%2==0? true : false );
    }
  for( int i=0; i<sigmaOrder; i++ )
    {
      sigma_in[i]  = i+1;
      sigma_fix[i] = ( i%2==0? true : false );
    }
  fill( eta_in.begin(), eta_in.end(), 0.0 );
  for( int i=0; i<etaLen; i++ )
    eta_fix[i] = false;

  fill( eps_in.begin(), eps_in.end(), 0.0 );
  for( int i=0; i<epsLen; i++ )
    eps_fix[i] = false;

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Generating a dataML document
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ofstream oData( fDataML );
  CPPUNIT_ASSERT( oData.good() );
  oData << "<spkdata version=\"0.1\">" << endl;
  oData << "<table columns=\"" << nLabels << "\" rows=\"" << nRecords + 1 << "\">" << endl;
  oData << "<description>" << endl;
  oData << "Diagonal Sigma and Diagonal Omega" << endl;
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
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred during parsing %s.\n   Message: %s\n",
	       fDataML, XMLString::transcode(e.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      XMLCh errText[maxChars + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
	{
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fDataML, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred during parsing %s.\n", fDataML );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  
  //=====================================================
  // Generate a sourceML document.
  //=====================================================
  ofstream oSource( fSourceML );
  assert( oSource.good() );
  oSource << "<spksource>" << endl;
  oSource << "<nonmem>" << endl;
      
  oSource << "<constraint>" << endl;
  // default: is_eta_out=no, is_restart=yes
  oSource << "<pop_analysis approximation=\"" << approx << "\" pop_size=\"" << pop_size << "\" ";
  oSource << "is_estimation=\"" << (isEstimate? "yes":"no") << "\" mitr =\"" << mitr << "\"" << " sig_digits=\"" << sig_digits << "\">" << endl;
  oSource << "<data_labels>" << endl;

  const char * alias = NULL;
  for( int i=0; i<nLabels; i++ )
    {
      oSource << "<label name=";
      oSource << "\"" << label[i] << "\"";
      if( (alias = label_alias[ label[i] ]) != NULL )
	oSource << " synonym=\"" << alias << "\"";
      oSource << "/>" << endl;
    }

  oSource << "</data_labels>" << endl;
  oSource << "<theta length=\"" << thetaLen << "\">" << endl;
  oSource << "<in>" << endl;
  for( int i=0; i<thetaLen; i++ )
    {
      oSource << "<value";
      if( theta_fix[i] )
	oSource << " fixed=\"yes\"";
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
      if( omega_fix[i] )
	oSource << " fixed=\"yes\"";
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
      if( sigma_fix[i] )
	oSource << " fixed=\"yes\"";
      oSource << ">" << sigma_in[i] << "</value>" << endl;
    }
  oSource << "</in>" << endl;
  oSource << "</sigma>" << endl;

  if( pop_stderr || pop_covariance || pop_inv_covariance || pop_confidence || pop_coefficient || pop_correlation )
    {
      oSource << "<pop_stat covariance_form=\"" << pop_cov_form << "\" ";
      oSource << "is_stderror_out=\""           << (pop_stderr?         "yes":"no") << "\" ";
      oSource << "is_covariance_out=\""         << (pop_covariance?     "yes":"no") << "\" ";
      oSource << "is_inverse_covariance_out=\"" << (pop_inv_covariance? "yes":"no") << "\" ";
      oSource << "is_confidence_out=\""         << (pop_confidence?     "yes":"no") << "\" ";
      oSource << "is_coefficient_out=\""        << (pop_coefficient?    "yes":"no") << "\" ";
      oSource << "is_correlation_out=\""        << (pop_correlation?    "yes":"no") << "\"/>" << endl;
    }
  if( isSimulate )
    {
      oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
    }
  oSource << "</pop_analysis>" << endl;
  oSource << "</constraint>" << endl;
      
  oSource << "<model>" << endl;
  oSource << "<pred>" << endl;
  oSource << PRED << endl;
  oSource << "</pred>" << endl;
  oSource << "</model>" << endl;
      
  oSource << "<presentation>" << endl;
  oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
  oSource << "<column label=\"" << strTHETA << "(1)\" appearance_order=\"2\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(3)\" appearance_order=\"4\"/>" << endl;
  oSource << "<column label=\"" << strTHETA << "(2)\" appearance_order=\"3\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "<table header=\"every\">" << endl;
  oSource << "<column label=\"" << strDV << "\" appearance_order=\"2\"/>" << endl;
  oSource << "</table>" << endl;
  oSource << "</presentation>" << endl;
      
  oSource << "</nonmem>" << endl;
  oSource << "</spksource>" << endl;

  oSource.close();

  xercesc::XercesDOMParser *sourceParser = new xercesc::XercesDOMParser;
  sourceParser->setValidationScheme( XercesDOMParser::Val_Auto );
  sourceParser->setDoNamespaces( true );
  sourceParser->setDoSchema( true );
  sourceParser->setValidationSchemaFullChecking( true );
  sourceParser->setCreateEntityReferenceNodes( true );
  
  //=====================================================
  // Let the Xerces DOM parser parse the sourceML document.
  //=====================================================
  try{
    sourceParser->parse( fSourceML );
    source = sourceParser->getDocument();
  }
  catch( const XMLException& e )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An error occurred during parsing\n   Message: %s\n",
	       XMLString::transcode(e.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  catch( const DOMException& e )
    {
      XMLCh errText[maxChars + 1]; 
      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
	{
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   fSourceML, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
	}
    }
  catch( ... )
    {
      XMLPlatformUtils::Terminate();
      char buf[maxChars + 1];
      sprintf( buf, "An unknown error occurred during parsing.\n" );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  
  //=====================================================
  // Instanciate the NONMEM translator.
  // It initializes the symbol table and allows us to
  // handle it.
  //=====================================================
  NonmemTranslator xlator( source, data );

  //=====================================================
  // Parse the dataML document
  //=====================================================
  xlator.parseData();
  SymbolTable *table = xlator.getSymbolTable();
//=====================================================
  // Parse the sourceML document.
  // Upon the successful return, the following files
  // shall be generated:
  //   * driver.cpp          --- SPK driver
  //   * Makefile.SPK        --- Make file
  //   * Pred.h              --- Def. of Pred class
  //   * DataSet.h           --- Def. of DataSet class
  //   * IndData.h           --- Def. of IndData class
  //=====================================================
  xlator.parseSource();

  //=====================================================
  // Test the contents of the symbol table after
  // parsing the sourceML document.  WRES/RES/PRED
  // should have been added for table/scatterplot
  // default requirements.
  // THETA/ETA/SIGMA/OMEGA/KA/KE/F/Y for PRED.
  //=====================================================
  map<const char*, const char*>::const_iterator pLabel = label_alias.begin();
  CPPUNIT_ASSERT( table->findi(strID)   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi(strTIME) != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi(strCP)   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi(strDV)   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi(strMDV)  != Symbol::empty() ); // from data set

  CPPUNIT_ASSERT( table->findi(strPRED) != Symbol::empty() ); // for table/scatterplot
  CPPUNIT_ASSERT( table->findi(strWRES) != Symbol::empty() ); // for table/scatterplot
  CPPUNIT_ASSERT( table->findi(strRES)  != Symbol::empty() ); // for table/scatterplot

  CPPUNIT_ASSERT( table->findi(strTHETA)!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strETA)  != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strEPS)  != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strOMEGA)!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strSIGMA)!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strKA)   != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strKE)   != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strF)    != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi(strY)    != Symbol::empty() ); // from PRED

  Symbol *theta = table->findi( strTHETA );
  CPPUNIT_ASSERT( theta != Symbol::empty() );
  CPPUNIT_ASSERT_EQUAL( thetaLen, theta->dimension[0] );
  CPPUNIT_ASSERT_EQUAL( thetaLen, static_cast<int>( theta->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( thetaLen, static_cast<int>( theta->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( thetaLen, static_cast<int>( theta->lower[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( thetaLen, static_cast<int>( theta->fixed[0].size() ) );

  for( int i=0; i<thetaLen; i++ )
    {
      CPPUNIT_ASSERT( theta_fix[i] == theta->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( theta_in[i],  atof( theta->initial[0][i].c_str() ) );
      CPPUNIT_ASSERT_EQUAL( theta_low[i], atof( theta->lower[0][i].c_str() ) );
      CPPUNIT_ASSERT_EQUAL( theta_up[i],  atof( theta->upper[0][i].c_str() ) );
    }
  
  Symbol *omega = table->findi( strOMEGA );
  CPPUNIT_ASSERT( omega != Symbol::empty() );
  CPPUNIT_ASSERT( omega->structure == omegaStruct );
  CPPUNIT_ASSERT_EQUAL( omegaDim, omega->dimension[0] );
  CPPUNIT_ASSERT_EQUAL( omegaOrder, static_cast<int>( omega->fixed[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaOrder, static_cast<int>( omega->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaOrder, static_cast<int>( omega->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaOrder, static_cast<int>( omega->lower[0].size() ) );

  for( int i=0; i<omegaOrder; i++ )
    {

      CPPUNIT_ASSERT( omega_fix[i] == omega->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( omega_in[i],  atof( omega->initial[0][i].c_str() ) );
   }

  Symbol *sigma = table->findi( strSIGMA );
  CPPUNIT_ASSERT( sigma != Symbol::empty() );
  CPPUNIT_ASSERT( sigma->structure == sigmaStruct );
  CPPUNIT_ASSERT_EQUAL( sigmaDim, sigma->dimension[0] );
  for( int i=0; i<sigmaOrder; i++ )
    {

      CPPUNIT_ASSERT( sigma_fix[i] == sigma->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( sigma_in[i],  atof( sigma->initial[0][i].c_str() ) );
   }

  Symbol *eta = table->findi( strETA );
  CPPUNIT_ASSERT( eta != Symbol::empty() );
  CPPUNIT_ASSERT_EQUAL( etaLen, static_cast<int>( eta->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( etaLen, static_cast<int>( eta->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( etaLen, static_cast<int>( eta->lower[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( etaLen, static_cast<int>( eta->fixed[0].size() ) );
  for( int i=0; i<etaLen; i++ )
    {      
      CPPUNIT_ASSERT( eta_fix[i] == eta->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( eta_in[i],  atof( eta->initial[0][i].c_str() ) );
    }

  Symbol *eps = table->findi( strEPS );
  CPPUNIT_ASSERT( eta != Symbol::empty() );
  CPPUNIT_ASSERT_EQUAL( epsLen, static_cast<int>( eps->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( epsLen, static_cast<int>( eps->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( epsLen, static_cast<int>( eps->lower[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( epsLen, static_cast<int>( eps->fixed[0].size() ) );
  for( int i=0; i<epsLen; i++ )
    {      
      CPPUNIT_ASSERT_EQUAL( eps_in[i],  atof( eps->initial[0][i].c_str() ) );
    }
  
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test IndData class to see if it has all necessary 
  // variables declared and sized.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //=====================================================
  //  Test if the generated IndData.h defines
  //  a correct IndData class.
  //  The IndData class in this particular test case
  //  should have all of the followings as class members:
  //  
  //  - Read-only Data Items
  //  * id      : S^n
  //  * time    : R^n
  //  * cp = dv : R^n
  //  * mdv     : I^n
  // 
  //  - Pred::eval() variables
  //  * pred    : R^n
  //  * res     : R^n
  //  * theta   : R^nTheta*n
  //  * eta     : R^nEta*n, where nEta = nTheta
  //  * eps     : R^orderSigma*n
  //
  //  - Variables external to Pred::eval()
  //  * omega   : R^orderOmega*n
  //  * sigma   : R^orderSigma*n
  //  * wres    : R^n
  //
  //  - User defined
  //  * ka      : R^n
  //  * ke      : R^n
  //  * f       : R^n
  //  * y       : R^n
  //=====================================================
  ofstream oIndDataDriver( fIndDataDriver_cpp );
  CPPUNIT_ASSERT( oIndDataDriver.good() );

  oIndDataDriver << "#include <cstdlib>" << endl;
  oIndDataDriver << "#include <vector>" << endl;
  oIndDataDriver << "#include <iostream>" << endl;
  oIndDataDriver << "#include <sys/signal.h>" << endl;
  oIndDataDriver << "#include \"IndData.h\"" << endl;
  oIndDataDriver << "using namespace std;" << endl;
  oIndDataDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oIndDataDriver << "   if( actual != expected ) \\" << endl;
  oIndDataDriver << "   { \\" << endl;
  oIndDataDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oIndDataDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oIndDataDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oIndDataDriver << "      raise( SIGABRT ); \\" << endl;
  oIndDataDriver << "   } " << endl;
  oIndDataDriver << endl;
  oIndDataDriver << "int main()" << endl;
  oIndDataDriver << "{" << endl;
  oIndDataDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oIndDataDriver << "   const int etaLen   = " << etaLen   << ";" << endl;
  oIndDataDriver << "   const int epsLen   = " << epsLen   << ";" << endl;
  for( int i=0; i<pop_size; i++ )
    {
      oIndDataDriver << "   vector<char*> " << label[0] << i+1 << "(" <<  N[i] << ");" << endl;
      for( int h=1; h<nLabels; h++ )
	{
	  oIndDataDriver << "   vector<double> " << label[h] << i+1 << "(" <<  N[i] << ");" << endl;
	}
      for( int j=0; j<N[i]; j++ )
	{
	  oIndDataDriver << "   " << label[0] << i+1 << "[" << j << "] = \"" << record[i][0] << "\";" << endl;
	  for( int h=1; h<nLabels; h++ )
	    {
	      oIndDataDriver << "   " << label[h] << i+1 << "[" << j << "] = " << record[i][h] << ";" << endl;
	    }
	}
      oIndDataDriver << endl;
    }

  for( int i=0; i<pop_size; i++ )
    {
      oIndDataDriver << "   IndData<double> subject" << i+1 << "( " << N[i];
      for( int j=0; j<nLabels; j++ )
	oIndDataDriver << ", " << label[j] << i+1;
      oIndDataDriver << " );" << endl;
    }
  oIndDataDriver << endl;

  for( int i=0; i<pop_size; i++ )
    {
      for( int j=0; j<N[i]; j++ )
	{
	  oIndDataDriver << "   assert( strcmp( subject" << i+1 << "." << strID << "[" << j << "], ";
	  oIndDataDriver << label[0] << i+1 << "[" << j << "] ) == 0 );" << endl;
	  for( int h=1; h<nLabels; h++ )
	    {
	      oIndDataDriver << "   MY_ASSERT_EQUAL( " << label[h] << i+1 << "[" << j << "], ";
              oIndDataDriver << "subject" << i+1 << "." << label[h] << "[" << j << "] );" << endl;
	    }
	}
      oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, subject" << i+1 << "." << strTHETA << "[" << i << "].size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   subject" << i+1 << "." << strEPS   << "[" << i << "].size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   subject" << i+1 << "." << strETA   << "[" << i << "].size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strRES   << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strWRES  << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strPRED  << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strF     << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strY     << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strKA    << ".size() );" << endl;
      oIndDataDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", subject" << i+1 << "." << strKE    << ".size() );" << endl;
      oIndDataDriver << endl;
    }
  oIndDataDriver << "}" << endl;
  oIndDataDriver.close();

  char command[256];
  sprintf( command, "\necho --- %s ---\n", fIndDataDriver );
  system( command );
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fIndDataDriver_cpp, fIndDataDriver );
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test DataSet class to see if it has the-only individual's
  // data set correctly.
  //
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ofstream oDataSetDriver( fDataSetDriver_cpp );
  CPPUNIT_ASSERT( oDataSetDriver.good() );

  oDataSetDriver << "#include <cstdlib>" << endl;
  oDataSetDriver << "#include <vector>" << endl;
  oDataSetDriver << "#include <iostream>" << endl;
  oDataSetDriver << "#include <sys/signal.h>" << endl;
  oDataSetDriver << "#include \"DataSet.h\"" << endl;
  oDataSetDriver << "using namespace std;" << endl;
  oDataSetDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oDataSetDriver << "   if( actual != expected ) \\" << endl;
  oDataSetDriver << "   { \\" << endl;
  oDataSetDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oDataSetDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oDataSetDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oDataSetDriver << "      raise( SIGABRT ); \\" << endl;
  oDataSetDriver << "   } " << endl;
  oDataSetDriver << endl;
  oDataSetDriver << "int main()" << endl;
  oDataSetDriver << "{" << endl;

  oDataSetDriver << "   DataSet<double> set;" << endl;
  oDataSetDriver << endl;

  oDataSetDriver << "   const int thetaLen = " << thetaLen << ";" << endl;
  oDataSetDriver << "   const int etaLen   = " << etaLen   << ";" << endl;
  oDataSetDriver << "   const int epsLen   = " << epsLen   << ";" << endl;
  oDataSetDriver << endl;

  for( int i=0, cnt=0; i<pop_size; i++ )
    {
      for( int j=0; j<N[i]; j++, cnt++ )
	{
	  oDataSetDriver << "   assert( strcmp( set.data[" << i << "]->" << label[0] << "[" << j << "], ";
	  oDataSetDriver << "\"" << record[cnt][0] << "\"" << " ) == 0 );" << endl;

	  for( int h=1; h<nLabels; h++ )
	    {
	      oDataSetDriver << "   MY_ASSERT_EQUAL( " << record[cnt][h] << ", ";
              oDataSetDriver << "set.data[" << i << "]->" << label[h] << "[" << j << "] );" << endl;
	    }
	}
      oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[" << i << "]->" << strTHETA << "[" << i << "].size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[" << i << "]->" << strEPS   << "[" << i << "].size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[" << i << "]->" << strETA   << "[" << i << "].size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strRES << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strWRES << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strPRED << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strF << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strY << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strKA << ".size() );" << endl;
      oDataSetDriver << "   MY_ASSERT_EQUAL( " << N[i] << ", set.data[" << i << "]->" << strKE << ".size() );" << endl;
      oDataSetDriver << endl;
    }
  oDataSetDriver << "}" << endl;
  
  oDataSetDriver.close();

  sprintf( command, "\necho --- %s ---\n", fDataSetDriver );
  system( command );
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fDataSetDriver_cpp, fDataSetDriver );
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

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Test Pred class to see if it has defined eval() properly.
  // Especially, the proper elements of the dependent variable-
  // vector given as an argument are replaced by the computed 
  // value of Y(j) and F(j).
  // Also, make sure the currently computed values, all of them,
  // are stored in memory for potential retrieval from the 
  // outside.
  //
  // $PRED
  //   KA=THETA(1) + ETA(1)
  //   KE=THETA(2) + ETA(2)
  //   F=KE*KA
  //   Y=F+EPS(1)
  //    
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ofstream oPredDriver( fPredDriver_cpp );
  CPPUNIT_ASSERT( oPredDriver.good() );

  oPredDriver << "#include \"Pred.h\"" << endl;
  oPredDriver << "#include \"DataSet.h\"" << endl;
  oPredDriver << "#include <cppad/include/CppAD.h>" << endl;
  oPredDriver << "#include <spkpred/PredBase.h>" << endl;
  oPredDriver << "#include <vector>" << endl;
  oPredDriver << "#include <sys/signal.h>" << endl;
  oPredDriver << "using namespace std;" << endl;
  oPredDriver << "#define MY_ASSERT_EQUAL( expected, actual ) \\" << endl;
  oPredDriver << "   if( actual != expected ) \\" << endl;
  oPredDriver << "   { \\" << endl;
  oPredDriver << "      cerr << __FILE__ << \"(\" << __LINE__ << \"): \"; \\" << endl;
  oPredDriver << "      cerr << \"Expected \" << expected; \\" << endl;
  oPredDriver << "      cerr << \" but was \" << actual << endl; \\" << endl;
  oPredDriver << "      raise( SIGABRT ); \\" << endl;
  oPredDriver << "   } " << endl;
  oPredDriver << endl;
  oPredDriver << "int main()" << endl;
  oPredDriver << "{" << endl;
  oPredDriver << "   bool ok = true;" << endl;
  oPredDriver << "   DataSet< CppAD::AD<double> > set;" << endl;
  oPredDriver << "   Pred< CppAD::AD<double> > pred( &set );" << endl;
  oPredDriver << "   const int nIndividuals = " << pop_size << ";" << endl;
  oPredDriver << "   vector<int> N(nIndividuals); // numbers of measurements" << endl;
  for( int i=0; i<pop_size; i++ )
    {
      oPredDriver << "N[" << i << "] = " << N[i] << ";" << endl;
    }
  
  oPredDriver << "   const int thetaLen    = " << thetaLen << ";" << endl;
  oPredDriver << "   const int etaLen      = " << etaLen << ";" << endl;
  oPredDriver << "   const int epsLen      = " << epsLen << ";" << endl;
  oPredDriver << "   const int thetaOffset = 0;" << endl;
  oPredDriver << "   const int etaOffset   = thetaLen;" << endl;
  oPredDriver << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > depVar( " << N.sum() << " * 2 );" << endl;
  oPredDriver << "   fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  oPredDriver << "   fill( depVar.begin(),   depVar.end(), 0.0 );" << endl;
  oPredDriver << "   const double C1       = 1.0;" << endl;
  oPredDriver << "   const double C2       = 2.0;" << endl;
  //---------------------------------------------------------------------------------
  // A complete iteration over j
  //
  oPredDriver << endl;
  oPredDriver << "   double expectedF, actualF, expectedY, actualY;" << endl;
  oPredDriver << "   for( int i=0; i<" << pop_size << "; i++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      int n = N[i];" << endl;
  oPredDriver << "      depVar.resize( n * 2 );" << endl;
  oPredDriver << "      int fOffset = 0;" << endl;
  oPredDriver << "      int yOffset = n;" << endl;
  oPredDriver << "      for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "      {" << endl;
  oPredDriver << "         indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  oPredDriver << "         indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
  oPredDriver << "         indepVar[thetaOffset+2] = C1*j; // theta(3)" << endl;
  oPredDriver << "         indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  oPredDriver << "         indepVar[etaOffset  +1] = C1*j; // eta(2)" << endl;
  oPredDriver << "         indepVar[epsOffset  +0] = C1*j; // eps(1)" << endl;
  oPredDriver << "         pred.eval( thetaOffset, thetaLen," << endl;
  oPredDriver << "                    etaOffset,   etaLen," << endl;
  oPredDriver << "                    epsOffset,   epsLen ," << endl;
  oPredDriver << "                    fOffset,     n, " << endl;
  oPredDriver << "                    yOffset,     n, " << endl;
  oPredDriver << "                    i, j, " << endl;
  oPredDriver << "                    indepVar, depVar );" << endl;
  // Test if F(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "         actualF   = CppAD::Value(depVar[ fOffset + j ]);" << endl;
  oPredDriver << "         expectedF = CppAD::Value(indepVar[thetaOffset+0] + indepVar[etaOffset+0] )" << endl;
  oPredDriver << "                   * CppAD::Value(indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( expectedF, actualF );" << endl;
  // Test if Y(j) gets placed in the proper location in the depVar vector.
  oPredDriver << "         actualY   = CppAD::Value(depVar[ yOffset + j ]);" << endl;
  oPredDriver << "         expectedY = expectedF + CppAD::Value(indepVar[epsOffset+0]);" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( expectedY, actualY );" << endl;
  oPredDriver << "      }" << endl;
  oPredDriver << "   } // End of the first complete iteration over i and j" << endl;
  // Test if the DataSet objects hold the complete set of computed values from the just-finished iteration.
  oPredDriver << "   for( int i=0; i<3; i++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      int n = N[i];" << endl;
  oPredDriver << "      for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "      {" << endl;
  oPredDriver << "         indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  oPredDriver << "         indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
  oPredDriver << "         indepVar[thetaOffset+2] = C1*j; // theta(3)" << endl;
  oPredDriver << "         indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  oPredDriver << "         indepVar[etaOffset  +1] = C1*j; // eta(2)" << endl;
  oPredDriver << "         indepVar[epsOffset  +0] = C1*j; // eps(1)" << endl;
  oPredDriver << "         expectedF = CppAD::Value(indepVar[thetaOffset+0] + indepVar[etaOffset+0] )" << endl;
  oPredDriver << "                   * CppAD::Value(indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  oPredDriver << "         double pred =expectedF;" << endl;
  for( int i=0; i<thetaLen; i++ )
    oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->" << strTHETA << "[j][" << i << "] );" << endl;
  for( int i=0; i<etaLen; i++ )
    oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->" << strETA << "[j][" << i << "] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( pred, set.data[i]->" << strPRED << "[j] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( pred, set.data[i]->" << strF << "[j] );" << endl;
  oPredDriver << "      }" << endl;
  oPredDriver << "   }" << endl;
  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  oPredDriver << "   return !ok;" << endl;
  oPredDriver << "}" << endl;
  oPredDriver.close();

  sprintf( command, "\necho --- %s ---\n", fPredDriver );
  system( command );
  sprintf( command, "g++ -g %s -o %s  -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -I/usr/local/include/spktest", fPredDriver_cpp, fPredDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fPredDriver_cpp );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  sprintf( command, "./%s", fPredDriver );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "A test driver, %s, failed!", fPredDriver );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Test driver.cpp to see if it compiles/links successfully.
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  int  exitcode      = 0;

  sprintf( command, "make -f %s test", fSpkMakefile );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fSpkDriver_cpp );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }

  sprintf( command, "./%s", fSpkDriver );
  
  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for convergence problem <%d>!", fSpkDriver, exitcode );
     CPPUNIT_ASSERT_MESSAGE( message, true );
    }
  if( exitcode == 2 )
    {
      char message[256];
      sprintf( message, "%s failed due to inproper file access permission <%d>!", fSpkDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode > 2 )
    {
      char message[256];
      sprintf( message, "%s failed for reasons other than convergence propblem or access permission <%d>!", fSpkDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, true );
    }

  /*
  //=====================================================
  // Test the generated C++ source code files
  // IndData.h, IndData.cpp, DataSet.h, DataSet.cpp
  //=====================================================
  // The order in which the variables appear in
  // the IndData constructor must be consistent with
  // with the order in which the variables are actually
  // passed in the construction of these objects
  // done in the DataSet constructor.
  char fTestPred[] = "testPopPred.cpp";
  ofstream oTestPred( fTestPred );
  if( oTestPred.good() )
  {
     oTestPred << "#include <iostream>" << endl;
     oTestPred << "#include <vector>" << endl;
     oTestPred << "#include <cppad/include/CppAD.h>" << endl;
     oTestPred << "#include \"IndData.h\"" << endl;
     oTestPred << "#include \"DataSet.h\"" << endl;
     oTestPred << "#include \"Pred.h\"" << endl;
     oTestPred << endl;
     oTestPred << "using namespace std;" << endl;
     oTestPred << endl;
     oTestPred << "int main( int argc, const char* argv[] )" << endl;
     oTestPred << "{" << endl;
     oTestPred << "  //////////////////////////////////////////////////////////" << endl;
     oTestPred << "  // *** Setting up a data set ***" << endl;
     oTestPred << "  //" << endl;
     oTestPred << "  const int  nIndividuals = " << pop_size << ";" << endl;
     oTestPred << "  const int  N[] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << N[i];
       }
     oTestPred << " };" << endl;

     for( int i=0; i<pop_size; i++ )
       {
	 oTestPred << "  const char ID" << i << "[] = ";
	 oTestPred << "\"" << id->initial[i][0] << "\";" << endl;
       }
     oTestPred << "  const char *ID[] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "ID" << i;
       }
     oTestPred << " };" << endl;
     oTestPred << endl; 

     for( int i=0; i<pop_size; i++ )
       {
	 oTestPred << "  double cp_" << i << "  [] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << cp->initial[i][j];
	   }
	 oTestPred << " };" << endl;
 	 oTestPred << "  double *dv_" << i << "    = cp_" << i << ";" << endl;
	 oTestPred << "  double mdv_" << i << " [] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << mdv->initial[i][j];
	   }
	 oTestPred << " };" << endl;
       }

     oTestPred << "  double *cp  [ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "cp_" << i;
       }
     oTestPred << " };" << endl;
     oTestPred << "  double *dv  [ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "dv_" << i;
       }
     oTestPred << " };" << endl;
     oTestPred << "  double *mdv [ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "mdv_" << i;
       }
     oTestPred << " };" << endl;
     oTestPred << endl;

     oTestPred << "  DataSet< CppAD::AD<double> > set;" << endl;

     oTestPred << "  for( int i=0; i<nIndividuals; i++ )" << endl;
     oTestPred << "  {" << endl;
     oTestPred << "    for( int j=0; j<N[i]; j++ )" << endl;
     oTestPred << "    {" << endl;
     oTestPred << "      assert( set.data[i]->id  [j] == string(ID[i]) );"    << endl;
     oTestPred << "      assert( set.data[i]->cp  [j] == cp  [i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->dv  [j] == dv  [i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->mdv [j] == mdv [i][j] );" << endl;
     oTestPred << "    }" << endl;
     oTestPred << "  }" << endl;

     oTestPred << "  //" << endl;
     oTestPred << "  //" << endl;
     oTestPred << "  //////////////////////////////////////////////////////////" << endl;
     oTestPred << endl;

     oTestPred << "  //////////////////////////////////////////////////////////" << endl;
     oTestPred << "  // Testing Pred" << endl;
     oTestPred << "  //" << endl;
     oTestPred << "  Pred< CppAD::AD<double> > pred( &set );" << endl;
     oTestPred << endl;
     oTestPred << "  int thetaLen = 3;" << endl;
     oTestPred << "  int etaLen   = thetaLen;" << endl;
     oTestPred << "  int epsLen   = 2;" << endl;
     oTestPred << "  const int nAlp = thetaLen + epsLen;" << endl;
     oTestPred << "  const int nB   = etaLen;" << endl;
     oTestPred << "  int fLen;" << endl;
     oTestPred << "  int yLen;" << endl;
     oTestPred << endl;
     oTestPred << "  int thetaOffset = 0;" << endl;
     oTestPred << "  int etaOffset   = thetaLen;" << endl;
     oTestPred << "  int epsOffset   = thetaLen + etaLen;" << endl;
     oTestPred << "  int fOffset;" << endl;
     oTestPred << "  int yOffset;" << endl;
     oTestPred << endl;
     oTestPred << "  vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
     oTestPred << "  vector<double> theta( thetaLen );" << endl;
     oTestPred << "  vector<double> eta  ( etaLen );" << endl;
     oTestPred << "  vector<double> eps  ( epsLen );" << endl;
     oTestPred << endl;
     oTestPred << "  double ka;" << endl;
     oTestPred << "  double ke;" << endl;
     oTestPred << endl;
     oTestPred << "  for( int itr=0; itr<2; itr++ )" << endl;
     oTestPred << "    {" << endl;
     oTestPred << "      for( int i=0; i<nIndividuals; i++ )" << endl;
     oTestPred << "	{" << endl;
     oTestPred << "	  fLen    = N[i];" << endl;
     oTestPred << "	  yLen    = N[i];" << endl;
     oTestPred << "	  fOffset = 0;" << endl;
     oTestPred << "	  yOffset = fLen;" << endl;
     oTestPred << "	  vector< CppAD::AD<double> > depVar( fLen + yLen );" << endl;
     oTestPred << "	  fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
     oTestPred << endl;
     oTestPred << "	  for( int k=0; k<thetaLen; k++ )" << endl;
     oTestPred << "	    theta[k] = (k+1+itr)*0.1;" << endl;
     oTestPred << "	  for( int k=0; k<etaLen; k++ )" << endl;
     oTestPred << "	    eta[k]   = (k+1+itr)*10.0;" << endl;
     oTestPred << "	  for( int k=0; k<epsLen; k++ )" << endl;
     oTestPred << "	    eps[k]   = (k+1+itr);" << endl;
     oTestPred << endl;
     oTestPred << "	  copy( theta.begin(), theta.end(), indepVar.begin() );" << endl;
     oTestPred << "	  copy( eta.  begin(), eta.  end(), indepVar.begin() + thetaLen );" << endl;
     oTestPred << "	  copy( eps.  begin(), eps.  end(), indepVar.begin() + thetaLen + etaLen );" << endl;
     oTestPred << endl;
     oTestPred << "	  try{" << endl;
     oTestPred << "	     pred.eval( thetaOffset, thetaLen," << endl;
     oTestPred << "			etaOffset,   etaLen," << endl;
     oTestPred << "			epsOffset,   epsLen," << endl;
     oTestPred << "			fOffset,     fLen," << endl;
     oTestPred << "			yOffset,     yLen," << endl;
     oTestPred << "			i, j," << endl;
     oTestPred << "			indepVar," << endl;
     oTestPred << "	                depVar );" << endl;
     oTestPred << "          }" << endl;
     oTestPred << "	   catch( ... )" << endl;
     oTestPred << "	     {" << endl;
     oTestPred << "	       cerr << \"Pred::eval() threw exception!!!\" << endl;" << endl;
     oTestPred << "	       clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "	       return -1;" << endl;
     oTestPred << "	     }" << endl;
     oTestPred << "	  }" << endl;
     oTestPred << "    }" << endl;
     oTestPred << "  cout << endl;" << endl;
     oTestPred << "  return 0;" << endl;
     oTestPred << "}" << endl;
  }
  else
  {
     char buf[256];
     sprintf( buf, "Failed to open %s as writable.", fTestPred );
     CPPUNIT_ASSERT_MESSAGE( buf, false );
  }
  if( system( "g++ testPopPred.cpp -g -I./ -o testPopPred" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link the generated \"testPopPred.cpp\".", false );
  }
  if( system( "./testPopPred" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "The generated/built \"testPopPred\" failed to run successfully.", false );
  }

  XMLPlatformUtils::Terminate();
  remove( fSourceML );
  //remove( fTestPred );
 
  rename( "spkDriver.cpp", "popSpkDriver.cpp" );

  if( system( "g++ popSpkDriver.cpp -g -lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm -o popDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link the generated \"spkDriver.cpp\".", false );
  }
  if( system( "./popDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "The generated/built \"popSpkDriver\" failed to run successfully.", false );
  }
  */

  okToClean = true;
}
CppUnit::Test * NonmemTranslatorPopTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemTranslatorPopTest" );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorPopTest>(
         "diagOmegaDiagSigma", 
	 &NonmemTranslatorPopTest::diagOmegaDiagSigma ) );
  
  return suiteOfTests;
}

