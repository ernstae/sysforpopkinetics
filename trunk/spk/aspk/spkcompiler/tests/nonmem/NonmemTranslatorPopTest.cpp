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
};

void NonmemTranslatorPopTest::setUp()
{
}
void NonmemTranslatorPopTest::tearDown()
{
}
void NonmemTranslatorPopTest::testParsePopSource()
{
  //=====================================================
  // Set up the test parameters
  // --------------------------
  //
  // Population size:          3
  // Data labels/items:        [ ID, CP/DV ]
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
  // Coefficent of variation?  yes
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
  //                           Y=F+EPS(1)
  //
  //=====================================================
  const int pop_size = 3;
  const int nLabels = 2;
  map<string, string> labels;
  labels["ID"]   = "";
  labels["CP"]   = "DV";

  const int thetaLen = 3;
  vector<double> theta_in (thetaLen);
  vector<double> theta_up (thetaLen);
  vector<double> theta_low(thetaLen);
  vector<bool>   theta_fix(thetaLen);
  for( int i=0; i<thetaLen; i++ )
    {
      theta_in[i]  =  i+1;
      theta_up[i]  = +10.0 * theta_in[i];
      theta_low[i] = -10.0 * theta_in[i];
      theta_fix[i] = ( i%2==0? true : false );
    }

  const int omegaDim = 2;
  const Symbol::Structure omegaStruct = Symbol::DIAGONAL;
  int omegaOrder = ( omegaStruct == Symbol::DIAGONAL? omegaDim : series(1,1,omegaDim) );
  vector<double> omega_in (omegaOrder);
  vector<bool>     omega_fix(omegaOrder);
  for( int i=0; i<omegaOrder; i++ )
    {
      omega_in[i]  = i+1;
      omega_fix[i] = ( i%2==0? true : false );
    }

  const int sigmaDim = 2;
  const Symbol::Structure sigmaStruct = Symbol::DIAGONAL;
  int sigmaOrder = ( sigmaStruct == Symbol::DIAGONAL? sigmaDim : series(1,1,sigmaDim) );
  vector<double> sigma_in (omegaOrder);
  vector<bool>     sigma_fix(omegaOrder);
  for( int i=0; i<sigmaOrder; i++ )
    {
      sigma_in[i]  = i+1;
      sigma_fix[i] = ( i%2==0? true : false );
    }

  const int etaLen = omegaOrder;
  vector<double> eta_in (etaLen);
  vector<bool>   eta_fix(etaLen);
  fill( eta_in.begin(), eta_in.end(), 0.0 );
  for( int i=0; i<etaLen; i++ )
    eta_fix[i] = false;

  const int epsLen = sigmaOrder;
  vector<double> eps_in (epsLen);
  vector<bool>   eps_fix(epsLen);
  fill( eps_in.begin(), eps_in.end(), 0.0 );
  for( int i=0; i<epsLen; i++ )
    eps_fix[i] = false;

  string pop_cov_form     = "r";
  bool pop_stderr         = true;
  bool pop_coefficent     = true;
  bool pop_confidence     = true;
  bool pop_covariance     = true;
  bool pop_inv_covariance = false;
  bool pop_correlation    = true;

  bool isSimulate = true;
  const int seed = 1;

  //=====================================================
  // Generate a sourceML document.
  //=====================================================
  const char gSource[] = "NonmemTranslatorPopTest.sourceML";
  ofstream oSource( gSource );
  if( oSource.good() )
    {
      oSource << "<spksource>" << endl;
      oSource << "<nonmem>" << endl;
      
      oSource << "<constraint>" << endl;
      // default: is_eta_out=no, is_restart=yes
      oSource << "<pop_analysis approximation=\"foce\" pop_size=\"3\" ";
      oSource << "is_estimation=\"yes\" sig_digits=\"3\">" << endl;
      oSource << "<data_labels>" << endl;

      map<string,string>::const_iterator pLabel = labels.begin();
      for( int i=0; i<nLabels, pLabel!=labels.end(); i++, pLabel++ )
	{
	  oSource << "<label name=";
          oSource << "\"" << pLabel->first << "\"";
          if( pLabel->second != "" )
	    oSource << " synonym=\"" << pLabel->second << "\"";
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

      oSource << "<pop_stat covariance_form=\"" << pop_cov_form << "\" ";
      oSource << "is_standarderr_out=\""        << (pop_stderr? "yes":"no") << "\" ";
      oSource << "is_covariance_out=\""         << (pop_covariance? "yes":"no") << "\" ";
      oSource << "is_inverse_covariance_out=\"" << (pop_inv_covariance? "yes":"no") << "\" ";
      oSource << "is_correlation_out=\""        << (pop_correlation? "yes":"no") << "\"/>" << endl;

      if( isSimulate )
	{
	  oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
	}
      oSource << "</pop_analysis>" << endl;
      oSource << "</constraint>" << endl;
      
      oSource << "<model>" << endl;
      oSource << "<pred>" << endl;
      oSource << "   KA=THETA(1) + ETA(1)" << endl;
      oSource << "   KE=THETA(2) + ETA(2)" << endl;
      oSource << "   F=KE*KA" << endl;
      oSource << "   Y=F+EPS(1)" << endl;
      oSource << "</pred>" << endl;
      oSource << "</model>" << endl;
      
      oSource << "<presentation>" << endl;
      oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
      oSource << "<column label=\"THETA(1)\" appearance_order=\"2\"/>" << endl;
      oSource << "<column label=\"THETA(3)\" appearance_order=\"4\"/>" << endl;
      oSource << "<column label=\"THETA(2)\" appearance_order=\"3\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "<table header=\"every\">" << endl;
      oSource << "<column label=\"DV\" appearance_order=\"2\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "</presentation>" << endl;
      
      oSource << "</nonmem>" << endl;
      oSource << "</spksource>" << endl;
    }
  oSource.close();

  //=====================================================
  // Initialize Xerces DOM parser
  //=====================================================  
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
    
  xercesc::XercesDOMParser *parser = new xercesc::XercesDOMParser;
  parser->setValidationScheme( XercesDOMParser::Val_Auto );
  parser->setDoNamespaces( true );
  parser->setDoSchema( true );
  parser->setValidationSchemaFullChecking( true );
  parser->setCreateEntityReferenceNodes( true );
  
  //=====================================================
  // Let the Xerces DOM parser parse the sourceML document.
  //=====================================================
  try{
    ifstream iSource( gSource );
    if( !iSource.good() )
      {
	XMLPlatformUtils::Terminate();
	char buf[maxChars + 1];
	sprintf( buf, "Failed to open %s!\n", gSource );
	CPPUNIT_ASSERT_MESSAGE( buf, false );
      }
    parser->parse( gSource );
    data = parser->getDocument();
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
                   gSource, e.code, XMLString::transcode(errText) );
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
  // Fill the symbol table with data records as if
  // an spkdataML document has been parsed.
  //
  // Data Set
  // --------
  //  
  //   ID       CP/DV     MDV
  //   #1         0.0       0
  //   #2         0.0       0
  //   #2        10.0       0
  //   #3         0.0       0
  //   #3        10.0       0
  //   #3        20.0       0
  //=====================================================
  NonmemTranslator xlator( data, source );
  SymbolTable *table = xlator.getSymbolTable();

  valarray<int> N(pop_size);
  for( int i=0; i<pop_size; i++ )
     N[i] = i+1;

  // By giving the vector, N, containing the #of measurements
  // for subjects, SymbolTable initializes the internal arrays
  // in proper dimensions.
  Symbol * id   = table->insertLabel( "ID",  "",   N );
  Symbol * cp   = table->insertLabel( "CP",  "DV", N );
  Symbol * mdv  = table->insertLabel( "MDV", "",   N );

  // Now, populate the properly sized arrays.
  // These values within the symbol table will be used by
  // the SPK Compiler to generate code for initializing
  // DataSet and IndData objects.
  id->  initial[0][0] = "#1";  cp->initial[0][0] =  "0.0";  mdv-> initial[0][0] = "0";

  id->  initial[1][0] = "#2";  cp->initial[1][0] =  "0.0";  mdv-> initial[1][0] = "0";
  id->  initial[1][1] = "#2";  cp->initial[1][1] = "10.0";  mdv-> initial[1][1] = "0";

  id->  initial[2][0] = "#3";  cp->initial[2][0] = "0.0";   mdv-> initial[2][0] = "0";
  id->  initial[2][1] = "#3";  cp->initial[2][1] = "10.0";  mdv-> initial[2][1] = "0";
  id->  initial[2][2] = "#3";  cp->initial[2][2] = "20.0";  mdv-> initial[2][2] = "0";

  //=====================================================
  // Parse the sourceML document.
  // Upon the successful return, the following files
  // shall be generated:
  //   * driver.cpp          --- SPK driver
  //   * generatedMakefile   --- Make file
  //   * Pred.h              --- Def. of Pred class
  //   * DataSet.h           --- Def. of DataSet class
  //   * IndData.h           --- Def. of IndData class
  //=====================================================
  xlator.parseSource();

  //  cout << *table << endl;

  //=====================================================
  // Test the contents of the symbol table after
  // parsing the sourceML document.  WRES/RES/PRED
  // should have been added for table/scatterplot
  // default requirements.
  // THETA/ETA/SIGMA/OMEGA/KA/KE/F/Y for PRED.
  //=====================================================
  map<string,string>::const_iterator pLabel = labels.begin();
  CPPUNIT_ASSERT( table->findi("id")   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi("cp")   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi("dv")   != Symbol::empty() ); // from data set
  CPPUNIT_ASSERT( table->findi("mdv")  != Symbol::empty() ); // from data set

  CPPUNIT_ASSERT( table->findi("pred") != Symbol::empty() ); // for table/scatterplot
  CPPUNIT_ASSERT( table->findi("wres") != Symbol::empty() ); // for table/scatterplot
  CPPUNIT_ASSERT( table->findi("res")  != Symbol::empty() ); // for table/scatterplot

  CPPUNIT_ASSERT( table->findi("theta")!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("eta")  != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("eps")  != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("omega")!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("sigma")!= Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("ka")   != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("ke")   != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("f")    != Symbol::empty() ); // from PRED
  CPPUNIT_ASSERT( table->findi("y")    != Symbol::empty() ); // from PRED

  Symbol *theta = table->findi( "theta" );
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
  
  Symbol *omega = table->findi( "omega" );
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

  Symbol *sigma = table->findi( "sigma" );
  CPPUNIT_ASSERT( sigma != Symbol::empty() );
  CPPUNIT_ASSERT( sigma->structure == sigmaStruct );
  CPPUNIT_ASSERT_EQUAL( sigmaDim, sigma->dimension[0] );
  for( int i=0; i<sigmaOrder; i++ )
    {

      CPPUNIT_ASSERT( sigma_fix[i] == sigma->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( sigma_in[i],  atof( sigma->initial[0][i].c_str() ) );
   }

  Symbol *eta = table->findi( "eta" );
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

  Symbol *eps = table->findi( "eps" );
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
  char fIndDataDriver[]     = "pop_IndDataDriver";
  char fIndDataDriver_cpp[] = "pop_IndDataDriver.cpp";
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
  oIndDataDriver << "   vector<char*> a_id(1);" << endl;
  oIndDataDriver << "   char id1[] = \"1\";" << endl;
  oIndDataDriver << "   a_id[0] = id1;" << endl;
  oIndDataDriver << "   vector<double> a_cp(1);" << endl;
  oIndDataDriver << "   a_cp[0] = 0.0;" << endl;
  oIndDataDriver << "   vector<double> a_mdv(1);" << endl;
  oIndDataDriver << "   a_mdv[0] = 0;" << endl;
  oIndDataDriver << endl;
  oIndDataDriver << "   vector<char*> b_id(2);" << endl;
  oIndDataDriver << "   char id2[] = \"2\";" << endl;
  oIndDataDriver << "   b_id[0] = id2;" << endl;
  oIndDataDriver << "   b_id[1] = id2;" << endl;
  oIndDataDriver << "   vector<double> b_cp(2);" << endl;
  oIndDataDriver << "   b_cp[0] = 0.0;" << endl;
  oIndDataDriver << "   b_cp[1] = 10.0;" << endl;
  oIndDataDriver << "   vector<double> b_mdv(2);" << endl;
  oIndDataDriver << "   b_mdv[0] = 0;" << endl;
  oIndDataDriver << "   b_mdv[1] = 0;" << endl;
  oIndDataDriver << endl;
  oIndDataDriver << "   vector<char*> c_id(3);" << endl;
  oIndDataDriver << "   char id3[] = \"3\";" << endl;
  oIndDataDriver << "   c_id[0] = id3;" << endl;
  oIndDataDriver << "   c_id[1] = id3;" << endl;
  oIndDataDriver << "   c_id[2] = id3;" << endl;
  oIndDataDriver << "   vector<double> c_cp(3);" << endl;
  oIndDataDriver << "   c_cp[0] = 0.0;" << endl;
  oIndDataDriver << "   c_cp[1] = 10.0;" << endl;
  oIndDataDriver << "   c_cp[2] = 20.0;" << endl;
  oIndDataDriver << "   vector<double> c_mdv(3);" << endl;
  oIndDataDriver << "   c_mdv[0] = 0;" << endl;
  oIndDataDriver << "   c_mdv[1] = 0;" << endl;
  oIndDataDriver << "   c_mdv[2] = 0;" << endl;
  oIndDataDriver << endl;

  oIndDataDriver << "   IndData<double> A( 1, a_id, a_cp, a_mdv );" << endl;
  oIndDataDriver << "   IndData<double> B( 2, b_id, b_cp, b_mdv );" << endl;
  oIndDataDriver << "   IndData<double> C( 3, c_id, c_cp, c_mdv );" << endl;
  oIndDataDriver << endl;

  oIndDataDriver << "   assert( strcmp( A.id[0], id1 ) == 0 );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0, A.cp[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0, A.dv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0,   A.mdv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, A.theta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   A.eps[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   A.eta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.res.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.wres.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.pred.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.f.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.y.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.ka.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 1, A.ke.size() );" << endl;
  oIndDataDriver << endl;

  oIndDataDriver << "   assert( strcmp( B.id[0], id2 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( B.id[1], id2 ) == 0 );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0,  B.cp[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, B.cp[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0,  B.dv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, B.dv[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0, B.mdv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0, B.mdv[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, B.theta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, B.theta[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   B.eps[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   B.eps[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   B.eta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   B.eta[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.res.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.wres.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.pred.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.f.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.y.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.ka.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 2, B.ke.size() );" << endl;
  oIndDataDriver << endl;

  oIndDataDriver << "   assert( strcmp( C.id[0], id3 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( C.id[1], id3 ) == 0 );" << endl;
  oIndDataDriver << "   assert( strcmp( C.id[2], id3 ) == 0 );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0,  C.cp[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, C.cp[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, C.cp[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0.0,  C.dv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 10.0, C.dv[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 20.0, C.dv[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0, C.mdv[0] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0, C.mdv[1] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 0, C.mdv[2] );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, C.theta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, C.theta[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( thetaLen, C.theta[2].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   C.eps[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   C.eps[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( epsLen,   C.eps[2].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   C.eta[0].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   C.eta[1].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( etaLen,   C.eta[2].size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.res.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.wres.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.pred.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.f.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.y.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.ka.size() );" << endl;
  oIndDataDriver << "   MY_ASSERT_EQUAL( 3, C.ke.size() );" << endl;
  oIndDataDriver << endl;

  oIndDataDriver << "}" << endl;
  oIndDataDriver.close();

  char command[256];
  sprintf( command, "g++ -g %s -o %s", fIndDataDriver_cpp, fIndDataDriver );
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
  char fDataSetDriver[]     = "pop_DataSetDriver";
  char fDataSetDriver_cpp[] = "pop_DataSetDriver.cpp";
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

  oDataSetDriver << "   assert( strcmp( set.data[0]->id[0], \"#1\" ) == 0 );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0, set.data[0]->cp[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0, set.data[0]->dv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0,   set.data[0]->mdv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[0]->theta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[0]->eps[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[0]->eta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->res.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->wres.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->pred.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->f.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->y.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->ka.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 1, set.data[0]->ke.size() );" << endl;
  oDataSetDriver << endl;

  oDataSetDriver << "   assert( strcmp( set.data[1]->id[0], \"#2\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[1]->id[1], \"#2\" ) == 0 );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0,  set.data[1]->cp[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[1]->cp[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0,  set.data[1]->dv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[1]->dv[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0, set.data[1]->mdv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0, set.data[1]->mdv[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[1]->theta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[1]->theta[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[1]->eps[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[1]->eps[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[1]->eta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[1]->eta[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->res.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->wres.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->pred.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->f.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->y.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->ka.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 2, set.data[1]->ke.size() );" << endl;
  oDataSetDriver << endl;

  oDataSetDriver << "   assert( strcmp( set.data[2]->id[0], \"#3\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[2]->id[1], \"#3\" ) == 0 );" << endl;
  oDataSetDriver << "   assert( strcmp( set.data[2]->id[2], \"#3\" ) == 0 );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0,  set.data[2]->cp[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[2]->cp[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[2]->cp[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0.0,  set.data[2]->dv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 10.0, set.data[2]->dv[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 20.0, set.data[2]->dv[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0, set.data[2]->mdv[0] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0, set.data[2]->mdv[1] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 0, set.data[2]->mdv[2] );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[2]->theta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[2]->theta[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( thetaLen, set.data[2]->theta[2].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[2]->eps[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[2]->eps[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( epsLen,   set.data[2]->eps[2].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[2]->eta[0].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[2]->eta[1].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( etaLen,   set.data[2]->eta[2].size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->res.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->wres.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->pred.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->f.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->y.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->ka.size() );" << endl;
  oDataSetDriver << "   MY_ASSERT_EQUAL( 3, set.data[2]->ke.size() );" << endl;
  oDataSetDriver << endl;

  oDataSetDriver << "}" << endl;
  
  oDataSetDriver.close();

  sprintf( command, "g++ -g %s -o %s", fDataSetDriver_cpp, fDataSetDriver );
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
  char fPredDriver[]     = "pop_PredDriver";
  char fPredDriver_cpp[] = "pop_PredDriver.cpp";
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
  oPredDriver << "   const int nIndividuals = 3;" << endl;
  oPredDriver << "   vector<int> N(nIndividuals); // numbers of measurements" << endl;
  oPredDriver << "   N[0] = 1; N[1] = 2; N[2] = 3;" << endl;
  oPredDriver << "   const int thetaLen    = " << thetaLen << ";" << endl;
  oPredDriver << "   const int etaLen      = " << etaLen << ";" << endl;
  oPredDriver << "   const int epsLen      = " << epsLen << ";" << endl;
  oPredDriver << "   const int thetaOffset = 0;" << endl;
  oPredDriver << "   const int etaOffset   = thetaLen;" << endl;
  oPredDriver << "   const int epsOffset   = thetaLen + etaLen;" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
  oPredDriver << "   vector< CppAD::AD<double> > depVar( (1+2+3) * 2 );" << endl;
  oPredDriver << "   fill( indepVar.begin(), indepVar.end(), 0.0 );" << endl;
  oPredDriver << "   fill( depVar.begin(), depVar.end(), 0.0 );" << endl;
  oPredDriver << "   const double C1       = 1.0;" << endl;
  oPredDriver << "   const double C2       = 2.0;" << endl;
  //---------------------------------------------------------------------------------
  // A complete iteration over j
  //
  oPredDriver << endl;
  oPredDriver << "   double expectedF, actualF, expectedY, actualY;" << endl;
  oPredDriver << "   for( int i=0; i<3; i++ )" << endl;
  oPredDriver << "   {" << endl;
  oPredDriver << "      int n = N[i];" << endl;
  oPredDriver << "      depVar.resize( n * 2 );" << endl;
  oPredDriver << "      int fOffset = 0;" << endl;
  oPredDriver << "      int yOffset = n;" << endl;
  oPredDriver << "      for( int j=0; j<n; j++ )" << endl;
  oPredDriver << "      {" << endl;
  oPredDriver << "         indepVar[thetaOffset+0] = C1*j; // theta(1)" << endl;
  oPredDriver << "         indepVar[thetaOffset+1] = C1*j; // theta(2)" << endl;
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
  oPredDriver << "         indepVar[etaOffset  +0] = C1*j; // eta(1)" << endl;
  oPredDriver << "         indepVar[etaOffset  +1] = C1*j; // eta(2)" << endl;
  oPredDriver << "         indepVar[epsOffset  +0] = C1*j; // eps(1)" << endl;
  oPredDriver << "         expectedF = CppAD::Value(indepVar[thetaOffset+0] + indepVar[etaOffset+0] )" << endl;
  oPredDriver << "                   * CppAD::Value(indepVar[thetaOffset+1] + indepVar[etaOffset+1] );" << endl;
  oPredDriver << "         double pred =expectedF;" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->theta[j][0] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->theta[j][1] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->eta[j][0] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( C1*j, set.data[i]->eta[j][1] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( pred, set.data[i]->pred[j] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( set.data[i]->dv[j] - pred, set.data[i]->res[j] );" << endl;
  oPredDriver << "         MY_ASSERT_EQUAL( pred, set.data[i]->f[j] );" << endl;
  oPredDriver << "      }" << endl;
  oPredDriver << "   }" << endl;
  //
  // End of a complete iteration over j
  //---------------------------------------------------------------------------------

  oPredDriver << "   return !ok;" << endl;
  oPredDriver << "}" << endl;
  oPredDriver.close();

  sprintf( command, "g++ -g %s -o %s", fPredDriver_cpp, fPredDriver );
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
  char fDriver[]     = "driver";
  char fDriver_cpp[] = "driver.cpp";
  int  exitcode      = 0;

  sprintf( command, "make -f generatedMakefile" );
  if( system( command ) != 0 )
    {
      char message[256];
      sprintf( message, "Compilation of the generated %s failed!", fDriver_cpp );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  /*
  sprintf( command, "./%s", fDriver );
  
  // The exist code of 0 indicates success.  1 indicates convergence problem.
  // 2 indicates some file access problem.
  // Since I didn't set the problem so that it makes sense in either scientifically
  // or mathematially, the return code of anything other than 2 is ignored here.
  exitcode = system( command );
  if( exitcode == 1 )
    {
      char message[256];
      sprintf( message, "%s failed for convergence problem <%d>!", fDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, true );
    }
  if( exitcode == 2 )
    {
      char message[256];
      sprintf( message, "%s failed due to inproper file access permission <%d>!", fDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, false );
    }
  if( exitcode > 2 )
    {
      char message[256];
      sprintf( message, "%s failed for reasons other than convergence propblem or access permission <%d>!", fDriver, exitcode );
      CPPUNIT_ASSERT_MESSAGE( message, true );
    }
  */

  /*
  remove( gSource ); // clean up
  remove( "driver.cpp" );
  remove( "Pred.h" );
  remove( "DataSet.h" );
  remove( "IndData.h" );
  remove( "generatedMakefile" );
  remove( "spk_error.tmp" );
  remove( "result.xml" );
  remove( "predEqn.cpp" );
  */

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
  remove( gSource );
  //remove( fTestPred );
 
  rename( "driver.cpp", "popDriver.cpp" );

  if( system( "g++ popDriver.cpp -g -lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm -o popDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link the generated \"driver.cpp\".", false );
  }
  if( system( "./popDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "The generated/built \"popDriver\" failed to run successfully.", false );
  }
  */
}
CppUnit::Test * NonmemTranslatorPopTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemTranslatorPopTest" );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorPopTest>(
         "testParsePopSource", 
	 &NonmemTranslatorPopTest::testParsePopSource ) );
  
  return suiteOfTests;
}

