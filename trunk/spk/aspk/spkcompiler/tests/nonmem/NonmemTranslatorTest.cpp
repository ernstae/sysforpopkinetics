#include <iostream>
#include <vector>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "NonmemTranslatorTest.h"
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

void NonmemTranslatorTest::setUp()
{

}
void NonmemTranslatorTest::tearDown()
{
}
void NonmemTranslatorTest::testInheritance()
{
}
void NonmemTranslatorTest::testParsePopSource()
{
  const int pop_size = 3;
  const int nLabels = 5;
  map<string, string> labels;
  labels["ID"]   = "";
  labels["TIME"] = "";
  labels["CP"]   = "DV";
  labels["WT"]   = "";
  labels["DOSE"] = "AMT";

  const int thetaLen = 3;
  vector<double> theta_in (thetaLen);
  vector<double> theta_up (thetaLen);
  vector<double> theta_low(thetaLen);
  vector<bool>     theta_fix(thetaLen);
  for( int i=0; i<thetaLen; i++ )
    {
      theta_in[i]  =  i+1;
      theta_up[i]  = +10.0 * theta_in[i];
      theta_low[i] = -10.0 * theta_in[i];
      theta_fix[i] = ( i%2==0? true : false );
    }

  const int omegaDim = 2;
  const Symbol::Structure omegaStruct = Symbol::TRIANGLE;
  int omegaElemNum = ( omegaStruct == Symbol::DIAGONAL? omegaDim : series(1,1,omegaDim) );
  vector<double> omega_in (omegaElemNum);
  vector<bool>     omega_fix(omegaElemNum);
  for( int i=0; i<omegaElemNum; i++ )
    {
      omega_in[i]  = i+1;
      omega_fix[i] = ( i%2==0? true : false );
    }

  const int sigmaDim = 2;
  const Symbol::Structure sigmaStruct = Symbol::TRIANGLE;
  int sigmaElemNum = ( sigmaStruct == Symbol::DIAGONAL? sigmaDim : series(1,1,sigmaDim) );
  vector<double> sigma_in (omegaElemNum);
  vector<bool>     sigma_fix(omegaElemNum);
  for( int i=0; i<sigmaElemNum; i++ )
    {
      sigma_in[i]  = i+1;
      sigma_fix[i] = ( i%2==0? true : false );
    }

  const int etaLen = thetaLen;
  vector<double> eta_in (etaLen);
  vector<bool>   eta_fix(etaLen);
  fill( eta_in.begin(), eta_in.end(), 0.0 );
  for( int i=0; i<etaLen; i++ )
    eta_fix[i] = false;

  const int epsLen = sigmaDim;
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

  const char gSource[] = "NonmemTranslatorPopTest.sourceML";
  ofstream oSource( gSource );
  if( oSource.good() )
    {
      oSource << "<spksourceML>" << endl;
      oSource << "<nonmem>" << endl;
      
      oSource << "<constraint>" << endl;
      // default: is_eta_out=no, is_restart=yes
      oSource << "<pop_analysis approximation=\"foce\" pop_size=\"3\" is_estimation=\"yes\">" << endl;
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
      oSource << "<theta length=\"3\">" << endl;
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

      for( int i=0; i<omegaElemNum; i++ )
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
      for( int i=0; i<sigmaElemNum; i++ )
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
      oSource << ";THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)" << endl;
      oSource << ";THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)" << endl;
      oSource << ";THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/kg)" << endl;
      oSource << ";DOSE=WT-ADJUSTED DOSE (mg/kg)" << endl;
      oSource << ";DS=NON-WT-ADJUSTED DOSE (mg)" << endl;
      oSource << "   IF (DOSE.NE.0) THEN" << endl;
      oSource << "      DS=DOSE*WT" << endl;
      oSource << "      W=WT" << endl;
      oSource << "   ENDIF" << endl;
      oSource << "   KA=THETA(1) + ETA(1)" << endl;
      oSource << "   KE=THETA(2) + ETA(2)" << endl;
      oSource << "   CL=THETA(3) * W + ETA(3)" << endl;
      oSource << "   D=EXP(-KE*TIME)-EXP(-KA*TIME)" << endl;
      oSource << "   E=CL*(KA-KE)" << endl;
      oSource << "   F=DS*KE*KA/E*D" << endl;
      oSource << "   Y=F+EPS(1)" << endl;
      oSource << "</pred>" << endl;
      oSource << "</model>" << endl;
      
      oSource << "<presentation>" << endl;
      oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
      oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
      oSource << "<column label=\"THETA(1)\" appearance_order=\"2\"/>" << endl;
      oSource << "<column label=\"THETA(3)\" appearance_order=\"4\"/>" << endl;
      oSource << "<column label=\"THETA(2)\" appearance_order=\"3\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "<table header=\"every\">" << endl;
      oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
      oSource << "<column label=\"DV\" appearance_order=\"2\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "<scatterplot>" << endl;
      oSource << "<x label=\"TIME\"/>" << endl;
      oSource << "<y label=\"PRED\"/>" << endl;
      oSource << "</scatterplot>" << endl;
      oSource << "</presentation>" << endl;
      
      oSource << "</nonmem>" << endl;
      oSource << "</spksourceML>" << endl;
    }
  oSource.close();
  
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
  
  valarray<int> N(pop_size);
  for( int i=0; i<pop_size; i++ )
     N[i] = i+1;
  NonmemTranslator xlator( data, source );
  SymbolTable *table = xlator.getSymbolTable();
  Symbol * id   = table->insertLabel( "ID",   "", N );
  Symbol * time = table->insertLabel( "TIME", "", N );
  Symbol * cp   = table->insertLabel( "CP",   "DV", N );
  Symbol * wt   = table->insertLabel( "WT", "", N );
  Symbol * dose = table->insertLabel( "DOSE", "", N );
  Symbol * mdv  = table->insertLabel( "MDV", "", N );

  dose->synonym = string("AMT");
 
  id->  initial[0][0] = "#1";
  time->initial[0][0] = "0.0";  cp->initial[0][0] = "0.0";   wt->initial[0][0] = "10.0";  dose->initial[0][0] = "10.0";
  mdv-> initial[0][0] = "0";

  id->  initial[1][0] = "#2"; 
  time->initial[1][0] = "0.0";  cp->initial[1][0] = "0.0";   wt->initial[1][0] = "20.0";  dose->initial[1][0] = "10.0";
  mdv-> initial[1][0] = "0";
  id->  initial[1][1] = "#2";
  time->initial[1][1] = "1.0";  cp->initial[1][1] = "10.0";  wt->initial[1][1] = "0.0";   dose->initial[1][1] = "0.0";
  mdv-> initial[1][1] = "0";

  id->  initial[2][0] = "#3";
  time->initial[2][0] = "0.0";  cp->initial[2][0] = "0.0";   wt->initial[2][0] = "30.0";  dose->initial[2][0] = "10.0";
  mdv-> initial[2][0] = "0";
  id->  initial[2][1] = "#3";   
  time->initial[2][1] = "1.0";  cp->initial[2][1] = "10.0";  wt->initial[2][1] = "0.0";   dose->initial[2][1] = "0.0";
  mdv-> initial[2][1] = "0";
  id->  initial[2][2] = "#3";
  time->initial[2][2] = "2.0";  cp->initial[2][2] = "20.0";  wt->initial[2][2] = "0.0";   dose->initial[2][2] = "0.0";
  mdv-> initial[2][2] = "0";

  xlator.parseSource();

  if( system( "g++ driver.cpp -g -lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm -o driver" ) != 0 )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation of the generated driver.cpp failed!", false );
    }

  remove( gSource );

  //cout << *table << endl;

  //=====================================================
  // Test the contents of the symbol table.
  //=====================================================
  map<string,string>::const_iterator pLabel = labels.begin();
  for( int i=0; i<nLabels, pLabel!=labels.end(); i++, pLabel++ )
    {
      Symbol * s = table->findi( pLabel->first );
      CPPUNIT_ASSERT( s != Symbol::empty() );
      CPPUNIT_ASSERT( s->name == pLabel->first );
      CPPUNIT_ASSERT( s->synonym == pLabel->second );
    }
  CPPUNIT_ASSERT( table->findi("pred") != Symbol::empty() );
  CPPUNIT_ASSERT( table->findi("wres") != Symbol::empty() );
  CPPUNIT_ASSERT( table->findi("res")  != Symbol::empty() );

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
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->fixed[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->lower[0].size() ) );

  for( int i=0; i<omegaElemNum; i++ )
    {

      CPPUNIT_ASSERT( omega_fix[i] == omega->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( omega_in[i],  atof( omega->initial[0][i].c_str() ) );
   }

  Symbol *sigma = table->findi( "sigma" );
  CPPUNIT_ASSERT( sigma != Symbol::empty() );
  CPPUNIT_ASSERT( sigma->structure == sigmaStruct );
  CPPUNIT_ASSERT_EQUAL( sigmaDim, sigma->dimension[0] );
  for( int i=0; i<sigmaElemNum; i++ )
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
     oTestPred << "#include <../cppad/CppAD.h>" << endl;
     oTestPred << "#include \"IndData.h\"" << endl;
     oTestPred << "#include \"DataSet.h\"" << endl;
     oTestPred << "#include \"Pred.h\"" << endl;
     oTestPred << endl;
     oTestPred << "using namespace std;" << endl;
     oTestPred << endl;
     oTestPred << "template <typename T>" << endl;
     oTestPred << "class Values{" << endl;
     oTestPred << "public:" << endl;
     oTestPred << "  Values( int n, int nTheta, int nEta, int nEps )" << endl;
     oTestPred << "  : theta(n)," << endl;
     oTestPred << "    eta  (n)," << endl;
     oTestPred << "    eps  (n)," << endl;
     oTestPred << "    ds   (n)," << endl;
     oTestPred << "    w    (n)," << endl;
     oTestPred << "    ka   (n)," << endl;
     oTestPred << "    ke   (n)," << endl;
     oTestPred << "    cl   (n)," << endl;
     oTestPred << "    d    (n), " << endl;
     oTestPred << "    e    (n), " << endl;
     oTestPred << "    f    (n), " << endl;
     oTestPred << "    y    (n)" << endl;
     oTestPred << "  {" << endl;
     oTestPred << "    for( int i=0; i<n; i++ )" << endl;
     oTestPred << "      theta[i].resize( nTheta );" << endl;
     oTestPred << "    for( int i=0; i<n; i++ )" << endl;
     oTestPred << "      eta[i].resize( nEta );" << endl;
     oTestPred << "    for( int i=0; i<n; i++ )" << endl;
     oTestPred << "      eps[i].resize( nEps );" << endl;
     oTestPred << "  }" << endl;
     oTestPred << "  vector<T> ds;" << endl;
     oTestPred << "  vector<T> w;" << endl;
     oTestPred << "  vector<T> ka;" << endl;
     oTestPred << "  vector<T> ke;" << endl;
     oTestPred << "  vector<T> cl;" << endl;
     oTestPred << "  vector<T> d;" << endl;
     oTestPred << "  vector<T> e;" << endl;
     oTestPred << "  vector<T> f;" << endl;
     oTestPred << "  vector<T> y;" << endl;
     oTestPred << "  vector< vector<T> > theta;" << endl;
     oTestPred << "  vector< vector<T> > eta;" << endl;
     oTestPred << "  vector< vector<T> > eps;" << endl;
     oTestPred << endl;
     oTestPred << "  Values( const Values<T>& right )" << endl;
     oTestPred << "  : ds(right.ds)," << endl;
     oTestPred << "    w (right.w)," << endl;
     oTestPred << "    ka(right.ka)," << endl;
     oTestPred << "    ke(right.ke)," << endl;
     oTestPred << "     cl(right.cl)," << endl;
     oTestPred << "     d(right.d)," << endl;
     oTestPred << "     e(right.e)," << endl;
     oTestPred << "     f(right.f)," << endl;
     oTestPred << "     y(right.y)" << endl;
     oTestPred << "  {" << endl;
     oTestPred << "    for( int i=0; i<right.theta.size(); i++ )" << endl;
     oTestPred << "      theta[i] = right.theta[i];" << endl;
     oTestPred << "    for( int i=0; i<right.eta.size(); i++ )" << endl;
     oTestPred << "      eta[i] = right.eta[i];" << endl;
     oTestPred << "    for( int i=0; i<right.eps.size(); i++ )" << endl;
     oTestPred << "      eps[i] = right.eps[i];" << endl;
     oTestPred << "  }" << endl;
     oTestPred << "  Values<T>& operator=( const Values<T>& right )" << endl;
     oTestPred << "  {" << endl;
     oTestPred << "    for( int i=0; i<right.theta.size(); i++ )" << endl;
     oTestPred << "      theta[i] = right.theta[i];" << endl;
     oTestPred << "    for( int i=0; i<right.eta.size(); i++ )" << endl;
     oTestPred << "      eta[i] = right.eta[i];" << endl;
     oTestPred << "    for( int i=0; i<right.eps.size(); i++ )" << endl;
     oTestPred << "      eps[i] = right.eps[i];" << endl;
     oTestPred << endl;
     oTestPred << "    ds = right.ds;" << endl;
     oTestPred << "    w  = right.w;" << endl;
     oTestPred << "    ka = right.ka;" << endl;
     oTestPred << "    ke = right.ke;" << endl;
     oTestPred << "    cl = right.cl;" << endl;
     oTestPred << "    d  = right.d;" << endl;
     oTestPred << "    e  = right.e;" << endl;
     oTestPred << "    f  = right.f;" << endl;
     oTestPred << "    y  = right.y;" << endl;
     oTestPred << "  }" << endl;
     oTestPred << "};" << endl;
     oTestPred << endl;
     oTestPred << "void clean( int n, Values<double>** a, Values<double>** b)" << endl;
     oTestPred << "{" << endl;
     oTestPred << "  for( int i=0; i<n; i++ )" << endl;
     oTestPred << "    {" << endl;
     oTestPred << "        delete a[i];" << endl;
     oTestPred << "        delete b[i];" << endl;
     oTestPred << "    }" << endl;
     oTestPred << "    return;" << endl;
     oTestPred << "}" << endl;
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
	 oTestPred << "  double time_" << i << "[] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << time->initial[i][j];
	   }
	 oTestPred << " };" << endl;
	 oTestPred << "  double cp_" << i << "  [] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << cp->initial[i][j];
	   }
	 oTestPred << " };" << endl;
 	 oTestPred << "  double *dv_" << i << "    = cp_" << i << ";" << endl;
	 oTestPred << "  double wt_" << i << "  [] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << wt->initial[i][j];
	   }
	 oTestPred << " };" << endl;
	 oTestPred << "  double dose_" << i << "[] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << dose->initial[i][j];
	   }
	 oTestPred << " };" << endl;
	 oTestPred << "  double mdv_" << i << " [] = { ";
	 for( int j=0; j<N[i]; j++ )
	   {
	     if( j > 0 )
	       oTestPred << ", ";
	     oTestPred << mdv->initial[i][j];
	   }
	 oTestPred << " };" << endl;
       }

     oTestPred << "  double *time[ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "time_" << i;
       }
     oTestPred << " };" << endl;
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
     oTestPred << "  double *wt  [ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "wt_" << i;
       }
     oTestPred << " };" << endl;
     oTestPred << "  double *dose[ nIndividuals ] = { ";
     for( int i=0; i<pop_size; i++ )
       {
	 if( i > 0 )
	   oTestPred << ", ";
	 oTestPred << "dose_" << i;
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
     oTestPred << "      assert( set.data[i]->time[j] == time[i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->cp  [j] == cp  [i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->dv  [j] == dv  [i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->wt  [j] == wt  [i][j] );" << endl;
     oTestPred << "      assert( set.data[i]->dose[j] == dose[i][j] );" << endl;
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
     oTestPred << "  const int MAX_ITR = 2;" << endl;
     oTestPred << endl;
     oTestPred << "  const int thetaLen = 3;" << endl;
     oTestPred << "  const int etaLen   = 3;" << endl;
     oTestPred << "  const int epsLen   = 2;" << endl;
     oTestPred << "  const int nAlp = thetaLen + epsLen;" << endl;
     oTestPred << "  const int nB   = etaLen;" << endl;
     oTestPred << "  int fLen;" << endl;
     oTestPred << "  int yLen;" << endl;
     oTestPred << endl;
     oTestPred << "  const int thetaOffset = 0;" << endl;
     oTestPred << "  const int etaOffset   = thetaLen;" << endl;
     oTestPred << "  const int epsOffset   = thetaLen + etaLen;" << endl;
     oTestPred << "  int fOffset;" << endl;
     oTestPred << "  int yOffset;" << endl;
     oTestPred << endl;
     oTestPred << "  vector<double> indepVar( thetaLen + etaLen + epsLen );" << endl;
     oTestPred << "  vector<double> theta( thetaLen );" << endl;
     oTestPred << "  vector<double> eta  ( etaLen );" << endl;
     oTestPred << "  vector<double> eps  ( epsLen );" << endl;
     oTestPred << endl;
     oTestPred << "  double ds;" << endl;
     oTestPred << "  double w;" << endl;
     oTestPred << "  double ka;" << endl;
     oTestPred << "  double ke;" << endl;
     oTestPred << "  double cl;" << endl;
     oTestPred << endl;
     oTestPred << "  Values<double> * perm[ nIndividuals ];" << endl;
     oTestPred << "  Values<double> * temp[ nIndividuals ];" << endl;
     oTestPred << "  for( int i=0; i<nIndividuals; i++ )" << endl;
     oTestPred << "  {" << endl;
     oTestPred << "    perm[i] = new Values<double>( N[i], thetaLen, etaLen, epsLen );" << endl;
     oTestPred << "    temp[i] = new Values<double>( N[i], thetaLen, etaLen, epsLen );" << endl;
     oTestPred << "  }" << endl;
     oTestPred << endl;
     oTestPred << "  for( int itr=0; itr<MAX_ITR; itr++ )" << endl;
     oTestPred << "    {" << endl;
     oTestPred << "      for( int i=0; i<nIndividuals; i++ )" << endl;
     oTestPred << "	{" << endl;
     oTestPred << "	  fLen    = N[i];" << endl;
     oTestPred << "	  yLen    = N[i];" << endl;
     oTestPred << "	  fOffset = 0;" << endl;
     oTestPred << "	  yOffset = fLen;" << endl;
     oTestPred << "	  vector<double> depVar( fLen + yLen );" << endl;
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
     oTestPred << "	  for( int j=0; j<N[i]; j++, fOffset++, yOffset++ )" << endl;
     oTestPred << "	    {" << endl;
     oTestPred << "	      //" << endl;
     oTestPred << "	      //         *** PRED definition ***" << endl;
     oTestPred << "	      //" << endl;
     oTestPred << "	      // if( dose != 0 )" << endl;
     oTestPred << "	      //   {" << endl;
     oTestPred << "	      //     ds = dose * wt;" << endl;
     oTestPred << "	      //     w = wt;" << endl;
     oTestPred << "	      //   }" << endl;
     oTestPred << "	      //" << endl;                                                                  
     oTestPred << "	      // ka = theta[ ( 1 ) - 1 ] + eta[ ( 1 ) - 1 ];" << endl;
     oTestPred << "	      // ke = theta[ ( 2 ) - 1 ] + eta[ ( 2 ) - 1 ];" << endl;
     oTestPred << "	      // cl = theta[ ( 3 ) - 1 ] * w + eta[ ( 3 ) - 1 ];" << endl;
     oTestPred << "	      // d = exp( -ke * time ) - exp( -ka * time );" << endl;
     oTestPred << "	      // e = cl * ( ka - ke );" << endl;
     oTestPred << "	      // f = ds * ke * ka / e * d;" << endl;
     oTestPred << "	      // y = f + eps[ ( 1 ) - 1 ];" << endl;
     oTestPred << "	      //" << endl;
     oTestPred << endl;
     oTestPred << "	      if( dose[i][j] != 0 )" << endl;
     oTestPred << "		{" << endl;
     oTestPred << "		  ds = dose[i][j] * wt[i][j];" << endl;
     oTestPred << "		  w = wt[i][j];" << endl;
     oTestPred << "		}" << endl;
     oTestPred << "	      double ka = theta[0] + eta[0];" << endl;
     oTestPred << "	      double ke = theta[1] + eta[1];" << endl;
     oTestPred << "	      double cl = theta[2] * w + eta[2];" << endl;
     oTestPred << "	      double d = exp( -ke * time[i][j] ) - exp( -ka * time[i][j] );" << endl;
     oTestPred << "	      double e = cl * ( ka - ke );" << endl;
     oTestPred << "	      double f = ds * ke * ka / e * d;" << endl;
     oTestPred << "	      double y = f + eps[ 0 ];" << endl;
     oTestPred << endl;
     oTestPred << "           temp[i]->ds[j] = ds;" << endl;
     oTestPred << "	      temp[i]->w [j] = w;" << endl;
     oTestPred << "           temp[i]->ka[j] = ka;" << endl;
     oTestPred << "           temp[i]->ke[j] = ke;" << endl;
     oTestPred << "           temp[i]->cl[j] = cl;" << endl;
     oTestPred << "           temp[i]->d [j] = d;" << endl;
     oTestPred << "           temp[i]->e [j] = e;" << endl;
     oTestPred << "           temp[i]->f [j] = f;" << endl;
     oTestPred << "           temp[i]->y [j] = y;" << endl;
     oTestPred << "	      copy( theta.begin(), theta.end(), temp[i]->theta[j].begin() );" << endl;
     oTestPred << "	      copy( eta.begin(),   eta.end(),   temp[i]->eta  [j].begin() );" << endl;
     oTestPred << "	      copy( eps.begin(),   eps.end(),   temp[i]->eps  [j].begin() );" << endl;
     oTestPred << endl;
     oTestPred << "	      if( i==nIndividuals-1 && j==N[i]-1 )" << endl;
     oTestPred << "		{" << endl;
     oTestPred << "		  for( int inner_i=0; inner_i<nIndividuals; inner_i++ )" << endl;
     oTestPred << "		    {" << endl;
     oTestPred << "		      copy( temp[inner_i]->ds.begin(), temp[inner_i]->ds.end(), " << endl;
     oTestPred << "			    perm[inner_i]->ds.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->w. begin(), temp[inner_i]->w. end(), " << endl;
     oTestPred << "			    perm[inner_i]->w .begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->ka.begin(), temp[inner_i]->ka.end(), " << endl;
     oTestPred << "			    perm[inner_i]->ka.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->ke.begin(), temp[inner_i]->ke.end(), " << endl;
     oTestPred << "			    perm[inner_i]->ke.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->cl.begin(), temp[inner_i]->cl.end(), " << endl;
     oTestPred << "			    perm[inner_i]->cl.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->d .begin(), temp[inner_i]->d .end(), " << endl;
     oTestPred << "			    perm[inner_i]->d .begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->e .begin(), temp[inner_i]->e .end(), " << endl;
     oTestPred << "			    perm[inner_i]->e .begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->f .begin(), temp[inner_i]->f .end(), " << endl;
     oTestPred << "			    perm[inner_i]->f .begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->y .begin(), temp[inner_i]->y .end(), " << endl;
     oTestPred << "			    perm[inner_i]->y .begin() );" << endl;
     oTestPred << endl;
     oTestPred << "		      copy( temp[inner_i]->theta.begin(), temp[inner_i]->theta.end()," << endl;
     oTestPred << "			    perm[inner_i]->theta.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->eta.begin(),   temp[inner_i]->eta.end(),  " << endl;
     oTestPred << "			    perm[inner_i]->eta.begin() );" << endl;
     oTestPred << "		      copy( temp[inner_i]->eps.begin(),   temp[inner_i]->eps.end(),  " << endl;
     oTestPred << "			    perm[inner_i]->eps.begin() );" << endl;
     oTestPred << "		    }" << endl;
     oTestPred << "		}" << endl;
     oTestPred << "	      try{" << endl;
     oTestPred << "		pred.eval( thetaOffset, thetaLen," << endl;
     oTestPred << "			   etaOffset,   etaLen," << endl;
     oTestPred << "			   epsOffset,   epsLen," << endl;
     oTestPred << "			   fOffset,     fLen," << endl;
     oTestPred << "			   yOffset,     yLen," << endl;
     oTestPred << "			   i, j," << endl;
     oTestPred << "			   indepVar," << endl;
     oTestPred << "			   depVar );" << endl;
     oTestPred << "	        }" << endl;
     oTestPred << "	      catch( ... )" << endl;
     oTestPred << "		{" << endl;
     oTestPred << "		  cerr << \"Pred::eval() threw exception!!!\" << endl;" << endl;
     oTestPred << "		  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "		  return -1;" << endl;
     oTestPred << "		}" << endl;
     oTestPred << endl;
     oTestPred << "	      for( int inner_i=0; inner_i<nIndividuals; inner_i++ )" << endl;
     oTestPred << "		{" << endl;
     oTestPred << "		  for( int inner_j=0; inner_j<N[inner_i]; inner_j++ )" << endl;
     oTestPred << "		    {" << endl;
     oTestPred << "                   cout << \".\";" << endl;
     oTestPred << "		      if( set.data[inner_i]->ds[inner_j] != perm[inner_i]->ds[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->ds[%d].\\n\"," << endl;
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->w[inner_j] != perm[inner_i]->w[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->w[%d].\\n\"," << endl; 
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->ka[inner_j] != perm[inner_i]->ka[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->ka[%d].\\n\"," << endl;
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->ke[inner_j] != perm[inner_i]->ke[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->ke[%d].\\n\"," << endl;
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->cl[inner_j] != perm[inner_i]->cl[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->cl[%d].\\n\"," << endl; 
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->d[inner_j] != perm[inner_i]->d[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->d[%d].\\n\"," << endl; 
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->e[inner_j] != perm[inner_i]->e[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->e[%d].\\n\"," << endl; 
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->f[inner_j] != perm[inner_i]->f[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->f[%d].\\n\"," << endl; 
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		      else if( set.data[inner_i]->y[inner_j] != perm[inner_i]->y[inner_j] )" << endl;
     oTestPred << "			{" << endl;
     oTestPred << "			  fprintf( stderr, " << endl;
     oTestPred << "				   \"Failed!!!  The actual data set in DataSet object failed to keep a complete set of computed values!!!  Oppressive element: DataSet::data[%d]->y[%d].\\n\"," << endl;
     oTestPred << "				   inner_i, inner_j );" << endl;
     oTestPred << "			  clean( nIndividuals, perm, temp );" << endl;
     oTestPred << "			  return -1;" << endl;
     oTestPred << "			}" << endl;
     oTestPred << "		    }" << endl;
     oTestPred << "		}" << endl;
     oTestPred << "	    }" << endl;
     oTestPred << "	  }" << endl;
     oTestPred << "    }" << endl;
     oTestPred << "  cout << endl;" << endl;
     oTestPred << "  clean( nIndividuals, perm, temp );" << endl;
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
}
void NonmemTranslatorTest::testParseIndSource()
{
  const int nLabels = 5;
  map<string, string> labels;
  labels["ID"]   = "";
  labels["TIME"] = "";
  labels["CP"]   = "DV";
  labels["DOSE"] = "";
  labels["WT"]   = "";

  const int thetaLen = 3;
  vector<double> theta_in (thetaLen);
  vector<double> theta_up (thetaLen);
  vector<double> theta_low(thetaLen);
  vector<bool>     theta_fix(thetaLen);
  for( int i=0; i<thetaLen; i++ )
    {
      theta_in[i]  =  i+1;
      theta_up[i]  = +10.0 * theta_in[i];
      theta_low[i] = -10.0 * theta_in[i];
      theta_fix[i] = ( i%2==0? true : false );
    }

  const int etaLen = 0;
  const int epsLen = 0;

  const int omegaDim = 2;
  const Symbol::Structure omegaStruct = Symbol::TRIANGLE;
  int omegaElemNum = ( omegaStruct == Symbol::DIAGONAL? omegaDim : series(1,1,omegaDim) );
  vector<double> omega_in (omegaElemNum);
  vector<bool>     omega_fix(omegaElemNum);
  for( int i=0; i<omegaElemNum; i++ )
    {
      omega_in[i]  = i+1;
      omega_fix[i] = ( i%2==0? true : false );
    }

  bool ind_stderr         = true;
  bool ind_coefficent     = true;
  bool ind_confidence     = true;
  bool ind_covariance     = true;
  bool ind_inv_covariance = false;
  bool ind_correlation    = true;

  bool isSimulate = true;
  const int seed = 1;

  const char gSource[] = "NonmemTranslatorIndTest.sourceML";
  ofstream oSource( gSource );
  if( oSource.good() )
    {
      oSource << "<spksourceML>" << endl;
      oSource << "<nonmem>" << endl;
      
      oSource << "<constraint>" << endl;
      // default: is_eta_out=no, is_restart=yes
      oSource << "<ind_analysis approximation=\"foce\" mitr=\"100\" is_estimation=\"yes\">" << endl;
      oSource << "<data_labels filename=\"xxx.dat\">" << endl;

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
      oSource << "<theta length=\"3\">" << endl;
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

      for( int i=0; i<omegaElemNum; i++ )
	{
	  oSource << "<value";
          if( omega_fix[i] )
	    oSource << " fixed=\"yes\"";
	  oSource << ">" << omega_in[i] << "</value>" << endl;
	}
      oSource << "</in>" << endl;
      oSource << "</omega>" << endl;

      oSource << "<ind_stat ";
      oSource << "is_standarderr_out=\""        << (ind_stderr? "yes":"no") << "\" ";
      oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
      oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
      oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

      if( isSimulate )
	{
	  oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
	}
      oSource << "</ind_analysis>" << endl;
      oSource << "</constraint>" << endl;

      oSource << "<model>" << endl;
      oSource << "<pred>" << endl;
      oSource << ";THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)" << endl;
      oSource << ";THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)" << endl;
      oSource << ";THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/kg)" << endl;
      oSource << ";DOSE=WT-ADJUSTED DOSE (mg/kg)" << endl;
      oSource << ";DS=NON-WT-ADJUSTED DOSE (mg)" << endl;
      oSource << "   IF (DOSE.NE.0) THEN" << endl;
      oSource << "      DS=DOSE*WT" << endl;
      oSource << "      W=WT" << endl;
      oSource << "   ENDIF" << endl;
      oSource << "   KA=THETA(1)" << endl;
      oSource << "   KE=THETA(2)" << endl;
      oSource << "   CL=THETA(3) * W" << endl;
      oSource << "   D=EXP(-KE*TIME)-EXP(-KA*TIME)" << endl;
      oSource << "   E=CL*(KA-KE)" << endl;
      oSource << "   F=DS*KE*KA/E*D" << endl;
      oSource << "</pred>" << endl;
      oSource << "</model>" << endl;
      
      oSource << "<presentation>" << endl;
      oSource << "<table header=\"one\" save_as=\"xxx\">" << endl;
      oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
      oSource << "<column label=\"THETA(1)\" appearance_order=\"2\"/>" << endl;
      oSource << "<column label=\"THETA(3)\" appearance_order=\"4\"/>" << endl;
      oSource << "<column label=\"THETA(2)\" appearance_order=\"3\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "<table header=\"every\">" << endl;
      oSource << "<column label=\"TIME\" appearance_order=\"1\" sort_order=\"1\"/>" << endl;
      oSource << "<column label=\"DV\" appearance_order=\"2\"/>" << endl;
      oSource << "</table>" << endl;
      oSource << "<scatterplot>" << endl;
      oSource << "<x label=\"TIME\"/>" << endl;
      oSource << "<y label=\"PRED\"/>" << endl;
      oSource << "</scatterplot>" << endl;
      oSource << "</presentation>" << endl;
      
      oSource << "</nonmem>" << endl;
      oSource << "</spksourceML>" << endl;
    }
  oSource.close();
  
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
  
  valarray<int> N(1);
  N[0] = 3;
  NonmemTranslator xlator( data, source );
  SymbolTable *table = xlator.getSymbolTable();
  Symbol * id   = table->insertLabel( "ID",   "", N );
  Symbol * time = table->insertLabel( "TIME", "", N );
  Symbol * cp   = table->insertLabel( "CP",   "DV", N );
  Symbol * wt   = table->insertLabel( "WT", "", N );
  Symbol * dose = table->insertLabel( "DOSE", "", N );
  time->initial[0][0] = "0.0";  cp->initial[0][0] = "0.0";   wt->initial[0][0] = "30.0";  dose->initial[0][0] = "10.0";
  time->initial[0][1] = "1.0";  cp->initial[0][1] = "10.0";  wt->initial[0][1] = "0.0";   dose->initial[0][1] = "0.0";
  time->initial[0][2] = "2.0";  cp->initial[0][2] = "20.0";  wt->initial[0][2] = "0.0";   dose->initial[0][2] = "0.0";

  xlator.parseSource();

  if( system( "g++ driver.cpp -g -lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm -o driver" ) != 0 )
    {
      CPPUNIT_ASSERT_MESSAGE( "Compilation of the generated driver.cpp failed!", false );
    }

  remove( gSource );

  //cout << *table << endl;
  map<string,string>::const_iterator pLabel = labels.begin();
  for( int i=0; i<nLabels, pLabel!=labels.end(); i++, pLabel++ )
    {
      Symbol * s = table->findi( pLabel->first );
      CPPUNIT_ASSERT( s != Symbol::empty() );
      CPPUNIT_ASSERT( s->name == pLabel->first );
      CPPUNIT_ASSERT( s->synonym == pLabel->second );
    }
  CPPUNIT_ASSERT( table->findi("pred") != Symbol::empty() );
  CPPUNIT_ASSERT( table->findi("res")  != Symbol::empty() );

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
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->fixed[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->initial[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->upper[0].size() ) );
  CPPUNIT_ASSERT_EQUAL( omegaElemNum, static_cast<int>( omega->lower[0].size() ) );

  for( int i=0; i<omegaElemNum; i++ )
    {

      CPPUNIT_ASSERT( omega_fix[i] == omega->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( omega_in[i],  atof( omega->initial[0][i].c_str() ) );
    }
  //=====================================================
  // Test the generated C++ source code files
  // IndData.h, IndData.cpp, DataSet.h, DataSet.cpp
  //=====================================================
  // The order in which the variables appear in
  // the IndData constructor must be consistent with
  // with the order in which the variables are actually
  // passed in the construction of these objects
  // done in the DataSet constructor.
  char fTestPred[] = "testIndPred.cpp";
  ofstream oTestPred( fTestPred );
  const int pop_size = 1;
  if( oTestPred.good() )
    {
      oTestPred << "#include <iostream>" << endl;
      oTestPred << "#include <vector>" << endl;
      oTestPred << "#include <../cppad/CppAD.h>" << endl;
      oTestPred << "#include \"IndData.h\"" << endl;
      oTestPred << "#include \"DataSet.h\"" << endl;
      oTestPred << "#include \"Pred.h\"" << endl;
      oTestPred << "using namespace std;" << endl;
      oTestPred << "int main()" << endl;
      oTestPred << "{" << endl;
      oTestPred << "DataSet< CppAD::AD<double> > set;" << endl;
      
      for( int i=0; i<pop_size; i++ )
	{
	  for( int j=0; j<N[i]; j++ )
	    { 
	      oTestPred << "if( set.data[" << i << "]->time[" << j << "] != ";
	      oTestPred << time->initial[i][j] << " )" << endl;
	      oTestPred << "{" << endl;
	      oTestPred << "   cerr << \"set[" << i << "]->time[" << j << "] != \\\"";
	      oTestPred << time->initial[i][j] << "\\\"\" << endl; " << endl;
	      oTestPred << "   cerr << \"was \" << set.data[";
	      oTestPred << i << "]->time[" << j << "] << \".\" << endl;" << endl;
	      oTestPred << "return 1;" << endl;
	      oTestPred << "}" << endl;
	      
	      oTestPred << "if( set.data[" << i << "]->cp[" << j << "] != ";
	      oTestPred << cp->initial[i][j] << " )" << endl;
	      oTestPred << "{" << endl;
	      oTestPred << "   cerr << \"set[" << i << "]->cp[" << j << "] != \\\"";
	      oTestPred << cp->initial[i][j] << "\\\"\" << endl; " << endl;
	      oTestPred << "   cerr << \"was \" << set.data[";
	      oTestPred << i << "]->cp[" << j << "] << \".\" << endl;" << endl;
	      oTestPred << "return 1;" << endl;
	      oTestPred << "}" << endl;
	      
	      oTestPred << "if( set.data[" << i << "]->dv[" << j << "] != ";
	      oTestPred << cp->initial[i][j] << " )" << endl;
	      oTestPred << "{" << endl;
	      oTestPred << "   cerr << \"set[" << i << "]->dv[" << j << "] != \\\"";
	      oTestPred << cp->initial[i][j] << "\\\"\" << endl; " << endl;
	      oTestPred << "   cerr << \"was \" << set.data[";
	      oTestPred << i << "]->dv[" << j << "] << \".\" << endl;" << endl;
	      oTestPred << "return 1;" << endl;
	      oTestPred << "}" << endl;
	      oTestPred << endl;
	    }
	}
      oTestPred << "double thetaIn[] = {";
      for( int i=0; i<thetaLen; i++ )
	{
	  if( i>0 )
	    oTestPred << ", ";
	  oTestPred << i+1;
	}
      oTestPred << "};" << endl;
      oTestPred << "double *epsIn = 0;" << endl;
      oTestPred << "double *etaIn = 0;" << endl;
      oTestPred << endl;

      oTestPred << "int thetaLen = " << thetaLen << ";" << endl;
      oTestPred << "int etaLen   = " << etaLen   << ";" << endl;
      oTestPred << "int epsLen   = " << epsLen   << ";" << endl;
      oTestPred << "int index_theta = 0;" << endl;
      oTestPred << "int index_eta   = index_theta + thetaLen;" << endl;
      oTestPred << "int index_eps   = index_eta + etaLen;" << endl;
      oTestPred << "vector< CppAD::AD<double> > indepVar( thetaLen + etaLen + epsLen );" << endl;
      oTestPred << "copy( thetaIn, thetaIn + thetaLen , indepVar.begin() + index_theta );" << endl;
      oTestPred << "copy( etaIn, etaIn + etaLen, indepVar.begin() + index_eta );" << endl;
      oTestPred << "copy( epsIn, epsIn + epsLen, indepVar.begin() + index_eps );" << endl;
      oTestPred << endl;

      oTestPred << "int fLen = " << N[0] << ";" << endl;
      oTestPred << "int yLen = " << N[0] << ";" << endl;
      oTestPred << "int index_f = 0;" << endl;
      oTestPred << "int index_y = 1;" << endl;
      oTestPred << "vector< CppAD::AD<double> > depVar( fLen + yLen );" << endl;
      oTestPred << "double yOut = 0.0;" << endl;
      oTestPred << "double fOut = 0.0;" << endl;
      oTestPred << endl;
      oTestPred << "double tol  = 0.0;" << endl;
      oTestPred << "double ans  = 0.0;" << endl;
      oTestPred << endl;
      
      oTestPred << "Pred< CppAD::AD<double> > pred(&set);" << endl;
      oTestPred << "bool ok = pred.eval( index_theta, thetaLen," << endl;
      oTestPred << "                     index_eta,   etaLen," << endl;
      oTestPred << "                     index_eps,   epsLen," << endl;
      oTestPred << "                     index_f,     fLen," << endl;
      oTestPred << "                     index_y,     yLen," << endl;
      oTestPred << "                     0, 0, " << endl;
      oTestPred << "                     indepVar," << endl;
      oTestPred << "                     depVar ); " << endl;
      oTestPred << "fOut = CppAD::Value(depVar[index_f]);" << endl;
      oTestPred << "yOut = CppAD::Value(depVar[index_y]);" << endl;
      oTestPred << "if( !ok )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   std::cerr << \"pred.eval() returned false, which is wrong.\" << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" << endl;
      oTestPred << "if( fOut != -0.0 )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"fOut should've been -0.0 but it was \" << fOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      oTestPred << "if( yOut !=0.0 )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"yOut should've been 0.0 but it was \" << yOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      
      oTestPred << "ok = pred.eval( index_theta, thetaLen," << endl;
      oTestPred << "                index_eta,   etaLen," << endl;
      oTestPred << "                index_eps,   epsLen," << endl;
      oTestPred << "                index_f,     fLen," << endl;
      oTestPred << "                index_y,     yLen," << endl;
      oTestPred << "                0, 1, " << endl;
      oTestPred << "                indepVar," << endl;
      oTestPred << "                depVar ); " << endl;
      oTestPred << "fOut = CppAD::Value(depVar[index_f]);" << endl;
      oTestPred << "yOut = CppAD::Value(depVar[index_y]);" << endl;
      oTestPred << "if( !ok )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   std::cerr << \"pred.eval() returned false, which is wrong.\" << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" << endl;
      oTestPred << "ans = 1.55029;" << endl;
      oTestPred << "tol = fabs(ans-fOut)/ans * 10.0;" << endl;
      oTestPred << "if( !( fOut >= ans-tol && fOut <= ans+tol ) )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"fOut should've been \" << ans << \" but it was \" << fOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      oTestPred << "ans = 2.55029;" << endl;
      oTestPred << "tol = fabs(ans-yOut)/ans * 10.0;" << endl;
      oTestPred << "if( !( yOut >= ans-tol && yOut <= ans+tol ) )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"yOut should've been \" << ans << \" but it was \" << yOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      
      oTestPred << "fOut = 0.0;" << endl;
      oTestPred << "yOut = 0.0;" << endl;
      oTestPred << "ok = pred.eval( index_theta, thetaLen," << endl;
      oTestPred << "                index_eta,   etaLen," << endl;
      oTestPred << "                index_eps,   epsLen," << endl;
      oTestPred << "                index_f,     fLen," << endl;
      oTestPred << "                index_y,     yLen," << endl;
      oTestPred << "                0, 2, " << endl;
      oTestPred << "                indepVar," << endl;
      oTestPred << "                depVar ); " << endl;
      oTestPred << "fOut = CppAD::Value(depVar[index_f]);" << endl;
      oTestPred << "yOut = CppAD::Value(depVar[index_y]);" << endl;
      oTestPred << "if( !ok )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   std::cerr << \"pred.eval() returned false, which is wrong.\" << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" << endl;
      oTestPred << "ans = 0.780131;" << endl;
      oTestPred << "tol = fabs(ans-fOut)/ans * 10.0;" << endl;
      oTestPred << "if( !( fOut >= ans-tol && fOut <= ans+tol ) )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"fOut should've been \" << ans << \" but it was \" << fOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      oTestPred << "ans = 1.78013;" << endl;
      oTestPred << "tol = fabs(ans-yOut)/ans * 10.0;" << endl;
      oTestPred << "if( !( yOut >= ans-tol && yOut <= ans+tol ) )" << endl;
      oTestPred << "{" << endl;
      oTestPred << "   cerr << \"yOut should've been \" << ans << \" but it was \" << yOut << endl;" << endl;
      oTestPred << "   return 1;" << endl;
      oTestPred << "}" <<endl;
      
      oTestPred << "return 0;" << endl;
      oTestPred << "}" << endl;
    }
  else
    {
      char buf[256];
      sprintf( buf, "Failed to open %s as writable.", fTestPred );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
  if( system( "g++ testIndPred.cpp -g -I./ -o testIndPred" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link the generated \"testIndPred.cpp\".", false );
  }
  if( system( "./testIndPred" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "The generated/built \"testIndPred\" failed to run successfully.", false );
  }

  XMLPlatformUtils::Terminate();
  //remove( fTestDriver );
  remove( gSource );

  rename( "driver.cpp", "indDriver.cpp" );

  if( system( "g++ indDriver.cpp -g -lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm -o indDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link the generated \"driver.cpp\".", false );
  }
  if( system( "./indDriver" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "The generated/built \"indDriver\" failed to run successfully.", false );
  }
}
CppUnit::Test * NonmemTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemTranslatorTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorTest>(
         "testInheritance", 
	 &NonmemTranslatorTest::testInheritance ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorTest>(
         "testParseIndSource", 
	 &NonmemTranslatorTest::testParseIndSource ) );
  /*
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorTest>(
         "testParsePopSource", 
	 &NonmemTranslatorTest::testParsePopSource ) );
  */
  return suiteOfTests;
}

