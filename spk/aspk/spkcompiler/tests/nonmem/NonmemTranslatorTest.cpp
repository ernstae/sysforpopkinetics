#include <iostream>
#include <valarray>
#include <fstream>
#include <string>
#include <stdio.h>
#include <map>

#include "NonmemTranslatorTest.h"

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

static const unsigned int maxChars = 2047;

static int factorial( int n )
{
  if( n == 0 )
    return 0;
  else
    return n + factorial( n-1 );
}

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
  const int nLabels = 3;
  map<string, string> labels;
  labels["ID"]   = "";
  labels["TIME"] = "";
  labels["CP"]   = "DV";

  const int thetaLen = 3;
  valarray<double> theta_in (thetaLen);
  valarray<double> theta_up (thetaLen);
  valarray<double> theta_low(thetaLen);
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
  int omegaElemNum = ( omegaStruct == Symbol::DIAGONAL? omegaDim : factorial(omegaDim) );
  valarray<double> omega_in (omegaElemNum);
  vector<bool>     omega_fix(omegaElemNum);
  for( int i=0; i<omegaElemNum; i++ )
    {
      omega_in[i]  = i+1;
      omega_fix[i] = ( i%2==0? true : false );
    }

  const int sigmaDim = 2;
  const Symbol::Structure sigmaStruct = Symbol::TRIANGLE;
  int sigmaElemNum = ( sigmaStruct == Symbol::DIAGONAL? sigmaDim : factorial(sigmaDim) );
  valarray<double> sigma_in (omegaElemNum);
  vector<bool>     sigma_fix(omegaElemNum);
  for( int i=0; i<sigmaElemNum; i++ )
    {
      sigma_in[i]  = i+1;
      sigma_fix[i] = ( i%2==0? true : false );
    }

  const int etaLen = thetaLen;
  valarray<double> eta_in (etaLen);
  vector<bool>     eta_fix(etaLen);
  eta_in = 0.0;
  for( int i=0; i<etaLen; i++ )
    eta_fix[i] = false;

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

      oSource << "</pop_analysis>" << endl;

      oSource << "<pop_stat covariance_form=\"" << pop_cov_form << "\" ";
      oSource << "is_standarderr_out=\""        << (pop_stderr? "yes":"no") << "\" ";
      oSource << "is_covariance_out=\""         << (pop_covariance? "yes":"no") << "\" ";
      oSource << "is_inverse_covariance_out=\"" << (pop_inv_covariance? "yes":"no") << "\" ";
      oSource << "is_correlation_out=\""        << (pop_correlation? "yes":"no") << "\"/>" << endl;

      if( isSimulate )
	{
	  oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
	}
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
      oSource << "   CL=THETA(3) * WT + ETA(3)" << endl;
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
  
  vector<int> N(pop_size);
  for( int i=0; i<pop_size; i++ )
     N[i] = i+1;
  NonmemTranslator xlator( data, source );
  SymbolTable *table = xlator.getSymbolTable();
  Symbol * id   = table->insertLabel( "ID",   "", N );
  Symbol * time = table->insertLabel( "TIME", "", N );
  Symbol * cp   = table->insertLabel( "CP",   "DV", N );

  id->  initial[0][0] = "#1";
  time->initial[0][0] = "0.0";
  cp->  initial[0][0] = "0.0";

  id->  initial[1][0] = "#2";
  id->  initial[1][1] = "#2";
  time->initial[1][0] = "0.0";
  time->initial[1][1] = "1.0";
  cp->  initial[1][0] = "0.0";
  cp->  initial[1][1] = "10.0";

  id->  initial[2][0] = "#3";
  id->  initial[2][1] = "#3";
  id->  initial[2][2] = "#3";
  time->initial[2][0] = "0.0";
  time->initial[2][1] = "1.0";
  time->initial[2][2] = "2.0";
  cp->  initial[2][0] = "0.0";
  cp->  initial[2][1] = "10.0";
  cp->  initial[2][2] = "20.0";

  xlator.parseSource();
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

  Symbol *theta = table->find( "theta" );
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
  
  Symbol *omega = table->find( "omega" );
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

  Symbol *sigma = table->find( "sigma" );
  CPPUNIT_ASSERT( sigma != Symbol::empty() );
  CPPUNIT_ASSERT( sigma->structure == sigmaStruct );
  CPPUNIT_ASSERT_EQUAL( sigmaDim, sigma->dimension[0] );
  for( int i=0; i<sigmaElemNum; i++ )
    {

      CPPUNIT_ASSERT( sigma_fix[i] == sigma->fixed[0][i] );
      CPPUNIT_ASSERT_EQUAL( sigma_in[i],  atof( sigma->initial[0][i].c_str() ) );
   }

  Symbol *eta = table->find( "eta" );
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

  //=====================================================
  // Test the generated C++ source code files
  // IndData.h, IndData.cpp, DataSet.h, DataSet.cpp
  //=====================================================
  // The order in which the variables appear in
  // the IndData constructor must be consistent with
  // with the order in which the variables are actually
  // passed in the construction of these objects
  // done in the DataSet constructor.
  char fTestDriver[] = "testDriver.cpp";
  ofstream oTestDriver( fTestDriver );
  if( oTestDriver.good() )
  {
     oTestDriver << "#include <iostream>" << endl;
     oTestDriver << "#include \"IndData.h\"" << endl;
     oTestDriver << "#include \"DataSet.h\"" << endl;
     oTestDriver << "using namespace std;" << endl;
     oTestDriver << "int main()" << endl;
     oTestDriver << "{" << endl;
     oTestDriver << "DataSet set;" << endl;

     for( int i=0; i<pop_size; i++ )
     {
        for( int j=0; j<N[i]; j++ )
        {
           oTestDriver << "if( set.dataset[" << i << "]->ID[";
           oTestDriver << j << "] != string(\"";
           oTestDriver << id->initial[i][j] << "\") )" << endl;
           oTestDriver << "{" << endl;
           oTestDriver << "   cerr << \"set[" << i << "]->ID[";
           oTestDriver << j << "] != \\\"";
           oTestDriver << id->initial[i][j] << "\\\"\" << endl; " << endl;
           oTestDriver << "   cerr << \"was \" << set.dataset[";
           oTestDriver << i << "]->ID[" << j << "] << \".\" << endl;" << endl;
           oTestDriver << "return 1;" << endl;
           oTestDriver << "}" << endl;

           oTestDriver << "if( set.dataset[" << i << "]->TIME[" << j << "] != ";
           oTestDriver << time->initial[i][j] << " )" << endl;
           oTestDriver << "{" << endl;
           oTestDriver << "   cerr << \"set[" << i << "]->TIME[" << j << "] != \\\"";
           oTestDriver << time->initial[i][j] << "\\\"\" << endl; " << endl;
           oTestDriver << "   cerr << \"was \" << set.dataset[";
           oTestDriver << i << "]->TIME[" << j << "] << \".\" << endl;" << endl;
           oTestDriver << "return 1;" << endl;
           oTestDriver << "}" << endl;
        
           oTestDriver << "if( set.dataset[" << i << "]->CP[" << j << "] != ";
           oTestDriver << cp->initial[i][j] << " )" << endl;
           oTestDriver << "{" << endl;
           oTestDriver << "   cerr << \"set[" << i << "]->CP[" << j << "] != \\\"";
           oTestDriver << cp->initial[i][j] << "\\\"\" << endl; " << endl;
           oTestDriver << "   cerr << \"was \" << set.dataset[";
           oTestDriver << i << "]->CP[" << j << "] << \".\" << endl;" << endl;
           oTestDriver << "return 1;" << endl;
           oTestDriver << "}" << endl;
           
           oTestDriver << "if( set.dataset[" << i << "]->DV[" << j << "] != ";
           oTestDriver << cp->initial[i][j] << " )" << endl;
           oTestDriver << "{" << endl;
           oTestDriver << "   cerr << \"set[" << i << "]->DV[" << j << "] != \\\"";
           oTestDriver << cp->initial[i][j] << "\\\"\" << endl; " << endl;
           oTestDriver << "   cerr << \"was \" << set.dataset[";
           oTestDriver << i << "]->DV[" << j << "] << \".\" << endl;" << endl;
           oTestDriver << "return 1;" << endl;
           oTestDriver << "}" << endl;
        }
     }
     oTestDriver << "return 0;" << endl;
     oTestDriver << "}" << endl;
  }
  else
  {
     char buf[256];
     sprintf( buf, "Failed to open %s as writable.", fTestDriver );
     CPPUNIT_ASSERT_MESSAGE( buf, false );
  }
  if( system( "g++ DataSet.cpp IndData.cpp testDriver.cpp -g -I./ -o test" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "Failed to compile/link.", false );
  }
  if( system( "./test" ) != 0 )
  {
     CPPUNIT_ASSERT_MESSAGE( "\"test\" failed.", false );
  }
  XMLPlatformUtils::Terminate();
}
void NonmemTranslatorTest::testParseIndSource()
{
  const int nLabels = 3;
  map<string, string> labels;
  labels["ID"]   = "";
  labels["TIME"] = "";
  labels["CP"]   = "DV";

  const int thetaLen = 3;
  valarray<double> theta_in (thetaLen);
  valarray<double> theta_up (thetaLen);
  valarray<double> theta_low(thetaLen);
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
  int omegaElemNum = ( omegaStruct == Symbol::DIAGONAL? omegaDim : factorial(omegaDim) );
  valarray<double> omega_in (omegaElemNum);
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

      oSource << "</ind_analysis>" << endl;

      oSource << "<ind_stat ";
      oSource << "is_standarderr_out=\""        << (ind_stderr? "yes":"no") << "\" ";
      oSource << "is_covariance_out=\""         << (ind_covariance? "yes":"no") << "\" ";
      oSource << "is_inverse_covariance_out=\"" << (ind_inv_covariance? "yes":"no") << "\" ";
      oSource << "is_correlation_out=\""        << (ind_correlation? "yes":"no") << "\"/>" << endl;

      if( isSimulate )
	{
	  oSource << "<simulation seed=\"" << seed << "\"/>" << endl;
	}
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
      oSource << "   CL=THETA(3) * WT" << endl;
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
  
  vector<int> N(3);
  N[0] = 1;
  N[1] = 2;
  N[2] = 3;
  NonmemTranslator xlator( data, source );
  SymbolTable *table = xlator.getSymbolTable();
  Symbol * id   = table->insertLabel( "ID",   "", N );
  Symbol * time = table->insertLabel( "TIME", "", N );
  Symbol * cp   = table->insertLabel( "CP",   "DV", N );

  id->  initial[0][0] = "#1";
  time->initial[0][0] = "0.0";
  cp->  initial[0][0] = "0.0";

  id->  initial[1][0] = "#2";
  id->  initial[1][1] = "#2";
  time->initial[1][0] = "0.0";
  time->initial[1][1] = "1.0";
  cp->  initial[1][0] = "0.0";
  cp->  initial[1][1] = "10.0";

  id->  initial[2][0] = "#3";
  id->  initial[2][1] = "#3";
  id->  initial[2][2] = "#3";
  time->initial[2][0] = "0.0";
  time->initial[2][1] = "1.0";
  time->initial[2][2] = "2.0";
  cp->  initial[2][0] = "0.0";
  cp->  initial[2][1] = "10.0";
  cp->  initial[2][2] = "20.0";

  xlator.parseSource();

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

  XMLPlatformUtils::Terminate();
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
         "testParsePopSource", 
	 &NonmemTranslatorTest::testParsePopSource ) );
  /*
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemTranslatorTest>(
         "testParseIndSource", 
	 &NonmemTranslatorTest::testParseIndSource ) );
  */
  return suiteOfTests;
}

