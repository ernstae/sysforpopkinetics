#include <iostream>
#include <valarray>
#include <fstream>

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

#include "SymbolTable.h"
#include "SpkParameters.h"
#include "NonmemTranslator.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void NonmemTranslatorTest::setUp()
{
}
void NonmemTranslatorTest::tearDown()
{
}

void NonmemTranslatorTest::testConstructor()
{
  char input[] = "NonmemTranslatorTestInput.xml";

  ifstream ifs( input );
  if( !ifs.good() )
    {
      char buf[256];
      sprintf( buf, "Failed to open %s!\n", input);
      fprintf( stderr, buf );
      exit(-1);
    }
  else
    ifs.close();

  SpkMLToCpp compiler( input );
}
void NonmemTranslatorTest::testTranslate()
{
  char input[] = "NonmemTranslatorTestInput.xml";

  ifstream ifs( input );
  if( !ifs.good() )
    {
      char buf[256];
      sprintf( buf, "Failed to open %s!\n", input);
      fprintf( stderr, buf );
      exit(-1);
    }
  else
    ifs.close();

  SpkMLToCpp compiler( input );

  try{
    compiler.translate();
  }
  catch(...)
    {
      CPPUNIT_ASSERT_MESSAGE( "Failed interpreting", false );
    }

  const struct NonmemParameters * nonmem = static_cast<const NonmemParameters*>(compiler.getClientParameters());

  const struct SpkParameters * spkRequired = compiler.getSpkParameters();

  const int nIndividuals = 12;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "#of individuals is supposed to be 12!", 
			  nIndividuals, spkRequired->nIndividuals );

  CPPUNIT_ASSERT_MESSAGE( "The analysis type is supposed to be SpkParameters::POPULATION!",
			  spkRequired->analysis == SpkParameters::POPULATION );
  // <optimization> section
  CPPUNIT_ASSERT_MESSAGE( "The objective is supposed to be FIRST_ORDER!", 
			  spkRequired->objective == FIRST_ORDER );
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(pop) epsilon is supposed to be 0.001!",
			  0.001, spkRequired->popEpsilon );
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(pop) mitr is supposed be 450!",
			  450, spkRequired->popMaxItr );
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(pop) trace is supposed to be 4!",
			  4, spkRequired->popTrace );
  CPPUNIT_ASSERT_MESSAGE( "(pop) popParOut should be requested!",
			  spkRequired->isPopParOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) popObjOut should be requeseted!", 
			  spkRequired->isPopObjOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) popObj_popParOut should be requeseted!", 
			  spkRequired->isPopObj_popParOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) popObj_popPar_popParOut should be declined!", 
			  !spkRequired->isPopObj_popPar_popParOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) warm start is supposed to be requested!",
			  spkRequired->isPopWarmStart );

  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(ind) epsilon is supposed to be 0.01!",
			  0.01, spkRequired->indEpsilon );
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(ind) mitr is supposed be 100!",
			  100, spkRequired->indMaxItr );
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "(ind) trace is supposed to be 0!",
			  0, spkRequired->indTrace );
  CPPUNIT_ASSERT_MESSAGE( "(ind) indParOut should be requested!",
			  spkRequired->isIndParOut);
  CPPUNIT_ASSERT_MESSAGE( "(ind) indObjOut should be requeseted!", 
			  spkRequired->isIndObjOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) indObj_indParOut should be requeseted!", 
			  spkRequired->isIndObj_indParOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) indObj_indPar_indParOut should be declined!", 
			  !spkRequired->isIndObj_indPar_indParOut );
  CPPUNIT_ASSERT_MESSAGE( "(ind) warm start is supposed to be requested!",
			  spkRequired->isIndWarmStart );

  // <statistics> section
  CPPUNIT_ASSERT_MESSAGE( "(pop) standard error computation is supposed to be requested!",
			  spkRequired->isPopStderrorOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) correlation computation is supposed to be requested!",
			  spkRequired->isPopCorrelationOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) covariance computation is supposed to be requested!",
			  spkRequired->isPopCovarianceOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) coefficient computation is supposed to be requested!",
			  spkRequired->isPopCoefficientOut );
  CPPUNIT_ASSERT_MESSAGE( "(pop) confidence interval computation is supposed to be requested!",
			  spkRequired->isPopConfidenceOut );
  
  CPPUNIT_ASSERT_MESSAGE( "(ind) standard error computation is supposed to be requested!",
			  !spkRequired->isIndStderrorOut );
  CPPUNIT_ASSERT_MESSAGE( "(ind) correlation computation is supposed to be requested!",
			  !spkRequired->isIndCorrelationOut );
  CPPUNIT_ASSERT_MESSAGE( "(ind) covariance computation is supposed to be requested!",
			  !spkRequired->isIndCovarianceOut );
  CPPUNIT_ASSERT_MESSAGE( "(ind) coefficient computation is supposed to be requested!",
			  !spkRequired->isIndCoefficientOut );
  CPPUNIT_ASSERT_MESSAGE( "(ind) confidence interval computation is supposed to be requested!",
			  !spkRequired->isIndConfidenceOut );

  int omegaElemNum = 3*(3+1)/2;
  valarray<double> omega( 0.1, omegaElemNum );
  valarray<double> omegaActual = nonmem->omegaIn;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "Omega: #of elements", omegaElemNum, static_cast<int>(omegaActual.size()) );

  for( int i=0; i<omegaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "Omega", omega[i], omegaActual[i] );

  valarray<bool> omegaFixed( false, omegaElemNum );
  valarray<bool> omegaFixedActual = nonmem->omegaFixed;
  omegaFixed[0] = true;
  omegaFixed[3] = true;
  for( int i=0; i<omegaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "Omega fixed", omegaFixed[i], omegaFixedActual[i] );

  int sigmaElemNum = 1*(1+1)/2;
  valarray<double> sigma( 0.3, sigmaElemNum );
  valarray<double> sigmaActual = nonmem->sigmaIn;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "Sigma: #of elements", sigmaElemNum, static_cast<int>(sigmaActual.size()) );

  for( int i=0; i<sigmaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "Sigma", sigma[i], sigmaActual[i] );

  int etaElemNum = 3;
  valarray<double> eta( 0.0, etaElemNum );
  valarray<double> etaActual = nonmem->etaIn;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "eta: #of elements", etaElemNum, static_cast<int>(etaActual.size()) );

  for( int i=0; i<etaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "eta", eta[i], etaActual[i] );

  valarray<bool> etaFixed( false, etaElemNum );
  valarray<bool> etaFixedActual = nonmem->etaFixed;
  etaFixed[1] = true;

  for( int i=0; i<etaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "eta fixed", etaFixed[i], etaFixedActual[i] );

  int thetaElemNum = 3;
  valarray<double> thetaIn( 0.1, thetaElemNum );
  valarray<double> thetaInActual = nonmem->thetaIn;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaIn: #of elements", 
				thetaElemNum, static_cast<int>(thetaInActual.size()) );

  for( int i=0; i<thetaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaIn", thetaIn[i], thetaInActual[i] );
  
  valarray<bool> thetaFixed(false, thetaElemNum);
  thetaFixed[0] = true;

  valarray<bool> thetaFixedActual = nonmem->thetaFixed;
  for( int i=0; i<thetaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaFixed", thetaFixed[i], thetaFixedActual[i] );

  valarray<double> thetaLow( -0.1, thetaElemNum );
  valarray<double> thetaLowActual = nonmem->thetaLow;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaLow: #of elements", 
				thetaElemNum, static_cast<int>(thetaLowActual.size()) );

  for( int i=0; i<thetaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaIn", thetaLow[i], thetaLowActual[i] );

  valarray<double> thetaUp( 1.0, thetaElemNum );
  valarray<double> thetaUpActual = nonmem->thetaUp;
  CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaUp: #of elements", 
				thetaElemNum, static_cast<int>(thetaUpActual.size()) );

  for( int i=0; i<thetaElemNum; i++ )
    CPPUNIT_ASSERT_EQUAL_MESSAGE( "thetaUp", thetaUp[i], thetaUpActual[i] );

}

CppUnit::Test * NonmemTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemTranslatorTest" );

  suiteOfTests->addTest( new CppUnit::TestCaller<NonmemTranslatorTest>("testConstructor",
						    &NonmemTranslatorTest::testConstructor ) );

  suiteOfTests->addTest( new CppUnit::TestCaller<NonmemTranslatorTest>("testTranslate",
						    &NonmemTranslatorTest::testTranslate ) );

   return suiteOfTests;
}

