#include <iostream>
#include <valarray>
#include <fstream>

#include "read_nonmem_modelTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <spk/printInMatrix.h>
#include <nonmem/read_nonmem_model.h>
#include <nonmem/NonmemTranslator.h>
#include <explang.h>

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void read_nonmem_modelTest::setUp()
{
  XMLPlatformUtils::Initialize();
}
void read_nonmem_modelTest::tearDown()
{
  XMLPlatformUtils::Terminate();
}

//
// ADVAN2: One Compartmental Linear Model with First Order Absorption
//
// Comp(1) : absorption/input
// Comp(2) : observation/central
// Comp(3) : elimination/output
//
// Should be used with either TRANS1 or TRANS2
// Should be used with SS2 (default) or SS6
// Must follow $PK and $ERROR records
//
// <Possible PK Parameters>
// with TRANS1: K, KA
// with TRANS2: K, KA, S1, S2(=SC), S3(=SO), F1, F2, R1, R2, D1, D2, ALAG1, ALAG2, F0(=F3, FO), XSCALE
//
// <Required TRANS2 Parameters>
// K, KA, CL, V --- where K = CL/V
//
// <Required ERROR Parameters>
// Y on the left hand side
// F and ETAn on the right hand side
//
void read_nonmem_modelTest::create_advan2_trans2(
   DOMDocument * doc, SymbolTable * table )
{  
  DOMElement* rootElem  = doc->getDocumentElement();  

  const int nMeasurements = 5;
  //
  // Assumption:
  // A data file contains WT data for each individual.
  //
  // <model base="advan2" [tolerance="interger"] >
  //    <pk>
  //       KA=THETA(1)+ETA(1)             --- KA: rate constant of absorption
  //       K =THETA(2)+ETA(2)             --- K:  rate constant of elimination
  //       CL=THETA(3)+WT*ETA(3)          --- CL: CL=K*V, where V is volume(t)
  //       SC=CL/K/WT                     --- Scale for central compartment
  //    </pk>
  //    <error>
  //       Y = F*EPS(1)
  //    </error>
  // </model>
  //
  DOMElement* model  = doc->createElement( XMLString::transcode( "model" ) );
  rootElem->appendChild( model ); 

  model->setAttribute( XMLString::transcode("base"), XMLString::transcode( "advan2" ) );
  model->setAttribute( XMLString::transcode("tolerance"),  XMLString::transcode( "3" ) );

  DOMElement * pkNode = doc->createElement( XMLString::transcode( "pk" ) );
  DOMNode * pk = doc->createTextNode( XMLString::transcode( "pk" ) );
  char pk_def[256];
  sprintf( pk_def, "ka=theta(1)+eta(1)\nK=THETA(2)+ETA(2)\n\
                    CL=THETA(3)+WT*ETA(3)\nSC=CL/K/WT\n" );

  Symbol ka    ("ka",    Symbol::SCALAR, Symbol::DOUBLE, true );
  Symbol theta ("theta", Symbol::VECTOR, Symbol::DOUBLE, true );
  theta.size(3);
  Symbol eta   ("eta",   Symbol::VECTOR, Symbol::DOUBLE, true );
  eta.size(3);
  Symbol k     ("k",     Symbol::SCALAR, Symbol::DOUBLE, true );
  Symbol cl    ("cl",    Symbol::SCALAR, Symbol::DOUBLE, true );
  Symbol sc    ("sc",    Symbol::SCALAR, Symbol::DOUBLE, true );
  Symbol wt    ("wt",    Symbol::VECTOR, Symbol::DOUBLE, false );
  wt.size(nMeasurements);

  table->insert( ka );
  table->insert( theta );
  table->insert( eta );
  table->insert( k );
  table->insert( cl );
  table->insert( sc );
  table->insert( wt );

  pk->setNodeValue( XMLString::transcode(pk_def) );

  pkNode->appendChild( pk );
  model->appendChild( pkNode );

  DOMElement * errorNode = doc->createElement( XMLString::transcode( "error" ) );
  DOMNode * error = doc->createTextNode( XMLString::transcode( "error" ) );
  char error_def[256];
  sprintf( error_def, "Y = F*EPS(1)\n" );
  error->setNodeValue( XMLString::transcode(error_def) );

  errorNode->appendChild( error );
  model->appendChild( errorNode );

  DOMWriter * writer = ((DOMImplementationLS*)impl)->createDOMWriter();
  writer->setNewLine( XMLString::transcode("\n") );
  StdOutFormatTarget destination;
  //  writer->writeNode( &destination, *rootElem );
}

void read_nonmem_modelTest::test()
{
  const int nIndividuals = 12;

  impl =  DOMImplementationRegistry::getDOMImplementation(XMLString::transcode("Core"));
  
  doc = impl->createDocument(
		0,                               // root element namespace URI.
		XMLString::transcode("spkinml"), // root element name
		0);                              // document type object (DTD).

  SymbolTable table;
  create_advan2_trans2( doc, &table );

  DOMElement * modelNode = dynamic_cast<DOMElement*>(
	     doc->getElementsByTagName( XMLString::transcode( "model" ) )->item(0) );
  //
  // register names found in data file
  //
  yydebug = 0;

  gSpkExpOutput = fopen( "junk", "w" );
  read_nonmem_model( modelNode, nIndividuals, &table );
  fclose( gSpkExpOutput );
  remove( "junk" );
  return;
}

CppUnit::Test * read_nonmem_modelTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "read_nonmem_modelTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<read_nonmem_modelTest>(
         "test",
	 &read_nonmem_modelTest::test ) );

   return suiteOfTests;
}
