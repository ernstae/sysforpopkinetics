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
#include <spkcompiler/nonmem/read_nonmem_model.h>
#include <spkcompiler/nonmem/NonmemTranslator.h>
#include <spkcompiler/nonmem/explang.h>

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

void read_nonmem_modelTest::create_pred(
   DOMDocument * doc, SymbolTable * table )
{
  //
  // <model [tolerance=INT]>
  //    <pred>
  //       ;THETA(1) = MEAN OBESRVATION RATE CONSTANT (1/HR)
  //       ;THETA(2) = MEAN ELIMINATION RATE CONSTANT (1/HR)
  //       ;THETA(3) = SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITER/HR/KG)
  //       ;DOSE = WT-ADJUSTED DOSE (MG/KG)
  //       ;DS = NON-WT-ADJUSTED DOSE (MG)
  //       IF (DOSE.NE.0) THEN
  //          DS = DOSE*WT
  //          W  = WT
  //       ENDIF
  //       KA = THETA(1) + ETA(1)
  //       KE = THETA(2) + ETA(2)
  //       CL = THETA(3) * W + ETA(3)
  //       D  = EXP( -KE * TIME ) - EXP( -KA * TIME )
  //       E  = CL * ( KA - KE )
  //       F  = DS * KE * KA / E * D
  //       Y = F + EPS(1)
  //    </pred>
  // </model> 
  //
  //
  // --- target ---
  //
  // #include <libspkcompiler/nonmem.h>
  // #include "IndData.h"
  //
  // namespace{
  //    IndDataSet all;
  //    double ka;
  //    double ke;
  //    double cl;
  //    double d;
  //    double e;
  //    double ds;
  //    double w;
  // };
  //
  // bool evalPred( const Type* const theta, 
  //                int nTheta,
  //                const Type* const eta, 
  //                int nEta,
  //                const Type* const eps,
  //                int nEps,
  //                int i, // who
  //                int j, // t(j)
  //                double& f, 
  //                double& y )
  // {
  //    double wt   = all[i].wt[j];
  //    double dose = all[i].dose[j];
  //    double time = all[i].time[j];
  //
  //    //THETA(1) = MEAN OBESRVATION RATE CONSTANT (1/HR)
  //    //THETA(2) = MEAN ELIMINATION RATE CONSTANT (1/HR)
  //    //THETA(3) = SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (1/HR/KG)
  //    //DOSE = WT-ADJUSTED DOSE (MG/KG)
  //    //DS = NON_WT_ADJUSTED DOSE (MG)
  //    if( dose != 0 )
  //    {
  //       ds = dose * wt;
  //       w  = wt;
  //    }
  //    ka = theta[0] + eta[0];
  //    ke = theta[1] + eta[1];
  //    cl = theta[2] = w + eta[2];
  //    d  = exp( -ke * time ) - exp( -ka * time );
  //    e  = cl * ( ka - ke );
  //    f = ds * ke * ka / e * d;
  //    y = f + eps[0];
  //
  //    //if( all[i].evid[j] != nonmem::OBSERVATION )
  //    //   return false;
  //    return true;
  // }
  //
  DOMElement * rootElem = doc->getDocumentElement();
   
  DOMElement* model  = doc->createElement( XMLString::transcode( "model" ) );
  rootElem->appendChild( model ); 

  model->setAttribute( XMLString::transcode("tolerance"),  XMLString::transcode( "3" ) );

  DOMElement * predNode = doc->createElement( XMLString::transcode( "pred" ) );
  DOMNode * pred = doc->createTextNode( XMLString::transcode( "pred" ) );
  char pred_def[512];
  sprintf( pred_def, "IF( DOSE.NE.0 ) THEN\n \
		      DS=DOSE * WT\n \
		      W=WT\n \
		      ENDIF\n \
		      KA=THETA(1) + ETA(1)\n \
		      KE=THETA(2) + ETA(2)\n \
		      CL=THETA(3) * W + ETA(3)\n \
		      D=EXP(-KE*TIME)-EXP(-KA*TIME)\n \
		      E=CL*(KA-KE)\n \
		      F=DS * KE * KA / E * D\n \
		      Y=F + EPS(1)\n" );
   //
   // NONMEM keywords
   //
   Symbol theta( "theta", Symbol::VECTOR, Symbol::DOUBLE, true );
   theta.size( 3 );
   Symbol eta  ( "eta",   Symbol::VECTOR, Symbol::DOUBLE, true );
   eta.size( 3 );
   Symbol eps  ( "eps",   Symbol::VECTOR, Symbol::DOUBLE, true );
   eps.size( 2 );
   table->insert( theta );
   table->insert( eta );
   table->insert( eps );

   pred->setNodeValue( XMLString::transcode(pred_def) );

   predNode->appendChild( pred );
   model->appendChild( predNode );


   DOMWriter * writer = ((DOMImplementationLS*)impl)->createDOMWriter();
   writer->setNewLine( XMLString::transcode("\n") );
   StdOutFormatTarget destination;

   //writer->writeNode( &destination, *rootElem );
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
  create_pred( doc, &table );

  //
  // Data labels && keywords
  //
  int n = 5;
  map<string, string> label_alias;
  label_alias["time"] = "";
  label_alias["wt"]   = "";
  label_alias["dose"] = "";
  label_alias["dv"]   = "";
  map<string, string>::const_iterator label = label_alias.begin();
  while( label != label_alias.end() )
  {
     Symbol xxx( label->first, Symbol::VECTOR, Symbol::DOUBLE, true );
     xxx.size( n );
     table.insert( xxx );
     if( label->second != "" )
     {
        Symbol yyy( label->second, Symbol::VECTOR, Symbol::DOUBLE, true );
	yyy.size( n );
	table.insert( yyy );
     }
     ++label;
  }
   
  DOMElement * modelNode = dynamic_cast<DOMElement*>(
	     doc->getElementsByTagName( XMLString::transcode( "model" ) )->item(0) );

  nm_debug = 0;
  gSpkExpOutput = fopen( "pred.cpp", "w" );
  read_nonmem_model( modelNode, nIndividuals, &table );
  fclose( gSpkExpOutput );



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
