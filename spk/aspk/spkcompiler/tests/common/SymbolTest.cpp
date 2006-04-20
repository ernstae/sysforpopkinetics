#include <iostream>
#include <string>
#include <vector>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "../../spkcompiler/Symbol.h"
#include "SymbolTest.h"

using namespace std;
using namespace CppUnit;

void SymbolTest::setUp()
{
  scalar     = Symbol::SCALAR;
  vec        = Symbol::VECTOR;
  matrix     = Symbol::MATRIX;
  full       = Symbol::FULL;
  diagonal   = Symbol::DIAGONAL;
  triangle   = Symbol::TRIANGLE;
  user       = Symbol::USER;
  system     = Symbol::SYSTEM;
  readonly   = Symbol::READONLY;
  readwrite  = Symbol::READWRITE;
}

void SymbolTest::tearDown()
{
}
void SymbolTest::testDefaultConstructor()
{
  string empty("");
  Symbol shallBeEmpty;
  CPPUNIT_ASSERT_EQUAL( empty, shallBeEmpty.name );
}
void SymbolTest::testConstructor()
{
  //=================================
  // SCALAR
  //=================================
  // User, read-write, scalar
  string aaa_name = "aaa";
  string aaa_synonym = "AAA";
  valarray<int> aaa_dim( 1, 1 );
  valarray<Symbol::Structure> aaa_full(Symbol::FULL, 1);
  Symbol aaa( aaa_name, aaa_synonym, Symbol::USER, Symbol::READWRITE, Symbol::SCALAR, aaa_full, aaa_dim );
  CPPUNIT_ASSERT_EQUAL( aaa_name, aaa.name );
  CPPUNIT_ASSERT_EQUAL( aaa_synonym, aaa.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, aaa.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, aaa.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, aaa.object_type );
  CPPUNIT_ASSERT_EQUAL( aaa_full[0], aaa.structure[0] );
  CPPUNIT_ASSERT_EQUAL( aaa_dim[0], aaa.dimension[0] );

  // User, read only, scalar
  string bbb_name = "bbb";
  string bbb_synonym = "BBB";
  valarray<int> bbb_dim( 1, 1 );
  valarray<Symbol::Structure> bbb_full(Symbol::FULL, 1);
  Symbol bbb( bbb_name, bbb_synonym, Symbol::USER, Symbol::READONLY, Symbol::SCALAR, bbb_full, bbb_dim );
  CPPUNIT_ASSERT_EQUAL( bbb_name, bbb.name );
  CPPUNIT_ASSERT_EQUAL( bbb_synonym, bbb.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, bbb.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, bbb.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, bbb.object_type );
  CPPUNIT_ASSERT_EQUAL( bbb_full[0], bbb.structure[0] );
  CPPUNIT_ASSERT_EQUAL( bbb_dim[0], bbb.dimension[0] );

  // System, read-write, scalar
  string ccc_name = "ccc";
  string ccc_synonym = "CCC";
  valarray<int> ccc_dim( 1, 1 );
  valarray<Symbol::Structure> ccc_full(Symbol::FULL, 1);
  Symbol ccc( ccc_name, ccc_synonym, Symbol::SYSTEM, Symbol::READWRITE, Symbol::SCALAR, ccc_full, ccc_dim );
  CPPUNIT_ASSERT_EQUAL( ccc_name, ccc.name );
  CPPUNIT_ASSERT_EQUAL( ccc_synonym, ccc.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ccc.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, ccc.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ccc.object_type );
  CPPUNIT_ASSERT_EQUAL( ccc_full[0], ccc.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ccc_dim[0], ccc.dimension[0] );

  // System, read only, scalar
  string ddd_name = "ddd";
  string ddd_synonym = "DDD";
  valarray<int> ddd_dim( 1, 1 );
  valarray<Symbol::Structure> ddd_full(Symbol::FULL, 1);
  Symbol ddd( ddd_name, ddd_synonym, Symbol::SYSTEM, Symbol::READONLY, Symbol::SCALAR, ddd_full, ddd_dim );
  CPPUNIT_ASSERT_EQUAL( ddd_name, ddd.name );
  CPPUNIT_ASSERT_EQUAL( ddd_synonym, ddd.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ddd.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, ddd.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ddd.object_type );
  CPPUNIT_ASSERT_EQUAL( ddd_full[0], ddd.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ddd_dim[0], ddd.dimension[0] );

  // User, read-write, vector
  // User, read only , vector
  // System, read-wite, vector
  // System, read only, vector
  
  // User, read-write, matrix, diagonal
  // User, read only , matrix, diagonal
  // System, read-wite, matrix, diagonal
  // System, read only, matrix, diagonal

  // User, read-write, matrix, full
  // User, read only , matrix, full
  // System, read-wite, matrix, full
  // System, read only, matrix, full
}
void SymbolTest::testCopyConstructor()
{
  //=================================
  // SCALAR
  //=================================
  // User, read-write, scalar
  string aaa_name = "aaa";
  string aaa_synonym = "AAA";
  valarray<int> aaa_dim( 1, 1 );
  valarray<Symbol::Structure> aaa_full(Symbol::FULL, 1);
  Symbol aaa( aaa_name, aaa_synonym, Symbol::USER, Symbol::READWRITE, Symbol::SCALAR, aaa_full, aaa_dim );
  Symbol a( aaa );
  CPPUNIT_ASSERT_EQUAL( aaa_name, aaa.name );
  CPPUNIT_ASSERT_EQUAL( aaa_synonym, aaa.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, aaa.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, aaa.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, aaa.object_type );
  CPPUNIT_ASSERT_EQUAL( aaa_full[0], aaa.structure[0] );
  CPPUNIT_ASSERT_EQUAL( aaa_dim[0], aaa.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( aaa.name, a.name );
  CPPUNIT_ASSERT_EQUAL( aaa.synonym, a.synonym );
  CPPUNIT_ASSERT_EQUAL( aaa.owner, a.owner );
  CPPUNIT_ASSERT_EQUAL( aaa.access, a.access );
  CPPUNIT_ASSERT_EQUAL( aaa.object_type, a.object_type );
  CPPUNIT_ASSERT_EQUAL( aaa.structure[0], a.structure[0] );
  CPPUNIT_ASSERT_EQUAL( aaa.dimension[0], a.dimension[0] );


  // User, read only, scalar
  string bbb_name = "bbb";
  string bbb_synonym = "BBB";
  valarray<int> bbb_dim( 1, 1 );
  valarray<Symbol::Structure> bbb_full(Symbol::FULL, 1);
  Symbol bbb( bbb_name, bbb_synonym, Symbol::USER, Symbol::READONLY, Symbol::SCALAR, bbb_full, bbb_dim );
  Symbol b( bbb );
  CPPUNIT_ASSERT_EQUAL( bbb_name, bbb.name );
  CPPUNIT_ASSERT_EQUAL( bbb_synonym, bbb.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, bbb.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, bbb.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, bbb.object_type );
  CPPUNIT_ASSERT_EQUAL( bbb_full[0], bbb.structure[0] );
  CPPUNIT_ASSERT_EQUAL( bbb_dim[0], bbb.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( bbb.name, b.name );
  CPPUNIT_ASSERT_EQUAL( bbb.synonym, b.synonym );
  CPPUNIT_ASSERT_EQUAL( bbb.owner, b.owner );
  CPPUNIT_ASSERT_EQUAL( bbb.access, b.access );
  CPPUNIT_ASSERT_EQUAL( bbb.object_type, b.object_type );
  CPPUNIT_ASSERT_EQUAL( bbb.structure[0], b.structure[0] );
  CPPUNIT_ASSERT_EQUAL( bbb.dimension[0], b.dimension[0] );


  // System, read-write, scalar
  string ccc_name = "ccc";
  string ccc_synonym = "CCC";
  valarray<int> ccc_dim( 1, 1 );
  valarray<Symbol::Structure> ccc_full(Symbol::FULL, 1);
  Symbol ccc( ccc_name, ccc_synonym, Symbol::SYSTEM, Symbol::READWRITE, Symbol::SCALAR, ccc_full, ccc_dim );
  Symbol c( ccc );
  CPPUNIT_ASSERT_EQUAL( ccc_name, ccc.name );
  CPPUNIT_ASSERT_EQUAL( ccc_synonym, ccc.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ccc.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, ccc.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ccc.object_type );
  CPPUNIT_ASSERT_EQUAL( ccc_full[0], ccc.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ccc_dim[0], ccc.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( ccc.name, c.name );
  CPPUNIT_ASSERT_EQUAL( ccc.synonym, c.synonym );
  CPPUNIT_ASSERT_EQUAL( ccc.owner, c.owner );
  CPPUNIT_ASSERT_EQUAL( ccc.access, c.access );
  CPPUNIT_ASSERT_EQUAL( ccc.object_type, c.object_type );
  CPPUNIT_ASSERT_EQUAL( ccc.structure[0], c.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ccc.dimension[0], c.dimension[0] );


  // System, read only, scalar
  string ddd_name = "ddd";
  string ddd_synonym = "DDD";
  valarray<int> ddd_dim( 1, 1 );
  valarray<Symbol::Structure> ddd_full(Symbol::FULL, 1);
  Symbol ddd( ddd_name, ddd_synonym, Symbol::SYSTEM, Symbol::READONLY, Symbol::SCALAR, ddd_full, ddd_dim );
  Symbol d( ddd );
  CPPUNIT_ASSERT_EQUAL( ddd_name, ddd.name );
  CPPUNIT_ASSERT_EQUAL( ddd_synonym, ddd.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ddd.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, ddd.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ddd.object_type );
  CPPUNIT_ASSERT_EQUAL( ddd_full[0], ddd.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ddd_dim[0], ddd.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( ddd.name, d.name );
  CPPUNIT_ASSERT_EQUAL( ddd.synonym, d.synonym );
  CPPUNIT_ASSERT_EQUAL( ddd.owner, d.owner );
  CPPUNIT_ASSERT_EQUAL( ddd.access, d.access );
  CPPUNIT_ASSERT_EQUAL( ddd.object_type, d.object_type );
  CPPUNIT_ASSERT_EQUAL( ddd.structure[0], d.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ddd.dimension[0], d.dimension[0] );

  // User, read-write, vector
  // User, read only , vector
  // System, read-wite, vector
  // System, read only, vector
  
  // User, read-write, matrix, diagonal
  // User, read only , matrix, diagonal
  // System, read-wite, matrix, diagonal
  // System, read only, matrix, diagonal

  // User, read-write, matrix, full
  // User, read only , matrix, full
  // System, read-wite, matrix, full
  // System, read only, matrix, full
}
void SymbolTest::testAssign()
{
  //=================================
  // SCALAR
  //=================================
  // User, read-write, scalar
  string aaa_name = "aaa";
  string aaa_synonym = "AAA";
  valarray<int> aaa_dim( 1, 1 );
  valarray<Symbol::Structure> aaa_full(Symbol::FULL, 1);
  Symbol aaa( aaa_name, aaa_synonym, Symbol::USER, Symbol::READWRITE, Symbol::SCALAR, aaa_full, aaa_dim );
  Symbol a = aaa;
  CPPUNIT_ASSERT_EQUAL( aaa_name, aaa.name );
  CPPUNIT_ASSERT_EQUAL( aaa_synonym, aaa.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, aaa.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, aaa.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, aaa.object_type );
  CPPUNIT_ASSERT_EQUAL( aaa_full[0], aaa.structure[0] );
  CPPUNIT_ASSERT_EQUAL( aaa_dim[0], aaa.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( aaa.name, a.name );
  CPPUNIT_ASSERT_EQUAL( aaa.synonym, a.synonym );
  CPPUNIT_ASSERT_EQUAL( aaa.owner, a.owner );
  CPPUNIT_ASSERT_EQUAL( aaa.access, a.access );
  CPPUNIT_ASSERT_EQUAL( aaa.object_type, a.object_type );
  CPPUNIT_ASSERT_EQUAL( aaa.structure[0], a.structure[0] );
  CPPUNIT_ASSERT_EQUAL( aaa.dimension[0], a.dimension[0] );


  // User, read only, scalar
  string bbb_name = "bbb";
  string bbb_synonym = "BBB";
  valarray<int> bbb_dim( 1, 1 );
  valarray<Symbol::Structure> bbb_full(Symbol::FULL, 1);
  Symbol bbb( bbb_name, bbb_synonym, Symbol::USER, Symbol::READONLY, Symbol::SCALAR, bbb_full, bbb_dim );
  Symbol b = bbb;
  CPPUNIT_ASSERT_EQUAL( bbb_name, bbb.name );
  CPPUNIT_ASSERT_EQUAL( bbb_synonym, bbb.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::USER, bbb.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, bbb.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, bbb.object_type );
  CPPUNIT_ASSERT_EQUAL( bbb_full[0], bbb.structure[0] );
  CPPUNIT_ASSERT_EQUAL( bbb_dim[0], bbb.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( bbb.name, b.name );
  CPPUNIT_ASSERT_EQUAL( bbb.synonym, b.synonym );
  CPPUNIT_ASSERT_EQUAL( bbb.owner, b.owner );
  CPPUNIT_ASSERT_EQUAL( bbb.access, b.access );
  CPPUNIT_ASSERT_EQUAL( bbb.object_type, b.object_type );
  CPPUNIT_ASSERT_EQUAL( bbb.structure[0], b.structure[0] );
  CPPUNIT_ASSERT_EQUAL( bbb.dimension[0], b.dimension[0] );


  // System, read-write, scalar
  string ccc_name = "ccc";
  string ccc_synonym = "CCC";
  valarray<int> ccc_dim( 1, 1 );
  valarray<Symbol::Structure> ccc_full(Symbol::FULL, 1);
  Symbol ccc( ccc_name, ccc_synonym, Symbol::SYSTEM, Symbol::READWRITE, Symbol::SCALAR, ccc_full, ccc_dim );
  Symbol c = ccc;
  CPPUNIT_ASSERT_EQUAL( ccc_name, ccc.name );
  CPPUNIT_ASSERT_EQUAL( ccc_synonym, ccc.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ccc.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READWRITE, ccc.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ccc.object_type );
  CPPUNIT_ASSERT_EQUAL( ccc_full[0], ccc.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ccc_dim[0], ccc.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( ccc.name, c.name );
  CPPUNIT_ASSERT_EQUAL( ccc.synonym, c.synonym );
  CPPUNIT_ASSERT_EQUAL( ccc.owner, c.owner );
  CPPUNIT_ASSERT_EQUAL( ccc.access, c.access );
  CPPUNIT_ASSERT_EQUAL( ccc.object_type, c.object_type );
  CPPUNIT_ASSERT_EQUAL( ccc.structure[0], c.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ccc.dimension[0], c.dimension[0] );


  // System, read only, scalar
  string ddd_name = "ddd";
  string ddd_synonym = "DDD";
  valarray<int> ddd_dim( 1, 1 );
  valarray<Symbol::Structure> ddd_full(Symbol::FULL, 1);
  Symbol ddd( ddd_name, ddd_synonym, Symbol::SYSTEM, Symbol::READONLY, Symbol::SCALAR, ddd_full, ddd_dim );
  Symbol d = ddd;
  CPPUNIT_ASSERT_EQUAL( ddd_name, ddd.name );
  CPPUNIT_ASSERT_EQUAL( ddd_synonym, ddd.synonym );
  CPPUNIT_ASSERT_EQUAL( Symbol::SYSTEM, ddd.owner );
  CPPUNIT_ASSERT_EQUAL( Symbol::READONLY, ddd.access );
  CPPUNIT_ASSERT_EQUAL( Symbol::SCALAR, ddd.object_type );
  CPPUNIT_ASSERT_EQUAL( ddd_full[0], ddd.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ddd_dim[0], ddd.dimension[0] );

  CPPUNIT_ASSERT_EQUAL( ddd.name, d.name );
  CPPUNIT_ASSERT_EQUAL( ddd.synonym, d.synonym );
  CPPUNIT_ASSERT_EQUAL( ddd.owner, d.owner );
  CPPUNIT_ASSERT_EQUAL( ddd.access, d.access );
  CPPUNIT_ASSERT_EQUAL( ddd.object_type, d.object_type );
  CPPUNIT_ASSERT_EQUAL( ddd.structure[0], d.structure[0] );
  CPPUNIT_ASSERT_EQUAL( ddd.dimension[0], d.dimension[0] );

  // User, read-write, vector
  // User, read only , vector
  // System, read-wite, vector
  // System, read only, vector
  
  // User, read-write, matrix, diagonal
  // User, read only , matrix, diagonal
  // System, read-wite, matrix, diagonal
  // System, read only, matrix, diagonal

  // User, read-write, matrix, full
  // User, read only , matrix, full
  // System, read-wite, matrix, full
  // System, read only, matrix, full
}
void SymbolTest::testCreateLabel()
{  
  string label( "CP" );
  string synonym( "DV" );
  valarray<int> dim(3);
  dim[0] = 5;
  dim[1] = 2;
  dim[2] = 3;
  Symbol cp = Symbol::createLabel( label, synonym, dim );
  CPPUNIT_ASSERT_EQUAL( label,     cp.name );
  CPPUNIT_ASSERT_EQUAL( synonym,   cp.synonym );
  CPPUNIT_ASSERT_EQUAL( vec,       cp.object_type );
  CPPUNIT_ASSERT_EQUAL( full,      cp.structure[0] );   //????????????
  CPPUNIT_ASSERT_EQUAL( Symbol::DATASET,     cp.owner );
  CPPUNIT_ASSERT_EQUAL( readonly,  cp.access );
  CPPUNIT_ASSERT_EQUAL( 3,         static_cast<int>( cp.dimension.size() ) );
  CPPUNIT_ASSERT_EQUAL( 5,         cp.dimension[0] );
  CPPUNIT_ASSERT_EQUAL( 2,         cp.dimension[1] );
  CPPUNIT_ASSERT_EQUAL( 3,         cp.dimension[2] );
}
void SymbolTest::testCreateScalar()
{  
  // User & read-write
  string aaa_name( "aaa" );
  Symbol aaa = Symbol::createScalar( aaa_name, Symbol::USER, Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.name == aaa_name );
  CPPUNIT_ASSERT( aaa.object_type == Symbol::SCALAR );
  CPPUNIT_ASSERT( aaa.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.owner == Symbol::USER );

  aaa.initial[0][0] = "3.0";
  CPPUNIT_ASSERT( "3.0" ==  aaa.initial[0][0] );

  // User & read only
  string bbb_name( "bbb" );
  Symbol bbb = Symbol::createScalar( bbb_name, Symbol::USER, Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.name == bbb_name );
  CPPUNIT_ASSERT( bbb.object_type == Symbol::SCALAR );
  CPPUNIT_ASSERT( bbb.access == Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.owner == Symbol::USER );

  bbb.initial[0][0] = "3.0";
  CPPUNIT_ASSERT( "3.0" ==  bbb.initial[0][0] );

  // System & read-write
  string ccc_name( "ccc" );
  Symbol ccc = Symbol::createScalar( ccc_name, Symbol::SYSTEM, Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.name == ccc_name );
  CPPUNIT_ASSERT( ccc.object_type == Symbol::SCALAR );
  CPPUNIT_ASSERT( ccc.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.owner == Symbol::SYSTEM );

  ccc.initial[0][0] = "3.0";
  CPPUNIT_ASSERT( "3.0" ==  ccc.initial[0][0] );

  // System & read only
  string ddd_name( "ddd" );
  Symbol ddd = Symbol::createScalar( ddd_name, Symbol::SYSTEM, Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.name == ddd_name );
  CPPUNIT_ASSERT( ddd.object_type == Symbol::SCALAR );
  CPPUNIT_ASSERT( ddd.access == Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.owner == Symbol::SYSTEM );

  ddd.initial[0][0] = "3.0";
  CPPUNIT_ASSERT( "3.0" ==  ddd.initial[0][0] );
}
void SymbolTest::testCreateVector()
{  
  // User & read-write
  string aaa_name( "aaa" );
  int aaa_len = 2;
  Symbol aaa = Symbol::createVector( aaa_name, aaa_len, Symbol::USER, Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.name == aaa_name );
  CPPUNIT_ASSERT( aaa.object_type == Symbol::VECTOR );
  CPPUNIT_ASSERT( aaa.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.owner == Symbol::USER );
  CPPUNIT_ASSERT( aaa.dimension[0] == aaa_len );

  aaa.initial[0][0] = "3.0";
  aaa.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == aaa.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == aaa.initial[0][1] );

  // User & read only
  string bbb_name( "bbb" );
  int bbb_len = 2;
  Symbol bbb = Symbol::createVector( bbb_name, bbb_len, Symbol::USER, Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.name == bbb_name );
  CPPUNIT_ASSERT( bbb.object_type == Symbol::VECTOR );
  CPPUNIT_ASSERT( bbb.access == Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.owner == Symbol::USER );
  CPPUNIT_ASSERT( bbb.dimension[0] == bbb_len );

  bbb.initial[0][0] = "3.0";
  bbb.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == bbb.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == bbb.initial[0][1] );

  // System & read-write
  string ccc_name( "ccc" );
  int ccc_len = 2;
  Symbol ccc = Symbol::createVector( ccc_name, ccc_len, Symbol::SYSTEM, Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.name == ccc_name );
  CPPUNIT_ASSERT( ccc.object_type == Symbol::VECTOR );
  CPPUNIT_ASSERT( ccc.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( ccc.dimension[0] == ccc_len );

  ccc.initial[0][0] = "3.0";
  ccc.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == ccc.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == ccc.initial[0][1] );

  // System & read only
  string ddd_name( "ddd" );
  int ddd_len = 2;
  Symbol ddd = Symbol::createVector( ddd_name, ddd_len, Symbol::SYSTEM, Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.name == ddd_name );
  CPPUNIT_ASSERT( ddd.object_type == Symbol::VECTOR );
  CPPUNIT_ASSERT( ddd.access == Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( ddd.dimension[0] == ddd_len );

  ddd.initial[0][0] = "3.0";
  ddd.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == ddd.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == ddd.initial[0][1] );
}
void SymbolTest::testCreateSymmetricMatrix()
{
  //==================================
  // DIAGONAL
  //==================================
  // User & read-write, diagonal
  string aaa_name( "aaa" );
  valarray<unsigned int> aaa_dim( 2, 1 );
  valarray<Symbol::Structure> aaa_diag(Symbol::DIAGONAL, 1);
  Symbol aaa = Symbol::createSymmetricMatrix( aaa_name, aaa_diag, aaa_dim, Symbol::USER, Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.name == aaa_name );
  CPPUNIT_ASSERT( aaa.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( aaa.structure[0] == Symbol::DIAGONAL );
  CPPUNIT_ASSERT( aaa.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( aaa.owner == Symbol::USER );
  CPPUNIT_ASSERT( aaa.dimension[0] == aaa_dim[0] );

  aaa.initial[0][0] = "3.0";
  aaa.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == aaa.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == aaa.initial[0][1] );

  // User & read only, diagonal
  string bbb_name( "bbb" );
  valarray<unsigned int> bbb_dim( 2, 1 );
  valarray<Symbol::Structure> bbb_diag(Symbol::DIAGONAL, 1);
  Symbol bbb = Symbol::createSymmetricMatrix( bbb_name, bbb_diag, bbb_dim, Symbol::USER, Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.name == bbb_name );
  CPPUNIT_ASSERT( bbb.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( bbb.structure[0] == Symbol::DIAGONAL );
  CPPUNIT_ASSERT( bbb.access == Symbol::READONLY );
  CPPUNIT_ASSERT( bbb.owner == Symbol::USER );
  CPPUNIT_ASSERT( bbb.dimension[0] == bbb_dim[0] );

  bbb.initial[0][0] = "3.0";
  bbb.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == bbb.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == bbb.initial[0][1] );

  // System & read-write, diagonal
  string ccc_name( "ccc" );
  valarray<unsigned int> ccc_dim( 2, 1 );
  valarray<Symbol::Structure> ccc_diag(Symbol::DIAGONAL, 1);
  Symbol ccc = Symbol::createSymmetricMatrix( ccc_name, ccc_diag, ccc_dim, Symbol::SYSTEM, Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.name == ccc_name );
  CPPUNIT_ASSERT( ccc.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( ccc.structure[0] == Symbol::DIAGONAL );
  CPPUNIT_ASSERT( ccc.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( ccc.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( ccc.dimension[0] == ccc_dim[0] );

  ccc.initial[0][0] = "3.0";
  ccc.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == ccc.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == ccc.initial[0][1] );

  // System & read only, diagonal
  string ddd_name( "ddd" );
  valarray<unsigned int> ddd_dim( 2, 1 );
  valarray<Symbol::Structure> ddd_diag(Symbol::DIAGONAL, 1);
  Symbol ddd = Symbol::createSymmetricMatrix( ddd_name, ddd_diag, ddd_dim, Symbol::SYSTEM, Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.name == ddd_name );
  CPPUNIT_ASSERT( ddd.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( ddd.structure[0] == Symbol::DIAGONAL );
  CPPUNIT_ASSERT( ddd.access == Symbol::READONLY );
  CPPUNIT_ASSERT( ddd.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( ddd.dimension[0] == ddd_dim[0] );

  ddd.initial[0][0] = "3.0";
  ddd.initial[0][1] = "2.0";
  CPPUNIT_ASSERT( "3.0" == ddd.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == ddd.initial[0][1] );

  //==================================
  // FULL
  //==================================
  // User & read-write, full
  string eee_name( "eee" );
  valarray<unsigned int> eee_dim( 2, 1 );
  valarray<Symbol::Structure> eee_full(Symbol::FULL, 1);
  Symbol eee = Symbol::createSymmetricMatrix( eee_name, eee_full, eee_dim, Symbol::USER, Symbol::READWRITE );
  CPPUNIT_ASSERT( eee.name == eee_name );
  CPPUNIT_ASSERT( eee.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( eee.structure[0] == Symbol::FULL );
  CPPUNIT_ASSERT( eee.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( eee.owner == Symbol::USER );
  CPPUNIT_ASSERT( eee.dimension[0] == eee_dim[0] );

  eee.initial[0][0] = "3.0";
  eee.initial[0][1] = "2.0";
  eee.initial[0][2] = "1.0";
  CPPUNIT_ASSERT( "3.0" == eee.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == eee.initial[0][1] );
  CPPUNIT_ASSERT( "1.0" == eee.initial[0][2] );

  // User & read only, full
  string fff_name( "fff" );
  valarray<unsigned int> fff_dim( 2, 1 );
  valarray<Symbol::Structure> fff_full(Symbol::FULL, 1);
  Symbol fff = Symbol::createSymmetricMatrix( fff_name, fff_full, fff_dim, Symbol::USER, Symbol::READONLY );
  CPPUNIT_ASSERT( fff.name == fff_name );
  CPPUNIT_ASSERT( fff.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( fff.structure[0] == Symbol::FULL );
  CPPUNIT_ASSERT( fff.access == Symbol::READONLY );
  CPPUNIT_ASSERT( fff.owner == Symbol::USER );
  CPPUNIT_ASSERT( fff.dimension[0] == fff_dim[0] );

  fff.initial[0][0] = "3.0";
  fff.initial[0][1] = "2.0";
  fff.initial[0][2] = "1.0";
  CPPUNIT_ASSERT( "3.0" == fff.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == fff.initial[0][1] );
  CPPUNIT_ASSERT( "1.0" == fff.initial[0][2] );

  // System & read-write, full
  string ggg_name( "ggg" );
  valarray<unsigned int> ggg_dim( 2, 1 );
  valarray<Symbol::Structure> ggg_full(Symbol::FULL, 1);
  Symbol ggg = Symbol::createSymmetricMatrix( ggg_name, ggg_full, ggg_dim, Symbol::SYSTEM, Symbol::READWRITE );
  CPPUNIT_ASSERT( ggg.name == ggg_name );
  CPPUNIT_ASSERT( ggg.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( ggg.structure[0] == Symbol::FULL );
  CPPUNIT_ASSERT( ggg.access == Symbol::READWRITE );
  CPPUNIT_ASSERT( ggg.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( ggg.dimension[0] == ggg_dim[0] );

  ggg.initial[0][0] = "3.0";
  ggg.initial[0][1] = "2.0";
  ggg.initial[0][2] = "1.0";
  CPPUNIT_ASSERT( "3.0" == ggg.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == ggg.initial[0][1] );
  CPPUNIT_ASSERT( "1.0" == ggg.initial[0][2] );

  // System & read only, full
  string hhh_name( "hhh" );
  valarray<unsigned int> hhh_dim( 2, 1 );
  valarray<Symbol::Structure> hhh_full(Symbol::FULL, 1);
  Symbol hhh = Symbol::createSymmetricMatrix( hhh_name, hhh_full, hhh_dim, Symbol::SYSTEM, Symbol::READONLY );
  CPPUNIT_ASSERT( hhh.name == hhh_name );
  CPPUNIT_ASSERT( hhh.object_type == Symbol::MATRIX );
  CPPUNIT_ASSERT( hhh.structure[0] == Symbol::FULL );
  CPPUNIT_ASSERT( hhh.access == Symbol::READONLY );
  CPPUNIT_ASSERT( hhh.owner == Symbol::SYSTEM );
  CPPUNIT_ASSERT( hhh.dimension[0] == hhh_dim[0] );

  hhh.initial[0][0] = "3.0";
  hhh.initial[0][1] = "2.0";
  hhh.initial[0][2] = "1.0";
  CPPUNIT_ASSERT( "3.0" == hhh.initial[0][0] );
  CPPUNIT_ASSERT( "2.0" == hhh.initial[0][1] );
  CPPUNIT_ASSERT( "1.0" == hhh.initial[0][2] );
}

void SymbolTest::testEquality()
{
  string empty_string( "" );

  string str_aaa( "aaa" );
  string str_AAA( "AAA" );
  string str_bbb( "bbb" );
  string str_BBB( "BBB" );
  valarray<int> dims( 3 );
  dims[0] = 1;
  dims[1] = 2;
  dims[2] = 3;
  Symbol aaa = Symbol::createLabel( str_aaa, str_AAA, dims );
  Symbol bbb = Symbol::createLabel( str_bbb, str_BBB, dims );

  CPPUNIT_ASSERT( aaa == aaa );
  CPPUNIT_ASSERT( aaa != bbb );
}

void SymbolTest::testEmpty()
{
  Symbol nonEmpty;

  CPPUNIT_ASSERT( Symbol::empty()==Symbol::empty() );
  CPPUNIT_ASSERT( Symbol::empty() != &(nonEmpty) );
}
CppUnit::Test * SymbolTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );
  
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testDefaultConstructor",
							     &SymbolTest::testDefaultConstructor ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testConstructor",
							     &SymbolTest::testConstructor ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCopyConstructor",
							     &SymbolTest::testCopyConstructor ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testAssign",
							     &SymbolTest::testAssign ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateLabel",
							     &SymbolTest::testCreateLabel ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateScalar",
							     &SymbolTest::testCreateScalar ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateVector",
							     &SymbolTest::testCreateVector ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateSymmetricMatrix",
							     &SymbolTest::testCreateSymmetricMatrix ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testEquality",
							     &SymbolTest::testEquality ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testEmpty",
							     &SymbolTest::testEmpty ) );
  return suiteOfTests;
}

