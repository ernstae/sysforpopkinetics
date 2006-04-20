#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "../../spkcompiler/SymbolTable.h"
#include "SymbolTableTest.h"

using namespace std;
using namespace CppUnit;


void SymbolTableTest::setUp()
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
void SymbolTableTest::tearDown()
{
}
void SymbolTableTest::testEmpty()
{
  SymbolTable table;
  CPPUNIT_ASSERT( Symbol::empty() == Symbol::empty() );
}
void SymbolTableTest::testInsertScalar()
{
   SymbolTable table;

   // User & read-write
   string aaa_name( "aaa" );
   table.insertScalar( aaa_name, Symbol::USER, Symbol::READWRITE );
   Symbol * aaa = table.find( aaa_name );
   CPPUNIT_ASSERT( aaa != Symbol::empty() );
   CPPUNIT_ASSERT( aaa->name == aaa_name );
   CPPUNIT_ASSERT( aaa->object_type == Symbol::SCALAR );
   CPPUNIT_ASSERT( aaa->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( aaa->owner == Symbol::USER );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" ==  aaa->initial[0][0] );

   // User & read only
   string bbb_name( "bbb" );
   table.insertScalar( bbb_name, Symbol::USER, Symbol::READONLY );
   Symbol * bbb = table.find( bbb_name );
   CPPUNIT_ASSERT( bbb != Symbol::empty() );
   CPPUNIT_ASSERT( bbb->name == bbb_name );
   CPPUNIT_ASSERT( bbb->object_type == Symbol::SCALAR );
   CPPUNIT_ASSERT( bbb->access == Symbol::READONLY );
   CPPUNIT_ASSERT( bbb->owner == Symbol::USER );

   bbb->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" ==  bbb->initial[0][0] );

   // System & read-write
   string ccc_name( "ccc" );
   table.insertScalar( ccc_name, Symbol::SYSTEM, Symbol::READWRITE );
   Symbol * ccc = table.find( ccc_name );
   CPPUNIT_ASSERT( ccc != Symbol::empty() );
   CPPUNIT_ASSERT( ccc->name == ccc_name );
   CPPUNIT_ASSERT( ccc->object_type == Symbol::SCALAR );
   CPPUNIT_ASSERT( ccc->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( ccc->owner == Symbol::SYSTEM );

   ccc->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" ==  ccc->initial[0][0] );

   // System & read only
   string ddd_name( "ddd" );
   table.insertScalar( ddd_name, Symbol::SYSTEM, Symbol::READONLY );
   Symbol * ddd = table.find( ddd_name );
   CPPUNIT_ASSERT( ddd != Symbol::empty() );
   CPPUNIT_ASSERT( ddd->name == ddd_name );
   CPPUNIT_ASSERT( ddd->object_type == Symbol::SCALAR );
   CPPUNIT_ASSERT( ddd->access == Symbol::READONLY );
   CPPUNIT_ASSERT( ddd->owner == Symbol::SYSTEM );

   ddd->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" ==  ddd->initial[0][0] );
}
void SymbolTableTest::testInsertLabel()
{
   string empty_string( "" ); 
   SymbolTable table;
   string aaa_name( "aaa" );
   string aaa_alias( "AAA" );
   valarray<int> aaa_len( 2 );
   aaa_len[0] = 2;
   aaa_len[1] = 1;
   Symbol *aaa = table.insertLabel( aaa_name, aaa_alias, aaa_len );
   CPPUNIT_ASSERT( table.find( aaa_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( aaa_name ) == aaa );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );

}
void SymbolTableTest::testInsertVector()
{
   SymbolTable table;

   // User & read-write
   string aaa_name( "aaa" );
   int aaa_len = 2;
   table.insertVector( aaa_name, aaa_len, Symbol::USER, Symbol::READWRITE );
   Symbol *aaa = table.find( aaa_name );
   CPPUNIT_ASSERT( aaa != Symbol::empty() );
   CPPUNIT_ASSERT( aaa->name == aaa_name );
   CPPUNIT_ASSERT( aaa->object_type == Symbol::VECTOR );
   CPPUNIT_ASSERT( aaa->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( aaa->owner == Symbol::USER );
   CPPUNIT_ASSERT( aaa->dimension[0] == aaa_len );

   aaa->initial[0][0] = "3.0";
   aaa->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == aaa->initial[0][1] );

   // User & read only
   string bbb_name( "bbb" );
   int bbb_len = 2;
   table.insertVector( bbb_name, bbb_len, Symbol::USER, Symbol::READONLY );
   Symbol *bbb = table.find( bbb_name );
   CPPUNIT_ASSERT( bbb != Symbol::empty() );
   CPPUNIT_ASSERT( bbb->name == bbb_name );
   CPPUNIT_ASSERT( bbb->object_type == Symbol::VECTOR );
   CPPUNIT_ASSERT( bbb->access == Symbol::READONLY );
   CPPUNIT_ASSERT( bbb->owner == Symbol::USER );
   CPPUNIT_ASSERT( bbb->dimension[0] == bbb_len );

   bbb->initial[0][0] = "3.0";
   bbb->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == bbb->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == bbb->initial[0][1] );

   // System & read-write
   string ccc_name( "ccc" );
   int ccc_len = 2;
   table.insertVector( ccc_name, ccc_len, Symbol::SYSTEM, Symbol::READWRITE );
   Symbol *ccc = table.find( ccc_name );
   CPPUNIT_ASSERT( ccc != Symbol::empty() );
   CPPUNIT_ASSERT( ccc->name == ccc_name );
   CPPUNIT_ASSERT( ccc->object_type == Symbol::VECTOR );
   CPPUNIT_ASSERT( ccc->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( ccc->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( ccc->dimension[0] == ccc_len );

   ccc->initial[0][0] = "3.0";
   ccc->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == ccc->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == ccc->initial[0][1] );

   // System & read only
   string ddd_name( "ddd" );
   int ddd_len = 2;
   table.insertVector( ddd_name, ddd_len, Symbol::SYSTEM, Symbol::READONLY );
   Symbol *ddd = table.find( ddd_name );
   CPPUNIT_ASSERT( ddd != Symbol::empty() );
   CPPUNIT_ASSERT( ddd->name == ddd_name );
   CPPUNIT_ASSERT( ddd->object_type == Symbol::VECTOR );
   CPPUNIT_ASSERT( ddd->access == Symbol::READONLY );
   CPPUNIT_ASSERT( ddd->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( ddd->dimension[0] == ddd_len );

   ddd->initial[0][0] = "3.0";
   ddd->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == ddd->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == ddd->initial[0][1] );
}
void SymbolTableTest::testInsertSymmetricMatrix()
{
   SymbolTable table;

   // User & read-write, diagonal
   string aaa_name( "aaa" );
   valarray<unsigned int> aaa_dim( 2, 1 );
   valarray<Symbol::Structure> aaa_diag(Symbol::DIAGONAL, 1);
   table.insertSymmetricMatrix( aaa_name, aaa_diag , aaa_dim, Symbol::USER, Symbol::READWRITE );
   Symbol *aaa = table.find( aaa_name );
   CPPUNIT_ASSERT( aaa != Symbol::empty() );
   CPPUNIT_ASSERT( aaa->name == aaa_name );
   CPPUNIT_ASSERT( aaa->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( aaa->structure[0] == Symbol::DIAGONAL );
   CPPUNIT_ASSERT( aaa->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( aaa->owner == Symbol::USER );
   CPPUNIT_ASSERT( aaa->dimension[0] == aaa_dim[0] );

   aaa->initial[0][0] = "3.0";
   aaa->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == aaa->initial[0][1] );

   // User & read only, diagonal
   string bbb_name( "bbb" );
   valarray<unsigned int> bbb_dim( 2, 1 );
   valarray<Symbol::Structure> bbb_diag(Symbol::DIAGONAL, 1);
   table.insertSymmetricMatrix( bbb_name, bbb_diag, bbb_dim, Symbol::USER, Symbol::READONLY );
   Symbol *bbb = table.find( bbb_name );
   CPPUNIT_ASSERT( bbb != Symbol::empty() );
   CPPUNIT_ASSERT( bbb->name == bbb_name );
   CPPUNIT_ASSERT( bbb->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( bbb->structure[0] == Symbol::DIAGONAL );
   CPPUNIT_ASSERT( bbb->access == Symbol::READONLY );
   CPPUNIT_ASSERT( bbb->owner == Symbol::USER );
   CPPUNIT_ASSERT( bbb->dimension[0] == bbb_dim[0] );

   bbb->initial[0][0] = "3.0";
   bbb->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == bbb->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == bbb->initial[0][1] );

   // System & read-write, diagonal
   string ccc_name( "ccc" );
   valarray<unsigned int> ccc_dim( 2, 1 );
   valarray<Symbol::Structure> ccc_diag(Symbol::DIAGONAL, 1);
   table.insertSymmetricMatrix( ccc_name, ccc_diag, ccc_dim, Symbol::SYSTEM, Symbol::READWRITE );
   Symbol *ccc = table.find( ccc_name );
   CPPUNIT_ASSERT( ccc != Symbol::empty() );
   CPPUNIT_ASSERT( ccc->name == ccc_name );
   CPPUNIT_ASSERT( ccc->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( ccc->structure[0] == Symbol::DIAGONAL );
   CPPUNIT_ASSERT( ccc->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( ccc->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( ccc->dimension[0] == ccc_dim[0] );

   ccc->initial[0][0] = "3.0";
   ccc->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == ccc->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == ccc->initial[0][1] );

   // System & read only, diagonal
   string ddd_name( "ddd" );
   valarray<unsigned int> ddd_dim( 2, 1 );
   valarray<Symbol::Structure> ddd_diag(Symbol::DIAGONAL, 1);
   table.insertSymmetricMatrix( ddd_name, ddd_diag, ddd_dim, Symbol::SYSTEM, Symbol::READONLY );
   Symbol *ddd = table.find( ddd_name );
   CPPUNIT_ASSERT( ddd != Symbol::empty() );
   CPPUNIT_ASSERT( ddd->name == ddd_name );
   CPPUNIT_ASSERT( ddd->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( ddd->structure[0] == Symbol::DIAGONAL );
   CPPUNIT_ASSERT( ddd->access == Symbol::READONLY );
   CPPUNIT_ASSERT( ddd->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( ddd->dimension[0] == ddd_dim[0] );

   ddd->initial[0][0] = "3.0";
   ddd->initial[0][1] = "2.0";
   CPPUNIT_ASSERT( "3.0" == ddd->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == ddd->initial[0][1] );



   // User & read-write, full
   string eee_name( "eee" );
   valarray<unsigned int> eee_dim( 2, 1 );
   valarray<Symbol::Structure> eee_full(Symbol::FULL, 1);
   table.insertSymmetricMatrix( eee_name, eee_full, eee_dim, Symbol::USER, Symbol::READWRITE );
   Symbol *eee = table.find( eee_name );
   CPPUNIT_ASSERT( eee != Symbol::empty() );
   CPPUNIT_ASSERT( eee->name == eee_name );
   CPPUNIT_ASSERT( eee->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( eee->structure[0] == Symbol::FULL );
   CPPUNIT_ASSERT( eee->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( eee->owner == Symbol::USER );
   CPPUNIT_ASSERT( eee->dimension[0] == eee_dim[0] );

   eee->initial[0][0] = "3.0";
   eee->initial[0][1] = "2.0";
   eee->initial[0][2] = "1.0";
   CPPUNIT_ASSERT( "3.0" == eee->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == eee->initial[0][1] );
   CPPUNIT_ASSERT( "1.0" == eee->initial[0][2] );

   // User & read only, full
   string fff_name( "fff" );
   valarray<unsigned int> fff_dim( 2, 1 );
   valarray<Symbol::Structure> fff_full(Symbol::FULL, 1);
   table.insertSymmetricMatrix( fff_name, fff_full, fff_dim, Symbol::USER, Symbol::READONLY );
   Symbol *fff = table.find( fff_name );
   CPPUNIT_ASSERT( fff != Symbol::empty() );
   CPPUNIT_ASSERT( fff->name == fff_name );
   CPPUNIT_ASSERT( fff->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( fff->structure[0] == Symbol::FULL );
   CPPUNIT_ASSERT( fff->access == Symbol::READONLY );
   CPPUNIT_ASSERT( fff->owner == Symbol::USER );
   CPPUNIT_ASSERT( fff->dimension[0] == fff_dim[0] );

   fff->initial[0][0] = "3.0";
   fff->initial[0][1] = "2.0";
   fff->initial[0][2] = "1.0";
   CPPUNIT_ASSERT( "3.0" == fff->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == fff->initial[0][1] );
   CPPUNIT_ASSERT( "1.0" == fff->initial[0][2] );

   // System & read-write, full
   string ggg_name( "ggg" );
   valarray<unsigned int> ggg_dim( 2, 1 );
   valarray<Symbol::Structure> ggg_full(Symbol::FULL, 1);
   table.insertSymmetricMatrix( ggg_name, ggg_full, ggg_dim, Symbol::SYSTEM, Symbol::READWRITE );
   Symbol *ggg = table.find( ggg_name );
   CPPUNIT_ASSERT( ggg != Symbol::empty() );
   CPPUNIT_ASSERT( ggg->name == ggg_name );
   CPPUNIT_ASSERT( ggg->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( ggg->structure[0] == Symbol::FULL );
   CPPUNIT_ASSERT( ggg->access == Symbol::READWRITE );
   CPPUNIT_ASSERT( ggg->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( ggg->dimension[0] == ggg_dim[0] );

   ggg->initial[0][0] = "3.0";
   ggg->initial[0][1] = "2.0";
   ggg->initial[0][2] = "1.0";
   CPPUNIT_ASSERT( "3.0" == ggg->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == ggg->initial[0][1] );
   CPPUNIT_ASSERT( "1.0" == ggg->initial[0][2] );

   // System & read only, full
   string hhh_name( "hhh" );
   valarray<unsigned int> hhh_dim( 2, 1 );
   valarray<Symbol::Structure> hhh_full(Symbol::FULL, 1);
   table.insertSymmetricMatrix( hhh_name, hhh_full, hhh_dim, Symbol::SYSTEM, Symbol::READONLY );
   Symbol *hhh = table.find( hhh_name );
   CPPUNIT_ASSERT( hhh != Symbol::empty() );
   CPPUNIT_ASSERT( hhh->name == hhh_name );
   CPPUNIT_ASSERT( hhh->object_type == Symbol::MATRIX );
   CPPUNIT_ASSERT( hhh->structure[0] == Symbol::FULL );
   CPPUNIT_ASSERT( hhh->access == Symbol::READONLY );
   CPPUNIT_ASSERT( hhh->owner == Symbol::SYSTEM );
   CPPUNIT_ASSERT( hhh->dimension[0] == hhh_dim[0] );

   hhh->initial[0][0] = "3.0";
   hhh->initial[0][1] = "2.0";
   hhh->initial[0][2] = "1.0";
   CPPUNIT_ASSERT( "3.0" == hhh->initial[0][0] );
   CPPUNIT_ASSERT( "2.0" == hhh->initial[0][1] );
   CPPUNIT_ASSERT( "1.0" == hhh->initial[0][2] );
}

CppUnit::Test * SymbolTableTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );

  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testEmpty",
						    &SymbolTableTest::testEmpty ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertLabel",
						    &SymbolTableTest::testInsertLabel ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertScalar",
						    &SymbolTableTest::testInsertScalar ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertVector",
						    &SymbolTableTest::testInsertVector ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertSymmetricMatrix",
						    &SymbolTableTest::testInsertSymmetricMatrix ) );

  return suiteOfTests;
}
