#include <iostream>
#include <valarray>
#include <fstream>
#include <string>

#include "explangTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <spkcompiler/SymbolTable.h>
#include <spkcompiler/nonmem/explang.h>

using namespace std;
using namespace CppUnit;
using namespace xercesc;

extern "C"{
  int nm_lex(void);  
  int nm_parse(void);
};
extern int                gSpkExpLines;
extern int                gSpkExpErrors;
extern SymbolTable      * gSpkExpSymbolTable;
extern FILE             * gSpkExpOutput;
extern FILE             * nm_in;
extern int                nm_debug;


void explangTest::setUp()
{
}
void explangTest::tearDown()
{
}
void explangTest::testScalarAssignmentToScalar()
{
  // Attention!!! nm_lex() converts any capitalized letter to lower case.

  SymbolTable table;
  char input[]        = "testScalarAssignmentToScalar.in";
  char output[]       = "testScalarAssignmentToScalar.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "A = 1\n",       pInput );
  fputs( "A = 1.0\n",     pInput );
  fputs( "A = 1.E01\n",   pInput );
  fputs( "A = 1.0E01\n",  pInput );
  fputs( "A = 1.0E1\n",   pInput );
  fputs( "A = +1\n",      pInput );
  fputs( "A = +1.0\n",    pInput );
  fputs( "A = -1.E01\n",  pInput );
  fputs( "A = -1.0E01\n", pInput );
  fputs( "A = -1.0E1\n",  pInput );
  fputs( "A = (1)\n",     pInput );
  fputs( "A = +(1)\n",    pInput );
  fputs( "A = -(1)\n",    pInput );

  fclose( pInput );

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 13, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  string buf;
  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1.0;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1.e01;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1.0e01;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1.0e1;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "+1;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "+1.0;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-1.e01;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-1.0e01;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-1.0e1;" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "+(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );
  
  pOutput >> buf;  
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput.close();

  remove( input );
  remove( output );
}
void explangTest::testVectorElementAssignmentToScalar()
{
  SymbolTable table;
  
  char input[]        = "testVectorElementAssignmentToScalar.in";
  char output[]       = "testVectorElementAssignmentToScalar.out";
  char statementIn[]  = "A(1) = 1\n\n";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( statementIn, pInput );
  fclose( pInput );

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 2, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "A" ) != Symbol::empty() );
  
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  string buf;
  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "]" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1;" );

  pOutput.close();
  remove( input );
  remove( output );
}
void explangTest::testFunctions()
{
  SymbolTable table;
  char input[]        = "testFunctions.in";
  char output[]       = "testFunctions.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "A = EXP(X)\n", pInput );
  fputs( "B(1) = EXP(X)\n", pInput );
  fputs( "A = LOG(1)\n", pInput );
  fputs( "A = LOG10(100)\n", pInput );
  fputs( "A = SQRT(4.0)\n", pInput );
  fputs( "A = B( X**Y )\n", pInput );
  fputs( "A = EXP(X) * SQRT(Y)\n", pInput );
  fputs( "A = 1.0 * ( EXP(X) + Y )\n", pInput ); 
  
  fclose( pInput );

  table.insertUserVar( "X" );
  table.insertUserVar( "Y" );
  
  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 8, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "A" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.findi( "B" ) != Symbol::empty() );
  
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  string buf;
  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "exp(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "]" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "exp(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "log(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "log10(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "100" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "sqrt(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "4.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "pow(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X," );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "];" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "exp(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "*" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "sqrt(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "*" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "exp(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "+" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testIfStmt()
{
  SymbolTable table;
  
  char input[]        = "testIfStmt.in";
  char output[]       = "testIfStmt.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) A = B(1)\n", pInput );
  fputs( "IF( X.NE.Y ) A = B(1) ;comment\n", pInput );
  fclose( pInput );

  table.insertUserVar( "X" );
  table.insertUserVar( "Y" );
  table.insertUserVar( "B" );
  
  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 2, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "A" ) != Symbol::empty() );
  
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  string buf;
  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "if(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "!=" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "{" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "];" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "}" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "if(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "!=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "{" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "];" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "}" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "//" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "comment" );

  pOutput.close();
  remove( input );
  remove( output );
}
void explangTest::testIfThenStmt()
{
  SymbolTable table;
  
  char input[]        = "testIfThenStmt.in";
  char output[]       = "testIfTenStmt.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "\t;comment1\n", pInput );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "A = B(1)\n", pInput );
  fputs( "C = D   ;comment2\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertUserVar( "X" );
  table.insertUserVar( "Y");
  table.insertUserVar( "B" );
  table.insertUserVar( "D" );

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "C" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.findi( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  string buf;
  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "//" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "comment1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "if(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "X" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "!=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Y" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "{" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ")" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "-" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "1" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "];" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "C" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "D;" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "//" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "comment2" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "}" );

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testHAHN1_1()
{
  // Attention!!! nm_lex() converts any capitalized letter to lower case.

  SymbolTable table;
  char input[]        = "testHAHN1_1.in";
  char output[]       = "testHAHN1_1.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
/*
  fputs( "x=((x)**(x))\n", pInput ); // This passes
  fputs( "x=EXP(((x)+(x)))\n", pInput );  // this passes 
  fputs( "x=EXP(((x)-(x)))\n", pInput );  // this passes 
  fputs( "x=EXP(((x)*(x)))\n", pInput );  // this passes 
  fputs( "x=EXP(((x)/(x)))\n", pInput );  // this passes 
*/
  fputs( "x=EXP((x)**(x))\n", pInput );  // but this fails
  fputs( "x=EXP((x+x)**x)\n", pInput );  // this passes 
  table.insertUserVar( "b1" );
  table.insertUserVar( "b2" );
  table.insertUserVar( "b3" );
  table.insertUserVar( "b4" );
  table.insertUserVar( "b5" );
  table.insertUserVar( "b6" );
  table.insertUserVar( "b7" );
  table.insertUserVar( "x" );
  fputs ("x=(b1+b2*x+b3*x**2+b4*x**3) /(1+b5*x+b6*x**2+b7*x**3)\n", pInput );
  fclose( pInput );
  
  table.insertUserVar( "THETA" );
  table.insertUserVar( "TIME" );
  table.insertUserVar( "ETA" );

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 3, gSpkExpLines );
  CPPUNIT_ASSERT( table.findi( "x" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

  remove( input );
  remove( output );
}

CppUnit::Test * explangTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "explangTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testHAHN1_1", 
	 &explangTest::testHAHN1_1 ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testScalarAssignmentToScalar", 
	 &explangTest::testScalarAssignmentToScalar ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testVectorElementAssignmentToScalar", 
	 &explangTest::testVectorElementAssignmentToScalar ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testFunctions",
	 &explangTest::testFunctions ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testIfStmt",
	 &explangTest::testIfStmt) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testIfThenStmt",
	 &explangTest::testIfThenStmt) );

  return suiteOfTests;
}

