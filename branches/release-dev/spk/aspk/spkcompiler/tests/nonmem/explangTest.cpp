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

#include "../../spkcompiler/SymbolTable.h"
#include "../../spkcompiler/nonmem/explang.h"

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
extern bool               gSpkIsTInRhs;
extern char             * gSpkExpErrorMessages;
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
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
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
  char statementIn[]  = "A(1) = 1\n \
                         A(2) = 2.0 * A(1)\n";

  table.insertVector( "A", 2, Symbol::USER, Symbol::READWRITE );

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
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  
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
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A[" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "2" );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "2.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "*" );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "];" );

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
  fputs( "ETA(1) = EXP(X)\n", pInput );  //fputs( "B(1) = EXP(X)\n", pInput );
  fputs( "A = LOG(1)\n", pInput );
  fputs( "A = LOG10(100)\n", pInput );
  fputs( "A = SQRT(4.0)\n", pInput );
  fputs( "A = SIN(0.0)\n", pInput );
  fputs( "A = COS(0.0)\n", pInput );
  fputs( "A = ETA( X**Y )\n", pInput );   //fputs( "A = B( X**Y )\n", pInput );
  fputs( "A = EXP(X) * SQRT(Y)\n", pInput );
  fputs( "A = 1.0 * ( EXP(X) + Y )\n", pInput );
  fputs( "A = ABS(0.0)\n", pInput );
  fputs( "A = ACOS(0.0)\n", pInput );
  fputs( "A = ASIN(0.0)\n", pInput );
  fputs( "A = ATAN(0.0)\n", pInput );
  fputs( "A = ATAN2(0.0)\n", pInput );
  fputs( "A = COSH(0.0)\n", pInput );
  fputs( "A = MAX(0.0)\n", pInput );
  fputs( "A = MIN(0.0)\n", pInput );
  fputs( "A = MOD(0.0)\n", pInput );
  fputs( "A = SINH(0.0)\n", pInput );
  fputs( "A = TAN(0.0)\n", pInput );
  fputs( "A = TANH(0.0)\n", pInput );

  
  table.insertVector( "ETA", 1, Symbol::USER, Symbol::READWRITE );  //table.insertVector( "B", 1, Symbol::USER, Symbol::READWRITE );

  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE );
  
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

  CPPUNIT_ASSERT_EQUAL( 22, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "ETA" ) != Symbol::empty() );  //CPPUNIT_ASSERT( table.find( "B" ) != Symbol::empty() );
  
  fclose( pInput );
  fclose( gSpkExpOutput );

  if( gSpkExpErrors > 0 )
    cerr << gSpkExpErrorMessages << endl;
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "ETA[" );  //CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "sin(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "cos(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "ETA[" );  //CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
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

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "fabs(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "acos(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "asin(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "atan(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "atan2(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "cosh(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "max(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "min(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );


  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "mod(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "sinh(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "tan(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "A" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "tanh(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "0.0" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == ");" );

  pOutput.close();
  remove( input );
  remove( output );
}
void explangTest::testLinInterp()
{
  SymbolTable table;
  char input[]        = "testLinInterp.in";
  char output[]       = "testLinInterp.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "Z = LININTERP(Y)\n", pInput );

  
  table.insertVector( "B", 1, Symbol::USER, Symbol::READWRITE );

  fclose( pInput );

  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE );
  
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

  CPPUNIT_ASSERT_EQUAL( 1, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "Z" ) != Symbol::empty() );
  
  fclose( pInput );
  fclose( gSpkExpOutput );

  if( gSpkExpErrors > 0 )
    cerr << gSpkExpErrorMessages << endl;
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "Z" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "=" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "lininterp(" );
  pOutput >> buf;
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "\"Y\"" );
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
  fputs( "IF( X.NE.Y ) A = EPS(1)\n", pInput );//fputs( "IF( X.NE.Y ) A = B(1)\n", pInput );
  fputs( "IF( X.NE.Y ) A = EPS(1) ;comment\n", pInput );//fputs( "IF( X.NE.Y ) A = B(1) ;comment\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE );
  //table.insertVector( "B", 1, Symbol::USER, Symbol::READONLY );
  
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
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "EPS[" );//CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "EPS[" );//CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "}//" );
//  pOutput >> buf;
//  CPPUNIT_ASSERT_MESSAGE( buf, buf == "//" );
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
  fputs( "A = EPS(1)\n", pInput );
  fputs( "C = D   ;comment2\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);
  table.insertVector( "B", 1, Symbol::USER, Symbol::READONLY );
  table.insertScalar( "D", Symbol::USER, Symbol::READWRITE );

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
  CPPUNIT_ASSERT( table.find( "C" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
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
  CPPUNIT_ASSERT_MESSAGE( buf, buf == "EPS[" );//CPPUNIT_ASSERT_MESSAGE( buf, buf == "B[" );
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

void explangTest::testNestedIf()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "\t;comment1\n", pInput );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "A = EPS(1)\n", pInput );
  fputs( "C = D   ;comment2\n", pInput );
  fputs( "IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "A = D   ;comment3\n", pInput );
  fputs( "ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);
  table.insertScalar( "D", Symbol::USER, Symbol::READWRITE );

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 8, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "C" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "// comment1" ) == 0);
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "C = D;	// comment2" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = D;	// comment3" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf1()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) IF( Y.EQ.1 ) A = EPS(1)\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 1, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf2()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) A = EPS(1)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 3, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf3()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) A = EPS(1)\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  A = EPS(2)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf4()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  A = EPS(1)\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.1 ) A = EPS(2)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf5()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.2 ) A = EPS(1)\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.1 ) A = EPS(2)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 2 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "" ) == 0 );

  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf6()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf7()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  A = EPS(2)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 7, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf8()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  A = EPS(2)\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 7, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf9()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.2 ) THEN\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 9, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 2 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf10()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 7, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf11()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  A = EPS(3)\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 9, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 3 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf12()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  A = EPS(2)\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(3)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 9, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 3 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf13()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(3)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.2 ) THEN\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 11, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 3 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 2 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf14()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.2 ) THEN\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(3)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 11, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 2 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 3 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

  pOutput.close();
  remove( input );
  remove( output );
}

void explangTest::testNestedIf15()
{
  SymbolTable table;
  
  char input[]        = "testNestedIf.in";
  char output[]       = "testNestedIf.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fputs( "IF( X.NE.Y ) THEN\n", pInput );
  fputs( "  IF( Y.EQ.1 ) THEN\n", pInput );
  fputs( "    A = EPS(1)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(3)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ELSE\n", pInput );
  fputs( "  IF( Y.EQ.2 ) THEN\n", pInput );
  fputs( "    A = EPS(2)\n", pInput );
  fputs( "  ELSE\n", pInput );
  fputs( "    A = EPS(4)\n", pInput );
  fputs( "  ENDIF\n", pInput );
  fputs( "ENDIF\n", pInput );
  fclose( pInput );

  table.insertScalar( "X", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "Y", Symbol::USER, Symbol::READWRITE);

  pInput = fopen( input, "r" );
  CPPUNIT_ASSERT( pInput != NULL );

  nm_in = pInput;
  CPPUNIT_ASSERT( nm_in != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpSymbolTable = &table;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );
  gSpkExpOutput = fopen( output, "w+" );
  nm_debug = 0;

  nm_parse();

  CPPUNIT_ASSERT_EQUAL( 13, gSpkExpLines );
  CPPUNIT_ASSERT( table.find( "A" ) != Symbol::empty() );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==0 );

  ifstream pOutput( output );
  CPPUNIT_ASSERT( pOutput.good() );

// allocate memory
  int length = 100;
  char* buffer = new char [length];

// read a line and compare
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( X != Y )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 1 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 1 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 3 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "if( Y == 2 )" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 2 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "else" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "{" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "A = EPS[ ( 4 ) - 1 ];" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  pOutput.getline( buffer, length );
  CPPUNIT_ASSERT_MESSAGE( buffer, strcmp( buffer, "}" ) == 0 );
  
  // return memory
  delete[] buffer;

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
  table.insertScalar( "b1", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b2", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b3", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b4", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b5", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b6", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "b7", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "x", Symbol::USER, Symbol::READWRITE );
  fputs ("x=(b1+b2*x+b3*x**2+b4*x**3) /(1+b5*x+b6*x**2+b7*x**3)\n", pInput );
  fclose( pInput );
  
  table.insertScalar( "THETA", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "TIME", Symbol::USER, Symbol::READWRITE );
  table.insertScalar( "ETA", Symbol::USER, Symbol::READWRITE );

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
  CPPUNIT_ASSERT( table.find( "x" ) != Symbol::empty() );
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
void explangTest::testIsTInRhs()
{
  // Attention!!! nm_lex() converts any capitalized letter to lower case.

  SymbolTable table;
  char input[]        = "testIsTInRhs.in";
  char output[]       = "testIsTInRhs.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "CL=THETA(1)*EXP(ETA(1))\nV=THETA(2)\nK=CL/V\nS1=V\n" ); 
  fclose( pInput );
  
  table.insertVector( "THETA", 2, Symbol::SYSTEM, Symbol::READONLY );
  table.insertVector( "ETA", 2, Symbol::SYSTEM, Symbol::READONLY );

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

  CPPUNIT_ASSERT_EQUAL( 4, gSpkExpLines );
  CPPUNIT_ASSERT( !gSpkIsTInRhs );
  CPPUNIT_ASSERT( table.find( "CL" ) != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "V" )  != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "K" )  != Symbol::empty() );
  CPPUNIT_ASSERT( table.find( "S1" ) != Symbol::empty() );
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

void explangTest::testMissingOperator()
{
  SymbolTable table;
  char input[]        = "testMissingOperator.in";
  char output[]       = "testMissingOperator.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "DADT(2)=-2*A(2)+2*SI(LININTERP(1)-1)\n" ); 
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

  CPPUNIT_ASSERT_EQUAL( 1, gSpkExpLines );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==1 );

  remove( input );
  remove( output );
}

void explangTest::testReservedPhrase()
{
  SymbolTable table;
  char input[]        = "testReservedPhrase.in";
  char output[]       = "testReservedPhrase.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "SIGMA=0.003\n" );
  fprintf( pInput, "OMEGA=0.003\n" );
  fprintf( pInput, "THETA=0.003\n" );
  fprintf( pInput, "ETA=0.003\n" );
  fprintf( pInput, "EPS=0.003\n" );
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

  CPPUNIT_ASSERT_EQUAL( 5, gSpkExpLines );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==5 );

  remove( input );
  remove( output );
}

void explangTest::testMatrixNotSupported()
{
  SymbolTable table;
  char input[]        = "testMatrixNotSupported.in";
  char output[]       = "testMatrixNotSupported.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "K(1,2)=1\n" );
  fprintf( pInput, "B(1,2)=1\n" );
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
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==2 );

  remove( input );
  remove( output );
}

void explangTest::testLeftSideOfEquation()
{
  SymbolTable table;
  char input[]        = "testLeftSideOfEquation.in";
  char output[]       = "testLeftSideOfEquation.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "P(1)=1\n" );
  fprintf( pInput, "DADT(1)=1\n" );
  fprintf( pInput, "C(1)=1\n" );
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

  CPPUNIT_ASSERT_EQUAL( 3, gSpkExpLines );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==1 );

  remove( input );
  remove( output );
}

void explangTest::testFloatingIndex()
{
  SymbolTable table;
  char input[]        = "testFloatingIndex.in";
  char output[]       = "testFloatingIndex.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "K=THETA(1.1)\n" ); 
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

  CPPUNIT_ASSERT_EQUAL( 1, gSpkExpLines );
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==1 );

  remove( input );
  remove( output );
}

void explangTest::testZeroIndex()
{
  SymbolTable table;
  char input[]        = "testZeroIndex.in";
  char output[]       = "testZeroIndex.out";

  FILE * pInput = fopen( input, "w" );
  CPPUNIT_ASSERT( pInput != NULL );
  fprintf( pInput, "K=THETA(0)\n" );
  fprintf( pInput, "K=THETA(-1)\n" );
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
  fclose( pInput );
  fclose( gSpkExpOutput );

  CPPUNIT_ASSERT( gSpkExpErrors==2 );

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
//  suiteOfTests->addTest( 
//     new CppUnit::TestCaller<explangTest>(
//         "testVectorElementAssignmentToScalar", 
//	 &explangTest::testVectorElementAssignmentToScalar ) );
//  suiteOfTests->addTest( 
//     new CppUnit::TestCaller<explangTest>(
//         "testFunctions",
//	 &explangTest::testFunctions ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testLinInterp",
	 &explangTest::testLinInterp ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testIfStmt",
	 &explangTest::testIfStmt) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testIfThenStmt",
	 &explangTest::testIfThenStmt) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testIsTInRhs",
	 &explangTest::testIsTInRhs) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf",
	 &explangTest::testNestedIf) );  
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf1",
	 &explangTest::testNestedIf1) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf2",
	 &explangTest::testNestedIf2) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf3",
	 &explangTest::testNestedIf3) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf4",
	 &explangTest::testNestedIf4) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf5",
	 &explangTest::testNestedIf5) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf6",
	 &explangTest::testNestedIf6) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf7",
	 &explangTest::testNestedIf7) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf8",
	 &explangTest::testNestedIf8) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf9",
	 &explangTest::testNestedIf9) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf10",
	 &explangTest::testNestedIf10) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf11",
	 &explangTest::testNestedIf11) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf12",
	 &explangTest::testNestedIf12) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf13",
	 &explangTest::testNestedIf13) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf14",
	 &explangTest::testNestedIf14) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testNestedIf15",
	 &explangTest::testNestedIf15) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testMissingOperator",
	 &explangTest::testMissingOperator) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testReservedPhrase",
	 &explangTest::testReservedPhrase) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testMatrixNotSupported",
	 &explangTest::testMatrixNotSupported) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testLeftSideOfEquation",
	 &explangTest::testLeftSideOfEquation) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testFloatingIndex",
	 &explangTest::testFloatingIndex) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<explangTest>(
         "testZeroIndex",
	 &explangTest::testZeroIndex) );

  return suiteOfTests;
}

