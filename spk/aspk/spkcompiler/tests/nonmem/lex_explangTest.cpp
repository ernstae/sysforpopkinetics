#include <iostream>
#include <valarray>
#include <fstream>
#include <string>

#include "lex_explangTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include <spkcompiler/nonmem/explang.h>

using namespace std;
using namespace CppUnit;
extern "C"{
  int nm_lex(void);  
  int nm_parse(void);
  FILE * nm_in;
  void nm_restart( FILE* );
  void nm_error( char * m );
};

int gSpkExpLines  = 0;
int gSpkExpErrors = 0;
void nm_error( char * m )
{
}
void lex_explangTest::setUp()
{
}
void lex_explangTest::tearDown()
{
}
void lex_explangTest::testWhiteSpaces()
{
  // white space test.  

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // The first character is a white space, so it should be ignored
  // and the token for the second character which is a newline character
  // should be returned.
  fprintf( input, "\t\n" );

  fclose( input );

  gSpkExpLines  = 0;
  gSpkExpErrors = 0;
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  fclose( nm_in );
  CPPUNIT_ASSERT( TOKEN == '\n' );
  CPPUNIT_ASSERT_EQUAL( 0, gSpkExpErrors );
  CPPUNIT_ASSERT_EQUAL( 1, gSpkExpLines );
  remove( testInput );
}
void lex_explangTest::testComment()
{
  // comment line.  

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // A token COMMENT should be returned;
  fprintf( input, "; This is a comment line.\n" );

  fclose( input );
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( COMMENT, TOKEN );
  fclose( nm_in );
  remove( testInput );
}
void lex_explangTest::testNamedConstant()
{
  // named constant

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // A token NAME shall be returned and nm_lval.c_str shall contain
  // the constant in all lower case.
  fprintf( input, "Ab9DEf" );

  fclose( input );
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "ab9def" ) == 0 );
  CPPUNIT_ASSERT_EQUAL( NAME, TOKEN );

  fclose( nm_in );
  remove( testInput );  
}
void lex_explangTest::testNameLength()
{
  // The length of named constant cannot exceed 6.

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // This string has 7 characters.  gSpkExpErrors should be incremented.
  fprintf( input, "Ab9DEfG" );

  fclose( input );
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  gSpkExpErrors = 0;
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT( gSpkExpErrors > 0 );

  fclose( nm_in );
  remove( testInput );  
}
void lex_explangTest::testEngineeringNotation()
{
  // Engineering notation

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "1.0E1\n" );
  fprintf( input, "1.0e1\n" );
  fprintf( input, "1.E1\n" );
  fprintf( input, "1.e1\n" );
  fprintf( input, "1E1\n" );
  fprintf( input, "1e1\n" );
  
  fclose( input );
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1.0e1" ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1.0e1" ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1.e1" ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1.e1" ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1e1" ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENG_NOTATION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1e1" ) == 0 );

  fclose( nm_in );
  remove( testInput );  
}
void lex_explangTest::testFloatingPoint()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "1.0\n" );
  fprintf( input, "1.\n" );
  fprintf( input, ".1\n" );

  fclose( input );
  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( SIGNIFICAND, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1.0" ) == 0 );
    
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( SIGNIFICAND, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, "1." ) == 0 );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( SIGNIFICAND, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  CPPUNIT_ASSERT_MESSAGE( nm_lval.c_str, strcmp( nm_lval.c_str, ".1" ) == 0 );

  fclose( nm_in );
  remove( testInput );  
}
void lex_explangTest::testDigitString()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "12345" );
  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( DIGIT_STRING, TOKEN );
  fclose( nm_in );
  remove( testInput );
}
void lex_explangTest::testExit()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "exit" );
  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );
  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( EXIT, TOKEN );
  fclose( nm_in );
  remove( testInput );
 
}
void lex_explangTest::testControl()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "if\n" );
  fprintf( input, "then\n" );
  fprintf( input, "else\n" );
  fprintf( input, "endif\n" );
  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( IF, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( THEN, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ELSE, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( ENDIF, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  fclose( nm_in );
  remove( testInput );
 
}
void lex_explangTest::testBool()
{
  // Boolean values

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, ".tRUe.\n" );
  fprintf( input, ".FaLsE.\n" );
  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( TRUE, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( FALSE, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  fclose( nm_in );
  remove( testInput );
 
}
void lex_explangTest::testBinaryFunc()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  fprintf( input, "X ** y" );

  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NAME, TOKEN );
  
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( POWER_OP, TOKEN );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NAME, TOKEN );

  fclose( nm_in );
  remove( testInput );
}
void lex_explangTest::testUnaryFunc()
{
  // Predefined unary function

  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // A token DEFINED_UNARY_FUNCTION shall be returned
  fprintf( input, "exp\n" );
  fprintf( input, "log\n" );
  fprintf( input, "log10\n" );
  fprintf( input, "SQRT\n" );

  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( DEFINED_UNARY_FUNCTION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );
  
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( DEFINED_UNARY_FUNCTION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( DEFINED_UNARY_FUNCTION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( DEFINED_UNARY_FUNCTION, TOKEN );
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( static_cast<int>('\n'), TOKEN );

  fclose( nm_in );
  remove( testInput );
}
void lex_explangTest::testArray()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // An array definition, "(/ a, b, c /)"
  fprintf( input, "(/ x, Y, z /)\n" );

  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( OPEN_ARRAY_ELEM_LIST, TOKEN );
  /* x */ nm_lex(); 
  CPPUNIT_ASSERT_EQUAL( static_cast<int>(','), nm_lex() );
  /* Y */ nm_lex(); 
  CPPUNIT_ASSERT_EQUAL( static_cast<int>(','), nm_lex() );
  /* z */ nm_lex(); 
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( CLOSE_ARRAY_ELEM_LIST, TOKEN );

  fclose( nm_in );
  remove( testInput );

}
void lex_explangTest::testLogical()
{
  char testInput[] = "lex_explangTestInput.txt";
  FILE * input = fopen( testInput, "w" );
  CPPUNIT_ASSERT( input != NULL );

  // .EQ., ==
  // EQ_OP
  fprintf( input, ".EQ.\n" );
  fprintf( input, "==\n" );

  // .NE.
  // NE_OP
  fprintf( input, ".NE.\n" );
  fprintf( input, "/=\n" );

  // .LT.
  // LT_OP
  fprintf( input, ".LT.\n" );
  fprintf( input, "<\n" );

  // .LE.
  // LE_OP
  fprintf( input, ".LE.\n" );
  fprintf( input, "<=\n" );

  // .GT.
  // GT_OP
  fprintf( input, ".GT.\n" );
  fprintf( input, ">\n" );

  // .GE.
  // GE_OP
  fprintf( input, ".GE.\n" );
  fprintf( input, ">=\n" );

  // .AND.
  // AND_OP
  fprintf( input, ".AND.\n" );
  
  // .OR.
  // OR_OP
  fprintf( input, ".OR.\n" );

  // .NOT.
  // NOT_OP
  fprintf( input, ".NOT.\n" );

  // .NEQV.
  // NEQV_OP
  fprintf( input, ".NEQV.\n" );

  // EQV_OP
  fprintf( input, ".EQV.\n" );

  fclose( input );

  nm_in = fopen( testInput, "r" );
  nm_restart( nm_in );

  int TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( EQ_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( EQ_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NE_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NE_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( LT_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( LT_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( LE_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( LE_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( GT_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( GT_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( GE_OP, TOKEN );
  /* newline */ nm_lex();
  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( GE_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( AND_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( OR_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NOT_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( NEQV_OP, TOKEN );
  /* newline */ nm_lex();

  TOKEN = nm_lex();
  CPPUNIT_ASSERT_EQUAL( EQV_OP, TOKEN );
  /* newline */ nm_lex();

  fclose( nm_in );
  remove( testInput );
}

CppUnit::Test * lex_explangTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "lex_explangTest" );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testWhiteSpaces", 
	 &lex_explangTest::testWhiteSpaces ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testComment", 
	 &lex_explangTest::testComment ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testNamedConstant", 
	 &lex_explangTest::testNamedConstant ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testNameLength", 
	 &lex_explangTest::testNameLength ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testEngineeringNotation", 
	 &lex_explangTest::testEngineeringNotation ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testFloatingPoint", 
	 &lex_explangTest::testFloatingPoint ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testDigitString", 
	 &lex_explangTest::testDigitString ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testExit", 
	 &lex_explangTest::testExit ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testControl", 
	 &lex_explangTest::testControl ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testBool", 
	 &lex_explangTest::testBool ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testBinaryFunc", 
	 &lex_explangTest::testBinaryFunc ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testUnaryFunc", 
	 &lex_explangTest::testUnaryFunc ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testArray", 
	 &lex_explangTest::testArray ) );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<lex_explangTest>(
         "testLogical", 
	 &lex_explangTest::testLogical ) );

     return suiteOfTests;
}

