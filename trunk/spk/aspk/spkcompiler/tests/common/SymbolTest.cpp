#include <iostream>
#include <string>
#include <vector>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "spkcompiler/Symbol.h"
#include "SymbolTest.h"

using namespace std;
using namespace CppUnit;

void SymbolTest::setUp()
{
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
   string empty_string( "" );
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

   // Test a label entry
   string label( "CP" );
   string synonym( "DV" );
   valarray<int> label_dim( 3 );
   label_dim[0] = 5;
   label_dim[1] = 3;
   label_dim[2] = 2;
   Symbol cp( label, synonym, datalabel, vec, full, label_dim );
   CPPUNIT_ASSERT_EQUAL( label, cp.name );
   CPPUNIT_ASSERT_EQUAL( synonym, cp.synonym );
   CPPUNIT_ASSERT_EQUAL( datalabel, cp.symbol_type );
   CPPUNIT_ASSERT_EQUAL( vec, cp.object_type );
   CPPUNIT_ASSERT_EQUAL( full, cp.structure );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( cp.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( label_dim[0], cp.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( label_dim[1], cp.dimension[1] );
   CPPUNIT_ASSERT_EQUAL( label_dim[2], cp.dimension[2] );
   
   // Test a triangle matrix entry
   string omega_name( "omega" );
   valarray<int> omega_dim( 1 );
   omega_dim[0] = 3;
   Symbol omega( omega_name, "", nonmem, matrix, triangle, omega_dim );
   CPPUNIT_ASSERT_EQUAL( omega_name, omega.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, omega.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem, omega.symbol_type );
   CPPUNIT_ASSERT_EQUAL( matrix, omega.object_type );
   CPPUNIT_ASSERT_EQUAL( triangle, omega.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( omega.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( omega_dim[0], omega.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.fixed[0].size() ) );
   
   // Test a diagonal matrix entry
   string sigma_name( "sigma" );
   valarray<int> sigma_dim( 1 );
   sigma_dim[0] = 3;
   Symbol sigma( sigma_name, "", nonmem, matrix, diagonal, sigma_dim );
   CPPUNIT_ASSERT_EQUAL( sigma_name, sigma.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, sigma.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem, sigma.symbol_type );
   CPPUNIT_ASSERT_EQUAL( matrix, sigma.object_type );
   CPPUNIT_ASSERT_EQUAL( diagonal, sigma.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( sigma.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma_dim[0], sigma.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( sigma.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( sigma.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( sigma.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( sigma.fixed[0].size() ) );

   // Test a vector variable entry
   string theta_name( "theta" );
   valarray<int> theta_dim( 1 );
   theta_dim[0] = 3;
   Symbol theta( theta_name, "", nonmem, vec, full, theta_dim );
   CPPUNIT_ASSERT_EQUAL( theta_name, theta.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, theta.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem, theta.symbol_type );
   CPPUNIT_ASSERT_EQUAL( vec, theta.object_type );
   CPPUNIT_ASSERT_EQUAL( full, theta.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( theta.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( theta_dim[0], theta.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( theta.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( theta.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( theta.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 3, static_cast<int>( theta.fixed[0].size() ) );

   // Test a user defined variable entry
   string aaa_name( "aaa" );
   valarray<int> aaa_dim( 1 );
   aaa_dim[0] = 1;
   Symbol aaa( aaa_name, "", user, scalar, full, aaa_dim );
   CPPUNIT_ASSERT_EQUAL( aaa_name, aaa.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, aaa.synonym );
   CPPUNIT_ASSERT_EQUAL( user, aaa.symbol_type );
   CPPUNIT_ASSERT_EQUAL( scalar, aaa.object_type );
   CPPUNIT_ASSERT_EQUAL( full, aaa.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( aaa.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( aaa_dim[0], aaa.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( aaa.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( aaa.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( aaa.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( aaa.fixed[0].size() ) );
}
void SymbolTest::testCopy()
{
   string empty_string( "" ); 
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

   // Test a label entry
   string label( "CP" );
   string synonym( "DV" );
   valarray<int> label_dim( 3 );
   label_dim[0] = 5;
   label_dim[1] = 3;
   label_dim[2] = 2;
   Symbol cp( label, synonym, datalabel, vec, full, label_dim );
   Symbol cp2(cp);
   
   CPPUNIT_ASSERT_EQUAL( cp.name,          cp2.name );
   CPPUNIT_ASSERT_EQUAL( cp.synonym,       cp2.synonym );
   CPPUNIT_ASSERT_EQUAL( cp.symbol_type,   cp2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( cp.object_type,   cp2.object_type );
   CPPUNIT_ASSERT_EQUAL( cp.structure,     cp2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( cp. dimension.size() ), 
			 static_cast<int>( cp2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[0], cp2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[1], cp2.dimension[1] );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[2], cp2.dimension[2] );
   
   // Test a triangle matrix entry
   string omega_name( "omega" );
   valarray<int> omega_dim( 1 );
   omega_dim[0] = 3;
   Symbol omega( omega_name, "", nonmem, matrix, triangle, omega_dim );
   Symbol omega2( omega );
  
   CPPUNIT_ASSERT_EQUAL( omega.name,        omega2.name );
   CPPUNIT_ASSERT_EQUAL( omega.synonym,     omega2.synonym );
   CPPUNIT_ASSERT_EQUAL( omega.symbol_type, omega2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( omega.object_type, omega2.object_type );
   CPPUNIT_ASSERT_EQUAL( omega.structure,   omega2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega .dimension.size() ), 
			 static_cast<int>(  omega2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( omega.dimension[0],omega2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. initial[0].size() ), 
			 static_cast<int>(  omega2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. upper[0].size() ), 
			 static_cast<int>(  omega2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. lower[0].size() ), 
			 static_cast<int>(  omega2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. fixed[0].size() ), 
			 static_cast<int>(  omega2.fixed[0].size() ) );
   
   // Test a diagonal matrix entry
   string sigma_name( "sigma" );
   valarray<int> sigma_dim( 1 );
   sigma_dim[0] = 3;
   Symbol sigma( sigma_name, "", nonmem, matrix, diagonal, sigma_dim );
   Symbol sigma2( sigma );

   CPPUNIT_ASSERT_EQUAL( sigma.name,         sigma2.name );
   CPPUNIT_ASSERT_EQUAL( sigma.synonym,      sigma2.synonym );
   CPPUNIT_ASSERT_EQUAL( sigma.symbol_type,  sigma2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( sigma.object_type,  sigma2.object_type );
   CPPUNIT_ASSERT_EQUAL( sigma.structure,    sigma2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .dimension.size() ), 
			 static_cast<int>(   sigma2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma.dimension[0], sigma2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .initial[0].size() ), 
			 static_cast<int>(   sigma2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .upper[0].size() ), 
			 static_cast<int>(   sigma2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .lower[0].size() ), 
			 static_cast<int>(   sigma2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .fixed[0].size() ), 
			 static_cast<int>(   sigma2.fixed[0].size() ) );

   // Test a vector variable entry
   string theta_name( "theta" );
   valarray<int> theta_dim( 1 );
   theta_dim[0] = 3;
   Symbol theta( theta_name, "", nonmem, vec, full, theta_dim );
   Symbol theta2( theta );

   CPPUNIT_ASSERT_EQUAL( theta.name,         theta2.name );
   CPPUNIT_ASSERT_EQUAL( theta.synonym,      theta2.synonym );
   CPPUNIT_ASSERT_EQUAL( theta.symbol_type,  theta2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( theta.object_type,  theta2.object_type );
   CPPUNIT_ASSERT_EQUAL( theta.structure ,   theta2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .dimension.size() ), 
			 static_cast<int>(   theta2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( theta.dimension[0], theta2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .initial[0].size() ), 
			 static_cast<int>(   theta2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .upper[0].size() ), 
			 static_cast<int>(   theta2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .lower[0].size() ), 
			 static_cast<int>(   theta2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .fixed[0].size() ), 
			 static_cast<int>(   theta2.fixed[0].size() ) );

   // Test a user defined variable entry
   string aaa_name( "aaa" );
   valarray<int> aaa_dim( 1 );
   aaa_dim[0] = 1;
   Symbol aaa( aaa_name, "", user, scalar, full, aaa_dim );
   Symbol aaa2( aaa );

   CPPUNIT_ASSERT_EQUAL( aaa.name,         aaa2.name );
   CPPUNIT_ASSERT_EQUAL( aaa.synonym,      aaa2.synonym );
   CPPUNIT_ASSERT_EQUAL( aaa.symbol_type,  aaa2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( aaa.object_type,  aaa2.object_type );
   CPPUNIT_ASSERT_EQUAL( aaa.structure,    aaa2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa .dimension.size() ), 
			 static_cast<int>( aaa2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( aaa.dimension[0], aaa2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa .initial[0].size() ), 
			 static_cast<int>( aaa2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. upper[0].size() ), 
			 static_cast<int>( aaa2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. lower[0].size() ), 
			 static_cast<int>( aaa2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. fixed[0].size() ), 
			 static_cast<int>( aaa2.fixed[0].size() ) );
}
void SymbolTest::testAssign()
{
   string empty_string( "" );
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

   // Test a label entry
   string label( "CP" );
   string synonym( "DV" );
   valarray<int> label_dim( 3 );
   label_dim[0] = 5;
   label_dim[1] = 3;
   label_dim[2] = 2;
   Symbol cp( label, synonym, datalabel, vec, full, label_dim );
   Symbol cp2 = cp;
   
   CPPUNIT_ASSERT_EQUAL( cp.name,          cp2.name );
   CPPUNIT_ASSERT_EQUAL( cp.synonym,       cp2.synonym );
   CPPUNIT_ASSERT_EQUAL( cp.symbol_type,   cp2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( cp.object_type,   cp2.object_type );
   CPPUNIT_ASSERT_EQUAL( cp.structure,     cp2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( cp. dimension.size() ), 
			 static_cast<int>( cp2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[0], cp2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[1], cp2.dimension[1] );
   CPPUNIT_ASSERT_EQUAL( cp. dimension[2], cp2.dimension[2] );
   
   // Test a triangle matrix entry
   string omega_name( "omega" );
   valarray<int> omega_dim( 1 );
   omega_dim[0] = 3;
   Symbol omega( omega_name, "", nonmem, matrix, triangle, omega_dim );
   Symbol omega2 = omega;
  
   CPPUNIT_ASSERT_EQUAL( omega.name,        omega2.name );
   CPPUNIT_ASSERT_EQUAL( omega.synonym,     omega2.synonym );
   CPPUNIT_ASSERT_EQUAL( omega.symbol_type, omega2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( omega.object_type, omega2.object_type );
   CPPUNIT_ASSERT_EQUAL( omega.structure,   omega2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega .dimension.size() ), 
			 static_cast<int>(  omega2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( omega.dimension[0],omega2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. initial[0].size() ), 
			 static_cast<int>(  omega2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. upper[0].size() ), 
			 static_cast<int>(  omega2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. lower[0].size() ), 
			 static_cast<int>(  omega2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(  omega. fixed[0].size() ), 
			 static_cast<int>(  omega2.fixed[0].size() ) );
   
   // Test a diagonal matrix entry
   string sigma_name( "sigma" );
   valarray<int> sigma_dim( 1 );
   sigma_dim[0] = 3;
   Symbol sigma( sigma_name, "", nonmem, matrix, diagonal, sigma_dim );
   Symbol sigma2 = sigma;

   CPPUNIT_ASSERT_EQUAL( sigma.name,         sigma2.name );
   CPPUNIT_ASSERT_EQUAL( sigma.synonym,      sigma2.synonym );
   CPPUNIT_ASSERT_EQUAL( sigma.symbol_type,  sigma2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( sigma.object_type,  sigma2.object_type );
   CPPUNIT_ASSERT_EQUAL( sigma.structure,    sigma2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .dimension.size() ), 
			 static_cast<int>(   sigma2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma.dimension[0], sigma2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .initial[0].size() ), 
			 static_cast<int>(   sigma2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .upper[0].size() ), 
			 static_cast<int>(   sigma2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .lower[0].size() ), 
			 static_cast<int>(   sigma2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   sigma .fixed[0].size() ), 
			 static_cast<int>(   sigma2.fixed[0].size() ) );

   // Test a vector variable entry
   string theta_name( "theta" );
   valarray<int> theta_dim( 1 );
   theta_dim[0] = 3;
   Symbol theta( theta_name, "", nonmem, vec, full, theta_dim );
   Symbol theta2 = theta;

   CPPUNIT_ASSERT_EQUAL( theta.name,         theta2.name );
   CPPUNIT_ASSERT_EQUAL( theta.synonym,      theta2.synonym );
   CPPUNIT_ASSERT_EQUAL( theta.symbol_type,  theta2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( theta.object_type,  theta2.object_type );
   CPPUNIT_ASSERT_EQUAL( theta.structure ,   theta2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .dimension.size() ), 
			 static_cast<int>(   theta2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( theta.dimension[0], theta2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .initial[0].size() ), 
			 static_cast<int>(   theta2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .upper[0].size() ), 
			 static_cast<int>(   theta2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .lower[0].size() ), 
			 static_cast<int>(   theta2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>(   theta .fixed[0].size() ), 
			 static_cast<int>(   theta2.fixed[0].size() ) );

   // Test a user defined variable entry
   string aaa_name( "aaa" );
   valarray<int> aaa_dim( 1 );
   aaa_dim[0] = 1;
   Symbol aaa( aaa_name, "", user, scalar, full, aaa_dim );
   Symbol aaa2 = aaa;

   CPPUNIT_ASSERT_EQUAL( aaa.name,         aaa2.name );
   CPPUNIT_ASSERT_EQUAL( aaa.synonym,      aaa2.synonym );
   CPPUNIT_ASSERT_EQUAL( aaa.symbol_type,  aaa2.symbol_type );
   CPPUNIT_ASSERT_EQUAL( aaa.object_type,  aaa2.object_type );
   CPPUNIT_ASSERT_EQUAL( aaa.structure,    aaa2.structure );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa .dimension.size() ), 
			 static_cast<int>( aaa2.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( aaa.dimension[0], aaa2.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa .initial[0].size() ), 
			 static_cast<int>( aaa2.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. upper[0].size() ), 
			 static_cast<int>( aaa2.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. lower[0].size() ), 
			 static_cast<int>( aaa2.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( static_cast<int>( aaa. fixed[0].size() ), 
			 static_cast<int>( aaa2.fixed[0].size() ) );
}
void SymbolTest::testCreateLabel()
{  
   string empty_string( "" );
   enum Symbol::SymbolType datalabel      = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem         = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user           = Symbol::USERDEF;
   enum Symbol::ObjectType scalar         = Symbol::SCALAR;
   enum Symbol::ObjectType vec            = Symbol::VECTOR;
   enum Symbol::ObjectType matrix         = Symbol::MATRIX;
   enum Symbol::Structure full      = Symbol::FULL;
   enum Symbol::Structure diagonal  = Symbol::DIAGONAL;
   enum Symbol::Structure triangle  = Symbol::TRIANGLE;

   string label( "CP" );
   string synonym( "DV" );
   valarray<int> dim(3);
   dim[0] = 5;
   dim[1] = 2;
   dim[2] = 3;
   Symbol cp = Symbol::createLabel( label, synonym, dim );
   CPPUNIT_ASSERT_EQUAL( label,     cp.name );
   CPPUNIT_ASSERT_EQUAL( synonym,   cp.synonym );
   CPPUNIT_ASSERT_EQUAL( datalabel, cp.symbol_type );
   CPPUNIT_ASSERT_EQUAL( vec,       cp.object_type );
   CPPUNIT_ASSERT_EQUAL( full,      cp.structure );
   CPPUNIT_ASSERT_EQUAL( 3,         static_cast<int>( cp.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( 5,         cp.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 2,         cp.dimension[1] );
   CPPUNIT_ASSERT_EQUAL( 3,         cp.dimension[2] );
}
void SymbolTest::testCreateNMVar()
{  
   string empty_string( "" );
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

  // vector
   string theta_name( "THETA" );
   int theta_len = 5;
   Symbol theta = Symbol::createNMVector( theta_name, theta_len );
   CPPUNIT_ASSERT_EQUAL( theta_name,   theta.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, theta.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem,       theta.symbol_type );
   CPPUNIT_ASSERT_EQUAL( vec,          theta.object_type );
   CPPUNIT_ASSERT_EQUAL( full,         theta.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( theta.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( theta_len, theta.dimension[0] );
   
   // triangle matrix
   string omega_name( "OMEGA" );
   int omega_dim = 3;
   Symbol omega = Symbol::createNMMatrix( omega_name, triangle, omega_dim );
   CPPUNIT_ASSERT_EQUAL( omega_name,   omega.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, omega.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem,       omega.symbol_type );
   CPPUNIT_ASSERT_EQUAL( matrix,       omega.object_type );
   CPPUNIT_ASSERT_EQUAL( triangle,     omega.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( omega.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( omega_dim, omega.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( 6, static_cast<int>( omega.fixed[0].size() ) );

   // diagonal matrix
   string sigma_name( "sigma" );
   int sigma_dim = 3;
   Symbol sigma = Symbol::createNMMatrix( sigma_name, diagonal, sigma_dim );
   CPPUNIT_ASSERT_EQUAL( sigma_name,   sigma.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, sigma.synonym );
   CPPUNIT_ASSERT_EQUAL( nonmem,       sigma.symbol_type );
   CPPUNIT_ASSERT_EQUAL( matrix,       sigma.object_type );
   CPPUNIT_ASSERT_EQUAL( diagonal,     sigma.structure );
   CPPUNIT_ASSERT_EQUAL( 1, static_cast<int>( sigma.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma_dim, sigma.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( sigma_dim, static_cast<int>( sigma.initial[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma_dim, static_cast<int>( sigma.upper[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma_dim, static_cast<int>( sigma.lower[0].size() ) );
   CPPUNIT_ASSERT_EQUAL( sigma_dim, static_cast<int>( sigma.fixed[0].size() ) );

}
void SymbolTest::testCreateUserVar()
{  
   string empty_string( "" );
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

   string var( "aaa" );
   valarray<int> dim(1);
   dim[0] = 1;
   Symbol aaa = Symbol::createUserVar( var );
   CPPUNIT_ASSERT_EQUAL( var,          aaa.name );
   CPPUNIT_ASSERT_EQUAL( empty_string, aaa.synonym );
   CPPUNIT_ASSERT_EQUAL( user,         aaa.symbol_type );
   CPPUNIT_ASSERT_EQUAL( scalar,       aaa.object_type );
   CPPUNIT_ASSERT_EQUAL( full,         aaa.structure );
   CPPUNIT_ASSERT_EQUAL( 1,            static_cast<int>( aaa.dimension.size() ) );
   CPPUNIT_ASSERT_EQUAL( 1,            aaa.dimension[0] );
   CPPUNIT_ASSERT_EQUAL( 1,            static_cast<int>( aaa.upper.size() ) );
   CPPUNIT_ASSERT_EQUAL( 1,            static_cast<int>( aaa.initial.size() ) );
   CPPUNIT_ASSERT_EQUAL( 1,            static_cast<int>( aaa.lower.size() ) );
   CPPUNIT_ASSERT_EQUAL( 1,            static_cast<int>( aaa.fixed.size() ) );
}
void SymbolTest::testEquality()
{
   string empty_string( "" );
   enum Symbol::SymbolType datalabel = Symbol::DATALABEL;
   enum Symbol::SymbolType nonmem    = Symbol::NONMEMDEF;
   enum Symbol::SymbolType user      = Symbol::USERDEF;
   enum Symbol::ObjectType scalar    = Symbol::SCALAR;
   enum Symbol::ObjectType vec       = Symbol::VECTOR;
   enum Symbol::ObjectType matrix    = Symbol::MATRIX;
   enum Symbol::Structure full       = Symbol::FULL;
   enum Symbol::Structure diagonal   = Symbol::DIAGONAL;
   enum Symbol::Structure triangle   = Symbol::TRIANGLE;

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
  string empty_string("");
  const Symbol *empty = Symbol::empty();
  Symbol shallBeEmpty;

  CPPUNIT_ASSERT_EQUAL( empty_string, empty->name );
  CPPUNIT_ASSERT( *empty == shallBeEmpty );
  CPPUNIT_ASSERT( !( *empty != shallBeEmpty ) );
}
CppUnit::Test * SymbolTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testDefaultConstructor",
						    &SymbolTest::testDefaultConstructor ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testConstructor",
						    &SymbolTest::testConstructor ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCopy",
						    &SymbolTest::testCopy ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testAssign",
						    &SymbolTest::testAssign ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateLabel",
						    &SymbolTest::testCreateLabel ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateUserVar",
						    &SymbolTest::testCreateUserVar ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreateNMVar",
						    &SymbolTest::testCreateNMVar ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testEquality",
						    &SymbolTest::testEquality ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testEmpty",
						    &SymbolTest::testEmpty ) );
  return suiteOfTests;
}

