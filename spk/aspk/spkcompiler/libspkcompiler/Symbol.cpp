#include <iostream>
#include <string>

#include "Symbol.h"

using namespace std;

// BEGINNING OF STATIC MEMBERS
const string Symbol::STR_UNKNOWN   = "unknown";
const string Symbol::STR_SCALAR    = "scalar";
const string Symbol::STR_VECTOR    = "vector";
const string Symbol::STR_MATRIX    = "matrix";
const string Symbol::STR_INT       = "int";
const string Symbol::STR_DOUBLE    = "double";
const string Symbol::STR_BOOL      = "bool";

const char * const Symbol::C_UNKNOWN   = "unknown";
const char * const Symbol::C_SCALAR    = "scalar";
const char * const Symbol::C_VECTOR    = "vector";
const char * const Symbol::C_MATRIX    = "matrix";
const char * const Symbol::C_INT       = "int";
const char * const Symbol::C_DOUBLE    = "double";
const char * const Symbol::C_BOOL      = "bool";
// END OF STATIC MEMBERS

extern int gSpkExpErrors;

bool Symbol::isDefined( void ) const
{
  if( myObjectType == UNKNOWN || myDataType == UNKNOWN )
    return false;
  else true;
}

Symbol::Symbol( const string& nameIn, bool isKeyword )
  : myName(nameIn), keyword(isKeyword), myObjectType(UNKNOWN), myDataType(UNKNOWN)
{
}
Symbol::Symbol( const string& nameIn, 
	  enum SYMBOLTYPE objectTypeIn, 
	  enum SYMBOLTYPE dataTypeIn, 
	  bool isKeyword
 )
  : myName(nameIn), keyword(isKeyword), myObjectType(objectTypeIn), myDataType(dataTypeIn)
{
  if( !( myObjectType == Symbol::UNKNOWN 
	 || myObjectType == Symbol::SCALAR 
	 || myObjectType == Symbol::VECTOR 
	 || myObjectType == Symbol::MATRIX ) )
  {
    cerr << "Wrong structure type! (" << myObjectType << ")" << endl;
    ++gSpkExpErrors;
  }
  if( !( myDataType    == Symbol::UNKNOWN 
	 || myDataType == Symbol::DOUBLE
	 || myDataType == Symbol::INT 
	 || myDataType == Symbol::BOOL ) )
  {
    cerr << "Wrong data type! (" << myDataType << ")" << endl;
    ++gSpkExpErrors;
  }

}
Symbol::Symbol( const Symbol& original )
{
  this->myName = original.myName;
  this->myObjectType = original.myObjectType;
  this->myDataType = original.myDataType;
  this->myDimensions = original.myDimensions;
  this->keyword = original.keyword;
}
Symbol::~Symbol()
{
}
bool Symbol::isKeyword() const
{
  return keyword;
}
const string Symbol::name() const
{
  return myName;
}
enum Symbol::SYMBOLTYPE Symbol::objectType() const
{
  return myObjectType;
}
enum Symbol::SYMBOLTYPE Symbol::dataType() const
{
  return myDataType;
}
int Symbol::size() const
{
  return myDimensions.first * myDimensions.second;
}

std::pair< int, int > Symbol::dim() const
{
  return myDimensions;
}

void Symbol::objectType( enum Symbol::SYMBOLTYPE objectTypeIn )
{
  if( !( objectTypeIn == Symbol::UNKNOWN 
	 || objectTypeIn == Symbol::SCALAR 
	 || objectTypeIn == Symbol::VECTOR
	 || objectTypeIn == Symbol::MATRIX )
      )
    {
      cerr << "!!! Wrong structure type (" << toString(objectTypeIn) << ") !!!, " << __LINE__ << " in " << __FILE__ << endl;
      ++gSpkExpErrors;
    }
  if( objectTypeIn == SCALAR )
    {
      myDimensions.first  = 1;
      myDimensions.second = 1;
    }
  myObjectType = objectTypeIn;
}
void Symbol::dataType( enum Symbol::SYMBOLTYPE dataTypeIn )
{
  if( !( dataTypeIn == Symbol::UNKNOWN 
	 || dataTypeIn == Symbol::DOUBLE
	 || dataTypeIn == Symbol::INT
	 || dataTypeIn == Symbol::BOOL )
      )
    {
      cerr << "!!! Wrong structure type(" << toString(dataTypeIn) << ") !!!, " << __LINE__ << " in " << __FILE__ << endl;
      ++gSpkExpErrors;
    }
  myDataType = dataTypeIn;
}

void Symbol::size( int n )
{
  if( myObjectType == SCALAR )
    {
      myDimensions.first = 1;
      myDimensions.second = 1;
    }
  else if( myObjectType == VECTOR )
    {
      myDimensions.first = n;
      myDimensions.second = 1;
    }
  else // matrix
    {
      cerr << "!!! Wrong use of Symbol::size(int).  This is a matrix.  Use Symbol::dim(int, int)!!!" << endl;
    }
}
void Symbol::dim(int m, int n)
{
  if( myObjectType == SCALAR )
    {
      assert( m == 1 && n == 1 );
    }
  if( myObjectType == VECTOR )
    {
      assert( n == 1 );
    }
    myDimensions.first  = m;
    myDimensions.second = n;
}
ostream& operator<<( ostream & o, const Symbol& s )
{
      if( s.isKeyword() )
	o << "(R)";
      else
        o << "(U)";

      o << " " << s.name() << ": ";
      if( s.dataType() == Symbol::DOUBLE )
	o << "R";
      else if( s.dataType() == Symbol::INT )
	o << "Z";
      else if( s.dataType() == Symbol::BOOL )
	o << "B";
      else 
	o << "UNKNOWN";

      if( s.objectType() == Symbol::SCALAR )
      {
	// do nothing
      }
      else if( s.objectType() == Symbol::VECTOR )
	{
	  o << "^" << s.size();
	}
      else if( s.objectType() == Symbol::MATRIX )
	{
	  o << "^" << s.dim();
	}
      else
	{
	  // do nothing
	}
      return o;
}
const string Symbol::toString( enum SYMBOLTYPE e ) 
{
  switch( e )
    {
    case UNKNOWN: return STR_UNKNOWN;
      break;
    case DOUBLE:  return STR_DOUBLE;
      break;
    case INT:     return STR_INT;
      break;
    case BOOL:    return STR_BOOL;
      break;
    case MATRIX:  return STR_MATRIX;
      break;
    case VECTOR:  return STR_VECTOR;
      break;
    case SCALAR:  return STR_SCALAR;
      break;
    default:
      char buf[128];
      sprintf( buf, "Programming error! <%d> on %d, %s\n", e, __LINE__, __FILE__ );
      throw buf;
    }
}
const char * const Symbol::toCString( enum SYMBOLTYPE e ) 
{
  switch( e )
    {
    case UNKNOWN: return C_UNKNOWN;
      break;
    case DOUBLE:  return C_DOUBLE;
      break;
    case INT:     return C_INT;
      break;
    case BOOL:    return C_BOOL;
      break;
    case MATRIX:  return C_MATRIX;
      break;
    case VECTOR:  return C_VECTOR;
      break;
    case SCALAR:  return C_SCALAR;
      break;
    default:
      char buf[128];
      sprintf( buf, "Programming error! <%d> on %d, %s\n", e, __LINE__, __FILE__ );
      throw buf;
    }
}
enum Symbol::SYMBOLTYPE Symbol::toEnum( const string& str )
{
  if( str == STR_UNKNOWN )
    return UNKNOWN;
  if( str == STR_DOUBLE )
    return DOUBLE;
  if( str == STR_INT )
    return INT;
  if( str == STR_BOOL )
    return BOOL;
  if( str == STR_MATRIX )
    return MATRIX;
  if( str == STR_VECTOR )
    return VECTOR;
  if( str == STR_SCALAR )
    return SCALAR;

  char buf[128];
  sprintf( buf, "Programming error! <%s> on %d, %s\n", str.c_str(),  __LINE__, __FILE__ );
  throw buf;
}
