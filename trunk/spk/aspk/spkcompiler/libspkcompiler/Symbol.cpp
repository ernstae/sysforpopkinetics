#include "Symbol.h"
#include "series.h"
#include <valarray>

using namespace std;

static Symbol empty_ob;
const Symbol* Symbol::empty()
{
   return &(empty_ob);
}
Symbol::Symbol()
{
}
Symbol::Symbol( const string& nameIn,
                const string& synonymIn,
                enum SymbolType stIn,
                enum ObjectType otIn,
                enum Structure msIn,
                const valarray<int>& dimIn )
: name( nameIn ),
  synonym( synonymIn ),
  symbol_type( stIn ),
  object_type( otIn ),
  structure( msIn )
{
   if( dimIn.size() <= 0 )
     return;

   dimension.resize( dimIn.size() );
   dimension = dimIn;

   int n = dimension.size();
   assert( n > 0 );
   initial.resize( n );
   upper.resize( n );
   lower.resize( n );
   step.resize( n );
   fixed.resize( n );

   for( int i=0, len=0; i<n; i++ )
     {
       if( object_type == MATRIX )
	 {
	   if( structure == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure == FULL )
	     {
	       len = dimension[i] * dimension[i];
	     }
	   else
	     {
	       len = dimension[i];
	     }
	 }
       else
	 {
	   len = dimension[i];
	 }
       initial[i].resize( len );
       upper[i].resize( len );
       lower[i].resize( len );
       step[i].resize( len );
       fixed[i].resize( len );
     }
}
Symbol::Symbol( const Symbol& right )
: name( right.name ),
  synonym( right.synonym ),
  symbol_type( right.symbol_type ),
  object_type( right.object_type ),
  structure( right.structure )
{
   if( right.dimension.size() <= 0 )
     return;

   dimension.resize( right.dimension.size() );
   dimension = right.dimension;

   int n = dimension.size();
   assert( n > 0 );
   initial.resize( n );
   upper.resize( n );
   lower.resize( n );
   step. resize( n );
   fixed.resize( n );

   for( int i=0, len=0; i<n; i++ )
     {
       if( object_type == MATRIX )
	 {
	   if( structure == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure == FULL )
	     {
	       len = dimension[i] * dimension[i];
	     }
	   else
	     {
	       len = dimension[i];
	     }
	 }
       else
	 {
	   len = dimension[i];
	 }
       initial[i].resize( len );
       upper[i].resize( len );
       lower[i].resize( len );
       step [i].resize( len );
       fixed[i].resize( len );

       initial[i] = right.initial[i];
       upper[i]   = right.upper[i];
       lower[i]   = right.lower[i];
       step [i]   = right.step [i];
       fixed[i]   = right.fixed[i];
     }
}
Symbol& Symbol::operator=( const Symbol& right )
{
   name        = right.name;
   synonym     = right.synonym;
   symbol_type = right.symbol_type;
   object_type = right.object_type;
   structure   = right.structure;
   if( right.dimension.size() <= 0 )
     return *this;

   dimension.resize( right.dimension.size() );
   dimension = right.dimension;

   int n = dimension.size();
   assert( n > 0 );
   initial.resize( n );
   upper.resize( n );
   lower.resize( n );
   step. resize( n );
   fixed.resize( n );

   for( int i=0, len=0; i<n; i++ )
     {
       if( object_type == MATRIX )
	 {
	   if( structure == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure == FULL )
	     {
	       len = dimension[i] * dimension[i];
	     }
	   else
	     {
	       len = dimension[i];
	     }
	 }
       else
	 {
	   len = dimension[i];
	 }
       initial[i].resize( len );
       upper[i].resize( len );
       lower[i].resize( len );
       step [i].resize( len );
       fixed[i].resize( len );

       initial[i] = right.initial[i];
       upper[i]   = right.upper[i];
       lower[i]   = right.lower[i];
       step [i]   = right.step[i];
       fixed[i]   = right.fixed[i];
     }
   return *this;
}

bool Symbol::operator==( const Symbol& right ) const
{
  //
  // If both objects lack of the name, it means they are both empty.
  // The definition of *empty* Symbol object only requires 
  // the name field to be empty.
  //
  if( name == "" && right.name == "" )
    return true;

   if( name        != right.name )
     return false;
   if( synonym     != right.synonym )
     return false;
   if( symbol_type != right.symbol_type )
     return false;
   if( object_type != right.object_type )
     return false;
   if( structure   != right.structure )
     return false;
   if( dimension.size() != right.dimension.size() )
     return false;

   int n = dimension.size();
   for( int i=0, len=0; i<n; i++ )
     {
       if( initial[i].size() != right.initial[i].size() )
	 return false;
       if( upper[i].size() != right.upper[i].size() )
	 return false;
       if( lower[i].size() != right.lower[i].size() )
	 return false;
       if( step [i].size() != right.step [i].size() )
	 return false;
       if( fixed[i].size() != right.fixed[i].size() )
	 return false;
     }
   return true;
}
bool Symbol::operator!=( const Symbol& right ) const
{
  return !(*this == right);
}
Symbol Symbol::createUserVar( const string& var )
{
   valarray<int> one( 1 );
   one[0] = 1;
   return Symbol( var, "", USERDEF, SCALAR, FULL, one );
}
Symbol Symbol::createNMVector( const string& var, int veclen )
{
   valarray<int> len( 1 );
   len[0] = veclen;
   return Symbol( var, "", NONMEMDEF, VECTOR, FULL, len );
}
Symbol Symbol::createNMMatrix( const string& var, enum Structure mt, int matdim )
{
   valarray<int> dim( 1 );
   dim[0] = matdim;
   return Symbol( var, "", NONMEMDEF, MATRIX, mt, dim );
}
Symbol Symbol::createLabel( const string& label, 
                            const string& alias, 
                            const valarray<int>& dims )
{
  return Symbol( label, alias, DATALABEL, VECTOR, FULL, dims );
}
std::ostream& operator<<( std::ostream& o, const Symbol& s )
{
  o << "Symbol         : " << s.name << endl;
  o << "Symbol Type    : ";
  if( s.symbol_type == Symbol::DATALABEL )
    {
      o << "data label" << endl;
      int n = s.initial.size();
      o << "#Data Subsets  : " << n << endl;
      for( int i=0; i<n; i++ )
	{
	  int m = s.initial[i].size();
	  o << "[" << i << "]" "{ ";
	  for( int j=0; j<m; j++ )
	    {
	      if( j > 0 )
		o << ", ";
	      o << s.initial[i][j];
	    }
	  o << " }" << endl;
	}
    }
  else if( s.symbol_type == Symbol::USERDEF )
    {
      o << "user defined" << endl;
    }
  else if( s.symbol_type == Symbol::NONMEMDEF )
    {
      o << "NONMEM defined" << endl;
      o << "Vector Size : " << s.initial.size() << endl;
    }
  else
    o << "unknown" << endl;
  
  o << "Object Type    : ";
  if( s.object_type == Symbol::SCALAR )
    {
      o << "scalar";
    }
  else if( s.object_type == Symbol::VECTOR )
    {
      o << "vector";
    }
  else if( s.object_type == Symbol::MATRIX )
    {
      o << "matrix";
    }
  else
    o << "unknown";
  o << endl;
  o << "Data Structure : ";
  if( s.structure == Symbol::FULL )
    {
      o << "full";
    }
  else if( s.structure == Symbol::TRIANGLE )
    {
      o << "triangle";
    }
  else if( s.structure == Symbol::DIAGONAL )
    {
	  o << "diagonal";
    }
  else
    o << "unknown";
  o << endl;
  return o;
}
