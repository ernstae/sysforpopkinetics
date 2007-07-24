/**
 * @file Symbol.cpp
 * Define the Symbol class.
 */

#include "Symbol.h"
#include "series.h"
#include <valarray>
#include <iostream>
#include <cassert>

using namespace std;

const Symbol* Symbol::empty()
{
  return NULL;
}

Symbol::Symbol()
{
}
Symbol::Symbol( const string&   nameIn,
                const string&   synonymIn,
                enum Ownership  ownerIn,
                enum Access     accessIn,
                enum ObjectType objectTypeIn,
                valarray <enum Structure>&  structureIn,
                const valarray<int>& dimIn )

: name       ( nameIn ),
  synonym    ( synonymIn ),
  object_type( objectTypeIn ),
  structure  ( structureIn.size() ),
  access     ( accessIn ),
  owner      ( ownerIn )
{
   if( dimIn.size() <= 0 )
     return;
   dimension.resize( dimIn.size() );
   dimension = dimIn;
   structure = structureIn;

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
	   if( structure[i] == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure[i] == FULL )
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
: name       ( right.name ),
  synonym    ( right.synonym ),
  object_type( right.object_type ),
  structure  ( right.structure.size() ),
  access     ( right.access ),
  owner      ( right.owner )
{
   if( right.dimension.size() <= 0 )
     return;

   dimension.resize( right.dimension.size() );
   dimension = right.dimension;
   structure = right.structure;

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
	   if( structure[i] == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure[i] == FULL )
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
   object_type = right.object_type;
   structure.resize( right.structure.size() );
   structure   = right.structure;
   access      = right.access;
   owner       = right.owner;

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
	   if( structure[i] == TRIANGLE )
	     {
	       len = series( 1, 1, dimension[i] );
	     }
	   else if( structure[i] == FULL )
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
   if( object_type != right.object_type )
     return false;
   if( structure.size()   != right.structure.size() )
     return false;
   if( access      != right.access )
     return false;
   if( owner       != right.owner )
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

Symbol Symbol::createScalar( const string& var, enum Ownership owner, enum Access access )
{
   valarray<enum Structure> st( 1 );
   st[0] = FULL;
   valarray<int> one( 1 );
   one[0] = 1;
   return Symbol( var, "", owner, access, SCALAR, st, one );
}

Symbol Symbol::createVector( const string& var, int veclen, enum Ownership ownerIn, enum Access accessIn )
{
   valarray<enum Structure> st( 1 );
   st[0] = FULL;
   valarray<int> len( 1 );
   len[0] = veclen;
   return Symbol( var, "", ownerIn, accessIn, VECTOR, st, len );
}

Symbol Symbol::createSymmetricMatrix( const string& var, 
				   valarray<enum Structure>& mt, 
				   valarray<unsigned int>& matdim, 
				   enum Ownership ownerIn, 
				   enum Access accessIn )
{
  //Revisit:  Dave.  had to cast valarray<unsigned int> to valarray<int>.
  // This is lame, but Symbol expects valarray<int>.
  valarray<int> dim( matdim.size() ) ;
  for(int i=0; i<matdim.size(); i++ )
	  {
	    dim[i] = (int)matdim[i];
	  }
   return Symbol( var, "", ownerIn, accessIn, MATRIX, mt, dim );
}
Symbol Symbol::createLabel( const string& label, 
                            const string& alias,
			    const valarray<int>& N )
{
  valarray<enum Structure> st( 1 );
  st[0] = FULL; 
  return Symbol( label, alias, DATASET, READONLY, VECTOR, st, N );
}

/**
 * The extractor of Symbol object.
 * @param o The output stream.
 * @param s The Symbol object from which the contents are extracted.
 * @return The modified output stream.
 */
std::ostream& operator<<( std::ostream& o, const Symbol& s )
{
  o << "Symbol         : " << s.name << endl;
  o << "Symbol Type    : ";
  if( s.owner == Symbol::DATASET )
    {
      o << "data label" << endl;
      o << "Alias (if any) : " << s.synonym << endl;
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
  else if( s.owner == Symbol::USER ) // all scalars
    {
      o << "user defined" << endl;
    }
  else if( s.owner == Symbol::SYSTEM )
    {
      o << "System defined" << endl;
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
  else
    o << "unknown" << endl;
  
  o << "Object Type    : ";
  if( s.object_type == Symbol::SCALAR )
    {
      o << "scalar";
    }
  else if( s.object_type == Symbol::VECTOR )
    {
      int n=s.initial.size();
      o << (n>1? "vectors" : "vector" ) << " { ";
      for( int i=0; i<n; i++ )
	{
	  if( i>0 )
	    o << ", ";
	  o << s.initial[i].size();
	}
      o << " }";
    }
  else if( s.object_type == Symbol::MATRIX )
    {
      int n= s.initial.size();
      o << (n>1? "matrices" : "matrix") << " { ";
      for( int i=0; i<n; i++ )
	{
	  if( i > 0 )
	    o << ", ";
	  o << "(" << s.dimension[i] << " by " << s.dimension[i] << ")";
	}
      o << " }";
    }
  else
    o << "unknown";
  o << endl;

  o << "Data Structure : ";
  int n= s.structure.size();
  if( n > 1 )
    o << "block diagonal" << endl;;
  for (int i=0; i<n; i++ )
    {
      if( s.structure[i] == Symbol::FULL )
	o << "   full";
      else if( s.structure[i] == Symbol::TRIANGLE )
	o << "   triangle";
      else if( s.structure[i] == Symbol::DIAGONAL )
	o << "   diagonal";
      else
	o << "   unknown";
      o << endl;
    }

  o << "Access         : ";
  if( s.access == Symbol::READONLY )
    o << "read-only";
  else if( s.access == Symbol::READWRITE )
    o << "read & write";
  else
    o << "unknown";
  o << endl;

  o << "Ownership      : ";
  if( s.owner == Symbol::SYSTEM )
    o << "system";
  else if( s.owner == Symbol::USER )
    o << "user";
  else
    o << "unknown";
  o << endl;

  return o;
}
