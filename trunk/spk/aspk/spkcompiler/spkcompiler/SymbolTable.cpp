#include "SymbolTable.h"
#include "lower.h"
#include "upper.h"

#include <vector>
#include <iostream>

using namespace std;

SymbolTable::SymbolTable()
{
}
SymbolTable::SymbolTable( const SymbolTable& )
{
}
SymbolTable& SymbolTable::operator=( const SymbolTable& )
{
}
Symbol* SymbolTable::find( const string& name )
{
   map<string, Symbol>::iterator p = table.begin();
   while( p != table.end() )
     {
       if( p->second.name == name || p->second.synonym == name )
	 return &(p->second);
       p++;
     }
   
   return NULL;
}
const Symbol* SymbolTable::find( const string& name ) const
{
   map<string, Symbol>::const_iterator p = table.begin();
   while( p != table.end() )
     {
       if( p->second.name == name || p->second.synonym == name )
	 return &(p->second);
       p++;
     }
   return NULL;
}

Symbol* SymbolTable::findi( const string& name )
{
   string low = key( name );
   map<string, Symbol>::iterator p = table.begin();
   while( p != table.end() )
   {
     if( key( p->second.name ) == low || key( p->second.synonym ) == low )
       return &(p->second);
     p++;
   }
   return NULL;
}
const Symbol* SymbolTable::findi( const string& name ) const
{
   string low = key( name );
   map<string, Symbol>::const_iterator p = table.begin();
   while( p != table.end() )
   {
     if( key( p->second.name ) == low || key( p->second.synonym ) == low )
       return &(p->second);
     p++;
   }
   return NULL;
}
const string SymbolTable::key( const string& str )
{
  return lower( str );
}
Symbol* SymbolTable::insertScalar( const string& name )
{
   Symbol a = Symbol::createScalar( name );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]); 
}
Symbol* SymbolTable::insertScalar( const string& name, 
				   enum Symbol::Ownership owner, 
				   enum Symbol::Access access )
{
  Symbol a = Symbol::createScalar( name, owner, access );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]); 
}
Symbol* SymbolTable::insertVector( const string& name, int len )
{
   Symbol a = Symbol::createVector( name, len );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertVector( const string& name, int len, 
				   enum Symbol::Ownership owner, 
				   enum Symbol::Access access )
{
   Symbol a = Symbol::createVector( name, len, owner, access );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertMatrix( const string& name, 
                                Symbol::Structure mt, int dim )
{
   Symbol a = Symbol::createMatrix( name, mt, dim );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertMatrix( const string& name, 
				   Symbol::Structure mt, int dim,
				   enum Symbol::Ownership owner,
				   enum Symbol::Access access )
{
   Symbol a = Symbol::createMatrix( name, mt, dim, owner, access );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertLabel( const string& label, 
				  const string& alias, 
				  const valarray<int>& N )
{
   Symbol a = Symbol::createLabel( label, alias, N );
   const string LABEL = key(label);
   table[ LABEL ] = a;
   labels.push_back( label );
   return &(table[ LABEL ]);
}
const vector<string> * SymbolTable::getLabels() const
{
  return &labels;
}
const map<const string, Symbol> * SymbolTable::getTable() const
{
  return &table;
}
ostream& operator<<( ostream& o, const SymbolTable& t )
{
  map<string, Symbol>::const_iterator itr = t.table.begin();
  for( ; itr != t.table.end(); itr++ )
    {
      o << "search key     : " << itr->first << endl;
      o << itr->second << endl;
    }
  return o;
}
