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
   
   return (const_cast<Symbol*>( Symbol::empty() ) );
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
   return Symbol::empty();
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
   return (const_cast<Symbol*>( Symbol::empty() ) );
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
   return Symbol::empty();
}
const string SymbolTable::key( const string& str )
{
  return lower( str );
}
Symbol* SymbolTable::insertUserVar( const string& name )
{
   Symbol a = Symbol::createUserVar( name );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]); 
}
Symbol* SymbolTable::insertNMVector( const string& name, int len )
{
   Symbol a = Symbol::createNMVector( name, len );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertNMMatrix( const string& name, 
                                Symbol::Structure mt, int dim )
{
   Symbol a = Symbol::createNMMatrix( name, mt, dim );
   const string NAME = key(name);
   table[ NAME ] = a;
   return &(table[ NAME ]);
}
Symbol* SymbolTable::insertLabel( const string& label, const string& alias,
                                  valarray<int>& len )
{
   Symbol a = Symbol::createLabel( label, alias, len );
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
