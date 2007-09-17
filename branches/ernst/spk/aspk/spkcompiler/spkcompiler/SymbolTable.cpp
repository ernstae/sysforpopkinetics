/**
 * @file SymbolTable.cpp
 *
 * Define the SymbolTable class.
 */

#include "SymbolTable.h"

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

Symbol* SymbolTable::insertScalar( const string& name, 
				   enum Symbol::Ownership owner, 
				   enum Symbol::Access access )
{
  Symbol a = Symbol::createScalar( name, owner, access );
  table[ name ] = a;
  return &(table[ name ]); 
  
}
Symbol* SymbolTable::insertVector( const string& name, int len, 
				   enum Symbol::Ownership owner, 
				   enum Symbol::Access access )
{
   Symbol a = Symbol::createVector( name, len, owner, access );
   table[ name ] = a;
   return &(table[ name ]);
}

Symbol* SymbolTable::insertSymmetricMatrix( const string& name, 
					    valarray<Symbol::Structure>& mt,
					    valarray<unsigned int>& dim,
					    enum Symbol::Ownership owner,
					    enum Symbol::Access access )
{
   Symbol a = Symbol::createSymmetricMatrix( name, mt, dim, owner, access );
   table[ name ] = a;
   return &(table[ name ]);
}
Symbol* SymbolTable::insertLabel( const string& label, 
				  const string& alias, 
				  const valarray<int>& N )
{
   Symbol a = Symbol::createLabel( label, alias, N );
   table[ label ] = a;
   labels.push_back( label );
   return &(table[ label ]);
}
const vector<string> * SymbolTable::getLabels() const
{
  return &labels;
}
const map<const string, Symbol> * SymbolTable::getTable() const
{
  return &table;
}

/**
 * Extractor.
 * Print out all registered Symbol objects.
 *
 * @return The reference to the modified/filled output stream.
 * @param o The reference to the output stream.
 * @param t The symbol table from which the contents to be extracted.
 */
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
