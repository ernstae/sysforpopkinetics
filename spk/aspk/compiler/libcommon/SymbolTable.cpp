#include <iostream>

#include "SymbolTable.h"
#include "client.h"

using namespace std;
SymbolTable::SymbolTable( client::type whoIn )
  : who( whoIn )
{
  spkSymbols = new FitParameters;
}
SymbolTable::~SymbolTable()
{ 
  delete spkSymbols;
}
void SymbolTable::dump() const
{
  
  UserSymbols::const_iterator itr = userSymbols.begin();
  while( itr != userSymbols.end() )
    {
      cout << itr->second << endl;
      ++itr;
    }
}
Symbol * const SymbolTable::insert( Symbol& symbol )
{
  pair<string, Symbol> p(symbol.name(), symbol);
  if( ( userSymbols.insert( p ) ).second )
  {
    return &symbol;
  }
  else
  {
    return NULL;
  }
}

Symbol * const SymbolTable::insert( const string& name )
{
  Symbol s( name, false );
  pair<string, Symbol> p(name, s);
  pair<UserSymbols::iterator, bool> o = userSymbols.insert( p );
  if( o.second )
    {
      return &(o.first->second);
    }
  else
    return NULL;
}
Symbol * const SymbolTable::find( const string& name )
{
  /*
  UserSymbols::iterator itr = userSymbols.begin();
  while( itr != userSymbols.end() )
    {
      if( itr->first == name )
	return &(itr->second);
      ++itr;
    }
  return NULL;
  */
  UserSymbols::iterator itr = userSymbols.find( name );
  if( itr == userSymbols.end() )
    return NULL;

  return &(itr->second);
}

