#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <map>
#include <valarray>
#include <string>
#include "SpkParameters.h"
#include "Symbol.h"
#include "client.h"

/**
 * @file SymbolTable.h
 *
 * Declares SymbolTable class
 */
/**
 * @example SymbolTableTest.cpp
 */
/**
 * SymbolTable maintains a symbol table that is the central 
 * place where information 
 * about variables/functions appeared in source files are 
 * gathered and maintained and
 * provide methods for the caller to manipulate the table.
 *
 * The table is implemented using std::map.  It is a set 
 * of (unique) key-entry pairs.
 * The keys are null termimated arrays of characters.  
 * Each key reflects a name discovered 
 * and associated with a Symbol object which contains 
 * information such as data type and structure 
 * related to the name.
 */
class SymbolTable{
  
 public:
  typedef std::map < std::string, Symbol > UserSymbols;
  /**
   * Default constructor
   */
  SymbolTable( );

  /**
   * Destructor
   */
  ~SymbolTable();

  /**
   * Searches an entry matches to the name.
   *
   * @param name is the user-defined symbol to be sought.
   *
   * @return a pointer to the Symbol object when a match is found in the table.
   * The pointer value itself may not be modified but the values pointed by it can be modified.
   * If no match is found, it returns NULL.
   */
  Symbol * const find( const std::string& name );

  /**
   * Inserts a Symbol object to the table.
   *
   * @param symbol is a Symbol object associated with a user-defined name (symbol).  
   * If an  entry with the same symbol name
   * already exists, the new Symbol object replaces the old.
   * 
   * @return a pointer to the Symbol object if the symbol were new to the table.  
   * NULL if the symbol was found.
   * The pointer value itself may not be modified while the values associated 
   * with the Symbol object may be modified.
   * 
   */
  Symbol * const insert( Symbol & symbol );

  /**
   * Inserts a user-defined symbol to the table.
   *
   * @param uName is a user-defined name (symbol) to be registered.
   * It creates an Symbol object with the name and is entered in the table.
   * If an entry with the same symbol name already exists, the new symbol replaces the old.
   * 
   * @return a pointer to a newly created Symbol object if the symbol were new to the table.  
   * NULL if the symbol was found.
   * The pointer value itself may not be modified while the values associated with 
   * the Symbol object may be modified.
   */
  Symbol * const insert( const std::string& uName );

  /**
   * Determine whether there is any user defined symbol registered at all.
   *
   * @return true if there is at least one user defined symbol in the table.
   */
  inline bool empty() const { if( userSymbols.size() < 1 ) return true; else return false; }

  /**
   * Determine the number of user defined symbols.
   *
   * @return the number of user defined symbols..
   */
  inline int size() const{ return userSymbols.size(); }

  /**
   * Obtain an constant iterator to the first entry, pair <string, Symbol>, in the table.
   * 
   * @return A pair<string,Symbol> iterator pointing to the first entry in the table.
   */
  UserSymbols::const_iterator begin() const ;

  /**
   * Obtain an contant iterator to the last entry, pair <string, Symbol>, in the table.
   *
   * @return A pair<string, Sybmol> iterator pointing to one after the last entry in the table.
   * */
  UserSymbols::const_iterator end() const;
  /**
   * Dumps the contents of the table to the standard output.
   */
  void dump() const;

 protected:
  SymbolTable( const SymbolTable& ){};
  SymbolTable & operator=( const SymbolTable& ){};


 private:
  client::type who;

  UserSymbols userSymbols;

};

#endif
