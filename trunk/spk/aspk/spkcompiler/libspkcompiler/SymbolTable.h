/**
 * @file SymbolTable.h
 *
 * Declare the SymbolTable class.
 */
#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <string>
#include <map>
#include <vector>
#include <iostream>
#include <valarray>

#include "Symbol.h"

/**
 * The symbol table used during compilation.
 *
 * The table entries are accessed by the names associated with
 * Symbol objects.  The table is guaranteed to have no duplicate names
 * (ie. keys).
 */
class SymbolTable{

  // This is the table.
  std::map< const std::string, Symbol> table;

  std::vector<std::string> labels;

  public:

  /**
   * The default constructor.  The table is initialized to be empty.
   */
  SymbolTable();

   /**
    * Make a search key word for the string.
    *
    * @param str The source string.
    * @param A string object that represents the search key word for str.
    */
   static const std::string key( const std::string& str );

  /**
   * Search the table by the given name [case sensitive].
   * 
   *
   * @return A pointer to the found Symbol object that has
   *         either the name or the synonym (ie. alias) field string
   *         matches.  If no entry is found, it returns the reference
   *         to an empty Symbol object.
   *
   * @param str The string by which a search is performed.
   *         Both the name and the synonym (ie. alias) are compared.
   */
  Symbol* find( const std::string& str );

  /**
   * Search a Symbol object in the table by the name [case sensitive].
   *
   * @return A (constan ) pointer to the found Symbol object that has
   *         either the name or the synonym (ie. alias) field string
   *         matches.  If no entry is found, it returns the reference
   *         to an empty Symbol object.
   *
   * @param str The string by which a search is performed.
   *         Both the name and the synonym (ie. alias) are compared.
   */
  const Symbol* find( const std::string& name ) const;

  /**
   * Search a Symbol object in the table by the name [case INsensitive].
   * @return A  pointer to the found Symbol object that has
   *         either the name or the synonym (ie. alias) field string
   *         matches.  If no entry is found, it returns the reference
   *         to an empty Symbol object.
   *
   * @param str The string by which a search is performed.
   *         Both the name and the synonym (ie. alias) are compared.
   */
  Symbol* findi( const std::string& name );

  /**
   * Search a Symbol object in the table by the name [case INsensitive].
   * @return A (constant) pointer to the found Symbol object that has
   *         either the name or the synonym (ie. alias) field string
   *         matches.  If no entry is found, it returns the reference
   *         to an empty Symbol object.
   *
   * @param str The string by which a search is performed.
   *         Both the name and the synonym (ie. alias) are compared.
   */
  const Symbol* findi( const std::string& name ) const;

  /**
   * Create and insert a user-defined (scalar) variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name The name of the user-defined (scalar) variable.
   *
   */
  Symbol* insertUserVar(  const std::string& name );

  /**
   * Create and insert a NONMEM vector variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name The name of the NONMEM vector variable.
   * @param len The length of the vector.
   */
  Symbol* insertNMVector( const std::string& name, int len );

  /**
   * Create and insert a NONMEM (square) matrix variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name The name of the NONMEM (square) matrix variable.
   * @param structure The (square) matrix structure (ie. sparseness): 
   * Symbol::FULL, Symbol::TRIANGLE, Symbol::DIAGONAL.
   * @param dim The dimension of the <em>square</em> matrix.
   */
  Symbol* insertNMMatrix( const std::string& name, 
                          const Symbol::Structure structure, 
                          int dim );

  /**
   * Create and insert a data label into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to a newly created Symbol object.
   *
   * @param label The label for the data subsets.
   * @param synonym An alias for the label.
   * @param lengths The lengths of data subsets associated with the label.
   */
  Symbol* insertLabel( const std::string& label, 
                       const std::string& synonym, 
                       std::valarray<int>& lengths );

  /**
   * Return a list of data labels.
   *
   * @return A pointer to a vector containing the labels (string). 
   */
  const std::vector<std::string> * getLabels() const;


  const std::map<const std::string, Symbol> * getTable() const;

/**
 * Print out all registered symbols with their attributes.
 *
 * @return The reference to the modified/filled output stream.
 * @param o The reference to the output stream.
 * @param t The symbol table from which the contents to be extracted.
 */
friend std::ostream& operator<<( std::ostream& o, const SymbolTable& t );

  protected:
  SymbolTable( const SymbolTable& );
  SymbolTable& operator=( const SymbolTable& );
};


#endif
