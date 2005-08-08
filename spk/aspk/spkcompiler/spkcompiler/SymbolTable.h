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
 * This represents the symbol table used during compilation.
 * Do not get confused with a data set or a table containing
 * actual values.  This is only a table for registering and
 * keeping track of new variable names appeared in the 
 * source XML file (i.e. an instance of SpkSourceML document).
 *
 * The table guanranteers no duplicate keys.
 *
 * The table has the following fields per entry:
 *
 * - key               : the search key
 * - name              : the variable name being registered
 * - alias             : an alias for the variable if any
 * - user-/pre-defined : a flag indicating as to whether the variable is user-defined or pre-defined.
 * - read/write        : a flag indicating as to whether the variable is read only or write permitted.
 * - structure         : the structure of the variable { scalar, vector, matrix }
 * - matrix structure  : the structure of the "matrix" variable { full, triangle, diagonal }
 *
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
    * Return the search key for the variable you will be looking for.
    * Each table entry has a key associated with it, which may be
    * different from the original variable name.
    *
    * @param str The source string for which the key is generated.
    */
   static const std::string key( const std::string& str );

  /**
   * Search the table for the string [case sensitive].
   *
   * @return A pointer to the Symbol object that has str as 
   *         the variable name or the alias name.
   *         If no matching is found, it returns NULL.
   *
   * @param str The string by which a search is performed.
   */
  Symbol* find( const std::string& str );

  /**
   * Search the table for the string [case sensitive].
   *
   * @return A (constant) pointer to the found Symbol object that has
   *         str as either the variable name or the alias name.
   *         If no entry matches, it returns NULL.
   *
   * @param str The string by which a search is performed.
   */
  const Symbol* find( const std::string& str ) const;

  /**
   * Search the table for the string [case INsensitive].
   * @return A pointer to the found Symbol object that has str as 
   *         either the variable name or the alias.
   *         If no entry matches, it returns NULL.
   *
   * @param str The string by which a search is performed.
   */
  Symbol* findi( const std::string& str );

  /**
   * Search the table for the string [case INsensitive].
   * @return A (constant) pointer to the found Symbol object that has
   *         str as either the variable name or the alias name.  
   *         If no entry matches, it returns NULL.
   *
   * @param str The string by which a search is performed.
   */
  const Symbol* findi( const std::string& str ) const;

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
  Symbol* insertScalar( const std::string& name );

  /**
   * Create and insert a vector variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name The name of the NONMEM vector variable.
   * @param len The length of the vector.
   */
  Symbol* insertVector( const std::string& name, int len );

  /**
   * Create and insert a square matrix variable into the table.
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
  Symbol* insertMatrix( const std::string& name, 
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
   * @param N The number of data records for each individual.
   */
  Symbol* insertLabel( const std::string& label, 
                       const std::string& synonym,
		       const std::valarray<int>& N );

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
