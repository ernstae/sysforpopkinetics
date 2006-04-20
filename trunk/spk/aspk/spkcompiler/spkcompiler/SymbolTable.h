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
 * This class represents the symbol table used during compilation.
 * Variable names appeared in the user's expressions, data labels,
 * NONMEM/SPK reserved words that are relevent to the particular
 * transaction are registered and maintained in here.
 *
 * The table allows no duplicate keys.
 *
 * The table has the following fields per entry:
 *
 * - key               : the search key
 * - name              : the variable name being registered
 * - synonym           : an alias for the variable if any
 * - object type       : the type of the object (scalar, vector, matrix)
 * - structure         : the data structure (triangle, diagonal)
 * - access            : an access permission (readonly, readwrite, datalabel)
 * - user-/pre-defined : a flag indicating as to whether the variable is user-defined or pre-defined.
 * - read/write        : a flag indicating as to whether the variable is read only or write permitted.
 * - structure         : the structure of the variable { scalar, vector, matrix }
 * - matrix structure  : the structure of the "matrix" variable { full, triangle, diagonal }
 *
 */
class SymbolTable{

  /**
   * This is the table itself.
   */
  std::map< const std::string, Symbol> table;

  /**
   * A list of symbols that are marked as "data labels".
   */
  std::vector<std::string> labels;

  public:

  /**
   * The default constructor.  The table is initialized to be empty.
   */
  SymbolTable();

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
   * Create and insert a scalar variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name   The name of the scalar variable.
   * @param owner  The owner of the object.
   * @param access The access permission to the object.
   *
   */
  Symbol* insertScalar( const std::string& name, 
			enum Symbol::Ownership owner, 
			enum Symbol::Access access );

  /**
   * Create and insert a vector variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name   The name of the NONMEM vector variable.
   * @param len    The length of the vector.
   * @param owner  The ownder of the object.
   * @param access The access permission to the object.
   */
  Symbol* insertVector( const std::string& name, 
                        int len,
			enum Symbol::Ownership owner, 
			enum Symbol::Access access );

  /**
   * Create and insert a square matrix variable into the table.
   * If the label already exists in the table, the new object replaces
   * the existing entry.
   *
   * @return A pointer to the created Symbol object.
   *
   * @param name      The name of the NONMEM (square) matrix variable.
   * @param structure The (square) matrix structure (ie. sparseness): 
   * @param dim       The dimension of the <em>square</em> matrix.
   * @param owner     The owner of the object.
   * @param access    The access permission to the object.
   */
  Symbol* insertSymmetricMatrix( const std::string& name, 
				 std::valarray<Symbol::Structure>& structure, 
				 std::valarray<unsigned int>& dim,
				 enum Symbol::Ownership owner, 
				 enum Symbol::Access access );

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
   * Return a pointer to the list of data labels.
   *
   * @return A pointer to a vector containing the labels (string). 
   */
  const std::vector<std::string> * getLabels() const;

  /**
   * Return a pointer to the core symbol table.
   */
  const std::map<const std::string, Symbol> * getTable() const;

/**
 * Extractor.
 * Print out all registered symbols.
 *
 * @return The reference to the modified/filled output stream.
 * @param o The reference to the output stream.
 * @param t The symbol table from which the contents to be extracted.
 */
friend std::ostream& operator<<( std::ostream& o, const SymbolTable& t );

  protected:

  /**
   * Copy constructor (prohibited).
   */
  SymbolTable( const SymbolTable& );

  /**
   * Assignment operator (prohibited).
   */
  SymbolTable& operator=( const SymbolTable& );
};


#endif
