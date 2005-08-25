#ifndef SYMBOL_H
#define SYMBOL_H
#include <valarray>
#include <string>
#include <vector>

/**
 * @file Symbol.h
 *
 * Declare the Symbol class.
 */

/**
 * The Symbol class abstracts the concept of string or word that is used as a reference
 * to some other object.
 *
 * In our case, a symbol would indicate a variable name and the object associated with it
 * may be a scalar, a vector, a list of vectors or a matrix.
 * These objects have their own peculiar properties such as the length if it is a vector or
 * the lengths of vectors for a list of vectors.  For a matrix, a property may indicate
 * the sparceness of the matrixe: diagonal, triangle or full.
 */
class Symbol{
public:

  /**
   * The types of symbols.
   */
  enum SymbolType { DATALABEL,   /** Data label **/
		    USERDEFINED, /** User defined variable **/
		    PREDEFINED   /** Predefined variable **/ };
  /**
   * The types of data object associated with symbols.
   */
  enum ObjectType { SCALAR,      /** Scalar **/
		    VECTOR,      /** Vector **/
		    MATRIX       /** Matrix **/ };

  /**
   * The types of object structures.  For objects other than matrix, use FULL.
   */
  enum Structure  { FULL,        /** Full matrix **/
		    TRIANGLE,    /** Triangle (symmetrical) matrix **/
		    DIAGONAL     /** Diagonal matrix **/ };

  /**
   * The access permissions.
   **/
  enum Access     { READONLY,    /** Read only **/
                    READWRITE,   /** Read and write **/ };
  /**
   * Ownership, indicating who defines it.
   */
  enum Ownership  { SYSTEM,      /** System defined **/
                    USER,        /** User defined **/ 
                    DATASET      /** Bound to Data Set **/ };
                 
  /**
   * Return a Symbol object that defines "empty".
   *
   * A Symbol object which lacks of the name (ie. the <em>name</em> field is empty)
   * is considered empty no matter how the other fields are defined.
   */
   static const Symbol* empty();

  /**
   * The default constructor.  This initializes the object as an empty Symbol.
   *
   * A Symbol object which lacks of the name (ie. the <em>name</em> field is empty)
   * is considered empty no matter how the other fields are defined.
   *
   */
   Symbol();

  /**
   * The constructor that initializes the object with the given parameters.
   *
   * @param nameIn    The symbol/identifier/name for this object.
   * @param synonymIn The synonym for the symbol.  The string may be empty.
   * @param stIn      The symbol type: data label, user-defined variable or NONMEM keyword
   * @param otIn      The object type: scalar, vector or matrix
   * @param msIn      The type of the data structure: full, triangle or diagonal.
   *                  The value shall be always FULL for all non-matrices.
   * @param dimIn     The dimension of the data object.  
   *                  For scalars, the value is always 1.
   *                  For vectors, it is its length.
   *                  All matrices are assumed to be sqaure, so the value here is 3 if 
   *                  the square matrix is 3 by 3.
   */
   Symbol( const std::string& nameIn,
           const std::string& synonymIn,
           enum SymbolType stIn,
           enum ObjectType otIn,
           enum Structure msIn,
           const std::valarray<int>& dimIn );

   Symbol( const std::string& nameIn,
	   const std::string& synonymIn,
	   enum  Ownership    ownerIn,
	   enum  Access       accessIn,
	   enum  ObjectType   objectTypeIn,
	   enum  Structure    structureIn,
	   const std::valarray<int>& dimIn );
  /**
   * The copy constructor. 
   * The values in the given Symbol object are all copied to this object.
   */
   Symbol( const Symbol& );

  /**
   * The assignment operator.
   * The values in the right hand object are all copied to this object.
   */
   Symbol& operator=( const Symbol& );

  /**
   * Return true if the names, the synonyms, the symbol types, the data object types, 
   * the data structures and the dimensions all match.  Otherwise, false is returned.
   */
   bool operator==( const Symbol& ) const;

  /**
   * Return true if any of the names, the synonyms, the symbol types, the data object types,
   * the data structures or the dimensions mismatches.  If all match, false is returned.
   */
   bool operator!=( const Symbol& ) const;

  /**
   * Create and return a Symbol object that represents a user-defined (scalar) variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>symbol type</dt>     <dd>SymbolType::USERDEF</dd>
   *   <dt>data object type</dt><dd>ObjectType::SCALAR</dd>
   *   <dt>data structure</dt>  <dd>Structure::FULL</dd>
   *   <dt>dimension</dt>       <dd>1</dd>
   * </dl>
   *
   * @param var The name of the user-defined (scalar) variable.
   */
   static Symbol createScalar( const std::string& var );
   static Symbol createScalar( const std::string& var, 
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a NONMEM (vector) variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>symbol type</dt>     <dd>SymbolType::NONMEMDEF</dd>
   *   <dt>data object type</dt><dd>ObjectType::VECTOR</dd>
   *   <dt>data structure</dt>  <dd>Structure::FULL</dd>
   *   <dt>dimension</dt>       <dd>The value of <em>length</em></dd>
   * </dl>
   * @param var The name of the NONMEM (vector) variable.
   * @param length The length of the vector.
   */
   static Symbol createVector( const std::string& var, int length );
   static Symbol createVector( const std::string& var, int veclen, 
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a NONMEM (matrix) variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>symbol type</dt>     <dd>SymbolType::NONMEMDEF</dd>
   *   <dt>data object type</dt><dd>ObjectType::MATRIX</dd>
   *   <dt>data structure</dt>  <dd>The value of <em>structure</em></dd>
   *   <dt>dimension</dt>       <dd>The value of <em>dim</em></dd>
   * </dl>
   * @param var The name of the NONMEM (matrix) variable.
   * @param structure  The matrix structure (ie. sparseness).
   * @param dim The dimension of the square matrix.
   */
   static Symbol createMatrix( const std::string& var, enum Structure structure, int dim );
   static Symbol createSymmetricMatrix( const std::string& var, 
			       enum Structure structure, 
			       int dim,
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a data (as in Data Set) label.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>The value of <em>alias</em></dd>
   *   <dt>symbol type</dt>     <dd>SymbolType::DATALABEL</dd>
   *   <dt>data object type</dt><dd>ObjectType::VECTOR</dd>
   *   <dt>data structure</dt>  <dd>Structure::FULL</dd>
   *   <dt>dimension</dt>       <dd>N</dd>
   * </dl>
   * @param label The label for the data subset.
   * @param alias  The synonym/alias for the data label.
   * @param N The number of data records for each individual.
   */
   static Symbol createLabel( const std::string& label, 
                              const std::string& alias,
			      const std::valarray<int>& N );
public:

   /**
    * Set the access permission associated with the symbol.
    * Returns the previous access value.
    */
   //   enum Access setAccess( enum Access p );

   /**
    * The name/symbol/identifier that refers to the object.
    */
   std::string name;
   
   /**
    * The alias/synonym for the symbol.  This can be empty.
    */
   std::string synonym;

   /**
    * The symbol type.
    */
   enum SymbolType symbol_type;

   /**
    * The data object type.
    */
   enum ObjectType object_type;

   /**
    * The data structure (ie. sparseness).
    */
   enum Structure  structure;
  
   /**
    * The access permission.
    */
   enum Access access;

   /**
    * Ownership
    */
   enum Ownership owner;

   /**
    * The dimension(s) of the data object(s) refered by the symbol.
    *
    * For scalars, the first element of <em>dimension</em> is set to one.
    * For vectors, the first element of <em>dimension</em> is set to the length of vector.
    * For data subsets (ie. vectors), the i-th element of <em>dimension</em> is set to the
    * length of the i-th data subset.
    * For matrices, the first element of <em>dimension</em> is set to the dimension.  
    * Note that the matrices are all assumed to be square.
    */
   std::valarray< int > dimension;

   /**
    * The typical or initial values for the data object(s) refered by the symbol.
    * 
    * This is a list (ie. vector) of vectors.  For symbols like data labels,
    * there are n number of data subsets associated with the label, 
    * where n is the number of subjects.  For other symbols, the list will have
    * only a single vector, namely the first element (vector) of initial, initial[0].
    */
   std::vector< std::valarray<std::string> > initial;

   /**
    * The upper boundary values for the data object refered by the symbol.
    *
    * It makes sense to use this field only if the symbol is associated with a
    * NONMEM vector or matrix.  The vector is sized to the same value as of 
    * the first element of <em>dimension</em> vector for NONMEM vectors or matrices.
    * For other data objects, this vector remains empty.
    */
   std::vector< std::valarray<std::string> > upper;

   /**
    * The lower boundary values for the data object refered by the symbol.
    *
    * It makes sense to use this field only if the symbol is associated with a
    * NONMEM vector or matrix.  The vector is sized to the same value as of 
    * the first element of <em>dimension</em> vector for NONMEM vectors or matrices.
    * For other data objects, this vector remains empty.
    */   
   std::vector< std::valarray<std::string> > lower;

   /**
    * The step size for the data object refered by the symbol.
    *
    * It makes sense to use this field only if the symbol is associated with a
    * NONMEM vector or matrix.  The vector is sized to the same value as of 
    * the first element of <em>dimension</em> vector for NONMEM vectors or matrices.
    * For other data objects, this vector remains empty.
    */   
   std::vector< std::valarray<std::string> > step;

   /**
    * The boolean values indicating as to whether the corresponding NONMEM vector
    * value is fixed during the optimization effort.
    *
    * It makes sense to use this field only if the symbol is associated with a
    * NONMEM vector or matrix.  The vector is sized to the same value as of 
    * the first element of <em>dimension</em> vector for NONMEM vectors or matrices.
    * For other data objects, this vector remains empty.
    */
   std::vector< std::valarray<bool> >   fixed;

   /**
    * Extract the contents into the ostream.
    *
    */
   friend std::ostream& operator<<( std::ostream& o, const Symbol& s );

};

#endif
