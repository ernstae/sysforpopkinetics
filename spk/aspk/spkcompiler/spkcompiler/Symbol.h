/**
 * @file Symbol.h
 * Declare the Symbol class.
 */

#ifndef SYMBOL_H
#define SYMBOL_H
#include <valarray>
#include <string>
#include <vector>

/**
 * This class represents a symbol.
 */
class Symbol{
public:

  /**
   * Object type. 
   */
  enum ObjectType { SCALAR,      /**< Scalar */
		    VECTOR,      /**< Vector */
		    MATRIX       /**< Matrix */ };

  /**
   * Object structure. For objects other than MATRIX, use FULL.
   */
  enum Structure  { FULL,        /**< Full matrix */
		    TRIANGLE,    /**< Triangle (symmetrical) matrix */
		    DIAGONAL,     /**< Diagonal matrix */ };

  /**
   * Access permission.
   **/
  enum Access     { READONLY,    /**< Read only */
                    READWRITE,   /**< Read and write */ };
  /**
   * Ownership.  
   * Data labels are owned by DATASET.  NONMEM/SPK reserved words are by SYSTEM
   * and variables found in user models are by USER.
   */
  enum Ownership  { SYSTEM,      /**< System defined */
                    USER,        /**< User defined */ 
                    DATASET      /**< Bound to Data Set */ };
                 
  /**
   * Return a Symbol object that defines the "empty" Symbol.
   *
   * Implementation: 
   * A Symbol object that has no value assigned to "name" fieled
   * is considered empty regardless of the values in the other fields.
   */
   static const Symbol* empty();

  /**
   * The default constructor.  
   * This initializes the object to an empty Symbol.
   * A Symbol object which lacks of the name (ie. the <em>name</em> field is empty)
   * is considered empty no matter how the other fields are defined.
   *
   */
   Symbol();

  /**
   * The constructor that initializes the object with the given parameters.
   *
   * @param nameIn       The symbol/identifier/name for this object.
   * @param synonymIn    The synonym for the symbol.  The string may be empty.
   * @param ownerIn      The owener of this symbol.
   * @param accessIn     The access permision to this object.
   * @param objectTypeIn The type of object.
   * @param structureIn  The object structure.
   * @param dimIn        The dimension of the data object.  
   *                     For scalars, the value is always 1.
   *                     For vectors, it is its length.
   *                     All matrices are assumed to be sqaure, so the value here is 3 if 
   *                     the square matrix is 3 by 3.
   */
   Symbol( const std::string& nameIn,
	   const std::string& synonymIn,
	   enum  Ownership    ownerIn,
	   enum  Access       accessIn,
	   enum  ObjectType   objectTypeIn,
	   std::valarray<enum  Structure>&  structureIn,
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
   * Create and return a Symbol object that represents an arbitrary variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>owner</dt>           <dd>The value of <em>owner</em></dd>
   *   <dt>object_type</dt>     <dd>ObjectType::SCALAR</dd>
   *   <dt>structure</dt>       <dd>Structure::FULL</dd>
   *   <dt>access</dt>          <dd>The value of <em>access</em></dd>
   *   <dt>dimension</dt>       <dd>1</dd>
   * </dl>
   *
   * @param var The variable name.
   * @param owner The owner of this variable.
   * @param access The access permission to this object.
   */
   static Symbol createScalar( const std::string& var, 
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a vector variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>owner</dt>           <dd>The value of <em>owner</em></dd>
   *   <dt>object_type</dt>     <dd>ObjectType::VECTOR</dd>
   *   <dt>structure</dt>       <dd>Structure::FULL</dd>
   *   <dt>access</dt>          <dd>The value of <em>access</em></dd>
   *   <dt>dimension</dt>       <dd>The value of <em>veclen</em></dd>
   * </dl>
   * @param var The name of the vector variable.
   * @param veclen The length of the vector.
   * @param owner The owner of the variable.
   * @param access The access permission to the object.
   */
   static Symbol createVector( const std::string& var, 
                               int veclen, 
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a (square) matrix variable.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>var</em></dd>
   *   <dt>synonym</dt>         <dd>empty</dd>
   *   <dt>owner</dt>           <dd>The value of <em>owner</em></dd>
   *   <dt>object type</dt>     <dd>ObjectType::MATRIX</dd>
   *   <dt>structure</dt>       <dd>The value of <em>structure</em></dd>
   *   <dt>access</dt>          <dd>The value of <em>access</em></dd>
   *   <dt>dimension</dt>       <dd>The value of <em>dim</em></dd>
   * </dl>
   *
   * @param var        The name of the NONMEM (matrix) variable.
   * @param structure  The matrix structure (ie. sparseness).
   * @param dim        The dimension of the square matrix.
   * @param owner      The owner of the matrix.
   * @param access     The access permission of the matrix.
   */
   static Symbol createSymmetricMatrix( const std::string& var, 
			       std::valarray<enum Structure>& structure, 
			       std::valarray <unsigned int>& dim,
			       enum Symbol::Ownership owner, 
			       enum Symbol::Access access );

  /** 
   * Create and return a Symbol object that represents a data (as in Data Set) label.
   *
   * @return A new Symbol object which has the following values set:
   *
   * <dl>
   *   <dt>name</dt>            <dd>The value of <em>label</em></dd>
   *   <dt>synonym</dt>         <dd>The value of <em>alias</em></dd>
   *   <dt>owner</dt>           <dd>Ownership::DATASET</dd>
   *   <dt>object type</dt>     <dd>ObjectType::VECTOR</dd>
   *   <dt>structure</dt>       <dd>Structure::FULL</dd>
   *   <dt>access</dt>          <dd>Access::READONLY</dd>
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
    * The data object type.
    */
   enum ObjectType object_type;

   /**
    * The data structure.
    */
   std::valarray<enum Structure>  structure;
  
   /**
    * The access permission.
    */
   enum Access access;

   /**
    * Ownership
    */
   enum Ownership owner;

   /**
    * An array of dimension(s) of the data object(s) associated with the symbol.
    *
    * The array has a different size depending on the type of object.
    *
    * When the object associated with the symbol is a scalar, a vector or a (square) matrix,
    * the array has only one element.
    * For scalars, the first element of <em>dimension</em> is set to one.
    * For vectors, the first element of <em>dimension</em> is set to the length of vector.
    * For (square) matrices, the first element of <em>dimension</em> is set to the dimension
    *
    * When the object is a data label, the array has the number of elements
    * equal to the number of individuals.  
    * In which case, the i-th element indicates the
    * number of i-th individual's data records.
    */
   std::valarray< int > dimension;

   /**
    * The initial values for the data object(s) refered by the symbol.
    * 
    * This is a list (ie. vector) of vectors.  The number of vectors withint the list
    * depends on the type of data object associated with the symbol.
    * 
    * When the object associated with the symbol is a scalar, a vector or a matrix,
    * the list will contain only one element (a vector).  The element vector
    * has the length of the data object associated with the symbol.
    * For example, if the symbol is "THETA" and the THETA vector has a length of 3,
    * The only element vector has the length of 3.
    * <code><pre>
    * initial[0:0][0:2]
    * </pre></code>
    * where <code>0:0</code> reads "The smallest index is 0 and the largest index is 0";
    * Therefore, the list "initial" has a length of 1.
    *
    * When the object is a data label, the list will contain n number of elements (vectors),
    * where n is the number of individuals.  Each element vector has the number
    * elements equal to the number of i-th individual's data records.
    */
   std::vector< std::valarray<std::string> > initial;

   /**
    * The upper boundary values for the data object(s) by the symbol.
    * 
    * This is a list (ie. vector) of vectors.  The number of vectors withint the list
    * depends on the type of data object associated with the symbol.
    * 
    * When the object associated with the symbol is a scalar, a vector or a matrix,
    * the list will contain only one element (a vector).  The element vector
    * has the length of the data object associated with the symbol.
    * For example, if the symbol is "THETA" and the THETA vector has a length of 3,
    * The only element vector has the length of 3.
    * <code><pre>
    * upper[0:0][0:2]
    * </pre></code>
    * where <code>0:0</code> reads "The smallest index is 0 and the largest index is 0";
    * Therefore, the list "initial" has a length of 1.
    *
    * When the object is a data label, the list will contain n number of elements (vectors),
    * where n is the number of individuals.  Each element vector has the number
    * elements equal to the number of i-th individual's data records.
    */
   std::vector< std::valarray<std::string> > upper;

   /**
    * The lower boundary values for the data object refered by the symbol.
    * 
    * This is a list (ie. vector) of vectors.  The number of vectors withint the list
    * depends on the type of data object associated with the symbol.
    * 
    * When the object associated with the symbol is a scalar, a vector or a matrix,
    * the list will contain only one element (a vector).  The element vector
    * has the length of the data object associated with the symbol.
    * For example, if the symbol is "THETA" and the THETA vector has a length of 3,
    * The only element vector has the length of 3.
    * <code><pre>
    * lower[0:0][0:2]
    * </pre></code>
    * where <code>0:0</code> reads "The smallest index is 0 and the largest index is 0";
    * Therefore, the list "initial" has a length of 1.
    *
    * When the object is a data label, the list will contain n number of elements (vectors),
    * where n is the number of individuals.  Each element vector has the number
    * elements equal to the number of i-th individual's data records.
    */   
   std::vector< std::valarray<std::string> > lower;

   /**
    * The step size for the data object refered by the symbol.
    *
    * This is a list (ie. vector) of vectors.  The number of vectors withint the list
    * depends on the type of data object associated with the symbol.
    * 
    * When the object associated with the symbol is a scalar, a vector or a matrix,
    * the list will contain only one element (a vector).  The element vector
    * has the length of the data object associated with the symbol.
    * For example, if the symbol is "THETA" and the THETA vector has a length of 3,
    * The only element vector has the length of 3.
    * <code><pre>
    * step[0:0][0:2]
    * </pre></code>
    * where <code>0:0</code> reads "The smallest index is 0 and the largest index is 0";
    * Therefore, the list "initial" has a length of 1.
    *
    * When the object is a data label, the list will contain n number of elements (vectors),
    * where n is the number of individuals.  Each element vector has the number
    * elements equal to the number of i-th individual's data records.
    */   
   std::vector< std::valarray<std::string> > step;

   /**
    * The boolean values indicating as to whether the corresponding NONMEM vector
    * value is fixed during the optimization effort.
    *
    * This is a list (ie. vector) of vectors.  The number of vectors withint the list
    * depends on the type of data object associated with the symbol.
    * 
    * When the object associated with the symbol is a scalar, a vector or a matrix,
    * the list will contain only one element (a vector).  The element vector
    * has the length of the data object associated with the symbol.
    * For example, if the symbol is "THETA" and the THETA vector has a length of 3,
    * The only element vector has the length of 3.
    * <code><pre>
    * fixed[0:0][0:2]
    * </pre></code>
    * where <code>0:0</code> reads "The smallest index is 0 and the largest index is 0";
    * Therefore, the list "initial" has a length of 1.
    *
    * When the object is a data label, the list will contain n number of elements (vectors),
    * where n is the number of individuals.  Each element vector has the number
    * elements equal to the number of i-th individual's data records.
    */
   std::vector< std::valarray<bool> >   fixed;

   /**
    * The extractor.
    */
   friend std::ostream& operator<<( std::ostream& o, const Symbol& s );

};

#endif
