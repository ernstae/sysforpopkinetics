#ifndef SYMBOL_H
#define SYMBOL_H

#include <string>
/**
 * @file Symbol.h
 *
 * Declares Symbol class
 */
/**
 * A class of symbols.
 *
 * A symbol object represents a symbol with attributes like data type and structure.
 */
class Symbol{
 public:

  /**
   * An enum for data or structure types of symbols.
   *
   * This enum list is composed of values from two different groups.
   * One is to describe the structure (i.e. scalar, vector...) of the data associated with a symbol and
   * another is for the type of data (i.e. interger, real...).
   */
  enum SYMBOLTYPE { UNKNOWN,    /**< for symbols whose nature has not been discovered */ 
		    SCALAR,     /**< for symbols which represent a scalar value */
		    VECTOR,     /**< for symbols which represent a vector (one dimensional array) */
		    MATRIX,     /**< for symbols which represent a matrix (two dimensional array) */
		    INT,        /**< for symbols whose data type is integer */
		    DOUBLE,     /**< for symbols whose data type is (double-precision) real */
		    BOOL        /**< for symbols whose data type is logical (true/false) */
                  };

  /**
   * Convert an SYMBOLTYPE enumulator to a equivalent string.
   *
   * @param e is a member of SYMBOLTYPE enum.
   * @return a std::string representation of e.
   */
  static const std::string toString( enum SYMBOLTYPE e );

  /**
   * Convert an SYMBOLTYPE enumulator to a equivalent string.
   *
   * @param e is a member of SYMBOLTYPE enum.
   * @return a C character array representation of e.
   */
  static const char * const toCString( enum SYMBOLTYPE e );

  /**
   * Convert a string to an equivalent SYMBOLTYPE enumulator.
   * @param str is a text description of the enum value.
   * @return an enum assigned to the string.
   */
  static enum SYMBOLTYPE toEnum( const std::string& str );

  static const std::string STR_UNKNOWN;   /**< String representation of enum SYMBOLTYPE::UNKNOWN */
  static const std::string STR_SCALAR;    /**< String representation of enum SYMBOLTYPE::SCALAR */
  static const std::string STR_VECTOR;    /**< String representation of enum SYMBOLTYPE::VECTOR */
  static const std::string STR_MATRIX;    /**< String representation of enum SYMBOLTYPE::MATRIX */
  static const std::string STR_INT;       /**< String representation of enum SYMBOLTYPE::INT */
  static const std::string STR_DOUBLE;    /**< String representation of enum SYMBOLTYPE::DOUBLE */
  static const std::string STR_BOOL;      /**< String representation of enum SYMBOLTYPE::BOOL */

  static const char * const C_UNKNOWN;   /**< C string representation of enum SYMBOLTYPE::UNKNOWN */
  static const char * const C_SCALAR;    /**< C string representation of enum SYMBOLTYPE::SCALAR */
  static const char * const C_VECTOR;    /**< C string representation of enum SYMBOLTYPE::VECTOR */
  static const char * const C_MATRIX;    /**< C string representation of enum SYMBOLTYPE::MATRIX */
  static const char * const C_INT;       /**< C string representation of enum SYMBOLTYPE::INT */
  static const char * const C_DOUBLE;    /**< C string representation of enum SYMBOLTYPE::DOUBLE */
  static const char * const C_BOOL;      /**< C string representation of enum SYMBOLTYPE::BOOL */



  /**
   * Constusts a symbol object whose nature is yet to be known (typically used for non-keywords).
   *
   * @param name points to a string that holds the name.
   * @param isKeyword indicates as to whether this symbol is a name reserved by Spk or the client language.
   */  
  Symbol( const std::string& name, 
	  bool isKeyword = false );

  /**
   * Constructs a symbol object whose data type and structure are known (typically used for keywords).
   *
   * @param name points to a character array that holds the name.
   * @param objectType is a SYMBOLTYPE enumulator indicating the structure of the data associated with the name.
   *        Valid values are UNKNOWN, SCALAR, VECTOR, and MATRIX.
   * @param dataType is a SYMBOLTYPE enumulator indicating the type of the data associated with the name.
   *        Valid values are UNKNOWN, INT, DOUBLE and BOOL.
   * @param isKeyword indicates as to whether this symbol is a name reserved by Spk or the client language.
   */
  Symbol( const std::string& name, 
	  enum SYMBOLTYPE objectType, 
	  enum SYMBOLTYPE dataType, 
  	  bool isKeyword = true);

  /**
   * Copy constructor
   *
   * @param original is the source Symbol object.
   */
  Symbol( const Symbol& original );

  /**
   * Destructor
   */
  ~Symbol();
  
  /**
   * Determines if the type and structure of the data associated with the symbol have been descovered.
   *
   * @return true if both of the type and the structure of the data 
   * associated with the symbole have been discovered: false otherwise.
   */
  bool isDefined( void ) const;

  /** 
   * Determines if the symbol is a name reserved by Spk or the client language.
   *
   * @return true if it is a keyword; false otherwise.
   */
  bool isKeyword() const;

  /** 
   * Determines the name associated with this object.
   *
   * @return a constant pointer to a string object that holds the symbol (name).
   */
  const std::string name() const;

  /**
   * Determines the strucuture/object type of data associated with the symbol.
   *
   * @return a SYMBOLTYPE enumulator indicating the structure of the data (UNKNOWN, INT, DOUBLE, BOOL).
   */
  enum SYMBOLTYPE objectType() const;

  /**
   * Determines the type of data associated with the symbol.
   *
   * @return a SYMBOLTYPE enumulator indicating the data type (ie. UNKNOWN, SCALAR, VECTOR, MATRIX).
   */
  enum SYMBOLTYPE dataType() const;

  /**
   * Determines the size of data associated with the symbol.
   *
   * @return the size of data.  For scalars, the return value is always 1.  For vectors, the return value is
   * the length of the vector (>=0).  For matrices, the return value is the number of total elements in the matrix (>=0).
   */
  int size() const;

  /**
   * Determines the dimensions of data associated with the symbol.
   *
   * @return a pair of integers that specifies the dimensions of data.  
   *         For scalars, the return value is always (1,1). Vectors are treated as the same as n by 1 matrices (column vectors).
   *         For matrices, the first element of the returned pair indicates the row dimension 
   *         the second element for the column dimension.
   */
  std::pair< int, int > dim() const;

  /**
   * Associates the object type (strucutre) of data to the symbol.
   *
   * @param objTypeIn shall be one of the following values from SYMBOLTYPE enumulator: UNKNOWN, SCALAR, VECTOR or MATRIX.
   */
  void objectType( enum SYMBOLTYPE objTypeIn );

  /**
   * Associates the data type to the symbol.
   *
   * @param dataTypeIn shall be one of the following values from SYMBOLTYPE numulator: UNKNOWN, INT, DOUBLE, BOOL.
   */
  void dataType( enum SYMBOLTYPE dataTypeIn );

  /**
   * Associates the length of data to the symbol.  
   *
   * This method can be used by only scalar or vector type data.  It will be typically used to set the size of vectors 
   * since the size is fixed no matter what value of @code n @endcode the user gives if the structure of data is 
   * previously set to SCALAR through Symbol::dataType().  
   *
   * @exception For matrices, it will throw an exception.
   *
   * @todo At this point, error handling is crude.  Although the above statement says an exception would be thrown
   * for matrices, it actually only displays an error message to the standard output.
   *
   * @param n specifies the length of data (n >= 0).
   *
   */
  void size(int n);

  /**
   * Associates the dimensions to the symbol.
   *
   * This method can be used for all three data structures: scalar, vector and matrices.
   * For scalars, the dimensions are fixed to one by one, no matter what value of @code rows @endcode and
   * @code cols @endcode the user give.
   * Vectors are treated as column vectors; thus the column dimension is fixed to one, no matter what.
   *
   * @param rows specifies the number of rows (>=0).
   * @param cols specifies the number of columns (>=0).
   */
  void dim(int rows, int cols);

  /**
   * The extractor for Symbol objects.
   *
   * Extract the contents of the Symbol object to the ostream.  For example:
   * @code 
   * (K) theta: R^0
   * @endcode
   * for a variable named "theta" which is a sclar of type real and is a keyword.
   * Another example:
   * @code
   * (U) x: Z^(3,3)
   * @endcode
   * for a variable named "x" which is a integer matrix of dimensions 3 by 3 and is user-defined.
   * Yet another example:
   * @code
   * (U) y: B^(5)
   * for a variable named "y" which is a boolean vector of length 5 and is user-defined.
   * @endcode 
   */
  friend std::ostream& operator<<( std::ostream& o, const Symbol& s );
   
 protected:
  Symbol(){};
  Symbol * operator=( const Symbol& ) {};

 private:
  std::string myName;
  enum SYMBOLTYPE myObjectType;
  enum SYMBOLTYPE myDataType;
  std::pair<int, int> myDimensions;

  bool keyword;
};

/**
 * An extractor for std::pair.
 *
 * This extractor extracts the first and second elements of the pair 
 * and returns an ostream object that contains the numbers seperated by a camma followed by a single space,
 * enclosed by peranthesies.  For example, for a pair of m and n, @code (m, n) @endcode is returned.
 *
 * @param o is an ostream object to which the information is insereted into.
 * @param p is a pair from which first and second elements are extracted.  It is required that an extractor
 * is implemneted for each type.
 * @return a reference to the modified ostream object.
 */
template <class FIRST, class SECOND>
std::ostream& operator<<( std::ostream & o, const std::pair<FIRST, SECOND> &p )
{
  return  o << "(" << p.first << ", " << p.second << ")";
}

#endif
