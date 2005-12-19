/**
 * @file SpkCompilerError.h
 * Declare SpkCompilerError class
 */
#ifndef SPKCOMPILERERROR_H
#define SPKCOMPILERERROR_H

#include <map>
#include <exception>
#include <string>

/**
 * Representation of an error object. 
 *
 * An SpkCompilerError object has a predetermined fixed size so that
 * the construction of an object never fails for memory allocation.
 */
class SpkCompilerError
{

 public:
  /** The maximum number of digits for an error code */
  static const unsigned int ERRORCODE_FIELD_LEN;

  /** The maximum number of characters for an error cord description */
  static const unsigned int ERRORCODE_DESCRIPTION_FIELD_LEN;

  /** The maximum number of digits for a line number */
  static const unsigned int LINENUM_FIELD_LEN;

  /** The maximum number of charcaters in a name of file in which the error was detected. */
  static const unsigned int FILENAME_FIELD_LEN;
  
  /** The maximum number of characters in an error message */
  static const unsigned int MESSAGE_FIELD_LEN;

  /** The total number of characters and digits held by this object */
  static const unsigned int ERROR_SIZE;

  /** The name of error code field */
  static const char ERRORCODE_FIELD_NAME[];

  /** The name of error code description field */
  static const char ERRORCODE_DESCRIPTION_FIELD_NAME[];

  /** The name of filename field */
  static const char FILENAME_FIELD_NAME[];

  /** The name of line-number field */
  static const char LINENUM_FIELD_NAME[];

  /** The name of message field */
  static const char MESSAGE_FIELD_NAME[];

  enum ErrorCode {
    /** C++ Standard library errors */
    ASPK_STD_ERR,

    /** XML DOM parser error */
    ASPK_XMLDOM_ERR,

    /** SpkSourceML->C++ compilation error */
    ASPK_SOURCEML_ERR,

    /** SpkDataML->C++ compilation error */
    ASPK_DATAML_ERR,

    /** Fortran->C++ compilation error */
    ASPK_FTOC_ERR,

    /** ASPK Compiler implemntator's programming error */
    ASPK_PROGRAMMER_ERR,

    /** User input error */
    ASPK_USER_ERR,

    /** Unknown */
    ASPK_UNKNOWN_ERR
  };
  /** The type definition of a map that maps error codes to short descriptions */
  typedef std::map< enum ErrorCode, const char* > ErrorMap;

 private:
  /** The mapping between error codes and short descriptions */
  static const ErrorMap mapping;

  /** 
   * Initialize <em>mapping</em> object. 
   *
   * @return the initialized map.
   */
  static const ErrorMap initErrorMap();

  /** Error code */
  enum ErrorCode myErrorCode;

  /** Line number at which the error was detected (reported by the caller) */
  unsigned int myLineNum;

  /** Filename in which the error was detected (reported by the caller) */
  char myFileName[128+1]; //[SpkCompilerError::FILENAME_FIELD_LEN+1];

  /** Error message (genreated by the caller) */
  char myMessage[256+1]; // Must be same as MESSAGE_FIELD_LEN + 1.
                         // Can't use the constant because it is not intialized at this point.

  /** 
   * Returns the maximum value that can be used as an error code.
   * @return the maximum value that can be used as an error code.
   */
  unsigned int maxErrorcode() throw();

 public:

  /** Displays a default error message corresponding to the error code */
  static const char* describe( enum ErrorCode key );

  /** Returns the max value that can be used for a line number */
  static unsigned int maxLinenum() throw();

  /** Returns the max number of characters for a filename */
  static unsigned int maxFilenameLen() throw();

  /** Returns the max number of characters in a message */
  static unsigned int maxMessageLen() throw();

  /** Default constructor */
  SpkCompilerError() throw();

  /** Copy constructor (deep copy) */
  SpkCompilerError( const SpkCompilerError& ) throw();

  /** Constructor that takes a set of concrete information for an error */
  SpkCompilerError( enum ErrorCode, const char* message, unsigned int, const char* filename) throw();

  /** Constructor that takes std::exception as error object */
  SpkCompilerError( const std::exception& e, const char* message, unsigned int, const char* filename) throw();

  /** Destructor */
  ~SpkCompilerError() throw();

  /** Assignment operator (deep copy) */
  const SpkCompilerError& operator=(const SpkCompilerError&) throw();
	
  /** Returns the error code */
  enum ErrorCode code() const throw();    

  /** Returns the line number */
  unsigned int linenum() const throw();

  /** Returns the filename */
  const char* filename() const throw();

  /** Returns the message */
  const char* message() const throw();

  /** Serialize in an XML format 
   * @return a string containing the contents of this object in an XML format.
   */
  const std::string getXml() const;

  /** Extractor (to ostream) 
   * @return an ostream object.
   * @param stream The output stream from which contents are extracted.
   * @param e The object from which contents are extracted.
   */
  friend std::ostream& operator<<(std::ostream& stream, const SpkCompilerError& e);

  /** Extractor (to string) 
   * @return a string.
   * @param str The string to which contents are extracted. 
   * @param e The object from which contents are extracted.
   */
  friend std::string& operator<<(std::string& str, const SpkCompilerError& e);

};

#endif
