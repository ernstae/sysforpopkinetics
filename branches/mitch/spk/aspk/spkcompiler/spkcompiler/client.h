#ifndef CLIENT_H
#define CLIENT_H

#include <string>
/**
 * @file client.h
 * Define "client" namespace. 
 */
/**
 * The namespace, "client", declares an enumulator, type, that should be
 * used to identify the clients of Spk Compiler and helper functions to
 * convert back and force the enumulator values and the string representation
 * of the enum values.
 *
 */
namespace client{

  /**
   * An enumulator listing the clients of Spk Compiler.
   *
   * @todo Currently NONMEM is the only client registered.  
   * Should add values as Spk Compiler
   * support more client languages 
   * (so far, known potentials are SAAM-II, MatLab).
   */
   enum type{ NONMEM /**< identifies NONMEM users*/, NOT_SUPPORTED };

  /**
   * String representation of enum, client::NONMEM.
   */
   const char * const STR_NONMEM = "nonmem";

  /**
   * String representation of enum, client::NOT_SUPPORTED.
   */
   const char * const STR_NOT_SUPPORTED = "Not supported!";

   /**
    * Convert a type enumulator to a string.
    *
    * @param t is a type enum.
    * @return a corresponding character string.
    */
   inline const char * const toString( type t ){
     if( t == NONMEM )
       return STR_NONMEM;
     else
       return STR_NOT_SUPPORTED;
   }
   /**
    * Convert a character string to a type enumulator.
    *
    * @param t is a character string which should match 
    * one of the STR_xxx strings defined in this namespace.
    * @return a corresponding type enum.
    */
   inline enum type toEnum( const char * t ){
     if( strcmp( t, STR_NONMEM ) == 0 )
       return NONMEM;
     else
       return NOT_SUPPORTED;
   }
}

#endif
