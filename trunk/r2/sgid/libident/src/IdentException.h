/*************************************************************************
 *//**
 * @file IdentException.h
 * 
 * 
 * Declares IdentException class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef IDENTEXCEPTION_H
#define IDENTEXCEPTION_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Standard library header files.
#include <stdexcept>
#include <string>


/*************************************************************************
 *
 * Class: Identexception
 *
 *//**
 * This class is used to throw exceptions that occur during the
 * identifiability analysis.
 *//*
 *************************************************************************/

class IdentException : public std::runtime_error 
{
  //------------------------------------------------------------
  // Constructors.
  //------------------------------------------------------------

public:
  IdentException()
  :
  std::runtime_error( "An unknown identifiability error occurred.  Please submit a bug report." )
  {}

  IdentException( const std::string& message )
  :
  std::runtime_error( message )
  {}

  IdentException( const char* message )
  :
  std::runtime_error( message )
  {}

};

#endif
