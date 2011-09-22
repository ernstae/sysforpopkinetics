/**
 * @file countStrInLhs.cpp
 * Define the countStrInLhs() function.
 */

#include "countStrInLhs.h"
#include <string.h>

int countStrInLhs( const char* str, const char * equations )
{
  // Look only on LHS of equations
  int n = 0;
  char * dup = strdup( equations );
  char * lhs = NULL;
  char * rhs  = NULL;
  lhs = strtok( dup, "=" );
  while( lhs != NULL )
    {
      if( strstr( lhs, str ) != NULL )
        ++n;
      rhs = strtok( NULL, "\n" );
      lhs = strtok( NULL, "=" );
    }
  delete [] dup;
  return n;
}
