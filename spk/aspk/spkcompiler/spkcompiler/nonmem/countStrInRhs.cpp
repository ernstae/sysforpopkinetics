#include "countStrInRhs.h"
#include <string.h>

int countStrInRhs( const char* str, const char * equations )
{
  // Look only on LHS of equations
  int n = 0;
  char * dup = strdup( equations );
  char * lhs = NULL;
  char * rhs  = NULL;
  lhs = strtok( dup, "=" );
  rhs = strtok( NULL, "\n" );
  while( rhs != NULL )
    {
      if( strstr( rhs, str ) != NULL )
        ++n;
      lhs = strtok( NULL, "=" );
      rhs = strtok( NULL, "\n" );
    }
  delete [] dup;
  return n;
}
