#include "factorial.h"

unsigned int factorial( unsigned int n )
{
  if( n == 0 )
    return 0;
  else
    return n + factorial( n-1 );
}
