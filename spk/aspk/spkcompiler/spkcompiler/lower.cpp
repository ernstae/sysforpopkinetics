#include <ctype.h>
#include <string>
#include "lower.h"

using namespace std;

const string lower( const string& mix )
{
   int n=mix.size();
   string low(mix);
   string::iterator p = low.begin();
   for( int i=0; i<n; i++ )
   {
      *p = tolower( *p );
      ++p;
   }
   return low;
}
