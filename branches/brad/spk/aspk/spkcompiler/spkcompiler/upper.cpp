#include <ctype.h>
#include <string>
#include "upper.h"

using namespace std;

const string upper( const string& mix )
{
   int n=mix.size();
   string up(mix);
   string::iterator p = up.begin();
   for( int i=0; i<n; i++ )
   {
      *p = toupper( *p );
      ++p;
   }
   return up;
}
