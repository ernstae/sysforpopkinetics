#include <ctype.h>
#include <string>
#include "lower.h"
#include "upper.h"

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
//  For some reason, when the following routine
// is defined in an independent file, say upper.cpp,
// it doesn't get into the library.
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
