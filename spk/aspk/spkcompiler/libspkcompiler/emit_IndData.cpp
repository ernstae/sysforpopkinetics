/*
#ifndef INDDATA_H
#define INDDATA_H

#include <vector>
#include <valarray>

class IndData{
   public:
      IndData( 
         const SPK_VA::valarray<double>& xIn, 
         const SPK_VA::valarray<double>& zIn );
      const SPK_VA::valarray<double> x;
      const SPK_VA::valarray<double> y;
      const SPK_VA::valarray<double> z;
      ~IndData();

   protected:
      IndData();
      IndData( const IndData & );
      const IndData& operator=( const IndData& );
};

class IndDataSet{
   public:
      IndDataSet( int nIn, 
      const SPK_VA::valarray<double> xIn[], 
      const SPK_VA::valarray<double> zIn[] );
      ~IndDataSet();
      const IndData& operator[]( int n ) const;
   private:
      std::vector<IndData*> all;
      const int n;
   protected:
      IndDataSet();
      IndDataSet( const IndDataSet & );
      const IndDataSet& operator=( const IndDataSet& );
};
///////////////////////////////////////////////////////////
#include "IndData.h"
#include <valarray>
#include <string>
#include <vector>
#include <map>

IndData::IndData( 
         const SPK_VA::valarray<double>& xIn, 
         const SPK_VA::valarray<double>& zIn )
:
        x(xIn), y(xIn),
        z(zIn)
{}
IndData::~IndData(){}
IndData::IndData(){}
IndData::IndData( const IndData & ){}
const IndData& IndData::operator=( const IndData& ){}

IndDataSet::IndDataSet( int nIn, 
      const SPK_VA::valarray<double> xIn[], 
      const SPK_VA::valarray<double> zIn[] )
      : n(nIn), all(nIn)
{
   for( int i=0; i<n; i++ )
   {
      all[i] = new IndData( xIn[i], zIn[i] );
   }
}
IndDataSet::~IndDataSet()
{
   for( int i=0; i<n; i++ )
      delete all[i];
}
const IndData & IndDataSet::operator[]( int i ) const
{
   return *all[i];
}
IndDataSet::IndDataSet(){}
IndDataSet::IndDataSet( const IndDataSet & ){}
const IndDataSet & IndDataSet::operator=( const IndDataSet& ){}

#endif
*/

#include <cstdlib>
#include <vector>
#include <map>
#include <string>
#include <spk/SpkValarray.h>

using SPK_VA::valarray;
using namespace std;

void emit_IndData( FILE * header, FILE * cpp,
                  const std::map<std::string, std::string> &label_alias,
                  const std::vector< std::map<std::string, SPK_VA::valarray<double> > > &data_for,
                  const std::string IDs[] )
{
  const int n = data_for.size();
  assert( n > 0 );
  assert( header != NULL );
  assert( cpp    != NULL );

  const char * IndData = "IndData";

  fprintf( header, "#ifndef INDDATA_H\n" );
  fprintf( header, "#define INDDATA_H\n" );
  fprintf( header, "\n" );
  fprintf( header, "#include <vector>\n" );
  fprintf( header, "#include <spk/SpkValarray.h>\n" );
  fprintf( header, "\n" );

  //==========================================================
  // class IndData header
  //==========================================================
  fprintf( header, "class %s{\n", IndData );
  fprintf( header, "   public:\n" );
  fprintf( header, "      %s( const std::string & ID, \n" );

  std::map<std::string, std::string>::const_iterator names
    = label_alias.begin();
  while( names != label_alias.end() )
  {
    if( names != label_alias.begin() )
      fprintf( header, ", \n" );
    fprintf( header, "         const SPK_VA::valarray<double>& %sIn", names->first.c_str() );
    ++names;
  }
  fprintf( header, " );\n" );


  names = label_alias.begin();
  while( names != label_alias.end() )
  {
    fprintf( header, "      const SPK_VA::valarray<double> %s;\n", names->first.c_str() );
    if( names->second != "" )
	  {
	    fprintf( header, "      const SPK_VA::valarray<double> %s;\n", names->second.c_str() );
	  } 
    ++names;
  }
  fprintf( header, "      const std::string ID;\n" );

  fprintf( header, "      ~%s();\n", IndData );

  fprintf( header, "\n" );
  fprintf( header, "   protected:\n" );
  fprintf( header, "      %s();\n", IndData );
  fprintf( header, "      %s( const %s & );\n", IndData, IndData );
  fprintf( header, "      const %s& operator=( const %s& );\n", IndData, IndData );
  fprintf( header, "};\n" );

  //==========================================================
  // class IndData definition
  //==========================================================
  fprintf( cpp, "#include \"%s.h\"\n", IndData );
  fprintf( cpp, "#include <spk/SpkValarray.h>\n" );
  fprintf( cpp, "#include <string>\n" );
  fprintf( cpp, "#include <vector>\n" );
  fprintf( cpp, "#include <map>\n" );
  fprintf( cpp, "\n" );
  fprintf( cpp, "using SPK_VA::valarray;\n" );
  fprintf( cpp, "using namespace std;\n" );
  fprintf( cpp, "\n" );

  names = label_alias.begin();
  fprintf( cpp,    "%s::%s( const std::string& IDIn, \n", IndData, IndData );
  while( names != label_alias.end() )
  {
    if( names != label_alias.begin() )
      fprintf( cpp, ", \n" );
    fprintf( cpp, "         const SPK_VA::valarray<double>& %sIn", names->first.c_str() );
    ++names;
  }
  fprintf( cpp, " )\n" );
  fprintf( cpp, "        :\n" );

  fprintf( cpp, "        ID(IDIn), \n" );      
  names = label_alias.begin();
  while( names != label_alias.end() )
  {
    if( names != label_alias.begin() )
       fprintf( cpp, ",\n" );
    fprintf( cpp, "        %s(%sIn)", names->first.c_str(), names->first.c_str() );
    if( names->second != "" ||  !names->second.empty() )
    {
	    fprintf( cpp, ", %s(%sIn)", names->second.c_str(), names->first.c_str() );
	  }
    ++names;
  }
  fprintf( cpp, "\n" );
  fprintf( cpp, "{}\n" );
  
  fprintf( cpp, "%s::~%s(){}\n", IndData, IndData );

  fprintf( cpp, "%s::%s(){}\n", IndData, IndData );
  fprintf( cpp, "%s::%s( const %s & ){}\n", IndData, IndData, IndData );
  fprintf( cpp, "const %s& %s::operator=( const %s& ){}\n", IndData, IndData, IndData );

  //==========================================================
  // class IndDataSet header
  //==========================================================
  fprintf( header, "\n" );

  fprintf( header, "class %sSet{\n", IndData );
  fprintf( header, "   public:\n" );
  fprintf( header, "      %sSet();\n", IndData );
  fprintf( header, "      ~%sSet();\n", IndData );
  fprintf( header, "      const %s& operator[]( int i ) const;\n" );
  fprintf( header, "   private:\n" );
  fprintf( header, "      std::vector<IndData*> all;\n" );
  fprintf( header, "      const int n;\n" );
  fprintf( header, "   protected:\n" );
  fprintf( header, "      %sSet( const %sSet & );\n", IndData, IndData );
  fprintf( header, "      const %sSet& operator=( const %sSet& );\n", IndData, IndData );
  fprintf( header, "};\n" );
  fprintf( header, "#endif\n" );

  //==========================================================
  // class IndDataSet definition
  //==========================================================
  fprintf( cpp, "\n" );

  fprintf( cpp, "%sSet::%sSet()\n", IndData, IndData );
  fprintf( cpp, "      : n(%d), all(%d)\n", n, n );
  fprintf( cpp, "{\n" );

//======================
// data initialization
//

// for i-th individual
// there are (data_for[i]).size() number of observations.
// Each observation can be retrieved by its iterator.
// The label for the observation must be also found in label_alias
// table as either key or entry. 
  for( int i=0; i<n; i++ )
    {
      map<string, valarray<double> >::const_iterator itr = data_for[i].begin();
      while( itr != data_for[i].end() ) 
	{
	  int m = itr->second.size();
	  fprintf( cpp, "   double raw_%s_%s[] = { ",
		   itr->first.c_str(),
		   IDs[i].c_str() ); 
	  for( int j=0; j<m; j++ )
	    {
	      if( j > 0 )
		fprintf( cpp, ", " );
	  fprintf( cpp, "%f", itr->second[j] );
	    }
	  fprintf( cpp, " };\n" );
	  fprintf( cpp, "   valarray<double> %s_%s( raw_%s_%s, %d );\n", 
		   itr->first.c_str(), 
		   IDs[i].c_str(),
		   itr->first.c_str(),
		   IDs[i].c_str(), 
		   m );
	  itr++;
	}
      fprintf( cpp, "   all[%d] = new %s( \"%s\", ", i, IndData, IDs[i].c_str() );
      itr = data_for[i].begin();
      while( itr != data_for[i].end() ) 
	{
	  if( itr != data_for[i].begin() )
	    fprintf( cpp, ", " );
	  fprintf( cpp, "%s_%s", itr->first.c_str(), IDs[i].c_str() );
	  ++itr;
	}  
      fprintf( cpp, " );\n" );
      fprintf( cpp, "\n" );
    }
//  
//======================

  fprintf( cpp, "}\n" );

  fprintf( cpp, "%sSet::~%sSet()\n", IndData, IndData );
  fprintf( cpp, "{\n" );
  fprintf( cpp, "   for( int i=0; i<n; i++ )\n" );
  fprintf( cpp, "      delete all[i];\n" );
  fprintf( cpp, "}\n" );

  fprintf( cpp, "const %s & %sSet::operator[]( int i ) const\n", IndData, IndData );
  fprintf( cpp, "{\n" );
  fprintf( cpp, "   return *all[i];\n" );
  fprintf( cpp, "}\n" );

  
  fprintf( cpp, "%sSet::%sSet( const %sSet & ){}\n", IndData, IndData, IndData );
  fprintf( cpp, "const %sSet & %sSet::operator=( const %sSet& ){}\n", IndData, IndData, IndData );

  return;
}
