#include <map>
#include <string>
#include <cstdlib>

#include "../SymbolTable.h"
#include "emit_nonmem_model.h"

using namespace std;

void emit_nonmem_model(
		       FILE * out,         /* writable */
		       FILE * expressions, /* readable */
		       const SymbolTable &table,
		       const map<string, string> &label_alias 
		       )
{
  //=======================================================================
  // Output
  //-----------------------------------------------------------------------
  fprintf( out, "#include <cmath>\n" );
  fprintf( out, "#include <cfloat>\n" );
  fprintf( out, "#include <libspkcompiler/nonmem.h>\n" );
  fprintf( out, "#include \"IndData.h\"\n" );
  fprintf( out, "namespace spk_pred{\n" );
  fprintf( out, "IndDataSet spk_all;\n" );
  map<string, Symbol>::const_iterator itr = table.begin();
  while( itr != table.end() )
  {
     //
     // Keywords
     //
     if( itr->first == "theta" || itr->first == "eta" || itr->first == "eps" 
	   || itr->first == "y" || itr->first == "f" )
     {
        //fprintf( out, "double %s;\n", itr->first.c_str() );
     }
     else
     {
	fprintf( out, "double %s;\n", itr->first.c_str() );
     }
     ++itr;
     //
     // User defined
     //
  }
  fprintf( out, "};\n" );
  fprintf( out, "template< class Type >\n" );
  fprintf( out, "bool evalPred( const Type* const theta, \n" );
  fprintf( out, "               int         spk_nTheta, \n" );
  fprintf( out, "               const Type* const eta, \n" );
  fprintf( out, "               int         spk_nEta, \n" );
  fprintf( out, "               const Type* const eps, \n" );
  fprintf( out, "               int         spk_nEps, \n" );
  fprintf( out, "               int         spk_i, \n" );
  fprintf( out, "               int         spk_j, \n" );
  fprintf( out, "               Type        &f, \n" );
  fprintf( out, "               Type        &y )\n" );
  fprintf( out, "{\n" );
  fprintf( out, "using namespace spk_pred;\n" );
  if( label_alias.find( "time" ) != label_alias.end() )
    {
      // This is to re-enforce pred::time to be used instead of time.h/time.
      fprintf( out, "using spk_pred::time;\n" );
    }
  map<string, string>::const_iterator label = label_alias.begin();
  while( label != label_alias.end() )
  {
     fprintf( out, "%s = spk_all[spk_i].%s[spk_j];\n", label->first.c_str(), label->first.c_str() );
     if( label->second != "" )
     {
        fprintf( out, "%s = spk_all[spk_i].%s[spk_j];\n", label->second.c_str(), label->second.c_str() );
     }
     ++label;
  }
  char buf[128+1];
  assert( expressions );
  fprintf( out, "//============================================\n" );
  fprintf( out, "//   User's Code Begin\n" );
  fprintf( out, "//--------------------------------------------\n" );
  while( feof(expressions) == 0 )
  {
     fgets( buf, 128, expressions );
     fprintf( out, "%s", buf );
  };

  fprintf( out, "//--------------------------------------------\n" );
  fprintf( out, "//   End of User's Code\n" );
  fprintf( out, "//============================================\n" );

  fprintf( out, "if( spk_all[spk_i].evid[spk_j] == nonmem::EVID_OBSERVATION )\n" );
  fprintf( out, "   return true;\n" );
  fprintf( out, "return false;\n" );
  fprintf( out, "}\n" );
  //-----------------------------------------------------------------------
  // End of output
  //=======================================================================
}
