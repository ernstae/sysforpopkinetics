#include "emit_IndDataClass.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>

using namespace std;

static const char * classname = "IndData";

////////////////////////////////////////////////////////////////////////////////////
//
// LOCAL FUNCTIONS
//
////////////////////////////////////////////////////////////////////////////////////

void emit_IndDataClass( 		
                 FILE * out,
  		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
		 const string order_id_pair[]
		 )
{
  assert( out != NULL );
  //=================================================================================
  //
  // Define class IndData
  //
  //
  //=================================================================================

  fprintf( out, "#ifndef INDDATA_H\n" );
  fprintf( out, "#define INDDATA_H\n" );
  fprintf( out, "\n" );
  fprintf( out, "#include <cstdlib>\n" );
  fprintf( out, "#include <string>\n" );

  fprintf( out, "class %s{\n", classname );
  fprintf( out, "   public:\n" );
  fprintf( out, "      %s( int nMeasurementsIn", classname );

  map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS>::const_iterator names
    = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      fprintf( out, ", \n" );
      fprintf( out, "      const double %sIn[]", names->first.c_str() );
      ++names;
    }
  fprintf( out, " )\n" );
  fprintf( out, "      : nMeasurements(nMeasurementsIn)" );
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      fprintf( out, ",\n" );
      fprintf( out, "        " );
      fprintf( out, "%s(NULL)", names->first.c_str() );
      if( names->second.c_str() != "" )
	{
	  fprintf( out, ", %s(NULL)", names->second.c_str() );
	}
      ++names;
    }
  fprintf( out, "\n" );
  fprintf( out, "      {\n" );
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
  	fprintf( out, "         %s = new double[ nMeasurements ];\n", 
			names->first.c_str() );
	fprintf( out, "         memcpy( %s, %sIn, sizeof(double)*nMeasurements );\n", 
			names->first.c_str(), names->first.c_str());
	if( names->second.c_str() != "" )
	  {
	    fprintf( out, "         %s = %s;\n", names->second.c_str(), names->first.c_str() );
	  }
	++names;
    }
  fprintf( out, "      }\n" );
  fprintf( out, "      ~%s()\n", classname );
  fprintf( out, "      {\n" );
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
     {
        fprintf( out, "         delete [] %s;\n", names->first.c_str() );
	++names;
     }
  fprintf( out, "      }\n" );
  fprintf( out, "\n" );
  fprintf( out, "      const int nMeasurements;\n" );  

  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      fprintf( out, "      double * %s;\n", names->first.c_str() );
      if( names->second.c_str() != "" )
	{
	  fprintf( out, "      double * %s;\n", names->second.c_str() );
	}
      ++names;
    }

  fprintf( out, "\n" );
  fprintf( out, "   protected:\n" );
  fprintf( out, "      %s(){}\n", classname );
  fprintf( out, "      %s( const %s & ){}\n", classname, classname );
  fprintf( out, "      %s& operator=( const %s& ){}\n", classname, classname );
  fprintf( out, "};\n" );

  fprintf( out, "void initIndDataObjects  ( int nIndividuals, %s * data_for[] );\n", classname );
  fprintf( out, "void releseIndDataObjects( int nIndividuals, %s * data_for[] );\n", classname ); 
  fprintf( out, "#endif\n" );
  return;
}

void emit_initIndDataObjects( 		
                 FILE * out,
  		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
		 const string order_id_pair[]
		 )
{
  assert( out != NULL );

  //=================================================================================
  //
  // Define the section in main() that instantiates and initializes IndData objects
  // for individuals.
  //
  //=================================================================================
  fprintf( out, "#include \"IndData.h\"\n" );
  
  fprintf( out, "void initIndDataObjects( int nIndividuals, %s * data_for[] )\n", classname );
  fprintf( out, "{\n" );
  //
  // Declare an array of #nIndividual number of IndRecords objects.
  //
  //fprintf( out, "   const int nIndividuals = %d;\n", nIndividuals );
  //fprintf( out, "   %s * data_for[nIndividuals+1];\n", classname );
  //fprintf( out, "   \n" );

  //
  // Write the initialization code for IndRecords records for each individual.
  //
  map< NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT >::const_iterator map_records;
  for( int i=0; i<nIndividuals; i++ )
    {
      fprintf( out, "   // %s's data (process order = %d)\n", order_id_pair[i].c_str(), i+1 );
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
          fprintf( out, "   const double %s_%s[] = { ", map_records->first.c_str(), order_id_pair[i].c_str() );
	  for( int j=0; j<map_records->second.size(); j++ )
	    {
	      if( j>0 )
		fprintf( out, ", " );
	      fprintf( out, "%f", map_records->second[j] );
	    }
	  fprintf( out, " };\n" );
	  ++map_records;
	}
      
      map_records = data_for[i].begin();
      fprintf( out, "   %s * data_%s = new %s( %d", classname, order_id_pair[i].c_str(), classname,
	    map_records->second.size() );
      while( map_records != data_for[i].end() )
	{
	  fprintf( out, ", %s_%s", map_records->first.c_str(), order_id_pair[i].c_str() );
	  ++map_records;
	}
      fprintf( out, " );\n" );
      fprintf( out, "   data_for[%d] = data_%s;\n", i, order_id_pair[i].c_str() );
      fprintf( out, "\n" );
   }
  fprintf( out, "}\n" );
}

void emit_releaseIndDataObjects( 		
                 FILE * out,
  		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
		 const string order_id_pair[]
		 )
{
  //=================================================================================
  //
  // Define the section in main() that releases memory allocated for
  // IndData objects.
  //
  //=================================================================================
  fprintf( out, "#include \"IndData.h\"\n" );
  
  fprintf( out, "void releaseIndDataObjects( int nIndividuals, %s * data_for[] )\n", classname );
  fprintf( out, "{\n" );
  //
  // Clean-up code
  //
  fprintf( out, "   // Release memory allocated for %s objects.\n", classname );
  fprintf( out, "   for( int i=0; i<nIndividuals; i++ )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      delete data_for[i];\n" );
  fprintf( out, "   }\n" );

  fprintf( out, "}\n" );
}

