#include "emit_nonmem_driver.h"
#include "SpkParameters.h"
#include "NonmemTranslator.h"
#include <spk/Objective.h>
#include <cstdlib>

using namespace std;

// Header includes
static void insert_common_headers        ( FILE *, char indent[], const char model_name[] );
static void insert_simulate_headers      ( FILE *, char indent[], char simulate_h[] );
static void insert_pop_estimate_headers  ( FILE *, char indent[], char fitPopulation_h[] );
static void insert_pop_statistics_headers( FILE *, char indent[], char popStatistics_h[] );
static void insert_ind_estimate_headers  ( FILE *, char indent[], char fitIndividual_h[] );
static void insert_ind_statistics_headers( FILE *, char indent[], char indStatistics_h[] );

// Simulation
static void insert_simulation_only(
   FILE *             out,
   const char         modelClass_name[],
   const char         modelObject_init_block[],
   int                nIndividuals,
   int                nPopPar,
   int                nIndPar,
   valarray<double> & popParIn,
   valarray<int>    & nMeasurements,
   valarray<double> & indParLow,
   valarray<double> & indParUp,
   int seed
   );

static void insert_pop_statistics_init(
   FILE * out,
   char indent[],
   enum PopCovForm popCovarianceForm,
   bool isPopStderrorOut,
   bool isPopCorrelationOut,
   bool isPopCovarianceOut,
   bool isPopCoefficientOut,
   bool isPopConfidenceOut
   );

static void insert_ind_statistics_init(
   FILE * out,
   char indent[],
   bool isIndStderrorOut,
   bool isIndCorrelationOut,
   bool isIndCovarianceOut,
   bool isIndCoefficientOut,
   bool isIndConfidenceOut
   );
static void insert_pop_statistics_exec(
   FILE * out,
   char indent[],
   bool isPopStderrorOut,
   bool isPopCorrelationOut,
   bool isPopCovarianceOut,
   bool isPopCoefficientOut,
   bool isPopConfidenceOut
   );
static void insert_ind_statistics_exec(
   FILE * out,
   char indent[],
   bool isIndStderrorOut,
   bool isIndCorrelationOut,
   bool isIndCovarianceOut,
   bool isIndCoefficientOut,
   bool isIndConfidenceOut
   );
static void insert_pop_simulation_exec(
   FILE *             out,
   char               indent[],
   int                seed );

// Estimatation
static void insert_ind_estimate_init( FILE * out,
				      char               indent[],
				      bool               isSimulation,
				      int                seed,
				      int                nIndividuals,
				      valarray<int>    & nMeasurements,
				      valarray<double> & measurementsAll,
				      int                nIndPar,
				      valarray<double> & indParIn,
				      valarray<double> & indParLow,
				      valarray<double> & indParUp,
				      valarray<double> & indParStep,
				      double             indEpsilon,
				      int                indMaxItr,
				      int                indTrace,
				      bool               isIndWarmStart,
				      bool               isIndParOut,
				      bool               isIndObjOut,
				      bool               isIndObj_indParOut,
				      bool               isIndObj_indPar_indParOut );
  
static void insert_pop_estimate_init( FILE             * out,
				      char               indent[],
				      bool               isSimulation,
				      int                nIndividuals,
				      Objective          objective,
				      int                nPopPar,
				      valarray<double> & popParIn,
				      valarray<double> & popParLow,
				      valarray<double> & popParUp,
				      valarray<double> & popParStep,
				      double             popEpsilon,
				      int                popMaxItr,
				      int                popTrace,
				      bool               isPopWarmStart,
				      bool               isPopParOut,
				      bool               isPopObjOut,
				      bool               isPopObj_popParOut,
				      bool               isPopObj_popPar_popParOut );
static void insert_pop_estimate_exec( FILE * out,
				      char indent[],
				      bool isIndParOut,
				      bool isPopParOut,
				      bool isPopObjOut,
				      bool isPopObj_popParOut,
				      bool isPopObj_popPar_popParOut
				      );
// Statistics

// Clean up

void emit_nonmem_driver( 
		  FILE * out, 
		  int nIndividuals, 
		  const char modelClass_name[],
		  const char modelObject_init_block[],
		  struct SpkParameters & spkInfo,
	          struct NonmemParameters & nonmemInfo
		)
{
  const int nPopPar = spkInfo.popParIn.size();
  const int nIndPar = spkInfo.indParLow.size();

  /*
  char modelObject_name[ strlen( model_construction_str) + 1 ];
  unsigned int len = strstr( model_construction_str, "(" ) - model_construction_str;
  strncpy( modelObject_name, model_construction_str, len );
  modelObject_name[len] = '\0';
  */
  //=================================================
  // Data simulation ONLY case
  //=================================================
  if( spkInfo.isSimulation && !spkInfo.isEstimation )
    {
      insert_simulation_only( out,
			      modelClass_name,
			      modelObject_init_block,
			      nIndividuals,
			      nPopPar,
			      nIndPar,
			      spkInfo.popParIn,
			      spkInfo.nMeasurements,
			      spkInfo.indParLow,
			      spkInfo.indParUp,
			      spkInfo.seed );
      return;
    }

  //=================================================
  //  header include statements
  //=================================================
  insert_common_headers( out, "", modelClass_name );
  bool isPopStatistics = ( spkInfo.isPopStderrorOut 
		       | spkInfo.isPopCorrelationOut 
		       | spkInfo.isPopCovarianceOut 
		       | spkInfo.isPopCoefficientOut 
		       | spkInfo.isPopConfidenceOut );
  bool isIndStatistics = ( spkInfo.isIndStderrorOut 
		       | spkInfo.isIndCorrelationOut 
		       | spkInfo.isIndCovarianceOut 
		       | spkInfo.isIndCoefficientOut 
		       | spkInfo.isIndConfidenceOut );

  if( isPopStatistics )
    {
      insert_pop_statistics_headers( out, "", "popStatistics.h" );
    }
  if( isIndStatistics )
    {
      insert_ind_statistics_headers( out, "", "indStatistics.h" );
    }
  if( spkInfo.isSimulation )
    {
      insert_simulate_headers( out, "", "simulate.h" );
    }
  if( spkInfo.analysis == SpkParameters::POPULATION )
    {
      if( spkInfo.isEstimation )
	{
	  insert_pop_estimate_headers( out, "", "fitPopulation.h" );
	}
    }
  else
    {
      if( spkInfo.isEstimation )
	{
	  insert_ind_estimate_headers( out, "", "fitIndividual.h" );
	}
    }
  //=================================================
  // int main()
  //=================================================
  fprintf( out, "\n" );
  fprintf( out, "using namespace std;\n" );
  fprintf( out, "int main( int argc, const char* argv[] )\n" );
  fprintf( out, "{\n" );

  //=================================================
  // Prelimineraries
  //=================================================
  if( spkInfo.analysis == SpkParameters::POPULATION )
    {
      fprintf( out, "\tconst int nIndividuals = %d;\n", nIndividuals );
      fprintf( out, "\tconst int nPopPar      = %d;\n", nPopPar );
    }
  fprintf( out, "\tconst int nIndPar      = %d;\n", nIndPar );

  //=================================================
  // Individual para estimate initializations
  //=================================================
  insert_ind_estimate_init( out,
			    "\t",
			    spkInfo.isSimulation,
			    spkInfo.seed,
			    nIndividuals,
			    spkInfo.nMeasurements,
			    spkInfo.measurementsAll,
			    nIndPar,
			    spkInfo.indParIn,
			    spkInfo.indParLow,
			    spkInfo.indParUp,
			    spkInfo.indParStep,
			    spkInfo.indEpsilon,
			    spkInfo.indMaxItr,
			    spkInfo.indTrace,
			    spkInfo.isIndWarmStart,
			    spkInfo.isIndParOut,
			    spkInfo.isIndObjOut,
			    spkInfo.isIndObj_indParOut,
			    spkInfo.isIndObj_indPar_indParOut );
  
  //=================================================
  // Individual para statistics initializations
  //=================================================
  if( isIndStatistics )
    {
      insert_ind_statistics_init( out,
				  "\t",
				  spkInfo.isIndStderrorOut,
				  spkInfo.isIndCorrelationOut,
				  spkInfo.isIndCovarianceOut,
				  spkInfo.isIndCoefficientOut,
				  spkInfo.isIndConfidenceOut );
    }

  //=================================================
  // Population para estimate initializations
  //=================================================
  insert_pop_estimate_init( out,
			    "\t",
			    spkInfo.isSimulation,
			    nIndividuals,
			    spkInfo.objective,
			    nPopPar,
			    spkInfo.popParIn,
			    spkInfo.popParLow,
			    spkInfo.popParUp,
			    spkInfo.popParStep,
			    spkInfo.popEpsilon,
			    spkInfo.popMaxItr,
			    spkInfo.popTrace,
			    spkInfo.isPopWarmStart,
			    spkInfo.isPopParOut,
			    spkInfo.isPopObjOut,
			    spkInfo.isPopObj_popParOut,
			    spkInfo.isPopObj_popPar_popParOut );

  //=================================================
  // Population para statistics initializations
  //=================================================
  if( isPopStatistics )
    {
      insert_pop_statistics_init( out,
				  "\t",
				  spkInfo.popCovarianceForm,
				  spkInfo.isPopStderrorOut,
				  spkInfo.isPopCorrelationOut,
				  spkInfo.isPopCovarianceOut,
				  spkInfo.isPopCoefficientOut,
				  spkInfo.isPopConfidenceOut );
    }
			      
  //=================================================
  // Instanciate a model
  //=================================================
   fprintf( out, "%s\n", modelObject_init_block );

  //=================================================
  // Data simulation
  //=================================================
   if( spkInfo.isSimulation )
    {
      insert_pop_simulation_exec( out, "\t", spkInfo.seed );
    }
  //=================================================
  // Parameter estimation
  //=================================================
  insert_pop_estimate_exec( out, 
			    "\t",
			    spkInfo.isIndParOut,
			    spkInfo.isPopParOut,
			    spkInfo.isPopObjOut,
			    spkInfo.isPopObj_popParOut,
			    spkInfo.isPopObj_popPar_popParOut );
  
  //=================================================
  // Statistics
  //=================================================
  if( isPopStatistics )
    {
      insert_pop_statistics_exec( out,
				  "\t",
				  spkInfo.isPopStderrorOut,
				  spkInfo.isPopCorrelationOut,
				  spkInfo.isPopCovarianceOut,
				  spkInfo.isPopCoefficientOut,
				  spkInfo.isPopConfidenceOut );
    }

    if( isIndStatistics )
    {
      insert_ind_statistics_exec( out,
				  "\t",
				  spkInfo.isIndStderrorOut,
				  spkInfo.isIndCorrelationOut,
				  spkInfo.isIndCovarianceOut,
				  spkInfo.isIndCoefficientOut,
				  spkInfo.isIndConfidenceOut );
    }

  //=================================================
  //  return and close
  //=================================================
  fprintf( out, "\treturn 0;\n" );
  fprintf( out, "}\n" );

  return;
}

void insert_common_headers(
   FILE * out,
   char indent[],
   const char model_name[]
)
{
  fprintf( out, "%s#include <valarray>\n", indent );
  fprintf( out, "%s#include <spk/SpkException.h>\n", indent );
  //fprintf( out, "%s#include <spk/%s.h>\n", model_name );
}
void insert_simulate_headers( 
   FILE * out, 
   char indent[],
   char simulate_h[] 
)
{
  fprintf( out, "%s#include <spk/%s>\n", indent, simulate_h );
}
void insert_pop_estimate_headers( 
   FILE * out, 
   char indent[],
   char fitPopulation_h[] 
)
{
  fprintf( out, "%s#include <spk/%s>\n", indent, fitPopulation_h );
}
void insert_pop_statistics_headers( 
   FILE * out, 
   char indent[],
   char popStatistics_h[] 
)
{
  fprintf( out, "%s#include <spk/%s>\n", indent, popStatistics_h );
}

void insert_ind_simulate_headers( 
   FILE * out, 
   char indent[],
   char indSimulate_h[] )
{
  fprintf( out, "%s#include <spk/%s>\n", indent, indSimulate_h );
}
void insert_ind_estimate_headers( 
   FILE * out, 
   char indent[],
   char fitIndividual_h[] )
{
  fprintf( out, "%s#include <spk/%s>\n", indent, fitIndividual_h );
}
void insert_ind_statistics_headers( 
   FILE * out,
   char indent[],
   char indStatistics_h[] )
{
  fprintf( out, "%s#include <spk/%s>\n", indent, indStatistics_h );
}

void insert_simulation_only(
   FILE *             out,
   const char         modelClass_name[],
   const char         modelObject_init_block[],
   int                nIndividuals,
   int                nPopPar,
   int                nIndPar,
   valarray<double> & popParIn,
   valarray<int>    & nMeasurements,
   valarray<double> & indParLow,
   valarray<double> & indParUp,
   int seed
)
{
  fprintf( out, "#include <valarray>\n" );
  fprintf( out, "#include <spk/simulate.h>\n" );
  //fprintf( out, "#include <spk/%s.h>\n", modelClass_name );
  fprintf( out, "\n" );
  fprintf( out, "using namespace std;\n" );
  fprintf( out, "int main( int argc, const char argv[] )\n" );
  fprintf( out, "{\n" );
  fprintf( out, "\tconst int nIndividuals = %d;\n", nIndividuals );
  fprintf( out, "\tconst int nPopPar      = %d;\n", nPopPar );
  fprintf( out, "\tconst int nIndPar      = %d;\n", nIndPar );

  fprintf( out, "\tdouble alpIn[] = {" );
  for( int i=0; i<nPopPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", popParIn[i] );
    }
  fprintf( out, "};\n" );

  fprintf( out, "\tdouble bLow [] = {" );
  for( int i=0; i<nIndPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", indParLow[i] );
    }
  fprintf( out, "};\n" );

  fprintf( out, "\tdouble bUp  [] = {" );
  for( int i=0; i<nIndPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", indParUp[i] );
    }
  fprintf( out, "};\n" );

  fprintf( out, "\tint N    []    = {" );
  for( int i=0; i<nIndividuals; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%d", nMeasurements[i] );
    }
  fprintf( out, "};\n" );

  // actual input to simulate() and all others
  fprintf( out, "\tvalarray<double> popParIn ( alpIn, nPopPar );\n" );
  fprintf( out, "\tvalarray<double> indParLow( bLow, nIndPar  );\n" );
  fprintf( out, "\tvalarray<double> indParUp ( bUp, nIndPar );\n" );
  fprintf( out, "\tvalarray<int>    nMeasurements( N, nIndividuals );\n" );

  // simulate() output place holders
  fprintf( out, "\tvalarray<double> measurementsAll( nMeasurements.sum() );\n" );
  fprintf( out, "\tvalarray<double> indParIn( nIndPar * nIndividuals );\n" );

  fprintf( out, "\t%s\n", modelObject_init_block );

  insert_pop_simulation_exec( out, "\t", seed );

  fprintf( out, "\treturn 0;\n"                  );
  fprintf( out, "}\n"                            );

}
 void insert_ind_estimate_init( FILE * out,
				char               indent[],
				bool               isSimulation,
				int                seed,
				int                nIndividuals,
				valarray<int>    & nMeasurements,
				valarray<double> & measurementsAll,
				int                nIndPar,
				valarray<double> & indParIn,
				valarray<double> & indParLow,
				valarray<double> & indParUp,
				valarray<double> & indParStep,
				double             indEpsilon,
				int                indMaxItr,
				int                indTrace,
				bool               isIndWarmStart,
				bool               isIndParOut,
				bool               isIndObjOut,
				bool               isIndObj_indParOut,
				bool               isIndObj_indPar_indParOut )
{
  fprintf( out, "%sconst double indEpsilon                   = %f;\n", 
	   indent, indEpsilon );
  fprintf( out, "%sconst int    indMaxItr                    = %d;\n", 
	   indent, indMaxItr );
  fprintf( out, "%sconst int    indTrace                     = %d;\n",
	   indent, indTrace );
  fprintf( out, "%sconst bool   isIndWarmStart               = %s;\n", 
	   indent, (isIndWarmStart? "true" : "false") );

  fprintf( out, "%sOptimizer indOptimizer( indEpsilon, indMaxItr, indTrace );\n", indent );
  fprintf( out, "\n" );
  
  fprintf( out, "%s// Set the flag indicating as to whether warm start or not.\n", indent );
  fprintf( out, "%sindOptimizer.setupWarmStart( nIndPar );\n", indent );
  fprintf( out, "%sindOptimizer.setIsWarmStart( %s );\n", indent,
	   (isIndWarmStart? "true":"false") );  
  fprintf( out, "\n" );

  if( isSimulation )
    {
      fprintf( out, "%sint N[] = {", indent );
      for( int i=0; i<nIndividuals; i++ )
	{
	  if( i > 0 )
	    fprintf( out, ", " );
	  fprintf( out, "%d", nMeasurements[i] );
	}
      fprintf( out, "};\n" );
      fprintf( out, "%svalarray<int>    nMeasurements( N, nIndividuals );\n", 
	       indent );
      fprintf( out, "%svalarray<double> measurementsAll( nMeasurements.sum() );\n", 
	       indent );
      fprintf( out, "%svalarray<double> indParIn( nIndPar * nIndividuals );\n" );
    }
  else
    {
      fprintf( out, "%s// Initialize the parameter vectors.\n", indent );
      fprintf( out, "%sdouble bIn[]   = {", indent );
      for( int i=0; i<nIndPar*nIndividuals; i++ )
	{
	  if( i > 0 )
	    fprintf( out, ", " );
	  fprintf( out, "%f", indParIn[i] );
	}
      fprintf( out, "};\n" );
      fprintf( out, "%svalarray<double> indParIn( bIn, nIndPar*nIndividuals );\n", indent );

      fprintf( out, "%sint N[] = {", indent );
      for( int i=0; i<nIndividuals; i++ )
	{
	  if( i > 0 )
	    fprintf( out, ", " );
	  fprintf( out, "%d", nMeasurements[i] );
	}
      fprintf( out, "};\n" );
      fprintf( out, "%svalarray<int>    nMeasurements( N, nIndividuals );\n", 
	       indent );
      
      int totalY = nMeasurements.sum();
      fprintf( out, "%sdouble y[] = {", indent );
      for( int i=0; i<totalY; i++ )
	{
	  if( i > 0 )
	    fprintf( out, ", " );
	  fprintf( out, "%f", measurementsAll[i] );
	}
      fprintf( out, "};\n" );
      fprintf( out, "%svalarray<double> measurementsAll( y, nMeasurements.sum() );\n",
	       indent );
    }

  fprintf( out, "%sdouble bUp[]   = {", indent );
  for( int i=0; i<nIndPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", indParUp[i] );
    }
  fprintf( out, "};\n" );
  fprintf( out, "%svalarray<double> indParUp( bUp, nIndPar );\n", indent );

  fprintf( out, "%sdouble bLow[]  = {", indent );
  for( int i=0; i<nIndPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", indParLow[i] );
    }
  fprintf( out, "};\n" );
  fprintf( out, "%svalarray<double> indParLow( bLow, nIndPar );\n", indent );

  fprintf( out, "%sdouble bStep[] = {", indent );
  for( int i=0; i<nIndPar; i++ )
    {
      if( i > 0 )
	fprintf( out, ", " );
      fprintf( out, "%f", indParStep[i] );
    }
  fprintf( out, "};\n" );
  fprintf( out, "%svalarray<double> indParStep( bStep, nIndPar );\n", indent );
  
  fprintf( out, "%sbool isIndParOut = %s;\n", 
	   indent, (isIndParOut? "true":"false") );
  fprintf( out, "%sbool isIndObjOut = %s;\n", 
	   indent, (isIndObjOut? "true":"false") );
  fprintf( out, "%sbool isIndObj_indParOut = %s;\n", 
	   indent, (isIndObj_indParOut? "true":"false") );
  fprintf( out, "%sbool isIndObj_indPar_indParOut = %s;\n", 
	   indent, (isIndObj_indPar_indParOut? "true":"false") );

  if( isIndParOut )
    {
      fprintf( out, "%svalarray<double> indParOut( nIndPar * nIndividuals );\n", 
	       indent );
    }
  if( isIndObjOut )
    {
      fprintf( out, "%sdouble           indIndObjOut;\n", 
	       indent );
    }
  if( isIndObj_indParOut )
    {
      fprintf( out, "%svalarray<double> indIndObj_indParOut( nIndPar );\n", 
	       indent );
    }
  if( isIndObj_indPar_indParOut )
    {
      fprintf( out, "%svalarray<double> indIndObj_indPar_indParOut( nIndPar * nIndPar );\n", 
	       indent );
    }
}
void insert_pop_estimate_init( FILE             * out,
			       char               indent[],
			       bool               isSimulation,
			       int                nIndividuals,
			       Objective          objective,
			       int                nPopPar,
			       valarray<double> & popParIn,
			       valarray<double> & popParLow,
			       valarray<double> & popParUp,
			       valarray<double> & popParStep,
			       double             popEpsilon,
			       int                popMaxItr,
			       int                popTrace,
			       bool               isPopWarmStart,
			       bool               isPopParOut,
			       bool               isPopObjOut,
			       bool               isPopObj_popParOut,
			       bool               isPopObj_popPar_popParOut
			       )
{
  fprintf( out, "%s// Select the approximation method.\n", indent );
  fprintf( out, "%sObjective objective = " );
  if( objective == MODIFIED_LAPLACE )
	  fprintf( out, "MODIFIED_LAPLACE;\n" );
  else if( objective == EXPECTED_HESSIAN )
	  fprintf( out, "EXPECTD_HESSIAN;\n" );
  else //( objective == FIRST_ORDER )
	  fprintf( out, "FIRST_ORDER;\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "%s// Initialize the parameter vectors.\n", indent );
  fprintf( out, "%sOptimizer popOptimizer( %f, %d, %d );\n", indent,
		  popEpsilon, popMaxItr, popTrace );
  fprintf( out, "\n" );
  
  fprintf( out, "%s// Set the flag indicating as to whether warm start or not.\n", indent );
  fprintf( out, "%spopOptimizer.setupWarmStart( nPopPar );\n", indent );
  fprintf( out, "%spopOptimizer.setIsWarmStart( %s );\n", indent,
		  (isPopWarmStart? "true":"false") );  
  fprintf( out, "\n" );
  
  fprintf( out, "%s// Set the optimization parameters \n", indent );
  fprintf( out, "%s// [popParIn, popParUp, popParLow, popParStep].\n", indent );
  fprintf( out, "%sdouble alpIn[] = { " );
  for( int i=0; i<nPopPar; i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", popParIn[i] );
  }
  fprintf( out, " };\n", indent );
  fprintf( out, "%svalarray<double> popParIn( alpIn, nPopPar );\n", indent );
  fprintf( out, "\n" );
  
  fprintf( out, "%sdouble alpUp[] = { " );
  for( int i=0; i<nPopPar; i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", popParUp[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "%svalarray<double> popParUp( alpUp, nPopPar );\n", indent );
  fprintf( out, "\n" );
  
  fprintf( out, "%sdouble alpLow[] = { " );
  for( int i=0; i<nPopPar; i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", popParLow[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "%svalarray<double> popParLow( alpLow, nPopPar );\n", indent );
  fprintf( out, "\n" );
		  
  fprintf( out, "%sdouble alpStep[] = { " );
  for( int i=0; i<nPopPar; i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", popParStep[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "%svalarray<double> popParStep( alpStep, %d );\n", indent,
	   nPopPar );
  fprintf( out, "\n" );

  fprintf( out, "%s// Set flags indicating whether or not to compute the values.\n", indent );
  fprintf( out, "%sbool isPopParOut               = %s;\n", indent,              
		  (isPopParOut?                "true":"false") );
  fprintf( out, "%sbool isPopObjOut               = %s;\n", indent,            
		  (isPopObjOut?                "true":"false") );
  fprintf( out, "%sbool isPopObj_popParOut        = %s;\n", indent,       
		  (isPopObj_popParOut?         "true":"false") );
  fprintf( out, "%sbool isPopObj_popPar_popParOut = %s;\n", indent,
		  (isPopObj_popPar_popParOut?  "true":"false") );
  fprintf( out, "\n" );

  if( isPopParOut )
    {
      fprintf( out, "%svalarray<double> popParOut( nPopPar );\n", indent );
    }
  if( isPopObjOut )
    {
      fprintf( out, "%sdouble popObjOut;\n", indent );
    }
  if( isPopObj_popParOut )
    {
      fprintf( out, "%svalarray<double> popObj_popParOut( nPopPar );\n", indent );
    }
  if( isPopObj_popPar_popParOut )
    {
      fprintf( out, "%svalarray<double> popObj_popPar_popParOut( nPopPar * nPopPar );\n", indent );
    }

}
void insert_pop_simulation_exec(
   FILE *             out,
   char               indent[],
   int                seed )
{
      fprintf( out, "\tint seed = %d;\n", seed );
      
      fprintf( out, "\ttry{\n"                       );
      fprintf( out, "\t   simulate(\n"               );
      fprintf( out, "\t      model,\n"               );
      fprintf( out, "\t      popParIn,\n"            );
      fprintf( out, "\t      nMeasurements,\n"    );
      fprintf( out, "\t      indParLow,\n"           );
      fprintf( out, "\t      indParUp,\n"            );
      fprintf( out, "\t      measurementsAll,\n"     );
      fprintf( out, "\t      indParIn,\n"            );
      fprintf( out, "\t      seed );\n"              );
      fprintf( out, "\t}\n"                          );
      fprintf( out, "\tcatch( SpkException & e ){\n" );
      fprintf( out, "\t   cerr << e << endl;\n"      );
      fprintf( out, "\t   return -1;\n"              );
      fprintf( out, "\t}\n"                          );
      fprintf( out, "\tcatch( ... ){\n"              );
      fprintf( out, "\t   cerr << \"!!!Unknown error occured during simulation!!!\" << endl;\n" );
      fprintf( out, "\t   return -1;\n"              );
      fprintf( out, "\t}\n"                          );
}   
void insert_pop_estimate_exec( FILE * out,
			       char indent[],
			       bool isIndParOut,
			       bool isPopParOut,
			       bool isPopObjOut,
			       bool isPopObj_popParOut,
			       bool isPopObj_popPar_popParOut
			       )
{
  fprintf( out, "%stry{\n",                     indent );
  fprintf( out, "%s   fitPopulation(\n",        indent );
  fprintf( out, "%s   model,\n",                indent );
  fprintf( out, "%s   objective,\n",            indent );
  fprintf( out, "%s   nMeasurements,\n",     indent );
  fprintf( out, "%s   measurementsAll,\n",      indent );
  fprintf( out, "%s   popOptimizer,\n",         indent );
  fprintf( out, "%s   popParLow,\n",            indent );
  fprintf( out, "%s   popParUp,\n",             indent );
  fprintf( out, "%s   popParIn,\n",             indent );
  fprintf( out, "%s   popParStep,\n",           indent );
  fprintf( out, "%s   %s,\n",                   indent, 
	   ( isPopParOut?              "&popParOut" : "0" ) );
  fprintf( out, "%s   indOptimizer,\n",         indent );
  fprintf( out, "%s   indParLow,\n",            indent );
  fprintf( out, "%s   indParUp,\n",             indent );
  fprintf( out, "%s   indParIn,\n",             indent );
  fprintf( out, "%s   indParStep,\n",           indent );
  fprintf( out, "%s   %s,\n",                   indent,
	   ( isIndParOut?               "&indParOut" : "0" ) );
  fprintf( out, "%s   %s,\n",                   indent,
	   ( isPopObjOut?               "&popObjOut" : "0" ) );
  fprintf( out, "%s   %s,\n",                   indent,
	   ( isPopObj_popParOut?        "&popObj_popParOut" : "0" ) );
  fprintf( out, "%s   %s );\n",                 indent,
	   ( isPopObj_popPar_popParOut? "&popObj_popPar_popParOut" : "0" ) );
  fprintf( out, "%s}\n",                        indent );
  fprintf( out, "%scatch( SpkException& e )\n", indent );
  fprintf( out, "%s{\n",                        indent );
  fprintf( out, "%s   cerr << e << endl;\n",    indent );
  fprintf( out, "%s   return -1;\n",            indent );
  fprintf( out, "%s}\n",                        indent );
}

void insert_pop_statistics_init(
   FILE * out,
   char indent[],
   enum PopCovForm popCovarianceForm,
   bool isPopStderrorOut,
   bool isPopCorrelationOut,
   bool isPopCovarianceOut,
   bool isPopCoefficientOut,
   bool isPopConfidenceOut
)
{
  fprintf( out, "// Set the statistics parameters.\n" );
  fprintf( out, "enum PopCovForm covariance_form = " );
  if( popCovarianceForm == RSR ) fprintf( out, "RSR" );
  else if( popCovarianceForm == R ) fprintf( out, "R" );
  else fprintf( out, "S" );
  fprintf( out, ";\n" );
  fprintf( out, "bool isPopStderrorOut          = %s;\n", 
		  (isPopStderrorOut?           "true":"false") );
  fprintf( out, "bool isPopCorrelationOut       = %s;\n", 
		  (isPopCorrelationOut?        "true":"false") );
  fprintf( out, "bool isPopCovarianceOut        = %s;\n", 
		  (isPopCovarianceOut?         "true":"false") );
  fprintf( out, "bool isPopCoefficientOut       = %s;\n", 
		  (isPopCoefficientOut?        "true":"false") );
  fprintf( out, "bool isPopConfidenceOut        = %s;\n", 
		  (isPopConfidenceOut?         "true":"false") );
  fprintf( out, "\n" );

  fprintf( out, "valarray<double> popStderrorOut( nPopPar );\n" );
  fprintf( out, "valarray<double> popCorrelationOut( nPopPar * nPopPar );\n" );
  fprintf( out, "valarray<double> popCovarianceOut( nPopPar * nPopPar );\n" );
  fprintf( out, "valarray<double> popCoefficientOut( nPopPar );\n" );
  fprintf( out, "valarray<double> popConfidenceOut( nPopPar * 2 );\n" );
}
void insert_ind_statistics_init(
   FILE * out,
   char indent[],
   bool isIndStderrorOut,
   bool isIndCorrelationOut,
   bool isIndCovarianceOut,
   bool isIndCoefficientOut,
   bool isIndConfidenceOut
)
{
  fprintf( out, "   // Set the statistics parameters.\n" );
  fprintf( out, "   bool isIndStderrorOut          = %s;\n", 
		  (isIndStderrorOut?           "true":"false") );
  fprintf( out, "   bool isIndCorrelationOut       = %s;\n", 
		  (isIndCorrelationOut?        "true":"false") );
  fprintf( out, "   bool isIndCovarianceOut        = %s;\n", 
		  (isIndCovarianceOut?         "true":"false") );
  fprintf( out, "   bool isIndCoefficientOut       = %s;\n", 
		  (isIndCoefficientOut?        "true":"false") );
  fprintf( out, "   bool isIndConfidenceOut        = %s;\n", 
		  (isIndConfidenceOut?         "true":"false") );
  fprintf( out, "\n" );
  fprintf( out, "valarray<double> indStderrorOut( nIndPar );\n" );
  fprintf( out, "valarray<double> indCorrelationOut( nIndPar * nIndPar );\n" );
  fprintf( out, "valarray<double> indCovarianceOut( nIndPar * nIndPar );\n" );
  fprintf( out, "valarray<double> indCoefficientOut( nIndPar );\n" );
  fprintf( out, "valarray<double> indConfidenceOut( nIndPar * 2 );\n" );

}
void insert_pop_statistics_exec(
   FILE * out,
   char indent[],
   bool isPopStderrorOut,
   bool isPopCorrelationOut,
   bool isPopCovarianceOut,
   bool isPopCoefficientOut,
   bool isPopConfidenceOut
)
{
  fprintf( out, "   try{\n" );
  fprintf( out, "      popStatistics(   model,\n" );
  fprintf( out, "                       objective,\n" );
  fprintf( out, "                       nMeasurements,\n" );
  fprintf( out, "                       measurementsAll,\n" );
  fprintf( out, "                       popParOut,\n" );
  fprintf( out, "                       popObj_popPar_popParOut,\n" );
  fprintf( out, "                       indParOut,\n" );
  fprintf( out, "                       indParLow,\n" );
  fprintf( out, "                       indParUp,\n" );
  fprintf( out, "                       indParStep,\n" );
  fprintf( out, "                       covariance_form,\n" );
  fprintf( out, "                       %s,\n", (isPopCovarianceOut?  "&popCovarianceOut" :"0" ) );
  fprintf( out, "                       %s,\n", (isPopStderrorOut?    "&popStderrorOut"   :"0" ) );
  fprintf( out, "                       %s,\n", (isPopCorrelationOut? "&popCorrelationOut":"0" ) );
  fprintf( out, "                       %s,\n", (isPopCoefficientOut? "&popCoefficientOut":"0" ) );
  fprintf( out, "                       %s);\n",(isPopConfidenceOut?  "&popConfidenceOut" :"0" ) );
  fprintf( out, "   }\n" );
  fprintf( out, "   catch( SpkException & e )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      cerr << e << endl;\n" );
  fprintf( out, "      return -1;\n" );
  fprintf( out, "   }\n" );
  fprintf( out, "\n" );
}

void insert_ind_statistics_exec(
   FILE * out,
   char indent[],
   bool isIndStderrorOut,
   bool isIndCorrelationOut,
   bool isIndCovarianceOut,
   bool isIndCoefficientOut,
   bool isIndConfidenceOut
)
{
  fprintf( out, "   model.setPopPar( popParOut );\n" );
  fprintf( out, "   for( int i=0; i<nIndividuals; i++ )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      valarray<double> indPar = indParOut[ slice(nIndPar*i, nIndPar, 1) ];\n" );
  fprintf( out, "      valarray<double> dataMean_indParOut( N[i] * nIndPar );\n" );
  fprintf( out, "      valarray<double> dataVariance_indParOut( N[i] * N[i] * nIndPar );\n" );
  fprintf( out, "      valarray<double> dataVarianceInvOut( N[i] * N[i] );\n" );
  fprintf( out, "      model.selectIndividual( i );\n" );
  fprintf( out, "      model.setIndPar( indPar );\n" );
  fprintf( out, "      model.dataMean_indPar( dataMean_indParOut );\n" );
  fprintf( out, "      model.dataVariance_indPar( dataVariance_indParOut );\n" );
  fprintf( out, "      model.dataVarianceInv( dataVarianceInvOut );\n" );
  fprintf( out, "\n" );
  fprintf( out, "      try{\n" );
  fprintf( out, "         indStatistics( indPar,\n" );
  fprintf( out, "                        dataMean_indParOut,\n" );
  fprintf( out, "                        dataVariance_indParOut,\n" );
  fprintf( out, "                        dataVarianceInvOut,\n" );
  fprintf( out, "                        %s,\n", (isIndCovarianceOut?  "&indCovarianceOut" :"0" ) );
  fprintf( out, "                        %s,\n", (isIndStderrorOut?    "&indStderrorOut"   :"0" ) );
  fprintf( out, "                        %s,\n", (isIndCorrelationOut? "&indCorrelationOut":"0" ) );
  fprintf( out, "                        %s,\n", (isIndCoefficientOut? "&indCoefficientOut":"0" ) );
  fprintf( out, "                        %s);\n",(isIndConfidenceOut?  "&indConfidenceOut" :"0" ) );
  fprintf( out, "      }\n" );
  fprintf( out, "      catch( SpkException & e )\n" );
  fprintf( out, "      {\n" );
  fprintf( out, "         cerr << e << endl;\n" );
  fprintf( out, "         return -1;\n" );
  fprintf( out, "      }\n" );
  fprintf( out, "   }\n" );
}
