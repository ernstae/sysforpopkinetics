#include "emit_driver.h"
#include "SpkParameters.h"
#include <spk/Objective.h>
#include <cstdlib>

using namespace std;

void emit_driver( FILE * out, 
		  int nIndividuals, 
		  const char SpkModel_name[], 
		  struct SpkParameters & spk )
{
  fprintf( out, "#include <valarray>\n" );
  fprintf( out, "#include <spk/fitPopulation.h>\n" );
  fprintf( out, "#include <spk/popStatistics.h>\n" );
  fprintf( out, "#include <spk/indStatistics.h>\n" );
  fprintf( out, "#include <spk/SpkException.h>\n" );
  //fprintf( out, "#include <spk/%s.h>\n", SpkModel_name );
  fprintf( out, "\n" );

  fprintf( out, "using namespace std;\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "int main( int argc, const char * argv[] )\n" );
  fprintf( out, "{\n" );
  fprintf( out, "   const int nIndividuals = %d;\n", nIndividuals );
  fprintf( out, "   const int nPopPar = %d;\n",      spk.popParIn.size() );
  fprintf( out, "   const int nIndPar = %d;\n",      spk.indParIn.size() / nIndividuals );
  fprintf( out, "\n" );

  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Instantiate a NonmemModel.\n" );
  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   %s model;\n", SpkModel_name );
  fprintf( out, "\n" );

  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Data related initializations\n" );
  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Prepare an array containing ALL individuals' \n" );
  fprintf( out, "   // measurements.\n" );
  fprintf( out, "   double y[] = { " );
  for( int i=0; i<spk.measurementsAll.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.measurementsAll[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   std::valarray<double> measurementsAll( y, %d );\n", 
		  spk.measurementsAll.size() );
  fprintf( out, "\n" );
  
  fprintf( out, "   // Prepere an array containing the number of \n" );
  fprintf( out, "   // measurements for each individual.\n" );
  fprintf( out, "   int N[] = { " );
  for( int i=0; i<nIndividuals; i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%d", spk.nMeasurementsAll[i] );
  }
  fprintf( out, " };\n" );  
  fprintf( out, "   std::valarray<int> nMeasurementsAll( N, nIndividuals );\n" );
  fprintf( out, "\n" );

  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Population level parameters\n" );
  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Select the approximation method.\n" );
  fprintf( out, "   Objective objective = " );
  if( spk.objective == MODIFIED_LAPLACE )
	  fprintf( out, "MODIFIED_LAPLACE;\n" );
  else if( spk.objective == EXPECTED_HESSIAN )
	  fprintf( out, "EXPECTD_HESSIAN;\n" );
  else //( spk.objective == FIRST_ORDER )
	  fprintf( out, "FIRST_ORDER;\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "   // Initialize the parameter vectors.\n" );
  fprintf( out, "   Optimizer popOptimizer( %f, %d, %d );\n", 
		  spk.popEpsilon, spk.popMaxItr, spk.popTrace );
  fprintf( out, "\n" );
  
  fprintf( out, "   // Set the flag indicating as to whether warm start or not.\n" );
  fprintf( out, "   popOptimizer.setupWarmStart( 2 );\n" );
  fprintf( out, "   popOptimizer.setIsWarmStart( %s );\n", 
		  (spk.isPopWarmStart? "true":"false") );  
  fprintf( out, "\n" );
  
  fprintf( out, "   // Set the optimization parameters \n" );
  fprintf( out, "   // [popParIn, popParUp, popParLow, popParStep].\n" );
  fprintf( out, "   double alpIn[] = { " );
  for( int i=0; i<spk.popParIn.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.popParIn[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   valarray<double> popParIn( alpIn, nPopPar );\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "   double alpUp[] = { " );
  for( int i=0; i<spk.popParUp.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.popParUp[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   valarray<double> popParUp( alpUp, nPopPar );\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "   double alpLow[] = { " );
  for( int i=0; i<spk.popParLow.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.popParLow[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   valarray<double> popParLow( alpLow, nPopPar );\n" );
  fprintf( out, "\n" );
		  
  fprintf( out, "   double alpStep[] = { " );
  for( int i=0; i<spk.popParStep.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.popParStep[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   valarray<double> popParStep( alpStep, %d );\n", spk.popParStep.size() );
  fprintf( out, "\n" );

  fprintf( out, "   // Set flags indicating whether or not to compute the values.\n" );
  fprintf( out, "   bool isPopParOut               = %s;\n",               
		  (spk.isPopParOut?                "true":"false") );
  fprintf( out, "   bool isPopObjOut               = %s;\n",               
		  (spk.isPopObjOut?                "true":"false") );
  fprintf( out, "   bool isPopObj_popParOut        = %s;\n",        
		  (spk.isPopObj_popParOut?         "true":"false") );
  fprintf( out, "   bool isPopObj_popPar_popParOut = %s;\n", 
		  (spk.isPopObj_popPar_popParOut?  "true":"false") );
  fprintf( out, "\n" );

  fprintf( out, "   // Set the statistics parameters.\n" );
  fprintf( out, "   enum PopCovForm formulation = " );
  if( spk.popCovarianceForm == RSR ) fprintf( out, "RSR" );
  else if( spk.popCovarianceForm == R ) fprintf( out, "R" );
  else /*if( spk.popCovarianceForm == S ) */ fprintf( out, "S" );
  fprintf( out, ";\n" );
  fprintf( out, "   bool isPopStderrorOut          = %s;\n", 
		  (spk.isPopStderrorOut?           "true":"false") );
  fprintf( out, "   bool isPopCorrelationOut       = %s;\n", 
		  (spk.isPopCorrelationOut?        "true":"false") );
  fprintf( out, "   bool isPopCovarianceOut        = %s;\n", 
		  (spk.isPopCovarianceOut?         "true":"false") );
  fprintf( out, "   bool isPopCoefficientOut       = %s;\n", 
		  (spk.isPopCoefficientOut?        "true":"false") );
  fprintf( out, "   bool isPopConfidenceOut        = %s;\n", 
		  (spk.isPopConfidenceOut?         "true":"false") );
  fprintf( out, "\n" );

  fprintf( out, "   // Set the output value place holders.\n" );
  fprintf( out, "   valarray<double> popParOut              ( nPopPar );\n" );
  fprintf( out, "   double popObjOut;\n" );
  fprintf( out, "   valarray<double> popObj_popParOut       ( nPopPar );\n" );
  fprintf( out, "   valarray<double> popObj_popPar_popParOut( nPopPar * nPopPar );\n" );
  fprintf( out, "   valarray<double> popCovarianceOut       ( nPopPar * nPopPar );\n" );
  fprintf( out, "   valarray<double> popStderrorOut         ( nPopPar );\n" );
  fprintf( out, "   valarray<double> popCorrelationOut      ( nPopPar * nPopPar );\n" );
  fprintf( out, "   valarray<double> popCoefficientOut      ( nPopPar );\n" );
  fprintf( out, "   valarray<double> popConfidenceOut       ( nPopPar * 2 );\n" );
  fprintf( out, "\n" );

  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Individual level parameters\n" );
  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Initialize the parameter vectors \n" );
  fprintf( out, "   // [indParIn, indParUp, indParLow, indParStep].\n" );
  fprintf( out, "   double bIn[] = { " );
  for( int i=0; i<spk.indParIn.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.indParIn[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   std::valarray<double> indParIn( bIn, nIndividuals * nIndPar );\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "   double bUp[] = { " );
  for( int i=0; i<spk.indParUp.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.indParUp[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   std::valarray<double> indParUp( bUp, nIndPar );\n" );
  fprintf( out, "\n" );

  fprintf( out, "   double bLow[] = { " );
  for( int i=0; i<spk.indParLow.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.indParLow[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   std::valarray<double> indParLow( bLow, nIndPar );\n" );
  fprintf( out, "\n" );
  
  fprintf( out, "   double bStep[] = { " );
  for( int i=0; i<spk.indParStep.size(); i++ )
  {
     if( i > 0 )
        fprintf( out, ", " );
     fprintf( out, "%f", spk.indParStep[i] );
  }
  fprintf( out, " };\n" );
  fprintf( out, "   std::valarray<double> indParStep( bStep, %d );\n", 
		  spk.indParStep.size() );
  fprintf( out, "\n" );
  
  fprintf( out, "   Optimizer indOptimizer( %f, %d, %d );\n", 
		  spk.indEpsilon, spk.indMaxItr, spk.indTrace );
  fprintf( out, "   indOptimizer.setupWarmStart( 2 );\n" );
  fprintf( out, "   indOptimizer.setIsWarmStart( %s );\n",   
		  (spk.isIndWarmStart?            "true" : "false" ) );
  fprintf( out, "\n" );

  fprintf( out, "   // Set flags indicating whether or not to compute the values.\n" );
  fprintf( out, "   bool isIndParOut               = %s;\n", 
		  (spk.isIndParOut?               "true":"false") );
  fprintf( out, "   bool isIndObjOut               = %s;\n", 
		  (spk.isIndObjOut?               "true":"false") );
  fprintf( out, "   bool isIndObj_indParOut        = %s;\n", 
		  (spk.isIndObj_indParOut?        "true":"false") );
  fprintf( out, "   bool isIndObj_indPar_indParOut = %s;\n", 
		  (spk.isIndObj_indPar_indParOut? "true":"false") );
  fprintf( out, "\n" );
  
  fprintf( out, "   // Set the statistics parameters.\n" );
  fprintf( out, "   bool isIndStderrorOut          = %s;\n", 
		  (spk.isIndStderrorOut?           "true":"false") );
  fprintf( out, "   bool isIndCorrelationOut       = %s;\n", 
		  (spk.isIndCorrelationOut?        "true":"false") );
  fprintf( out, "   bool isIndCovarianceOut        = %s;\n", 
		  (spk.isIndCovarianceOut?         "true":"false") );
  fprintf( out, "   bool isIndCoefficientOut       = %s;\n", 
		  (spk.isIndCoefficientOut?        "true":"false") );
  fprintf( out, "   bool isIndConfidenceOut        = %s;\n", 
		  (spk.isIndConfidenceOut?         "true":"false") );
  fprintf( out, "\n" );

  fprintf( out, "   // Set the output value place holders.\n" );
  fprintf( out, "   valarray<double> indParOut              ( nIndPar );\n" );
  fprintf( out, "   double indObjOut;\n" );
  fprintf( out, "   valarray<double> indObj_indParOut       ( nIndPar );\n" );
  fprintf( out, "   valarray<double> indObj_indPar_indParOut( nIndPar * nIndPar );\n" );
  fprintf( out, "   valarray<double> indCovarianceOut       ( nIndPar * nIndPar );\n" );
  fprintf( out, "   valarray<double> indStderrorOut         ( nIndPar );\n" );
  fprintf( out, "   valarray<double> indCorrelationOut      ( nIndPar * nIndPar );\n" );
  fprintf( out, "   valarray<double> indCoefficientOut      ( nIndPar );\n" );
  fprintf( out, "   valarray<double> indConfidenceOut       ( nIndPar * 2 );\n" );
  fprintf( out, "\n" );

  fprintf( out, "   //===============================================\n" );
  fprintf( out, "   // Call fitPopulation()\n" );
  fprintf( out, "   //===============================================\n" );

  fprintf( out, "   try{\n" );
  fprintf( out, "      fitPopulation(\n" );
  fprintf( out, "         model,\n" );
  fprintf( out, "         objective,\n" );
  fprintf( out, "         nMeasurementsAll,\n" );
  fprintf( out, "         measurementsAll,\n" );
  fprintf( out, "         popOptimizer,\n" );
  fprintf( out, "         popParLow,\n" );
  fprintf( out, "         popParUp,\n" );
  fprintf( out, "         popParIn,\n" );
  fprintf( out, "         popParStep,\n" );
  fprintf( out, "        %s,\n",   ( spk.isPopParOut?               "&popParOut" : "0" ) );
  fprintf( out, "         indOptimizer,\n" );
  fprintf( out, "         indParLow,\n" );
  fprintf( out, "         indParUp,\n" );
  fprintf( out, "         indParIn,\n" );
  fprintf( out, "         indParStep,\n" );
  fprintf( out, "        %s,\n",   ( spk.isIndParOut?               "&indParOut" : "0" ) );
  fprintf( out, "        %s,\n",   ( spk.isPopObjOut?               "&popObjOut" : "0" ) );
  fprintf( out, "        %s,\n",   ( spk.isPopObj_popParOut?        "&popObj_popParOut" : "0" ) );
  fprintf( out, "        %s );\n", ( spk.isPopObj_popPar_popParOut? "&popObj_popPar_popParOut" : "0" ) );
  fprintf( out, "   }\n" );
  fprintf( out, "   catch( SpkException& e )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      cerr << e << endl;\n" );
  fprintf( out, "      return -1;\n" );
  fprintf( out, "   }\n" );
  fprintf( out, "\n" );
  fprintf( out, "   try{\n" );
  fprintf( out, "      popStatistics(   model,\n" );
  fprintf( out, "                       objective,\n" );
  fprintf( out, "                       nMeasurementsAll,\n" );
  fprintf( out, "                       measurementsAll,\n" );
  fprintf( out, "                       popParOut,\n" );
  fprintf( out, "                       popObj_popPar_popParOut,\n" );
  fprintf( out, "                       indParOut,\n" );
  fprintf( out, "                       indParLow,\n" );
  fprintf( out, "                       indParUp,\n" );
  fprintf( out, "                       indParStep,\n" );
  fprintf( out, "                       formulation,\n" );
  fprintf( out, "                       %s,\n", (spk.isPopCovarianceOut?  "&popCovarianceOut" :"0" ) );
  fprintf( out, "                       %s,\n", (spk.isPopStderrorOut?    "&popStderrorOut"   :"0" ) );
  fprintf( out, "                       %s,\n", (spk.isPopCorrelationOut? "&popCorrelationOut":"0" ) );
  fprintf( out, "                       %s,\n", (spk.isPopCoefficientOut? "&popCoefficientOut":"0" ) );
  fprintf( out, "                       %s);\n",(spk.isPopConfidenceOut?  "&popConfidenceOut" :"0" ) );
  fprintf( out, "   }\n" );
  fprintf( out, "   catch( SpkException & e )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      cerr << e << endl;\n" );
  fprintf( out, "      return -1;\n" );
  fprintf( out, "   }\n" );
  fprintf( out, "\n" );

  fprintf( out, "   model.setPopPar( popParOut );\n" );
  fprintf( out, "   for( int i=0, cnt=0; i<nIndividuals; i++ )\n" );
  fprintf( out, "   {\n" );
  fprintf( out, "      valarray<double> indPar( indParOut[ slice(cnt, N[i], 1) ] );\n" );
  fprintf( out, "      valarray<double> dataMean_indParOut( nIndPar );\n" );
  fprintf( out, "      valarray<double> dataVariance_indParOut( N[i] * N[i] * nIndPar );\n" );
  fprintf( out, "      valarray<double> dataVarianceInvOut( N[i] * N[i] );\n" );
  fprintf( out, "      model.selectIndividual( i );\n" );
  fprintf( out, "      model.setIndPar( indPar );\n" );
  fprintf( out, "      model.dataMean_indPar( dataMean_indParOut );\n" );
  fprintf( out, "      model.dataVariance_indPar( dataVariance_indParOut );\n" );
  fprintf( out, "      model.dataVarianceInv( dataVarianceInvOut );\n" );
  fprintf( out, "      cnt += N[i];\n" );
  fprintf( out, "\n" );
  fprintf( out, "      try{\n" );
  fprintf( out, "         indStatistics( indParOut,\n" );
  fprintf( out, "                        dataMean_indParOut,\n" );
  fprintf( out, "                        dataVariance_indParOut,\n" );
  fprintf( out, "                        dataVarianceInvOut,\n" );
  fprintf( out, "                        %s,\n", (spk.isIndCovarianceOut?  "&indCovarianceOut" :"0" ) );
  fprintf( out, "                        %s,\n", (spk.isIndStderrorOut?    "&indStderrorOut"   :"0" ) );
  fprintf( out, "                        %s,\n", (spk.isIndCorrelationOut? "&indCorrelationOut":"0" ) );
  fprintf( out, "                        %s,\n", (spk.isIndCoefficientOut? "&indCoefficientOut":"0" ) );
  fprintf( out, "                        %s);\n",(spk.isIndConfidenceOut?  "&indConfidenceOut" :"0" ) );
  fprintf( out, "      }\n" );
  fprintf( out, "      catch( SpkException & e )\n" );
  fprintf( out, "      {\n" );
  fprintf( out, "         cerr << e << endl;\n" );
  fprintf( out, "         return -1;\n" );
  fprintf( out, "      }\n" );
  fprintf( out, "   }\n" );



  fprintf( out, "   return 0;\n" );
  fprintf( out, "}\n" );
}
