/**
 * @file NM_generateIdentDriver.cpp
 * Define NonmemTranslator::generateIdentDriver().
 *//*
 * Author: Mitch Watrous
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;

void NonmemTranslator::generateIdentDriver( ) const
{
  //---------------------------------------------------------------------------------------
  // Generate the SPK driver
  //---------------------------------------------------------------------------------------
  ofstream oIdentDriver ( fFitDriver_cpp );
  if( !oIdentDriver.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
                SpkCompilerError::maxMessageLen(),
                "Failed to create %s file.", fFitDriver_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  oIdentDriver << "// " << myDescription << endl;
  oIdentDriver << "#include <iostream>" << endl;
  oIdentDriver << "#include <fstream>" << endl;
  oIdentDriver << "#include <sys/time.h>" << endl;
  oIdentDriver << "#include <vector>" << endl;
  oIdentDriver << endl;
  oIdentDriver << "#include <spk/SpkValarray.h>" << endl;
  oIdentDriver << "#include <spk/SpkException.h>" << endl;
  oIdentDriver << "#include <spk/WarningsManager.h>" << endl;
  oIdentDriver << endl;
  oIdentDriver << "// SPK Compiler generated headers/classes" << endl;
  oIdentDriver << "#include \"IndData.h\"" << endl;
  oIdentDriver << "#include \"DataSet.h\"" << endl;
  oIdentDriver << "#include \"NonmemPars.h\"" << endl;
  oIdentDriver << endl;
  oIdentDriver << "//   NONMEM PRED SPECIFIC" << endl;
  oIdentDriver << "#include \"IdentPred.h\"" << endl;
  oIdentDriver << "#include <spkpred/IdentPredBase.h>" << endl;
  oIdentDriver << endl;
  oIdentDriver << "using SPK_VA::valarray;" << endl;
  oIdentDriver << "using namespace std;" << endl;
  oIdentDriver << endl;
  oIdentDriver << endl;
  oIdentDriver << "enum RETURN_CODE { SUCCESS                   = 0," << endl;
  oIdentDriver << "                   UNKNOWN_ERROR             = 1," << endl;
  oIdentDriver << "                   UNKNOWN_FAILURE           = 2," << endl;
  oIdentDriver << "                   FILE_ACCESS_ERROR         = 10," << endl;
  oIdentDriver << "                   OPTIMIZATION_ERROR        = 12," << endl;
  oIdentDriver << "                   STATISTICS_ERROR          = 13," << endl;
  oIdentDriver << "                   USER_INPUT_ERROR          = 14," << endl;
  oIdentDriver << "                   PROGRAMMER_ERROR          = 15," << endl;
  oIdentDriver << "                   SIMULATION_ERROR          = 16," << endl;
  oIdentDriver << "                   MODEL_IDENT_ERROR         = 17," << endl;
  oIdentDriver << "                   FILE_ACCESS_FAILURE       = 100," << endl;
  oIdentDriver << "                   RESERVED_DO_NOT_USE       = 101," << endl;
  oIdentDriver << "                   OPTIMIZATION_FAILURE      = 102," << endl;
  oIdentDriver << "                   STATISTICS_FAILURE        = 103," << endl;
  oIdentDriver << "                   PROGRAMMER_FAILURE        = 105," << endl;
  oIdentDriver << "                   SIMULATION_FAILURE        = 106," << endl;
  oIdentDriver << "                   MODEL_IDENT_FAILURE       = 107" << endl;
  oIdentDriver << "                 };" << endl;
  oIdentDriver << endl;
  oIdentDriver << "int main( int argc, const char argv[] )" << endl;
  oIdentDriver << "{" << endl;
  oIdentDriver << "   /*******************************************************************/" << endl;
  oIdentDriver << "   /*                                                                 */" << endl;
  oIdentDriver << "   /*   Variable declarations and definitions                         */" << endl;
  oIdentDriver << "   /*                                                                 */" << endl;
  oIdentDriver << "   /*******************************************************************/" << endl;
  oIdentDriver << "   enum RETURN_CODE ret = SUCCESS;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "   SpkException errors;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "   // Enclose the entire calculation in a big TRY block and don't let" << endl;
  oIdentDriver << "   // any exceptions escape." << endl;
  oIdentDriver << "   try" << endl;
  oIdentDriver << "   {" << endl;
  oIdentDriver << "      const int nPop = 1;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      DataSet<GiNaC::ex> set;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      const bool isIdentRequested     = true;" << endl;
  oIdentDriver << "      bool isIdentSuccess             = !isIdentRequested;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      // Construct the identifiability PK-DES-ERROR block expression" << endl;
  oIdentDriver << "      // evaluator." << endl;
  oIdentDriver << "      IdentPred< GiNaC::ex > mPred( NonmemPars::nTheta," << endl;
  oIdentDriver << "                                    NonmemPars::nEta," << endl;
  oIdentDriver << "                                    &set," << endl;
  oIdentDriver << "                                    nPop," << endl;
  oIdentDriver << "                                    NonmemPars::isPkFunctionOfT," << endl;
  oIdentDriver << "                                    NonmemPars::nCompartments," << endl;
  oIdentDriver << "                                    NonmemPars::nParameters," << endl;
  oIdentDriver << "                                    NonmemPars::defaultDoseComp," << endl;
  oIdentDriver << "                                    NonmemPars::defaultObservationComp," << endl;
  oIdentDriver << "                                    NonmemPars::initialOff," << endl;
  oIdentDriver << "                                    NonmemPars::noOff," << endl;
  oIdentDriver << "                                    NonmemPars::noDose );" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      timeval identBegin, identEnd;" << endl;
  oIdentDriver << "      double identTimeSec = 0.0;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      ofstream oResults;" << endl;
  oIdentDriver << "      string warningsOut;" << endl;
  oIdentDriver << "      int thetaSeed = NonmemPars::seed;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      remove( \"result.xml\" );" << endl;
  oIdentDriver << endl;
  oIdentDriver << "IDENTIFIABILITY:" << endl;
  oIdentDriver << "      /*******************************************************************/" << endl;
  oIdentDriver << "      /*                                                                 */" << endl;
  oIdentDriver << "      /*   Parameter Identifiability Determination                       */" << endl;
  oIdentDriver << "      /*                                                                 */" << endl;
  oIdentDriver << "      /*******************************************************************/" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      // This will be the number of solutions of the nonlinear system" << endl;
  oIdentDriver << "      // of equations that make up the Groebner basis equations." << endl;
  oIdentDriver << "      int nGroebnerBasisSoln = 0;" << endl;
  oIdentDriver << "   " << endl;
  oIdentDriver << "      // This will be the status of the identifiability calculation." << endl;
  oIdentDriver << "      string identStatus;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      if( isIdentRequested )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         // Set the index for the individual in the population." << endl;
  oIdentDriver << "         int i = 0;" << endl;
  oIdentDriver << "      " << endl;
  oIdentDriver << "         // Set this so that intermediate information is printed." << endl;
  oIdentDriver << "         int level = 1;" << endl;
  oIdentDriver << "      " << endl;
  oIdentDriver << "         gettimeofday( &identBegin, NULL );" << endl;
  oIdentDriver << "         try" << endl;
  oIdentDriver << "         {" << endl;
  oIdentDriver << "            // Attempt to determine the identifiability of an individual's" << endl;
  oIdentDriver << "            // THETA parameter using the system-experiment model that is defined" << endl;
  oIdentDriver << "            // by the expressions from the PK, DES, and ERROR blocks and that is" << endl;
  oIdentDriver << "            // also defined by the data records from the data file." << endl;
  oIdentDriver << "            nGroebnerBasisSoln = mPred.checkIndParamIdent( i," << endl;
  oIdentDriver << "                                                           level," << endl;
  oIdentDriver << "                                                           NonmemPars::nTheta," << endl;
  oIdentDriver << "                                                           NonmemPars::nEta," << endl;
  oIdentDriver << "                                                           thetaSeed," << endl;
  oIdentDriver << "                                                           identStatus );" << endl;
  oIdentDriver << "   " << endl;
  oIdentDriver << "            isIdentSuccess = true;" << endl;
  oIdentDriver << "         }" << endl;
  oIdentDriver << "         catch( const SpkException& e )" << endl;
  oIdentDriver << "         {" << endl;
  oIdentDriver << "            ret = MODEL_IDENT_ERROR;" << endl;
  oIdentDriver << "            errors.cat( e );" << endl;
  oIdentDriver << "         }" << endl;
  oIdentDriver << "         catch( ... )" << endl;
  oIdentDriver << "         {" << endl;
  oIdentDriver << "            char message[] = \"Unknown exception: failed in parameter identifiability!!!\";" << endl;
  oIdentDriver << "            SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIdentDriver << "            errors.push( e );" << endl;
  oIdentDriver << "            isIdentSuccess = false;" << endl;
  oIdentDriver << "            ret = MODEL_IDENT_FAILURE;" << endl;
  oIdentDriver << "         }" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         gettimeofday( &identEnd, NULL );" << endl;
  oIdentDriver << "         identTimeSec = difftime( identEnd.tv_sec, identBegin.tv_sec );" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << endl;
  oIdentDriver << endl;
  oIdentDriver << "REPORT_GEN:" << endl;
  oIdentDriver << "      /*******************************************************************/" << endl;
  oIdentDriver << "      /*                                                                 */" << endl;
  oIdentDriver << "      /*   ReportML Document                                             */" << endl;
  oIdentDriver << "      /*                                                                 */" << endl;
  oIdentDriver << "      /*******************************************************************/" << endl;
  oIdentDriver << "      oResults.open( \"result.xml\", ios_base::app );" << endl;
  oIdentDriver << "      if( !oResults.good() )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIdentDriver << "           \"result.xml\" );" << endl;
  oIdentDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIdentDriver << "         oResults << \"</spkreport>\" << endl;" << endl;
  oIdentDriver << "         oResults.close();" << endl;
  oIdentDriver << "         goto END;" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIdentDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      // Print out <error_list> even when it is empty." << endl;
  oIdentDriver << "      oResults << \"<error_list length=\\\"\" << errors.size() << \"\\\">\" << endl;" << endl;
  oIdentDriver << "      if( !isIdentSuccess )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         // Print out ordinary-length error messages" << endl;
  oIdentDriver << "         oResults << errors << endl;" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << "      oResults << \"</error_list>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      // Print out <warning_list> even when it is empty." << endl;
  oIdentDriver << "      WarningsManager::getAllWarnings( warningsOut );" << endl;
  oIdentDriver << "      oResults << warningsOut;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "      if( isIdentRequested )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         oResults << \"<ind_analysis_result>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"<ind_ident_result elapsedtime=\\\"\" << identTimeSec << \"\\\">\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"<seed>\" << endl;" << endl;
  oIdentDriver << "         oResults << \"<value>\" << thetaSeed << \"</value>\" << endl;" << endl;
  oIdentDriver << "         oResults << \"</seed>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"<ind_ident_number_of_solutions>\" << endl;" << endl;
  oIdentDriver << "         oResults << \"<value>\" << nGroebnerBasisSoln << \"</value>\" << endl;" << endl;
  oIdentDriver << "         oResults << \"</ind_ident_number_of_solutions>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"<ind_ident_status>\" << endl;" << endl;
  oIdentDriver << "         oResults << identStatus << endl;" << endl;
  oIdentDriver << "         oResults << \"</ind_ident_status>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"</ind_ident_result>\" << endl;" << endl;
  oIdentDriver << endl;
  oIdentDriver << "         oResults << \"</ind_analysis_result>\" << endl;" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIdentDriver << "      oResults.close();" << endl;
  oIdentDriver << "END:" << endl;
  oIdentDriver << "      // The following statement is to trick g++ compiler, which" << endl;
  oIdentDriver << "      // complains that a label must be followed by a statement." << endl;
  oIdentDriver << "      int dummy = 0;" << endl;
  oIdentDriver << "   }" << endl;
  oIdentDriver << "   catch( const SpkException & e )" << endl;
  oIdentDriver << "   {" << endl;
  oIdentDriver << "      ofstream oResults;" << endl;
  oIdentDriver << "      oResults.open( \"result.xml\", ios_base::app );" << endl;
  oIdentDriver << "      if( !oResults.good() )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIdentDriver << "           \"result.xml\" );" << endl;
  oIdentDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIdentDriver << "         cout << \"exit code = \" << ret << endl;" << endl;
  oIdentDriver << "         return ret;" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIdentDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIdentDriver << "      oResults << \"<error_list length=\\\"\" << e.size() << \"\\\">\" << endl;" << endl;
  oIdentDriver << "      oResults << e << endl;" << endl;
  oIdentDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIdentDriver << "      oResults.close();" << endl;
  oIdentDriver << "      ret = UNKNOWN_ERROR;" << endl;
  oIdentDriver << "   }" << endl;
  oIdentDriver << "   catch( ... )" << endl;
  oIdentDriver << "   {" << endl;
  oIdentDriver << "      ofstream oResults;" << endl;
  oIdentDriver << "      oResults.open( \"result.xml\", ios_base::app );" << endl;
  oIdentDriver << "      if( !oResults.good() )" << endl;
  oIdentDriver << "      {" << endl;
  oIdentDriver << "         fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIdentDriver << "           \"result.xml\" );" << endl;
  oIdentDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIdentDriver << "         cout << \"exit code = \" << ret << endl;" << endl;
  oIdentDriver << "         return ret;" << endl;
  oIdentDriver << "      }" << endl;
  oIdentDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIdentDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIdentDriver << "      oResults << \"<error_list length=\\\"\" << 2 << \"\\\">\" << endl;" << endl;
  oIdentDriver << "      oResults << \"Unexpected exceptions are caught.\" << endl;" << endl;
  oIdentDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIdentDriver << "      oResults.close();" << endl;
  oIdentDriver << "      ret = UNKNOWN_FAILURE;" << endl;
  oIdentDriver << "   }" << endl;
  oIdentDriver << endl;
  oIdentDriver << "   cout << \"exit code = \" << ret << endl;" << endl;
  oIdentDriver << "   return ret;" << endl;
  oIdentDriver << "}" << endl;

  oIdentDriver.close();
}
