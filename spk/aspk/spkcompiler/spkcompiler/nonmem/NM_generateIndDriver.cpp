/**
 * @file NM_generateIndDriver.cpp
 * Define NonmemTranslator::generateIndDriver().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;

void NonmemTranslator::generateIndDriver( ) const
{
  //---------------------------------------------------------------------------------------
  // Generate the SPK driver
  //---------------------------------------------------------------------------------------
  ofstream oIndDriver ( fFitDriver_cpp );
  if( !oIndDriver.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fFitDriver_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const Symbol* pTheta = table->find(nonmem::THETA);
  const Symbol* pEta   = table->find(nonmem::ETA);
  const Symbol* pOmega = table->find(nonmem::OMEGA);

  oIndDriver << "// " << myDescription              << endl;
  oIndDriver << "#include <iostream>"               << endl;
  oIndDriver << "#include <fstream>"                << endl;
  oIndDriver << "#include <sys/time.h>"             << endl;
  oIndDriver << "#include <vector>"                 << endl;
  oIndDriver << "#include <pvm3.h>"                 << endl;
  oIndDriver << endl;

  oIndDriver << "#include <spk/spkpvm.h>"           << endl;
  oIndDriver << "#include <spk/scalarToDouble.h>"      << endl;
  oIndDriver << "#include <spk/SpkValarray.h>"      << endl;
  oIndDriver << "#include <spk/SpkException.h>"     << endl;
  oIndDriver << "#include <spk/WarningsManager.h>"  << endl;
  oIndDriver << "#include <CppAD/CppAD.h>"          << endl;
  oIndDriver << endl;

  oIndDriver << "// For parameter estimation"       << endl;
  oIndDriver << "#include <spk/fitIndividual.h>"    << endl;
  oIndDriver << "#include <spk/Optimizer.h>"        << endl;
  oIndDriver << endl;

  oIndDriver << "// For statistics"                 << endl;
  oIndDriver << "#include <spk/inverse.h>"          << endl;
  oIndDriver << "#include <spk/indStatistics.h>"    << endl;
  oIndDriver << "#include <spk/derParStatistics.h>" << endl;
  oIndDriver << "#include <spk/multiply.h>"         << endl;
  oIndDriver << "#include <spk/cholesky.h>"         << endl;
  oIndDriver << "#include <spk/indResiduals.h>"     << endl;
  oIndDriver << "#include <spk/symmetrize.h>"     << endl;
  oIndDriver << endl;

  oIndDriver << "// Helper" << endl;
  oIndDriver << "#include <spk/printInMatrix.h>"    << endl;
  oIndDriver << endl;

  oIndDriver << "// For data simulation"            << endl;
  oIndDriver << "#include <spk/simulate.h>"         << endl;
  oIndDriver << endl;

  oIndDriver << "// SPK Compiler generated headers/classes" << endl;
  oIndDriver << "#include \"IndData.h\""            << endl;
  oIndDriver << "#include \"DataSet.h\""            << endl;
  oIndDriver << "#include \"NonmemPars.h\""         << endl;
  oIndDriver << endl;

  oIndDriver << "//   NONMEM PRED SPECIFIC"         << endl;
  if( myModelSpec == PRED )
    oIndDriver << "#include \"Pred.h\""               << endl;
  else
    oIndDriver << "#include \"OdePred.h\""            << endl;
  oIndDriver << "#include <spkpred/IndPredModel.h>" << endl;
  oIndDriver << endl;

  oIndDriver << "using SPK_VA::valarray;" << endl;
  oIndDriver << "using namespace std;"    <<endl;
  oIndDriver << endl;

  oIndDriver << "enum RETURN_CODE { SUCCESS              = 0,"   << endl;
  oIndDriver << "                   UNKNOWN_ERROR        = 1,"   << endl;
  oIndDriver << "                   UNKNOWN_FAILURE      = 2,"   << endl;
  oIndDriver << "                   PVM_FAILURE          = 3,"   << endl;
  oIndDriver << "                   USER_ABORT           = 4,"   << endl;
  oIndDriver << "                   FILE_ACCESS_ERROR    = 10,"  << endl;
  oIndDriver << "                   OPTIMIZATION_ERROR   = 12,"  << endl;
  oIndDriver << "                   STATISTICS_ERROR     = 13,"  << endl;
  oIndDriver << "                   USER_INPUT_ERROR     = 14,"  << endl;
  oIndDriver << "                   PROGRAMMER_ERROR     = 15,"  << endl;
  oIndDriver << "                   SIMULATION_ERROR     = 16,"  << endl;
  oIndDriver << "                   FILE_ACCESS_FAILURE  = 100," << endl;
  oIndDriver << "                   RESERVED_DO_NOT_USE  = 101," << endl;
  oIndDriver << "                   OPTIMIZATION_FAILURE = 102," << endl;
  oIndDriver << "                   STATISTICS_FAILURE   = 103," << endl;
  oIndDriver << "                   PROGRAMMER_FAILURE   = 105," << endl;
  oIndDriver << "                   SIMULATION_FAILURE   = 106"  << endl;
  oIndDriver << "                 };"       << endl;
  oIndDriver << endl;

  oIndDriver << "static void finish(int exit_value)"  << endl;
  oIndDriver << "{"  << endl;
  oIndDriver << "   int parent_tid = pvm_parent();"  << endl;
  oIndDriver << "   pvm_initsend(PvmDataDefault);"  << endl;
  oIndDriver << "   pvm_pkint(&exit_value, 1, 1);"  << endl;
  oIndDriver << "   pvm_send(parent_tid, SpkPvmExitValue);"  << endl;
  oIndDriver << "   pvm_exit();"  << endl;
  oIndDriver << "}"  << endl;

  oIndDriver << "int main( int argc, const char* argv[] )" << endl;
  oIndDriver << "{" << endl;

  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*   Variable declarations and definitions                         */" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   enum RETURN_CODE ret = SUCCESS;" << endl;
  oIndDriver << endl;

  oIndDriver << "   bool isUsingPvm = false;" << endl;
  oIndDriver << "   if(argc > 1)" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      isUsingPvm = true;"<< endl;
  oIndDriver << "      pvm_mytid();" << endl;
  oIndDriver << "      int parent_tid = pvm_parent();" << endl;
  oIndDriver << endl;

  oIndDriver << "      // Disallow direct routing of messages between tasks; otherwise" << endl;
  oIndDriver << "      // messages will arrive out of sequence." << endl;
  oIndDriver << "      pvm_setopt(PvmRoute, PvmDontRoute);" << endl;

  oIndDriver << "      pvm_notify(PvmTaskExit, PvmTaskExit, 1, &parent_tid);" << endl;
  oIndDriver << endl;

  oIndDriver << "      if(chdir(argv[1]) != 0)" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         finish(FILE_ACCESS_FAILURE);" << endl;
  oIndDriver << "         return FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "   }" << endl;

  oIndDriver << "   // Redirect stdout and stderr to files" << endl;
  oIndDriver << "   const char* stdoutFileName = \"optimizer_trace.txt\";" << endl;
  oIndDriver << "   const char* stderrFileName = \"software_error\";" << endl;
  oIndDriver << "   freopen( stdoutFileName, \"w\", stdout );" << endl;
  oIndDriver << "   freopen( stderrFileName, \"w\", stderr );" << endl;
  oIndDriver << endl;

  oIndDriver << "   SpkException errors;" << endl;
  // Enclose the entire code in a big TRY block and don't let an exception leak."
  oIndDriver << "   try{" << endl;
  oIndDriver << "      ofstream oLongError;" << endl;
  oIndDriver << endl;

  oIndDriver << "      const int nPop = 1;" << endl;
  oIndDriver << endl;

  oIndDriver << "      DataSet< double > set;" << endl;
  oIndDriver << "      DataSet< CppAD::AD<double> > setAD;" << endl;
  oIndDriver << "      DataSet< CppAD::AD< CppAD::AD<double> > > setADAD;" << endl;
  oIndDriver << "      const int nObservs = set.getNObservs( 0 );" << endl;
  oIndDriver << "      valarray<double> y( nObservs );" << endl;
  oIndDriver << "      valarray<double> f( nObservs );" << endl;
  oIndDriver << endl;
  
  oIndDriver << "      const bool isSimRequested     = " << ( myIsSimulate? "true":"false" ) << ";" << endl;
  oIndDriver << "      bool haveCompleteData         = !isSimRequested;" << endl;
  oIndDriver << endl;

  oIndDriver << "      const bool isOptRequested     = " << ( myIsEstimate? "true":"false" ) << ";" << endl;
  oIndDriver << "      bool isOptSuccess             = !isOptRequested;" << endl;
  oIndDriver << endl;

  oIndDriver << "      const bool isStatRequested    = " << ( myIsStat? "true":"false" ) << ";"     << endl;
  oIndDriver << "      IndCovForm covForm            = " << myCovForm << ";" << endl;
  oIndDriver << "      bool isStatSuccess            = !isStatRequested;" << endl;
  oIndDriver << endl;
  oIndDriver << endl;

  oIndDriver << "      const bool isRestartRequested = " << ( myIsRestart? "true":"false" ) << ";"     << endl;
  oIndDriver << endl;

  oIndDriver << "      const int nRepeats            = " << mySubproblemsN << ";" << endl;
  oIndDriver << endl;

  oIndDriver << "      const bool withD              = false;" << endl;
  oIndDriver << endl;

  oIndDriver << "      valarray<double> thetaStep( NonmemPars::nTheta );" << endl;
  oIndDriver << "      valarray<double> thetaIn  ( NonmemPars::thetaIn );" << endl;
  oIndDriver << "      valarray<double> omegaIn  ( NonmemPars::omegaIn );" << endl;
  oIndDriver << "      valarray<double> thetaOut ( NonmemPars::nTheta );" << endl;
  oIndDriver << "      valarray<double> omegaOut ( NonmemPars::omegaOrder );" << endl;
  oIndDriver << endl;

  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "      //   Model Initialization" << endl;
  if( myModelSpec == PRED )
    {
      oIndDriver << "      Pred< double > mPred(&set);" << endl;
      oIndDriver << "      Pred< CppAD::AD<double> > mPredAD(&setAD);" << endl;
      oIndDriver << "      Pred< CppAD::AD< CppAD::AD<double> > > mPredADAD(&setADAD);" << endl;
    }
  else // ADVAN3
    {
      oIndDriver << "      OdePred<double > mPred( &set, " << endl;
      oIndDriver << "                              NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                              NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                              NonmemPars::nCompartments," << endl;
      oIndDriver << "                              NonmemPars::nParameters," << endl;
      oIndDriver << "                              NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                              NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                              NonmemPars::initialOff," << endl;
      oIndDriver << "                              NonmemPars::noOff," << endl;
      oIndDriver << "                              NonmemPars::noDose," << endl;
      oIndDriver << "                              NonmemPars::relTol" << endl;
      oIndDriver << "                             );" << endl;
      oIndDriver << "      OdePred<CppAD::AD<double> > mPredAD( &setAD, " << endl;
      oIndDriver << "                                            NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                                            NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                                            NonmemPars::nCompartments," << endl;
      oIndDriver << "                                            NonmemPars::nParameters," << endl;
      oIndDriver << "                                            NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                                            NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                                            NonmemPars::initialOff," << endl;
      oIndDriver << "                                            NonmemPars::noOff," << endl;
      oIndDriver << "                                            NonmemPars::noDose," << endl;
      oIndDriver << "                                            NonmemPars::relTol" << endl;
      oIndDriver << "                                         );" << endl;
      oIndDriver << "      OdePred<CppAD::AD< CppAD::AD<double> > > mPredADAD( &setADAD, " << endl;
      oIndDriver << "                                                          NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                                                          NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                                                          NonmemPars::nCompartments," << endl;
      oIndDriver << "                                                          NonmemPars::nParameters," << endl;
      oIndDriver << "                                                          NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                                                          NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                                                          NonmemPars::initialOff," << endl;
      oIndDriver << "                                                          NonmemPars::noOff," << endl;
      oIndDriver << "                                                          NonmemPars::noDose," << endl;
      oIndDriver << "                                                          NonmemPars::relTol" << endl;
      oIndDriver << "                                                        );" << endl;
    }
  oIndDriver << "      IndPredModel model( mPred, "                   << endl;
  oIndDriver << "                          mPredAD,"                  << endl;
  oIndDriver << "                          mPredADAD,"                << endl;
  oIndDriver << "                          NonmemPars::nTheta, "      << endl;
  oIndDriver << "                          NonmemPars::thetaLow, "    << endl;
  oIndDriver << "                          NonmemPars::thetaUp, "     << endl;
  oIndDriver << "                          NonmemPars::thetaIn, "     << endl;
  oIndDriver << "                          NonmemPars::nEta, "        << endl;
  oIndDriver << "                          NonmemPars::omegaStruct,"  << endl;
  oIndDriver << "                          NonmemPars::omegaIn,"      << endl;
  oIndDriver << "                          NonmemPars::omegaFixed,"   << endl;
  oIndDriver << "                          NonmemPars::omegaBlockStruct,"      << endl;
  oIndDriver << "                          NonmemPars::omegaBlockDims,"        << endl;
  oIndDriver << "                          NonmemPars::omegaBlockSameAsPrev );"<< endl;
  oIndDriver << "      //" << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "      //   DataSet and Model for disposal in order to save the last values"   << endl;
  oIndDriver << "      //   from parameter estimation."                                        << endl;
  oIndDriver << "      DataSet< double > dataForDisposal;" << endl;
  oIndDriver << "      DataSet< CppAD::AD<double> > dataForDisposalAD;" << endl;
  oIndDriver << "      DataSet< CppAD::AD< CppAD::AD<double> > > dataForDisposalADAD;" << endl;
  if( myModelSpec == PRED )
    {
      oIndDriver << "      Pred< double > predForDisposal(&dataForDisposal);" << endl;
      oIndDriver << "      Pred< CppAD::AD<double> > predForDisposalAD(&dataForDisposalAD);" << endl;
      oIndDriver << "      Pred< CppAD::AD< CppAD::AD<double> > > predForDisposalADAD(&dataForDisposalADAD);" << endl;
    }
  else // ADVAN3
    {
      oIndDriver << "      OdePred< double > predForDisposal( &dataForDisposal, " << endl;
      oIndDriver << "                                         NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                                         NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                                         NonmemPars::nCompartments," << endl;
      oIndDriver << "                                         NonmemPars::nParameters," << endl;
      oIndDriver << "                                         NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                                         NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                                         NonmemPars::initialOff," << endl;
      oIndDriver << "                                         NonmemPars::noOff," << endl;
      oIndDriver << "                                         NonmemPars::noDose," << endl;
      oIndDriver << "                                         NonmemPars::relTol" << endl;
      oIndDriver << "                                       );" << endl;
      oIndDriver << "      OdePred< CppAD::AD<double> > predForDisposalAD( &dataForDisposalAD, " << endl;
      oIndDriver << "                                         NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                                         NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                                         NonmemPars::nCompartments," << endl;
      oIndDriver << "                                         NonmemPars::nParameters," << endl;
      oIndDriver << "                                         NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                                         NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                                         NonmemPars::initialOff," << endl;
      oIndDriver << "                                         NonmemPars::noOff," << endl;
      oIndDriver << "                                         NonmemPars::noDose," << endl;
      oIndDriver << "                                         NonmemPars::relTol" << endl;
      oIndDriver << "                                       );" << endl;
      oIndDriver << "      OdePred< CppAD::AD< CppAD::AD<double> > > predForDisposalADAD( &dataForDisposalADAD, " << endl;
      oIndDriver << "                                         NonmemPars::nIndividuals, " << endl;
      oIndDriver << "                                         NonmemPars::isPkFunctionOfT," << endl;
      oIndDriver << "                                         NonmemPars::nCompartments," << endl;
      oIndDriver << "                                         NonmemPars::nParameters," << endl;
      oIndDriver << "                                         NonmemPars::defaultDoseComp," << endl;
      oIndDriver << "                                         NonmemPars::defaultObservationComp," << endl;
      oIndDriver << "                                         NonmemPars::initialOff," << endl;
      oIndDriver << "                                         NonmemPars::noOff," << endl;
      oIndDriver << "                                         NonmemPars::noDose," << endl;
      oIndDriver << "                                         NonmemPars::relTol" << endl;
      oIndDriver << "                                       );" << endl;
    }
  oIndDriver << "      IndPredModel modelForDisposal( predForDisposal, " << endl;
  oIndDriver << "                                     predForDisposalAD,"       << endl;
  oIndDriver << "                                     predForDisposalADAD,"     << endl;
  oIndDriver << "                          NonmemPars::nTheta, "         << endl;
  oIndDriver << "                          NonmemPars::thetaLow, "       << endl;
  oIndDriver << "                          NonmemPars::thetaUp, "        << endl;
  oIndDriver << "                          NonmemPars::thetaIn, "        << endl;
  oIndDriver << "                          NonmemPars::nEta, "           << endl;
  oIndDriver << "                          NonmemPars::omegaStruct,"     << endl;
  oIndDriver << "                          NonmemPars::omegaIn,"         << endl;
  oIndDriver << "                          NonmemPars::omegaFixed,"      << endl;
  oIndDriver << "                          NonmemPars::omegaBlockStruct,"      << endl;
  oIndDriver << "                          NonmemPars::omegaBlockDims,"        << endl;
  oIndDriver << "                          NonmemPars::omegaBlockSameAsPrev );"<< endl;
  oIndDriver << "      //" << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;

  oIndDriver << "      const int nB = model.getNIndPar();" << endl;
  oIndDriver << "      valarray<double> bIn  ( nB );"      << endl;
  oIndDriver << "      valarray<double> bStep( nB );"      << endl;
  oIndDriver << "      valarray<double> bLow ( nB );"      << endl;
  oIndDriver << "      valarray<double> bUp  ( nB );"      << endl;
  oIndDriver << "      valarray<double> bOut ( nB );"      << endl;
  oIndDriver << "      valarray<bool>   bMask( nB );"      << endl;
  oIndDriver << "      double           bObjOut;"                       << endl;
  oIndDriver << "      valarray<double> bObj_bOut( nB );"               << endl;
  oIndDriver << "      valarray<double> bObj_b_bOut( nB * nB );"        << endl;
  oIndDriver << endl;
  
  oIndDriver << "      timeval optBegin, optEnd;" << endl;
  oIndDriver << "      double optTimeSec = 0.0;" << endl;
  oIndDriver << endl;

  oIndDriver << "      const double indEps             = "   << myIndEpsilon    << ";" << endl;
  oIndDriver << "      const int    indMitr            = "   << myIndMitr       << ";" << endl;
  oIndDriver << "      const int    indTrace           = "   << myIndTraceLevel << ";" << endl;
  oIndDriver << "      const string indCheckpointFile  = \"" << fCheckpoint_xml << "\";"  << endl;
  oIndDriver << "      bool         indWriteCheckpoint = "   << (myIndWriteCheckpoint? "true" : "false") << ";" << endl;
  oIndDriver << "      ifstream     iCheckpoint( indCheckpointFile.c_str() );"  << endl;
  oIndDriver << "      // Error if the user asked to continue but no checkpoint.xml is found " << endl;
  oIndDriver << "      // in the current directory." << endl;
  oIndDriver << "      if( isRestartRequested && !iCheckpoint.good() )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char m[ SpkError::maxMessageLen()];" << endl;
  oIndDriver << "         snprintf( m, " << endl;
  oIndDriver << "                   SpkError::maxMessageLen()," << endl;
  oIndDriver << "                  \"Warm start is requested but no checkpoint file found.\" );" << endl;
  oIndDriver << "         SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__);" << endl;
  oIndDriver << "         errors.push( e );" << endl;
  oIndDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      // Flag to read the checkpoint file if that exists even " << endl;
  oIndDriver << "      // if the user didn't ask a continuation." << endl;
  oIndDriver << "      bool         indReadCheckpoint  = iCheckpoint.good();"   << endl;
  oIndDriver << "      iCheckpoint.close();"                                    << endl;
  oIndDriver << "      Optimizer    indOpt( indEps, "                           << endl;
  oIndDriver << "                           indMitr, "                          << endl;
  oIndDriver << "                           indTrace, "                         << endl;
  oIndDriver << "                           indCheckpointFile, "                << endl;
  oIndDriver << "                           indReadCheckpoint,"                 << endl;
  oIndDriver << "                           indWriteCheckpoint );"              << endl;
  oIndDriver << endl;

  oIndDriver << "      model.getIndPar       ( bIn );"       << endl;
  oIndDriver << "      model.getIndParLimits ( bLow, bUp );" << endl;
  oIndDriver << "      model.getIndParStep   ( bStep );"     << endl;
  oIndDriver << endl;

  oIndDriver << "      timeval statBegin, statEnd;"                         << endl;
  oIndDriver << "      double statTimeSec = 0.0;"                           << endl;
  oIndDriver << "      const int nDegOfFreedom = nObservs - nB;"            << endl;
  oIndDriver << "      valarray<double> bCov( nB * nB );"                   << endl;
  oIndDriver << "      valarray<double> stdPar( nB );"                      << endl;
  oIndDriver << "      valarray<double> stdPar_b( nB * nB );"               << endl;
  oIndDriver << "      valarray<bool>   stdParMask( nB );"                  << endl;
  oIndDriver << "      bool isCovOut         = " << ( myIsCov?         "true" : "false" ) << ";" << endl;    
  oIndDriver << "      bool isInvCovOut      = " << ( myIsInvCov?      "true" : "false" ) << ";" << endl;    
  oIndDriver << "      bool isStdErrOut      = " << ( myIsStderr?      "true" : "false" ) << ";" << endl;    
  oIndDriver << "      bool isCorrelationOut = " << ( myIsCorrelation? "true" : "false" ) << ";" << endl;    
  oIndDriver << "      bool isConfidenceOut  = " << ( myIsConfidence?  "true" : "false" ) << ";" << endl;    
  oIndDriver << "      bool isCoefficientOut = " << ( myIsCoefficient? "true" : "false" ) << ";" << endl;    
  oIndDriver << "      valarray<double> stdParCovOut( nB * nB );"         << endl;
  oIndDriver << "      valarray<double> stdParSEOut( nB );"               << endl;
  oIndDriver << "      valarray<double> stdParCorrelationOut( nB * nB );" << endl;
  oIndDriver << "      valarray<double> stdParCoefficientOut( nB );"      << endl;
  oIndDriver << "      valarray<double> stdParConfidenceOut( 2 * nB );"   << endl;
  oIndDriver << "      valarray<double> stdParInvCovOut( nB * nB );"      << endl;
	  
  oIndDriver << "      valarray<double> f_bOut( nObservs * nB );"      << endl;
  oIndDriver << "      valarray<double> R_bOut( nObservs * nObservs * nB );" << endl;
  oIndDriver << "      valarray<double> RInvOut( nObservs * nObservs );"     << endl;
  oIndDriver << endl;

  oIndDriver << "      const int nRecords = set.getNRecords(0);" << endl;
  oIndDriver << "      valarray<double> iPredOut              ( nRecords );"  << endl;
  oIndDriver << "      valarray<double> iResOut               ( nRecords );"  << endl;
  oIndDriver << "      valarray<double> iResWtdOut            ( nRecords );"  << endl;
  oIndDriver << "      valarray<double> iResTrancatedOut      ( nObservs );"  << endl;
  oIndDriver << "      valarray<double> iResWtdTrancatedOut   ( nObservs );"  << endl;
  oIndDriver << endl;

  oIndDriver << "      ofstream oResults;" << endl;
  oIndDriver << "      string warningsOut;" << endl;
  oIndDriver << "      int seed = NonmemPars::seed;" << endl;
  oIndDriver << "      srand( seed );" << endl;
  oIndDriver << "      int iSub = 0;" << endl;
  oIndDriver << endl;

  oIndDriver << "      if( ret != SUCCESS )" << endl;
  oIndDriver << "        goto REPORT_GEN;" << endl;
  oIndDriver << endl;

  oIndDriver << "      remove( \"result.xml\" );" << endl;
  oIndDriver << "      for( iSub=0; iSub<nRepeats; iSub++ )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*   Data Initialization                                           */" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         if( isSimRequested )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            valarray<double> yOut( nObservs );" << endl;
  oIndDriver << "            try" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               simulate( model, nObservs, bIn, yOut );" << endl;
  oIndDriver << "               set.replaceAllMeasurements( yOut );" << endl;
  oIndDriver << "               y = yOut;" << endl;
  oIndDriver << "               haveCompleteData = true;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( const SpkException& e )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               errors.cat( e );" << endl;
  oIndDriver << "               haveCompleteData = false;" << endl;
  oIndDriver << "               ret = SIMULATION_ERROR;" << endl;
  oIndDriver << "               goto REPORT_GEN;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( ... )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
  oIndDriver << "               SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "               errors.push( e );" << endl;
  oIndDriver << "               haveCompleteData = false;" << endl;
  oIndDriver << "               ret = SIMULATION_FAILURE;" << endl;
  oIndDriver << "               goto REPORT_GEN;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         else" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            y = set.getAllMeasurements();" << endl;
  oIndDriver << "            haveCompleteData = true;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << endl;
  
  oIndDriver << "OPTIMIZATION:" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*   Parameter Estimation                                          */" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         if( isOptRequested && haveCompleteData )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            gettimeofday( &optBegin, NULL );" << endl;
  oIndDriver << "            try" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               fitIndividual( model," << endl;
  oIndDriver << "                              y," << endl;
  oIndDriver << "                              indOpt," << endl;
  oIndDriver << "                              bLow," << endl;
  oIndDriver << "                              bUp," << endl;
  oIndDriver << "                              bIn," << endl;
  oIndDriver << "                              bStep," << endl;
  oIndDriver << "                             &bOut," << endl;
  oIndDriver << "                             &bObjOut," << endl;
  oIndDriver << "                             &bObj_bOut," << endl;
  oIndDriver << "                             &bObj_b_bOut," << endl;
  oIndDriver << "                              withD );" << endl;
  oIndDriver << "               isOptSuccess = true;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( const SpkException& e )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               if( e.find( SpkError::SPK_TOO_MANY_ITER ) )" << endl;
  oIndDriver << "                  ret = OPTIMIZATION_ERROR;" << endl;
  oIndDriver << "               else" << endl;
  oIndDriver << "                  ret = USER_INPUT_ERROR;" << endl;
  oIndDriver << "               errors.cat( e );" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( ... )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
  oIndDriver << "               SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "               errors.push( e );" << endl;
  oIndDriver << "               isOptSuccess = false;" << endl;
  oIndDriver << "               ret = OPTIMIZATION_FAILURE;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << endl;
  oIndDriver << "            // Get the latest value of theta and Omega." << endl;
  oIndDriver << "            // These values may be garbage if optimization had failed." << endl;
  oIndDriver << "            model.getTheta( thetaOut );" << endl;
  oIndDriver << "            model.getOmega( omegaOut );" << endl;
  oIndDriver << "            if( !isOptSuccess )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               string optErrHeader;" << endl;
  oIndDriver << "               string optErrMessage;" << endl;
  oIndDriver << "               // If individual level estimation failed, then get any details as to why." << endl;
  oIndDriver << "               if( indOpt.isThereErrorInfo() )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oLongError.open( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "                  if( !oLongError.good() )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     char m[ SpkError::maxMessageLen() ];" << endl;
  oIndDriver << "                     snprintf( m," << endl;
  oIndDriver << "                               SpkError::maxMessageLen()," << endl;
  oIndDriver << "                              \"Failed to create a temporary file, %s, for writing.\", " << endl;
  oIndDriver << "                              \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "                     SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__ );" << endl;
  oIndDriver << "                     errors.push( e );" << endl;
  oIndDriver << "                     ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "                     goto REPORT_GEN;" << endl;
  oIndDriver << "                  }" << endl;      
  oIndDriver << "                  optErrHeader  = \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oIndDriver << "                  optErrHeader += \"Individual level optimization failure details. \\n\";" << endl;
  oIndDriver << "                  optErrHeader += \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oIndDriver << "                  optErrHeader += \"\\n\";" << endl;
  oIndDriver << "                  indOpt.getErrorInfo(" << endl;
  oIndDriver << "                     optErrHeader," << endl;
  oIndDriver << "                     optErrMessage," << endl;
  oIndDriver << "                     __LINE__," << endl;
  oIndDriver << "                     __FILE__ );" << endl;
  oIndDriver << "                  oLongError << optErrMessage << endl;" << endl;
  oIndDriver << "                  oLongError.close();" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << endl;
  oIndDriver << endl;
  oIndDriver << "            gettimeofday( &optEnd, NULL );" << endl;
  oIndDriver << "            optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << endl;
  oIndDriver << "         if( isOptRequested && isOptSuccess )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            model.setIndPar( bOut );" << endl;
  oIndDriver << endl;
  oIndDriver << "            assert( haveCompleteData );" << endl;
  oIndDriver << "            try{" << endl;
  oIndDriver << "               // Make sure this individual's data mean values have" << endl;
  oIndDriver << "               // been calculated at the optimal parameter values." << endl;
  oIndDriver << "               model.dataMean( f );" << endl;
  oIndDriver << endl;
  oIndDriver << "               // Don't calculate the individual's PRED values." << endl;
  oIndDriver << "               valarray<double>* pVANull = 0;" << endl;
  oIndDriver << "               indResiduals( modelForDisposal,"     << endl;
  oIndDriver << "                             y, "                   << endl;
  oIndDriver << "                             bOut, "                << endl;
  oIndDriver << "                             pVANull, "             << endl;
  oIndDriver << "                            &iResTrancatedOut, "    << endl;
  oIndDriver << "                            &iResWtdTrancatedOut, " << endl;
  oIndDriver << "                             NULL, "                << endl;
  oIndDriver << "                             NULL );"               << endl;
  oIndDriver << "               dataForDisposal.expand( iResTrancatedOut,    iResOut );"    << endl;
  oIndDriver << "               dataForDisposal.expand( iResWtdTrancatedOut, iResWtdOut );" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( const SpkException& e )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               // This should not happen when optimization completed successfully." << endl;
  oIndDriver << "               // Thus, the error should result in generating a Bugzilla report." << endl;
  oIndDriver << "               errors.cat( e );" << endl;
  oIndDriver << "               ret = PROGRAMMER_FAILURE;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( ... )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               char message[] = \"Unknown exception: failed in residuals calculation!!!\";" << endl;
  oIndDriver << "               SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "               errors.push( e );" << endl;
  oIndDriver << "               ret = PROGRAMMER_FAILURE;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            // Set the IPRED values here so that they have values for" << endl;
  oIndDriver << "            // all of the data records." << endl;
  oIndDriver << "            for( int j=0; j<nRecords; j++)" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               // This assumes there is only one individual in the DataSet." << endl;
  oIndDriver << "               scalarToDouble( dataForDisposal.data[0]->PRED[j], iPredOut[j] );" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            set.replaceIPred( iPredOut );"   << endl;
  oIndDriver << "            set.replaceIRes ( iResOut );"    << endl;
  oIndDriver << "            set.replaceIWRes( iResWtdOut );" << endl;
  oIndDriver << endl;
  oIndDriver << "            set.replaceRes  ( iResOut );"    << endl;
  oIndDriver << "            set.replaceWRes ( iResWtdOut );" << endl;
  oIndDriver << "         }" << endl;
 
  // Statistics can be only computed when the parameter estimation has been done.
  oIndDriver << "STATISTICS:" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*   Statistics                                                    */" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
 oIndDriver << "         if( isStatRequested && isOptRequested && isOptSuccess )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            gettimeofday( &statBegin, NULL );"     << endl;
  oIndDriver << "            try" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  bMask[i] =!( bLow[i]==bUp[i] || bOut[i]==bLow[i] || bOut[i]==bUp[i] );" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "               model.getStandardParMask( bMask, stdParMask );" << endl;
  oIndDriver << "               model.getStandardPar( stdPar );"          << endl;
  oIndDriver << "               model.getStandardPar_indPar( stdPar_b );" << endl;
  oIndDriver << "               indStatistics(    modelForDisposal,"      << endl;
  oIndDriver << "                                 y,"             << endl;
  oIndDriver << "                                 bOut, "         << endl;
  oIndDriver << "                                 bMask,"         << endl;
  oIndDriver << "                                 bObj_b_bOut,"   << endl;
  oIndDriver << "                                 covForm,"       << endl;
  oIndDriver << "                                &bCov,"          << endl;
  oIndDriver << "                                 NULL,"          << endl;
  oIndDriver << "                                 NULL,"          << endl;
  oIndDriver << "                                 NULL,"          << endl;
  oIndDriver << "                                 NULL,"          << endl;
  oIndDriver << "                                 withD );"       << endl;
  oIndDriver << "               derParStatistics( bMask,"         << endl;
  oIndDriver << "                                 bCov,"          << endl;
  oIndDriver << "                                 stdParMask,"    << endl;
  oIndDriver << "                                 stdPar,"        << endl;
  oIndDriver << "                                 stdPar_b,"      << endl;
  oIndDriver << "                                 nDegOfFreedom," << endl;
  oIndDriver << "                                (isCovOut || isInvCovOut? &stdParCovOut        : NULL)," << endl;
  oIndDriver << "                                (isCovOut || isInvCovOut? &stdParInvCovOut     : NULL)," << endl;
  oIndDriver << "                                (isStdErrOut?             &stdParSEOut         : NULL)," << endl;
  oIndDriver << "                                (isCorrelationOut?        &stdParCorrelationOut: NULL)," << endl;
  oIndDriver << "                                (isCoefficientOut?        &stdParCoefficientOut: NULL)," << endl;
  oIndDriver << "                                (isConfidenceOut?         &stdParConfidenceOut : NULL) " << endl;
  oIndDriver << "                               );" << endl;
  oIndDriver << "               isStatSuccess &= true;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( const SpkException& e )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               errors.cat( e );" << endl;
  oIndDriver << "               isStatSuccess &= false;" << endl;
  oIndDriver << "               ret = STATISTICS_ERROR;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            catch( ... )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
  oIndDriver << "               SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "               errors.push( e );" << endl;
  oIndDriver << "               isStatSuccess &= false;" << endl;
  oIndDriver << "               ret = STATISTICS_FAILURE;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << endl;

  oIndDriver << "            gettimeofday( &statEnd, NULL );" << endl;
  oIndDriver << "            statTimeSec = difftime( statEnd.tv_sec, statBegin.tv_sec );" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << endl;

  oIndDriver << "REPORT_GEN:" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*   ReportML Document                                             */" << endl;
  oIndDriver << "         /*                                                                 */" << endl;
  oIndDriver << "         /*******************************************************************/" << endl;
  oIndDriver << "         oResults.open( \"" << fResult_xml << "\", ios_base::app );" << endl;
  oIndDriver << "         if( !oResults.good() )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIndDriver << "\"" << fResult_xml << "\" );" << endl;
  oIndDriver << "            ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "            oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "            oResults.close();" << endl;
  oIndDriver << "            goto END;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIndDriver << "         oResults << \"<spkreport>\" << endl;" << endl;

  // Print out <error_list> even when it is empty.
  oIndDriver << "         oResults << \"<error_list length=\\\"\" << errors.size() << \"\\\">\" << endl;" << endl;
  oIndDriver << "         if( !(haveCompleteData && isOptSuccess && isStatSuccess) )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            // Print out ordinary-length error messages" << endl;
  oIndDriver << "            oResults << errors << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "            // Print out a long error message if exists." << endl;
  oIndDriver << "            char ch;" << endl;
  oIndDriver << "            ifstream iLongError( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "            while( iLongError.get(ch) )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << ch;" << endl;   // Write a long error to the SpkReportML document.
  oIndDriver << "            }" << endl;
  oIndDriver << "            iLongError.close();" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         oResults << \"</error_list>\" << endl;" << endl;
  oIndDriver << "         remove( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << endl;

  // Print out <warning_list> even when it is empty.
  oIndDriver << "         WarningsManager::getAllWarnings( warningsOut );" << endl;
  oIndDriver << "         oResults << warningsOut;" << endl;
  oIndDriver << endl;

  oIndDriver << "         if( isSimRequested )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<simulation \";" << endl;
  oIndDriver << "            if( iSub == 0 )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"seed=\\\"\" << seed << \"\\\" \";" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"subproblem=\\\"\" << iSub+1 << \"\\\"/>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << endl;

  oIndDriver << "         if( isOptRequested )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_analysis_result>\" << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "            oResults << \"<ind_opt_result elapsedtime=\\\"\" << optTimeSec << \"\\\">\" << endl;" << endl;
  oIndDriver << "            oResults << \"<ind_obj_out>\" << endl;" << endl;
  oIndDriver << "            oResults << \"<value>\" << bObjOut << \"</value>\" << endl;" << endl;
  oIndDriver << "            oResults << \"</ind_obj_out>\" << endl;" << endl;
  oIndDriver << endl;
  
  oIndDriver << "            //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "            //    NONMEM Specific" << endl;

  // theta in
  oIndDriver << "            oResults << \"<theta_in length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << thetaIn[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</theta_in>\" << endl;" << endl;
  // theta out
  oIndDriver << "            oResults << \"<theta_out length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</theta_out>\" << endl;" << endl;
  // omega in
  oIndDriver << "            for(int ii=0, idx=0; ii<NonmemPars::nOmegaBlk; ii++)" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<omega_in dimension=\" << \"\\\"\" << NonmemPars::omegaBlockDims[ii] << \"\\\"\";" << endl;
  oIndDriver << "               oResults << \" struct=\" << \"\\\"\";" << endl;
  oIndDriver << "               if( NonmemPars::omegaBlockStruct[ii]==DIAGONAL )" << endl;
  oIndDriver << "                  oResults << \"diagonal\";" << endl;
  oIndDriver << "               else" << endl;
  oIndDriver << "                  oResults << \"block\";" << endl;
  oIndDriver << "               oResults << \"\\\"\" << \">\" << endl;" << endl;
  oIndDriver << "               if( NonmemPars::omegaBlockStruct[ii]==DIAGONAL )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  for( int i=0; i<NonmemPars::omegaBlockDims[ii]; i++, idx++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"<value>\" << omegaIn[idx] << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "               else // full" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  int n = NonmemPars::omegaDim;" << endl;
  oIndDriver << "                  int m = (n*(n+1))/2;" << endl;
  oIndDriver << "                  valarray<double> full( n * n );" << endl;
  oIndDriver << "                  valarray<double> row_minimal( m );" << endl;
  oIndDriver << endl;
  oIndDriver << "                  // First make a full matrix out of the minimal representation" << endl;
  oIndDriver << "                  for( int j=0; j<n; j++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int i=0; i<n-j; i++, idx++ ) // lower only" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        full[ (j*n)+(j+i) ] = omegaIn[idx];" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << endl;
  oIndDriver << "                  // Copy the lower triangle to the upper and complete fullification" << endl;
  oIndDriver << "                  symmetrize( full, n, full );" << endl;
  oIndDriver << endl;
  oIndDriver << "                  for( int i=0, k=0; i<n; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int j=0; j<i+1; j++, k++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        row_minimal[k] = full[i*n+j];" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << endl;
  oIndDriver << "                  for( int i=0; i<m; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"<value>\" << row_minimal[ i ];" << endl;
  oIndDriver << "                     oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               oResults << \"</omega_in>\" << endl;" << endl;
  oIndDriver << "            }" << endl;

  // omega out
  oIndDriver << "            for(int ii=0, idx=0; ii<NonmemPars::nOmegaBlk; ii++)" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<omega_out dimension=\" << \"\\\"\" << NonmemPars::omegaBlockDims[ii] << \"\\\"\";" << endl;
  oIndDriver << "               oResults << \" struct=\" << \"\\\"\";" << endl;
  oIndDriver << "               if( NonmemPars::omegaBlockStruct[ii]==DIAGONAL )" << endl;
  oIndDriver << "                  oResults << \"diagonal\";" << endl;
  oIndDriver << "               else" << endl;
  oIndDriver << "                  oResults << \"block\";" << endl;
  oIndDriver << "               oResults << \"\\\"\" << \">\" << endl;" << endl;
  oIndDriver << "               if( NonmemPars::omegaBlockStruct[ii]==DIAGONAL )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  for( int i=0; i<NonmemPars::omegaBlockDims[ii]; i++, idx++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"<value>\" << omegaOut[idx] << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "               else // full" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  int n = NonmemPars::omegaBlockDims[ii];" << endl;
  oIndDriver << "                  int m = (n*(n+1))/2;" << endl;
  oIndDriver << "                  valarray<double> full( n * n );" << endl;
  oIndDriver << "                  valarray<double> row_minimal( m );" << endl;
  oIndDriver << endl;
  oIndDriver << "                  for( int j=0; j<n; j++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int i=0; i<n-j; i++, idx++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        full[j*n+(j+i)] = omegaOut[idx];  " << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  symmetrize( full, n, full );" << endl;
  oIndDriver << endl;
  oIndDriver << "                  for( int i=0, k=0; i<n; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int j=0; j<i+1; j++, k++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        row_minimal[k] = full[i*n+j];" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  for( int i=0; i<m; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"<value>\" << row_minimal[ i ];" << endl;
  oIndDriver << "                     oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;

  oIndDriver << "               }" << endl;

  oIndDriver << "               oResults << \"</omega_out>\" << endl;" << endl;
  oIndDriver << "               //" << endl;
  oIndDriver << "            }" << endl;

  oIndDriver << "            //" << endl;
  oIndDriver << "            //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;
  oIndDriver << "            oResults << \"</ind_opt_result>\" << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "            if( isStatRequested )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<ind_stat_result elapsedtime=\\\"\" << statTimeSec << \"\\\">\" << endl;" << endl;
  oIndDriver << "               if( isCovOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        oResults << \"   <value>\" << stdParCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_covariance_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               if( isInvCovOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        oResults << \"   <value>\" << stdParInvCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_inverse_covariance_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               if( isStdErrOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_stderror_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"   <value>\" << stdParSEOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_stderror_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               if( isCorrelationOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "                     {" << endl;
  oIndDriver << "                        oResults << \"   <value>\" << stdParCorrelationOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "                     }" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_correlation_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;
  
  oIndDriver << "               if( isCoefficientOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_coefficient_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"   <value>\" << stdParCoefficientOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_coefficient_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               if( isConfidenceOut )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"<ind_confidence_out length=\\\"\" << nB*2 << \"\\\">\" << endl;" << endl;
  oIndDriver << "                  for( int i=0; i<nB*2; i++ )" << endl;
  oIndDriver << "                  {" << endl;
  oIndDriver << "                     oResults << \"   <value>\" << stdParConfidenceOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "                  }" << endl;
  oIndDriver << "                  oResults << \"</ind_confidence_out>\" << endl;" << endl;
  oIndDriver << "               }" << endl;

  oIndDriver << "               oResults << \"</ind_stat_result>\" << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_analysis_result>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         if( iSub == nRepeats-1 )" << endl;
  oIndDriver << "            oResults << set << endl;" << endl;
  oIndDriver << "         oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "         oResults.close();" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "END:" << endl;
  oIndDriver << "      // The following statement is to trick g++ compiler, which" << endl;
  oIndDriver << "      // complains that a label must be followed by a statement." << endl;
  oIndDriver << "      int dummy = 0;" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << "   catch( const SpkException & e )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      ofstream oResults;" << endl;
  oIndDriver << "      oResults.open( \"" << fResult_xml << "\", ios_base::app );" << endl;
  oIndDriver << "      if( !oResults.good() )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIndDriver << "\"" << fResult_xml << "\" );" << endl;
  oIndDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "         if(isUsingPvm)" << endl;
  oIndDriver << "            finish(ret);" << endl;
  oIndDriver << "         else" << endl;
  oIndDriver << "            cout << \"exit code = \" << ret << endl;" << endl;
  oIndDriver << "         fclose( stdout );" << endl;
  oIndDriver << "         fclose( stderr );" << endl;
  oIndDriver << "         return ret;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<error_list length=\\\"\" << e.size() << \"\\\">\" << endl;" << endl;
  oIndDriver << "      oResults << e << endl;" << endl;
  oIndDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults.close();" << endl;
  oIndDriver << "      ret = UNKNOWN_ERROR;" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << "   catch( ... )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      ofstream oResults;" << endl;
  oIndDriver << "      oResults.open( \"" << fResult_xml << "\", ios_base::app );" << endl;
  oIndDriver << "      if( !oResults.good() )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIndDriver << "\"" << fResult_xml << "\" );" << endl;
  oIndDriver << "         ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "         if(isUsingPvm)" << endl;
  oIndDriver << "            finish(ret);" << endl;
  oIndDriver << "         else" << endl;
  oIndDriver << "            cout << \"exit code = \" << ret << endl;" << endl;
  oIndDriver << "         fclose( stdout );" << endl;
  oIndDriver << "         fclose( stderr );" << endl;
  oIndDriver << "         return ret;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<error_list length=\\\"\" << 2 << \"\\\">\" << endl;" << endl;
  oIndDriver << "      oResults << \"Unexpected exceptions are caught.\" << endl;" << endl;
  oIndDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults.close();" << endl;
  oIndDriver << "      ret = UNKNOWN_FAILURE;" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << endl;
  oIndDriver << "   if(isUsingPvm)" << endl;
  oIndDriver << "      finish(ret);" << endl;
  oIndDriver << "   else" << endl;
  oIndDriver << "      cout << \"exit code = \" << ret << endl;" << endl;
  oIndDriver << "   fclose( stdout );" << endl;
  oIndDriver << "   fclose( stderr );" << endl;
  oIndDriver << "   return ret;" << endl;

  oIndDriver << "}" << endl;
  oIndDriver.close();
}
