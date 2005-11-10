#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;

void NonmemTranslator::generateNonmemParsNamespace() const
{
  //---------------------------------------------------------------------------------------
  // Generate the NonmemPars namespace.
  //---------------------------------------------------------------------------------------
  const Symbol* pTheta = table->findi(KeyStr.THETA);
  const Symbol* pOmega = table->findi(KeyStr.OMEGA);
  const Symbol* pSigma = table->findi(KeyStr.SIGMA);
  const Symbol* pEta   = table->findi(KeyStr.ETA);
  ofstream oNonmemPars( fNonmemPars_h );
  if( !oNonmemPars.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fNonmemPars_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
 
  oNonmemPars << "//=============================================================" << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// " << myDescription << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// The namespace NonmemPars exports the values " << endl;
  oNonmemPars << "// given by the user or values drived from the user-given values." << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// The user requested the " << (getTarget()==POP? "population":"individual") << " analysis." << endl;
  oNonmemPars << "// This means that this namespace would contain materials related to " << endl;
  if( getTarget()==POP )
    {
      oNonmemPars << "// all of THETA, OMEGA, ETA, SIGMA and EPS." << endl;
    }
  else
    {
      oNonmemPars << "// only THETA, OMEGA and ETA." << endl;
    }
  oNonmemPars << "// It also contains the input value(s) necessary to simulate a data set " << endl;
  oNonmemPars << "// when requested." << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "//=============================================================" << endl;

  oNonmemPars << "#ifndef NONMEMPARS_H" << endl;
  oNonmemPars << "#define NONMEMPARS_H" << endl;
  oNonmemPars << endl;

  oNonmemPars << "#include <valarray>" << endl;
  if( getTarget() == POP )
    oNonmemPars << "#include <spkpred/PopPredModel.h>" << endl;
  else
    oNonmemPars << "#include <spkpred/IndPredModel.h>" << endl;
  oNonmemPars << endl;
  

  oNonmemPars << "namespace NonmemPars{" << endl;

  oNonmemPars << "using namespace std;" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // Size of the population" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   const int nIndividuals = " << getPopSize() << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // THETA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // The length of THETA vector." << endl;
  oNonmemPars << "   const int nTheta = " << myThetaLen << ";" << endl;
  oNonmemPars << endl;
  oNonmemPars << "   // A C-arrary containing the upper boundary values for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaUp[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
        oNonmemPars << ", ";
      oNonmemPars << pTheta->upper[0][j];
    }
  oNonmemPars << "   };" << endl;
  oNonmemPars << "   const valarray<double> thetaUp ( c_thetaUp,  " << myThetaLen << " );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the lower boundary values for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaLow[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->lower[0][j];
    }
  oNonmemPars << "   };" << endl;
  oNonmemPars << "   const valarray<double> thetaLow( c_thetaLow, " << myThetaLen << " );" << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaIn[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->initial[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the fixation flags for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   bool c_thetaFixed[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->fixed[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   const valarray<bool> thetaFixed( c_thetaFixed, " << myThetaLen << " );" << endl;
  oNonmemPars << "   // A valarray object that *will* contain the initial values for THETA." << endl;
  oNonmemPars << "   // The object value may be replaced if a new data set is simulated." << endl;
  oNonmemPars << "   valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // ETA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // The length of ETA vector, which determines the dimension of OMEGA covariance." << endl;
  oNonmemPars << "   const int nEta = " << myEtaLen << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for ETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_etaIn[nEta] = { ";
  for( int i=0; i<myEtaLen; i++ )
    {
      if( i > 0 )
	oNonmemPars << ", ";
      oNonmemPars << pEta->initial[0][i];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<double> etaIn( c_etaIn, nEta );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // OMEGA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   // The structure of OMEGA matrix." << endl;
  oNonmemPars << "   // \"FULL\" indicates that possibly all elements of the symmetric matrix may be non-zero." << endl;
  oNonmemPars << "   // \"DIAGONAL\" indicates that only the diagonal elements are non-zero and the rest are all zero." << endl;
     
  oNonmemPars << "   const enum " << (getTarget()==POP? "Pop":"Ind") << "PredModel::covStruct omegaStruct = ";
  oNonmemPars << (getTarget()==POP? "Pop":"Ind") << "PredModel::";
  oNonmemPars << (myOmegaStruct == Symbol::TRIANGLE? "FULL" : "DIAGONAL" ) << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The dimension of OMEGA matrix is detemined by the length of ETA vector." << endl;
  oNonmemPars << "   const int omegaDim = nEta;" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The order of OMEGA matrix." << endl;
  oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
  oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
  oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
  oNonmemPars << "   const int omegaOrder = " << (myOmegaStruct==Symbol::DIAGONAL? "omegaDim" : "omegaDim * (omegaDim+1) / 2" ) << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for OMEGA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_omegaIn[ omegaOrder ] = { "; 
  for( int j=0; j<myOmegaOrder; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pOmega->initial[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<double> omegaIn( c_omegaIn, omegaOrder );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // EPS" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   // The length of EPS vector, which determines the dimension of SIGMA." << endl;
  if( getTarget() == POP )
    {
      oNonmemPars << "   const int nEps = " << myEpsLen << ";" << endl;
    }
  else
    {
      oNonmemPars << "// NOTE:" << endl;
      oNonmemPars << "// EPS related variable(s) do not appear in this namespace" << endl;
      oNonmemPars << "// because you requested the single individual analysis." << endl;
      oNonmemPars << "// const int nEps;" << endl;
    }
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // SIGMA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  if( getTarget() == POP )
    {
      oNonmemPars << "   // The structure of SIGMA matrix." << endl;
      oNonmemPars << "   // \"FULL\" indicates that possibly all elements of the symmetric matrix may be non-zero." << endl;
      oNonmemPars << "   // \"DIAGONAL\" indicates that only the diagonal elements are non-zero and the rest are all zero." << endl;
      oNonmemPars << "   const enum PopPredModel::covStruct sigmaStruct = ";
      oNonmemPars << "PopPredModel::" << (mySigmaStruct == Symbol::TRIANGLE? "FULL" : "DIAGONAL" ) << ";" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The dimension of SIGMA matrix is detemined by the length of EPS vector." << endl;
      oNonmemPars << "   const int sigmaDim = nEps;" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The order of SIGMA matrix." << endl;
      oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
      oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
      oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
      oNonmemPars << "   const int sigmaOrder = " << (mySigmaStruct==Symbol::DIAGONAL? "sigmaDim;" : "sigmaDim * ( sigmaDim + 1 ) / 2;") << endl;

      oNonmemPars << "   // A C-arrary containing the initial estimates for SIGMA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   double c_sigmaIn[ sigmaOrder ] = { ";
      for( int j=0; j<mySigmaOrder; j++ )
	{
	  if( j>0 )
	    oNonmemPars << ", ";
	  oNonmemPars << pSigma->initial[0][j];
	}
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );" << endl;
    }
  else
    {
      oNonmemPars << "// NOTE:" << endl;
      oNonmemPars << "// SIGMA related variables do not appear in this namespace" << endl;
      oNonmemPars << "// because you requested the single individual analysis." << endl;
      oNonmemPars << "// const enum PopPredModel::covStruct sigmaStruct;" << endl;
      oNonmemPars << "// const valarray<double> sigmaIn;" << endl;
    }
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // Data Simulation" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  if( myIsSimulate )
    {
      oNonmemPars << "   // The seed for data simulation." << endl;
      oNonmemPars << "   const int seed = " << mySeed << ";" << endl;
    }
  else
    {
      oNonmemPars << "   // No simulation is requested." << endl;
      oNonmemPars << "   const int seed = -1;" << endl;      
    }
  oNonmemPars << endl;

  if( myModelSpec != PRED /* means ADVAN */ )
    {
      oNonmemPars << "   //-------------------------------------------" << endl;
      oNonmemPars << "   // ODE related" << endl;
      oNonmemPars << "   //-------------------------------------------" << endl;  
      oNonmemPars << "   const bool isPkFunctionOfT        = " << myCompModel->isPkFunctionOfT()       << ";" << endl;
      oNonmemPars << "   const int  nCompartments          = " << myCompModel->getNCompartments()      << ";";
      oNonmemPars << "  // including the output compartment (i.e. the user specified nCompartments + 1)" << endl;
      oNonmemPars << "   const int  nParameters            = " << myCompModel->getNParameters()        << ";" << endl;
      oNonmemPars << "   const int  defaultDoseComp        = " << myCompModel->getDefaultDose()        << ";" << endl;
      oNonmemPars << "   const int  defaultObservationComp = " << myCompModel->getDefaultObservation() << ";" << endl;
      oNonmemPars << "   const double relTol               = " << myCompModel->getRelTol()             << ";" << endl;
      int n = myCompModel->getNCompartments();
      vector<bool> initialOff( n );
      myCompModel->getInitialOff( initialOff );
      oNonmemPars << "   const bool c_initialOff[] = { ";
      for( int i=0; i<n; i++ )
	{
	  if( i>0 )
	    oNonmemPars << ", ";
	  oNonmemPars << initialOff[i];
	}
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const std::valarray<bool> initialOff( c_initialOff, " << n << " );" << endl;

      vector<bool> noOff( n );
      myCompModel->getNoOff( noOff );
      oNonmemPars << "   const bool c_noOff[] = { ";
      for( int i=0; i<n; i++ )
	{
	  if( i>0 )
	    oNonmemPars << ", ";
	  oNonmemPars << noOff[i];
	}
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const std::valarray<bool> noOff( c_noOff, " << n << " );" << endl;

      vector<bool> noDose( n );
      myCompModel->getNoDose( noDose );
      oNonmemPars << "   const bool c_noDose[] = { ";
      for( int i=0; i<n; i++ )
	{
	  if( i>0 )
	    oNonmemPars << ", ";
	  oNonmemPars << noDose[i];
	}
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const std::valarray<bool> noDose( c_noDose, " << n << " );" << endl;
    }

  oNonmemPars << "};" << endl;
  oNonmemPars << "#endif" << endl;
  oNonmemPars.close();
}
