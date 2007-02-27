/** 
 * @file NM_generateNonmemPars.cpp
 * Define NonmemTranslator::generateNonmemParsNamespace().
 */
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
  const Symbol* pTheta = table->find(nonmem::THETA);
  const Symbol* pOmega = table->find(nonmem::OMEGA);
  const Symbol* pSigma = table->find(nonmem::SIGMA);
  const Symbol* pEta   = table->find(nonmem::ETA);
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
  oNonmemPars << "#include <spkpred/Cov.h>" << endl;
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

  if ( !myIsIdent )
    {
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
    }

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // ETA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // The length of ETA vector, which determines the dimension of OMEGA covariance." << endl;
  oNonmemPars << "   const int nEta = " << myEtaLen << ";" << endl;
  oNonmemPars << endl;

  if ( !myIsIdent )
    {
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
    }

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // OMEGA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   // The structure of OMEGA matrix." << endl;
  oNonmemPars << "   // \"FULL\" indicates that possibly all elements of the symmetric matrix may be non-zero." << endl;
  oNonmemPars << "   // \"DIAGONAL\" indicates that only the diagonal elements are non-zero and the rest are all zero." << endl;
  oNonmemPars << "   // \"BLOCKDIAG\" indicates that blocks along the diagonal are either FULL or DIAGONAL and the rest are all zero." << endl;

  int nOmegaBlock = myOmegaStruct.size();
     
  oNonmemPars << "   const enum covStruct omegaStruct = ";
  oNonmemPars << (nOmegaBlock > 1?  "BLOCKDIAG" : (myOmegaStruct[0] == Symbol::TRIANGLE? "FULL" : "DIAGONAL" )) << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The dimension of OMEGA matrix is detemined by the length of ETA vector." << endl;
  oNonmemPars << "   const int omegaDim = nEta;" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The order of OMEGA matrix." << endl;
  oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
  oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
  oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
  oNonmemPars << "   // If the matrix is block diagonal, it is equal to the sum of the orders of the individual blocks." << endl;

  //oNonmemPars << "   const int omegaOrder = " << (myOmegaStruct[0]==Symbol::DIAGONAL? "omegaDim" : "omegaDim * (omegaDim+1) / 2" ) << ";" << endl;
  oNonmemPars << "   const int omegaOrder = " << myOmegaOrder.sum() <<  ";" << endl;
  oNonmemPars << endl;

  if ( !myIsIdent )
    {
      oNonmemPars << "   // A C-arrary containing the initial estimates for OMEGA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   double c_omegaIn[ omegaOrder ] = { ";
      for( int i=0; i<nOmegaBlock; i++ )
        {
          for( int j=0; j<myOmegaOrder[i]; j++ )
            {
              if( i+j>0 )
                oNonmemPars << ", ";
              oNonmemPars << pOmega->initial[i][j];
            }
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<double> omegaIn( c_omegaIn, omegaOrder );" << endl;
      oNonmemPars << endl;
    
      oNonmemPars << "   // A C-arrary containing the fixation flags for OMEGA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   bool c_omegaFixed[ omegaOrder ] = { ";
      for( int i=0; i<nOmegaBlock; i++ )
        { 
          for( int j=0; j<myOmegaOrder[i]; j++ )
            {
              if( i+j>0 )
                oNonmemPars << ", ";
              oNonmemPars << (pOmega->fixed[i][j]? "true":"false");
            }
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<bool> omegaFixed( c_omegaFixed, omegaOrder );" << endl;
      oNonmemPars << endl;
    }

  //Block Diagonal Omega
  oNonmemPars << "   const int nOmegaBlk = " << nOmegaBlock <<  ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   enum covStruct c_omegaBlockStruct[ nOmegaBlk ] = { ";
  for( int i=0; i<nOmegaBlock; i++ )
    {
      if( i>0 )
        oNonmemPars << ", ";
      oNonmemPars << (myOmegaStruct[i] == Symbol::TRIANGLE? "FULL" : "DIAGONAL" );
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<covStruct> omegaBlockStruct( c_omegaBlockStruct, nOmegaBlk );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   int c_omegaBlockDims[ nOmegaBlk ] = { ";
  for( int i=0; i<nOmegaBlock; i++ )
    {
      if( i>0 )
        oNonmemPars << ", ";
      oNonmemPars << myOmegaDim[i];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<int> omegaBlockDims( c_omegaBlockDims, nOmegaBlk );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   bool c_omegaBlockSameAsPrev[ nOmegaBlk ] = { ";
  for( int i=0; i<nOmegaBlock; i++ )
    {
      if( i>0 )
        oNonmemPars << ", ";
      oNonmemPars << (myOmegaSameAsPrev[i]? "true":"false");
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<bool> omegaBlockSameAsPrev( c_omegaBlockSameAsPrev, nOmegaBlk );" << endl;
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
      oNonmemPars << "   // \"BLOCKDIAG\" indicates that blocks along the diagonal are either FULL or DIAGONAL and the rest are all zero." << endl;
      
      int nSigmaBlock = mySigmaStruct.size();

      oNonmemPars << "   const enum covStruct sigmaStruct = ";
      oNonmemPars << (nSigmaBlock > 1?  "BLOCKDIAG" : (mySigmaStruct[0] == Symbol::TRIANGLE? "FULL" : "DIAGONAL" )) << ";" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The dimension of SIGMA matrix is detemined by the length of EPS vector." << endl;
      oNonmemPars << "   const int sigmaDim = nEps;" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The order of SIGMA matrix." << endl;
      oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
      oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
      oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
      oNonmemPars << "   // If the matrix is block diagonal, it is equal to the sum of the orders of the individual blocks." << endl;
      
      //oNonmemPars << "   const int sigmaOrder = " << (mySigmaStruct==Symbol::DIAGONAL? "sigmaDim;" : "sigmaDim * ( sigmaDim + 1 ) / 2;") << endl;
      oNonmemPars << "   const int sigmaOrder = " << mySigmaOrder.sum() <<  ";" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // A C-arrary containing the initial estimates for SIGMA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   double c_sigmaIn[ sigmaOrder ] = { ";
      for( int i=0; i<nSigmaBlock; i++ )
        {
          for( int j=0; j<mySigmaOrder[i]; j++ )
            {
              if( i+j>0 )
                oNonmemPars << ", ";
              oNonmemPars << pSigma->initial[i][j];
            }
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // A C-arrary containing the fixation flags for SIGMA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   bool c_sigmaFixed[ sigmaOrder ] = { ";
      for( int i=0; i<nSigmaBlock; i++ )
        { 
          for( int j=0; j<mySigmaOrder[i]; j++ )
            {
              if( i+j>0 )
                oNonmemPars << ", ";
              oNonmemPars << (pSigma->fixed[i][j]? "true":"false");
            }
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<bool> sigmaFixed( c_sigmaFixed, sigmaOrder );" << endl;
      oNonmemPars << endl;

      //Block Diagonal SIGMA
      oNonmemPars << "   const int nSigmaBlk = " << nSigmaBlock <<  ";" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   enum covStruct c_sigmaBlockStruct[ nSigmaBlk ] = { ";
      for( int i=0; i<nSigmaBlock; i++ )
        {
          if( i>0 )
            oNonmemPars << ", ";
          oNonmemPars << (mySigmaStruct[i] == Symbol::TRIANGLE? "FULL" : "DIAGONAL" );
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<covStruct> sigmaBlockStruct( c_sigmaBlockStruct, nSigmaBlk );" << endl;
      oNonmemPars << endl;
      
      oNonmemPars << "   int c_sigmaBlockDims[ nSigmaBlk ] = { ";
      for( int i=0; i<nSigmaBlock; i++ )
        {
          if( i>0 )
            oNonmemPars << ", ";
          oNonmemPars << mySigmaDim[i];
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<int> sigmaBlockDims( c_sigmaBlockDims, nSigmaBlk );" << endl;
      oNonmemPars << endl;
      
      oNonmemPars << "   bool c_sigmaBlockSameAsPrev[ nSigmaBlk ] = { ";
      for( int i=0; i<nSigmaBlock; i++ )
        {
          if( i>0 )
            oNonmemPars << ", ";
          oNonmemPars << (mySigmaSameAsPrev[i]? "true":"false");
        }
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<bool> sigmaBlockSameAsPrev( c_sigmaBlockSameAsPrev, nSigmaBlk );" << endl;
      oNonmemPars << endl;
      // end sigma block
    }
  else
    {
      oNonmemPars << "// NOTE:" << endl;
      oNonmemPars << "// SIGMA related variables do not appear in this namespace" << endl;
      oNonmemPars << "// because you requested the single individual analysis." << endl;
      oNonmemPars << "// const enum covStruct sigmaStruct;" << endl;
      oNonmemPars << "// const valarray<double> sigmaIn;" << endl;
    }
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // Data Simulation" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  if( myIsSimulate || myIsNonparam )
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
      if ( !myIsIdent )
        {
          oNonmemPars << "   const double relTol               = " << myCompModel->getRelTol()             << ";" << endl;
        }
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
      oNonmemPars << endl;
    }

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // Nonparametric Estimation Information" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   const int nBMeasurePointIn       = " << myNonparamRandomMeasurePointIn << ";" << endl;  
  oNonmemPars << "   const int nBMeasurePointPerDimIn = " << myNonparamGridMeasurePointPerSideIn << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "};" << endl;
  oNonmemPars << "#endif" << endl;
  oNonmemPars.close();
}
