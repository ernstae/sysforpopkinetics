//=============================================================
// 
// Linear Model: FO 100
// 
// The namespace NonmemPars exports the values 
// given by the user or values drived from the user-given values.
// 
// The user requested the population analysis.
// This means that this namespace would contain materials related to 
// all of THETA, OMEGA, ETA, SIGMA and EPS.
// It also contains the input value(s) necessary to simulate a data set 
// when requested.
// 
//=============================================================
#ifndef NONMEMPARS_H
#define NONMEMPARS_H

#include <valarray>
#include <spkpred/Cov.h>

namespace NonmemPars{
using namespace std;

   //-------------------------------------------
   // Size of the population
   //-------------------------------------------
   const int nIndividuals = 100;

   //-------------------------------------------
   // THETA
   //-------------------------------------------
   // The length of THETA vector.
   const int nTheta = 1;

   // A C-arrary containing the upper boundary values for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaUp[nTheta] = { +10   };
   const valarray<double> thetaUp ( c_thetaUp,  1 );

   // A C-arrary containing the lower boundary values for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaLow[nTheta] = { -10   };
   const valarray<double> thetaLow( c_thetaLow, 1 );
   // A C-arrary containing the initial estimates for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaIn[nTheta] = { 0.977013 };

   // A C-arrary containing the fixation flags for THETA.
   // This array is used to initializes a valarray object that follows.
   bool c_thetaFixed[nTheta] = { 0 };

   const valarray<bool> thetaFixed( c_thetaFixed, 1 );
   // A valarray object that *will* contain the initial values for THETA.
   // The object value may be replaced if a new data set is simulated.
   valarray<double> thetaIn ( c_thetaIn, nTheta );

   //-------------------------------------------
   // ETA
   //-------------------------------------------
   // The length of ETA vector, which determines the dimension of OMEGA covariance.
   const int nEta = 1;

   // A C-arrary containing the initial estimates for ETA.
   // This array is used to initializes a valarray object that follows.
   double c_etaIn[nEta] = { 0.0 };
   const valarray<double> etaIn( c_etaIn, nEta );

   //-------------------------------------------
   // OMEGA
   //-------------------------------------------
   // The structure of OMEGA matrix.
   // "FULL" indicates that possibly all elements of the symmetric matrix may be non-zero.
   // "DIAGONAL" indicates that only the diagonal elements are non-zero and the rest are all zero.
   // "BLOCKDIAG" indicates that blocks along the diagonal are either FULL or DIAGONAL and the rest are all zero.
   const enum covStruct omegaStruct = DIAGONAL;

   // The dimension of OMEGA matrix is detemined by the length of ETA vector.
   const int omegaDim = nEta;

   // The order of OMEGA matrix.
   // If the matrix is full, the value is equal to the number of 
   // elements in a half triangle (diagonal elements included).
   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix.
   // If the matrix is block diagonal, it is equal to the sum of the orders of the individual blocks.
   const int omegaOrder = 1;

   // A C-arrary containing the initial estimates for OMEGA.
   // This array is used to initializes a valarray object that follows.
   double c_omegaIn[ omegaOrder ] = { 1.11871 };
   const valarray<double> omegaIn( c_omegaIn, omegaOrder );

   // A C-arrary containing the fixation flags for OMEGA.
   // This array is used to initializes a valarray object that follows.
   bool c_omegaFixed[ omegaOrder ] = { false };
   const valarray<bool> omegaFixed( c_omegaFixed, omegaOrder );

   const int nOmegaBlk = 1;

   enum covStruct c_omegaBlockStruct[ nOmegaBlk ] = { DIAGONAL };
   const valarray<covStruct> omegaBlockStruct( c_omegaBlockStruct, nOmegaBlk );

   int c_omegaBlockDims[ nOmegaBlk ] = { 1 };
   const valarray<int> omegaBlockDims( c_omegaBlockDims, nOmegaBlk );

   bool c_omegaBlockSameAsPrev[ nOmegaBlk ] = { false };
   const valarray<bool> omegaBlockSameAsPrev( c_omegaBlockSameAsPrev, nOmegaBlk );

   //-------------------------------------------
   // EPS
   //-------------------------------------------
   // The length of EPS vector, which determines the dimension of SIGMA.
   const int nEps = 1;

   //-------------------------------------------
   // SIGMA
   //-------------------------------------------
   // The structure of SIGMA matrix.
   // "FULL" indicates that possibly all elements of the symmetric matrix may be non-zero.
   // "DIAGONAL" indicates that only the diagonal elements are non-zero and the rest are all zero.
   // "BLOCKDIAG" indicates that blocks along the diagonal are either FULL or DIAGONAL and the rest are all zero.
   const enum covStruct sigmaStruct = DIAGONAL;

   // The dimension of SIGMA matrix is detemined by the length of EPS vector.
   const int sigmaDim = nEps;

   // The order of SIGMA matrix.
   // If the matrix is full, the value is equal to the number of 
   // elements in a half triangle (diagonal elements included).
   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix.
   // If the matrix is block diagonal, it is equal to the sum of the orders of the individual blocks.
   const int sigmaOrder = 1;

   // A C-arrary containing the initial estimates for SIGMA.
   // This array is used to initializes a valarray object that follows.
   double c_sigmaIn[ sigmaOrder ] = { 0.0114758 };
   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );

   // A C-arrary containing the fixation flags for SIGMA.
   // This array is used to initializes a valarray object that follows.
   bool c_sigmaFixed[ sigmaOrder ] = { false };
   const valarray<bool> sigmaFixed( c_sigmaFixed, sigmaOrder );

   const int nSigmaBlk = 1;

   enum covStruct c_sigmaBlockStruct[ nSigmaBlk ] = { DIAGONAL };
   const valarray<covStruct> sigmaBlockStruct( c_sigmaBlockStruct, nSigmaBlk );

   int c_sigmaBlockDims[ nSigmaBlk ] = { 1 };
   const valarray<int> sigmaBlockDims( c_sigmaBlockDims, nSigmaBlk );

   bool c_sigmaBlockSameAsPrev[ nSigmaBlk ] = { false };
   const valarray<bool> sigmaBlockSameAsPrev( c_sigmaBlockSameAsPrev, nSigmaBlk );


   //-------------------------------------------
   // Data Simulation
   //-------------------------------------------
   // No simulation is requested.
   const int seed = -1;

};
#endif
