//=============================================================
// 
// Linear Model: Estimation
// 
// The namespace NonmemPars exports the values 
// given by the user or values drived from the user-given values.
// 
// The user requested the population analysis.
// This means that this namespace would contain materials related to 
// all of THETA, OMEGA, ETA, SIGMA and EPS.
// 
//=============================================================
#ifndef NONMEMPARS_H
#define NONMEMPARS_H

#include <valarray>
#include <spkpred/PopPredModel.h>

namespace NonmemPars{
using namespace std;

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
   double c_thetaIn[nTheta] = { 1.15339 };

   // A C-arrary containing the fixation flags for THETA.
   // This array is used to initializes a valarray object that follows.
   bool c_thetaFixed[nTheta] = { 0 };

   const valarray<bool> thetaFixed( c_thetaFixed, 1 );
   // A valarray object containing the initial values for THETA.
   const valarray<double> thetaIn ( c_thetaIn, nTheta );

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
   const enum PopPredModel::covStruct omegaStruct = PopPredModel::DIAGONAL;

   // The dimension of OMEGA matrix is detemined by the length of ETA vector.
   const int omegaDim = nEta;

   // The order of OMEGA matrix.
   // If the matrix is full, the value is equal to the number of 
   // elements in a half triangle (diagonal elements included).
   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix.
   const int omegaOrder = omegaDim;

   // A C-arrary containing the initial estimates for OMEGA.
   // This array is used to initializes a valarray object that follows.
   double c_omegaIn[ omegaOrder ] = { 1.96734 };
   const valarray<double> omegaIn( c_omegaIn, omegaOrder );

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
   const enum PopPredModel::covStruct sigmaStruct = PopPredModel::DIAGONAL;

   // The dimension of SIGMA matrix is detemined by the length of EPS vector.
   const int sigmaDim = nEps;

   // The order of SIGMA matrix.
   // If the matrix is full, the value is equal to the number of 
   // elements in a half triangle (diagonal elements included).
   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix.
   const int sigmaOrder = sigmaDim;
   // A C-arrary containing the initial estimates for SIGMA.
   // This array is used to initializes a valarray object that follows.
   double c_sigmaIn[ sigmaOrder ] = { 0.00876342 };
   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );

   //-------------------------------------------
   // Data Simulation
   //-------------------------------------------
// No simulation is requested.
const int seed = -1;

};
#endif
