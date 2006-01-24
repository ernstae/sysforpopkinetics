//=============================================================
// 
// Population model of line with slope and intercept random effects.
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
#include <spkpred/PopPredModel.h>

namespace NonmemPars{
using namespace std;

   //-------------------------------------------
   // Size of the population
   //-------------------------------------------
   const int nIndividuals = 10;

   //-------------------------------------------
   // THETA
   //-------------------------------------------
   // The length of THETA vector.
   const int nTheta = 2;

   // A C-arrary containing the upper boundary values for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaUp[nTheta] = { +10, +10   };
   const valarray<double> thetaUp ( c_thetaUp,  2 );

   // A C-arrary containing the lower boundary values for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaLow[nTheta] = { -10, -10   };
   const valarray<double> thetaLow( c_thetaLow, 2 );
   // A C-arrary containing the initial estimates for THETA.
   // This array is used to initializes a valarray object that follows.
   double c_thetaIn[nTheta] = { 1.08912, 0.252003 };

   // A C-arrary containing the fixation flags for THETA.
   // This array is used to initializes a valarray object that follows.
   bool c_thetaFixed[nTheta] = { 0, 0 };

   const valarray<bool> thetaFixed( c_thetaFixed, 2 );
   // A valarray object that *will* contain the initial values for THETA.
   // The object value may be replaced if a new data set is simulated.
   valarray<double> thetaIn ( c_thetaIn, nTheta );

   //-------------------------------------------
   // ETA
   //-------------------------------------------
   // The length of ETA vector, which determines the dimension of OMEGA covariance.
   const int nEta = 2;

   // A C-arrary containing the initial estimates for ETA.
   // This array is used to initializes a valarray object that follows.
   double c_etaIn[nEta] = { 0.0, 0.0 };
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
   double c_omegaIn[ omegaOrder ] = { 0.706119, 0.727531 };
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
   double c_sigmaIn[ sigmaOrder ] = { 1.62501 };
   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );

   //-------------------------------------------
   // Data Simulation
   //-------------------------------------------
   // No simulation is requested.
   const int seed = -1;

};
#endif
