/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************
*/
//=======================================================================
//
// namespace nonmemEqvContainers
//
// This namespace declares or/defines constant values, variables and
// container classes that hold NONMEM-equivalent Spk results
// and provide methods to store them in a file in the xml format
// accepted by the compareSpkToNonmem.pl perl script.
//
// Author: Sachiko Honda
// Date:   01/15/2003
//
//=======================================================================
#ifndef NONMEM_EQN_CONTAINERS_H
#define NONMEM_EQN_CONTAINERS_H
#pragma warning( disable : 4786 )

#include <string>
#include <iostream>
#include <cmath>
#include <limits>

#include <spk/SpkValarray.h>
#include <spk/pi.h>
#include <spk/Objective.h>

namespace nonmemEqvContainers
{
  using SPK_VA::valarray;
  using namespace std;

  //---------------------------------------------------------------------
  //
  // DIRECT:   indicates the parameterization of a variable is
  //           the straight same as NONMEM's.
  //
  // INDIRECT: indicates otherwise.
  //
  //---------------------------------------------------------------------
  #define DIRECT   true
  #define INDIRECT false

  //---------------------------------------------------------------------
  //
  // "no_value": is placed where a value does not exist.
  //
  //---------------------------------------------------------------------
  const string NO_VAL = "no_value";

  //---------------------------------------------------------------------
  //
  // These constant strings are defined simply for convenience.
  //
  //---------------------------------------------------------------------
  const string OpenLB  = "<";
  const string CloseLB = "</";
  const string RB      = ">";

  //---------------------------------------------------------------------
  //
  // Function declarations
  //
  //---------------------------------------------------------------------
  int factorial( int x );

  //---------------------------------------------------------------------
  //
  // Class declarations
  //
  //---------------------------------------------------------------------    
  class Source
  {
  public:
    const string tag;
    const string version;
    const string driver;
    Source( const string & versionIn, const string & driverIn )
      : tag( "source" ), version( versionIn ), driver( driverIn )
    {}
    const string xml() const;
  };

  class Status
  {
  public:
    const string tag;
    bool isParamEstimateAttempted;
    bool isParamEstimateSucceeded;
    bool isStdErrorAttempted;
    bool isStdErrorSucceeded;

    Status( bool isParamEstimateAttemptedIn, bool isParamEstimateSucceededIn, bool isStdErrorAttemptedIn, bool isStdErrorSucceededIn )
      : isParamEstimateAttempted( isParamEstimateAttemptedIn ), 
        isParamEstimateSucceeded( isParamEstimateSucceededIn ),
        isStdErrorAttempted     ( isStdErrorAttemptedIn ),
        isStdErrorSucceeded     ( isStdErrorSucceededIn ),
        tag                     ( "status" )
    {}
    const string xml() const;
  };

  class Message
  {
  public:
    const string tag;
    string message;
    Message()
      : tag( "messages" )
    {}
    friend Message& operator+=( Message& left, const string& str );
    friend Message& operator+=( Message& left, const SpkException& e );
    const string xml() const;
  };

  class PopEpsilon
  {
  public:
    const string tag;
    const double epsilon;

    PopEpsilon( double valIn )
      : 
      tag( "popEpsilon" ),
      epsilon( valIn )
    {}
    const string xml() const;
  };

  class NonmemObjective
  {
  public:
    static double convertFromSpkObj( double lTilde, int nYTotal )
    {
      double val = 2.0 * lTilde - nYTotal * log(2.0 * PI);
      return val;
    }

    const string tag;
    const double relTol;
    enum Objective approximation;
    double val;

    NonmemObjective( double tol, enum Objective approxIn, double valIn )
     : 
     tag          ( "objective" ),
     relTol       ( tol ),
     approximation( approxIn ),
     val          ( valIn )
    {}
    const string xml( bool isEstimateValid ) const;
  };

  class Theta
  {
  public:

    const string tag;
    const int len;
    const double relTol;
    valarray<bool>   parameterization;
    valarray<double> low;
    valarray<double> in;
    valarray<double> up;
    valarray<double> out;
    valarray<double> stdError;

    Theta( int nParamIn, double relTolIn )
      :
      tag     ( "theta" ),
      len     ( nParamIn ),
      relTol  ( relTolIn )
    {
        parameterization.resize( len, false );
        low.resize             ( len,  numeric_limits<double>::quiet_NaN() );
        in.resize              ( len,  numeric_limits<double>::quiet_NaN() );
        up.resize              ( len,  numeric_limits<double>::quiet_NaN() );
        out.resize             ( len,  numeric_limits<double>::quiet_NaN() );
        stdError.resize        ( len,  numeric_limits<double>::quiet_NaN() );
    }

    const string xml( bool isEstimateValid, bool isStdErrorValid )  const;
  };

  class Sigma
  {
  public:

    enum STRUCT{ DIAGONAL, FULL };

    const string tag;
    const int    len;
    const double relTol;
    const enum   STRUCT structure;
    valarray<bool>   parameterization;
    valarray<double> low;
    valarray<double> up;
    valarray<double> in;
    valarray<double> out;
    valarray<double> stdError;

    Sigma( int lenIn, enum STRUCT structureIn, double relTolIn )
      : 
      tag             ( "Sigma" ), 
      len             ( lenIn ),
      structure       ( structureIn ), 
      relTol          ( relTolIn ),
      parameterization( false, len * len ),
      in              ( len * len ),
      low             ( len * len ),
      up              ( len * len ),
      out             ( len * len ),
      stdError        ( len * len )
    {}

    const string xml( bool isEstimateValid, bool isStdErrorValid ) const;
  };
  class Omega
  {
  public:
    enum STRUCT { DIAGONAL, FULL };

    const string     tag;
    enum STRUCT      structure;
    const double     relTol;
    const int        len;

    valarray<bool>   parameterization;
    valarray<double> in;
    valarray<double> low;
    valarray<double> up;
    valarray<double> out;
    valarray<double> stdError;

    Omega( int lenIn, enum STRUCT structureIn, double relTolIn ) 
      : 
      tag             ( "Omega" ),
      len             ( lenIn ),
      structure       ( structureIn ),
      relTol          ( relTolIn ),
      parameterization( false, len * len ),
      in              ( len * len ),
      low             ( len * len ),
      up              ( len * len ),
      out             ( len * len ),
      stdError        ( len * len )
    {}
    const string xml( bool isEstimateValid, bool isStdErrorValid ) const;
  };

  class CovarianceOfEstimate
  {
  public:
    const string tag;
    const int    nParameters;

    valarray<double> out;
    valarray<bool>   parameterization;

    CovarianceOfEstimate( int nParametersIn ) 
      : 
      tag             ( "covarianceOfEstimate" ),
      nParameters     ( nParametersIn ),
      parameterization( false, nParameters * nParameters ),
      out             ( nParameters * nParameters ) 
    {}
    const string xml( bool isCovarianceValid ) const;
  };

  class CorrelationOfEstimate
  {
  public:
    const string tag;
    const int    nParameters;

    valarray<double> out;
    valarray<bool>   parameterization;

    CorrelationOfEstimate( int nParametersIn ) 
      : 
      tag             ( "correlationOfEstimate" ),
      nParameters     ( nParametersIn ),
      out             ( nParameters * nParameters ),
      parameterization( false, nParameters * nParameters )
    {}
    const string xml( bool isCorrelationValid ) const;
  };

  class Eta
  {
  public:
    const string tag;
    const int nParameters;
    const int nIndividuals;
    const double relTol;

    valarray<bool>   parameterization;
    valarray<double> in;
    valarray<double> out;

    Eta( int nParamIn, int nIndividualsIn, double relTolIn )
      :
      tag             ( "etaForAllIndividuals" ),
      nParameters     ( nParamIn ),
      nIndividuals    ( nIndividualsIn ),
      relTol          ( relTolIn ),
      parameterization( false, nParameters ),
      in              ( nParameters * nIndividuals ),
      out             ( nParameters * nIndividuals )
    {
    }
    const string xml( bool isEstimateValid ) const;
  };

  class Prediction
  {
  public:
    const string tag;
    const int nIndividuals;
    const valarray<int> N;
    const double relTol;

    valarray<double> out;

    Prediction( const valarray<int> & NIn, double relTolIn )
      :
      tag         ( "predForAllIndividuals" ),
      nIndividuals( NIn.size() ),
      N           ( NIn ),
      relTol      ( relTolIn ),
      out         ( N.sum() )
    {}
    const string xml( bool isEstimateValid ) const;
  };
};

#endif
