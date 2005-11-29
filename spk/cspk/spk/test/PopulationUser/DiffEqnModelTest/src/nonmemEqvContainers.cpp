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
#pragma warning( disable : 4786 )

#include <string>
#include <iostream>
#include <cmath>
#include <limits>
#include "../../../../spk/SpkValarray.h"
#include "../../../../spk/pi.h"
#include "../../../../spk/Objective.h"

#include "nonmemEqvContainers.h"

#ifdef __GNUC__
    #include <sstream>
#elif WIN32
    #include <strstream>
#endif

using SPK_VA::valarray;
using namespace std;
using namespace nonmemEqvContainers;

//---------------------------------------------------------------------
//
// int factorial( int a )
//
// This function returns a!.
//
//---------------------------------------------------------------------
int nonmemEqvContainers::factorial( int x )
{
  if( x == 1 )
    return x;
  else
    return x + factorial( x - 1 );
}

//---------------------------------------------------------------------
//
// class Source
// 
// xml() produces the following text:
//
// <Source>
//      <spkVersion>VERSION</spkVersion>
//      <driver>DRIVER</driver>
// </Source>
//
// 
// VERSION: a text indicating the version of Spk.lib.
//
// DRIVER:  a text indicating the name of the driver.
//          ("DiffEqnModelTest" is the only valid value)
//
//---------------------------------------------------------------------    
const string nonmemEqvContainers::Source::xml() const
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#else
  cerr << "An equivalent of strstream(vc++)/stringstring(g++) not found!" << endl;
  exit(-1);
#endif

  s << OpenLB << tag << RB << endl;
  s << "\t" << "<spkVersion>" << version << "</spkVersion>" << endl;
  s << "\t" << "<driver>"     << driver  << "</driver>"     << endl;

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class Status
//
// xml() produces the following text:
//
// <status>
//    <paramEstimationStep>
//      <attempted>ESTIMATE_ATTEMPTED</attempated>
//      <succeeded>ESTIMATE_SUCCEEDED</succeeded>
//    </paramEstimationStep>
//    <standardErrorsStep>
//      <attempted>STDERROR_COMP_ATTEMPTED</attempated>
//      <succeeded>STDERROR_COMP_SUCCEEDED</succeeded>
//    </standardErrorsStep>
// </status>
//
//
// ESTIMATE_ATTEMPTED: is "true" if the paramater estimation was
//                     ever attempted.  "false" otherwise.
// ESTIMATE_SUCCEEDED: is "true" if the parameter estimation was
//                     successfully completed.  "false" otherwise.
// STDERROR_COMP_ATTEMPTED:
//                     is "true" if the standard error was ever
//                     attempted to be computed.
//                     "false" otherwise.
// STDERROR_COMP_SUCCEEDED:
//                     is "true" if the computation of standard error
//                     completed successfully.  "false" otherwise.
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Status::xml() const
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  s << OpenLB << tag << RB << endl;

  s << "\t" << "<paramEstimationStep>" << endl;
  s << "\t" << "\t" << "<attempted>" << ( isParamEstimateAttempted? "true" : "false" ) << "</attempted>" << endl;
  s << "\t" << "\t" << "<succeeded>" << ( isParamEstimateSucceeded? "true" : "false" ) << "</succeeded>" << endl;
  s << "\t" << "</paramEstimationStep>" << endl;

  s << "\t" << "<standardErrorsStep>" << endl;
  s << "\t" << "\t" << "<attempted>" << ( isStdErrorAttempted? "true" : "false" ) << "</attempted>" << endl;
  s << "\t" << "\t" << "<succeeded>" << ( isStdErrorSucceeded? "true" : "false" ) << "</succeeded>" << endl;
  s << "\t" << "</standardErrorsStep>" << endl;

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class Message
//
// xml() produces the following text:
//
// <messages>
//      TEXT
// </messages>
// 
//
// TEXT: could be of any length and can contain escape caracters.
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Message::xml() const
{

#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;
  if( message.length() > 0 )
    s << message << endl;
  else
    s << NO_VAL << endl;
  s << CloseLB << tag << RB << endl;
  return s.str();
}

nonmemEqvContainers::Message& nonmemEqvContainers::operator+=( nonmemEqvContainers::Message& left, const string& str )
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  if( left.message.length() > 0 )
    s << left.message << endl;
  s << str << endl;
  left.message = s.str();
  return left;
}
nonmemEqvContainers::Message& nonmemEqvContainers::operator+=( nonmemEqvContainers::Message& left, const SpkException& e )
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  if( left.message.length() > 0 )
    s << endl;
  s << e << endl;
  left.message += s.str();
  return left;
}

//---------------------------------------------------------------------
//
// class PopEpsilon
//
// xml() produces the following text:
//
// <popEpsilon>EPSILON</popEpsilon>
//
//
// EPSILON: is the epsilon tolerance value used
// during the population level optimization.
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::PopEpsilon::xml() const
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  s << OpenLB << tag << RB << epsilon << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class NonmemObjective
//
// xml() produces the following text:
//
// <objective>
//      <approximation>APPROXIMATION</approximation>
//      <nonmemEquiv>OBJECTIVE_VALUE</nonmemEquiv>
//      <relTol>TOLERANCE</relTol>
// </objective>
//
//
// APPROXIMATION: indicates an approximation method.
//                "FIRST_ORDER" is placed when using FO,
//                "EXPECTD_HESSIAN" is placed for FOCE and
//                "MODIFIED_LAPLACE" is place for Laplace.
//                
// OBJECTIVE_VALUE: is the NONMEM-equivalent final objective value.
// 
// TOLERANCE:     is a relative tolerance used to determine
//                whether the Spk result agrees with NONMEM.
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::NonmemObjective::xml( bool isEstimateValid ) const
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  s << OpenLB << tag << RB << endl;

  s << "\t" << "<approximation>";
  switch( approximation )
  {
  case MODIFIED_LAPLACE:
    s << "MODIFIED_LAPLACE";
    break;
  case FIRST_ORDER:
    s << "FIRST_ORDER";
    break;
  case NAIVE_FIRST_ORDER:
    s << "NAIVE_FIRST_ORDER";
    break;
  case EXPECTED_HESSIAN:
    s << "EXPECTED_HESSIAN";
    break;
  default:
    break;

  }
  s << "</approximation>" << endl;
  s << "\t" << "<nonmemEquiv>";
  if( isEstimateValid )
    s << val;
  else
    s << NO_VAL;
  s << "</nonmemEquiv>" << endl;
  s << "\t" << "<relTol>"        << relTol  << "</relTol>"        << endl;
  s << CloseLB << tag << RB << endl;

  return s.str();
}

//---------------------------------------------------------------------
//
// class Theta
//
// xml() produces the following text:
//
// <theta>
//      <nonmemParam>
//          <anon>TF1</anon><anon>TF2</anon><anon>TF3</anon>
//      </nonmemParam>
//      <low>
//          <anon>LOW1</anon><anon>DOUBLE</anon><anon>DOUBLE</anon>
//      </low>
//      <up>
//          <anon>UP1</anon><anon>DOUBLE</anon><anon>DOUBLE</anon>
//      </up>
//      <out>
//          <anon>OUT1</anon><anon>DOUBLE</anon><anon>DOUBLE</anon>
//      </out>
//      <standardErrors>
//          <anon>STD1</anon><anon>STD2</anon><anon>STD3</anon>
//      </standardErrors>
//      <relTol>TOLERANCE</relTol>
// </theta>
//
//
// TFi : is "true" if the corresponding parameter, say theta(i),
//       is implemented in Spk in the exact same way.
// 
// LOWi: is a double-precision number corresponding the lower
//       boundary value for theta(i).
//
// UPi:  is a double-precision number corresponding the upper
//       boundary value for theta(i).
//
// OUTi: is a double-precision number corresponding the output
//       value for theta(i). 
//
// STDi: is a double-precision number corresponding to the
//       standard error of theta(i) if the parameterization is
//       direct.  If parameterization not direct or the standard error
//       computation had failed, this will contain a string "no_value".
// 
// TOLERANCE: is a relative tolerance used to determine
//            whether the Spk result agrees with NONMEM.
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Theta::xml( bool isEstimateValid, bool isStdErrorValid )  const
{
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  s << OpenLB << tag << RB << endl;

  s << "\t" << "<nonmemParam>" << endl;

  s << "\t" << "\t";
  for( int i=0; i<3; i++ )
  {
    s << "<anon>" << ( parameterization[i] == DIRECT? "true" : "false" ) << "</anon>";
  }
  s << endl;
  s << "\t" << "</nonmemParam>" << endl;

  s << "\t" << "<low>" << endl;
  s << "\t" << "\t";
  for(int i=0; i<len; i++ )
  {
    s << "<anon>";
    if( parameterization[i] == DIRECT )
      s << low[i];
    else
      s << NO_VAL;
    s << "</anon>";
  }
  s << endl;
  s << "\t" << "</low>" << endl;

  s << "\t" << "<up>" << endl;
  s << "\t" << "\t";
  for(int i=0; i<len; i++ )
  {
    s << "<anon>";
    if( parameterization[i] == DIRECT )
      s << up[i];
    else
      s << NO_VAL;
    s << "</anon>";
  }
  s << endl;
  s << "\t" << "</up>" << endl;

  s << "\t" << "<out>" << endl;
  s << "\t" << "\t";
  for(int i=0; i<len; i++ )
  {
    s << "<anon>";
    if( isEstimateValid )
      s << out[i];
    else
      s << NO_VAL;
    s << "</anon>";
  }
  s << endl;
  s << "\t" << "</out>" << endl;

  s << "\t" << "<standardErrors>" << endl;
  s << "\t" << "\t";
  for(int i=0; i<len; i++ )
  {
    s << "<anon>";
    if( isStdErrorValid )
      s << stdError[i];
    else
      s << NO_VAL;
    s << "</anon>";
  }
  s << endl;
  s << "\t" << "</standardErrors>" << endl;

  s << "\t" << "<relTol>" << relTol << "</relTol>" << endl;

  s << CloseLB << tag << RB << endl;

  return s.str();
}

//---------------------------------------------------------------------
//
// class Sigma
//
// xml() produces different oututs depending on the structure of
// Sigma: diagonal or full-symmetric.
//
//
// IMPORTANT: The Sigma::in, low, up, out vectors holds
// all values in a *full* n by n matrix in *row* major.
// In other words, unlike the produced output, they are not
// the lower-half of the matrix only.
// 
//
// The following text will be produced when it is diagonal and 
// #of parameters (Sigma) is 3.
//
// <Sigma>
//    <structure>diagonal</structure>
//    <nonmemParam>
//        <anon>TF_SIGMA11</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_SIGMA22</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_SIGMA33</anon>
//    </nonmemParam>
//    <low>
//        <anon>LOW11</anon>
//    </low>
//    <low>
//        <anon>LOW22</anon>
//    </low>
//    <low>
//        <anon>LOW33</anon>
//    </low>
//    <up>
//        <anon>UP11</anon>
//    </up>
//    <up>
//        <anon>UP22</anon>
//    </up>
//    <up>
//        <anon>UP33</anon>
//    </up>
//    <out>
//        <anon>OUT11</anon>
//    </out>
//    <out>
//        <anon>OUT22</anon>
//    </out>
//    <out>
//        <anon>OUT33</anon>
//    </out>
//    <standardErrors>
//        <anon>STD_SIGMA11</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_SIGMA22</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_SIGMA33</anon>
//    </standardErrors>
// </Sigma>
//
// The following text will be produced when it is full-symmetric and 
// #of parameters (Sigma) is 3.
//
// <Sigma>
//    <structure>diagonal</structure>
//    <nonmemParam>
//        <anon>TF_SIGMA11</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_SIGMA21</anon><anon>TF_SIGMA22</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_SIGMA31</anon><anon>TF_SIGMA32</anon><anon>TF_SIGMA33</anon>
//    </nonmemParam>
//    <low>
//        <anon>LOW11</anon>
//    </low>
//    <low>
//        <anon>LOW21</anon><anon>LOW22</anon>
//    </low>
//    <low>
//        <anon>LOW31</anon><anon>LOW32</anon><anon>LOW33</anon>
//    </low>
//    <up>
//        <anon>UP11</anon>
//    </up>
//    <up>
//        <anon>UP21</anon><anon>UP22</anon>
//    </up>
//    <up>
//        <anon>UP31</anon><anon>UP32</anon><anon>UP33</anon>
//    </up>
//    <out>
//        <anon>OUT11</anon>
//    </out>
//    <out>
//        <anon>OUT21</anon><anon>OUT22</anon>
//    </out>
//    <out>
//        <anon>OUT31</anon><anon>OUT32</anon><anon>OUT33</anon>
//    </out>
//    <standardErrors>
//        <anon>STD_SIGMA11</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_SIGMA21</anon><anon>STD_SIGMA22</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_SIGMA31</anon><anon>STD_SIGMA32</anon><anon>STD_SIGMA33</anon>
//    </standardErrors>
//    <relTol>TOLERANCE</relTol>
// </Sigma>
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Sigma::xml( bool isEstimateValid, bool isStdErrorValid ) const
{
  int i;
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif

  s << OpenLB << tag << RB << endl;

  s << "\t" << "<structure>";
  if( structure == DIAGONAL )
  {
    s << "diagonal";
  }
  else
    s << "full";
  s << "</structure>" << endl;

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<nonmemParam>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>" << (parameterization[i]==DIRECT? "true" : "false" ) << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>" << (parameterization[i]==DIRECT? "true" : "false" ) << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</nonmemParam>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<low>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( parameterization[i] == DIRECT )
        s << low[i + i*len];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( parameterization[i] == DIRECT )
          s << low[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }

    s << endl;
    s << "\t" << "</low>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<up>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( parameterization[i] == DIRECT )
        s << up[i + i*len];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( parameterization[i] == DIRECT )
          s << up[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</up>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( isEstimateValid )
        s << out[i+i*len];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( isEstimateValid )
          s << out[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<standardErrors>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( isStdErrorValid )
        s << stdError[i];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( isStdErrorValid )
          s << stdError[i];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</standardErrors>" << endl;
  }

  s << "\t" << "<relTol>" << relTol << "</relTol>" << endl;

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class Omega
//
// xml() produces different oututs depending on the structure of
// Omega: diagonal or full-symmetric.
//
//
// IMPORTANT: The Sigma::in, low, up, out vectors holds
// all values in a *full* n by n matrix in *row* major.
// In other words, unlike the produced output, they are not
// the lower-half of the matrix only.
//
//
// The following text will be produced when it is diagonal and 
// #of parameters (Omega) is 3.
//
// <Omega>
//    <structure>diagonal</structure>
//    <nonmemParam>
//        <anon>TF_OMEGA11</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_OMEGA22</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_OMEGA33</anon>
//    </nonmemParam>
//    <low>
//        <anon>LOW11</anon>
//    </low>
//    <low>
//        <anon>LOW22</anon>
//    </low>
//    <low>
//        <anon>LOW33</anon>
//    </low>
//    <up>
//        <anon>UP11</anon>
//    </up>
//    <up>
//        <anon>UP22</anon>
//    </up>
//    <up>
//        <anon>UP33</anon>
//    </up>
//    <out>
//        <anon>OUT11</anon>
//    </out>
//    <out>
//        <anon>OUT22</anon>
//    </out>
//    <out>
//        <anon>OUT33</anon>
//    </out>
//    <standardErrors>
//        <anon>STD_OMEGA11</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_OMEGA22</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_OMEGA33</anon>
//    </standardErrors>
// </Omega>
//
// The following text will be produced when it is full-symmetric and 
// #of parameters (Omega) is 3.
//
// <Omega>
//    <structure>diagonal</structure>
//    <nonmemParam>
//        <anon>TF_OMEGA11</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_OMEGA21</anon><anon>TF_OMEGA22</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF_OMEGA31</anon><anon>TF_OMEGA32</anon><anon>TF_OMEGA33</anon>
//    </nonmemParam>
//    <low>
//        <anon>LOW11</anon>
//    </low>
//    <low>
//        <anon>LOW21</anon><anon>LOW22</anon>
//    </low>
//    <low>
//        <anon>LOW31</anon><anon>LOW32</anon><anon>LOW33</anon>
//    </low>
//    <up>
//        <anon>UP11</anon>
//    </up>
//    <up>
//        <anon>UP21</anon><anon>UP22</anon>
//    </up>
//    <up>
//        <anon>UP31</anon><anon>UP32</anon><anon>UP33</anon>
//    </up>
//    <out>
//        <anon>OUT11</anon>
//    </out>
//    <out>
//        <anon>OUT21</anon><anon>OUT22</anon>
//    </out>
//    <out>
//        <anon>OUT31</anon><anon>OUT32</anon><anon>OUT33</anon>
//    </out>
//    <standardErrors>
//        <anon>STD_OMEGA11</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_OMEGA21</anon><anon>STD_OMEGA22</anon>
//    </standardErrors>
//    <standardErrors>
//        <anon>STD_OMEGA31</anon><anon>STD_OMEGA32</anon><anon>STD_OMEGA33</anon>
//    </standardErrors>
//    <relTol>TOLERANCE</relTol>
// </Sigma>
//
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Omega::xml( bool isEstimateValid, bool isStdErrorValid ) const
{
  int i;

#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;
  
  s << "\t" << "<structure>" << ( structure == DIAGONAL? "diagonal" : "full" ) << "</structure>" << endl;

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<nonmemParam>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>" << ( parameterization[i]==DIRECT? "true" : "false" ) << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>" << ( parameterization[i]==DIRECT? "true" : "false" ) << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</nonmemParam>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<low>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( parameterization[i] == DIRECT )
        s << low[i];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( parameterization[i] == DIRECT )
          s << low[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</low>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<up>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( parameterization[i] == DIRECT )
        s << up[i];
      else 
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( parameterization[i] == DIRECT )
          s << up[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</up>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( isEstimateValid )
        s << out[i + i*len];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( isEstimateValid )
          s << out[j + i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }

  for( i=0; i<len; i++ )
  {
    s << "\t" << "<standardErrors>" << endl;
    s << "\t" << "\t";
    if( structure == DIAGONAL )
    {
      s << "<anon>";
      if( isStdErrorValid )
        s << stdError[i];
      else
        s << NO_VAL;
    s << "</anon>";
    }
    else
    {
      for( int j=0; j<=i; j++ )
      {
        s << "<anon>";
        if( isStdErrorValid )
          s << stdError[j+i*len];
        else
          s << NO_VAL;
        s << "</anon>";
      }
    }
    s << endl;
    s << "\t" << "</standardErrors>" << endl;
  }

  s << "\t" << "<relTol>" << relTol << "</relTol>" << endl;

  s << CloseLB << tag << RB << endl;

  return s.str();
}

//---------------------------------------------------------------------
//
// class CovarianceOfEstimate
//
// xml() produces the following text when the # of population parameters
// was 3.
//
// <covarianceOfEstimate>
//    <nonmemParam>
//        <anon>TF11</anon><anon>TF12</anon><anon>TF13</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF21</anon><anon>TF22</anon><anon>TF23</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF31</anon><anon>TF32</anon><anon>TF33</anon>
//    </nonmemParam>
//    <out>
//        <anon>COV11</anon><anon>COV12</anon><anon>COV13</anon>
//    </out>
//    <out>
//        <anon>COV21</anon><anon>COV22</anon><anon>COV23</anon>
//    </out>
//    <out>
//        <anon>COV31</anon><anon>COV32</anon><anon>COV33</anon>
//    </out>
// </covarianceOfEstimate>
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::CovarianceOfEstimate::xml( bool isCovarianceValid ) const
{
  int i, j;
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<nonmemParam>" << endl;
    s << "\t" << "\t";
    for( i=0; i<=j; i++ )
      s << "<anon>" << ( parameterization[i+j*nParameters]? "true" : "false" ) << "</anon>";
    s << endl;
    s << "\t" << "</nonmemParam>" << endl;
  }

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    for( i=0; i<=j; i++ )
    {
      s << "<anon>";
      if( isCovarianceValid )
        s << out[i+j*nParameters];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class CorrelationOfEstimate
//
// xml() produces the following text when the number of population
// parameters was 3.
//
// <CorrelationOfEstimate>
//    <nonmemParam>
//        <anon>TF11</anon><anon>TF12</anon><anon>TF13</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF21</anon><anon>TF22</anon><anon>TF23</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF31</anon><anon>TF32</anon><anon>TF33</anon>
//    </nonmemParam>
//    <out>
//        <anon>CORR11</anon><anon>CORR12</anon><anon>CORR13</anon>
//    </out>
//    <out>
//        <anon>CORR21</anon><anon>CORR22</anon><anon>CORR23</anon>
//    </out>
//    <out>
//        <anon>CORR31</anon><anon>CORR32</anon><anon>CORR33</anon>
//    </out>
// </CorrelationOfEstimate>
//
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::CorrelationOfEstimate::xml( bool isCorrelationValid ) const
{
  int i, j;
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<nonmemParam>" << endl;
    s << "\t" << "\t";
    for( i=0; i<=j; i++ )
      s << "<anon>" << ( parameterization[i + j*nParameters]? "true" : "false" ) << "</anon>";
    s << endl;
    s << "\t" << "</nonmemParam>" << endl;
  }

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    for( i=0; i<=j; i++ )
    {
      s << "<anon>";
      if( isCorrelationValid )
        s << out[i + j*nParameters];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class Eta
//
// xml() produces the following text when the number of population
// parameters was 3 and #of indivials are 2.
//
// <etaForAllIndividuals>
//    <nonmemParam>
//        <anon>TF11</anon><anon>TF12</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF21</anon><anon>TF22</anon>
//    </nonmemParam>
//    <nonmemParam>
//        <anon>TF31</anon><anon>TF32</anon>
//    </nonmemParam>
//    <out>
//        <anon>ETA11</anon><anon>ETA12</anon>
//    </out>
//    <out>
//        <anon>ETA21</anon><anon>ETA22</anon>
//    </out>
//    <out>
//        <anon>ETA31</anon><anon>ETA32</anon>
//    </out>
//    <relTol>TOLERANCE</relTol>
// </etaForAllIndividuals>
//---------------------------------------------------------------------
const string nonmemEqvContainers::Eta::xml( bool isEstimateValid ) const
{
  int i, j;
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<nonmemParam>" << endl;
    s << "\t" << "\t";
    for( i=0; i<nIndividuals; i++ )
      s << "<anon>" << ( parameterization[ j ]? "true" : "false" ) << "</anon>";
    s << endl;
    s << "\t" << "</nonmemParam>" << endl;
  }

  for( j=0; j<nParameters; j++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    for( i=0; i<nIndividuals; i++ )
    {
      s << "<anon>";
      if( isEstimateValid )
        s << out[ i + j * nIndividuals ];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }

  s << "\t" << "<relTol>" << relTol << "</relTol>" << endl;

  s << CloseLB << tag << RB << endl;
  return s.str();
}

//---------------------------------------------------------------------
//
// class Prediction
//
// xml() produces the following text when #of individuals is 2 and
// the first individual's #of measurements is 3 and for the 2nd 1.
//
// <predForAllIndividuals>
//    <out>
//      <anon>DATA11</anon>DATA12</anon><anon>DATA13</anon>
//    </out>
//    <out>
//      <anon>DATA21</anon>
//    </out>
//    <relTol>TOLERANCE</relTol>
// </predForAllIndividuals>
//
//---------------------------------------------------------------------
const string nonmemEqvContainers::Prediction::xml( bool isEstimateValid ) const
{
  int i, j, cnt;
#ifdef __GNUC__
  ostringstream s;
#elif WIN32
  strstream s;
#endif


  s << OpenLB << tag << RB << endl;

  for( j=0, cnt=0; j<nIndividuals; j++ )
  {
    s << "\t" << "<out>" << endl;
    s << "\t" << "\t";
    for( i=0; i<N[j]; i++, cnt++ )
    {
      s << "<anon>";
      if( isEstimateValid )
        s << out[cnt];
      else
        s << NO_VAL;
      s << "</anon>";
    }
    s << endl;
    s << "\t" << "</out>" << endl;
  }
  s << "\t" << "<relTol>" << relTol << "</relTol>" << endl;

  s << CloseLB << tag << RB << endl;
  return s.str();
}
