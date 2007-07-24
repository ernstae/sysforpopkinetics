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
/*************************************************************************
 *
 * File: IdentPredBaseTest.cpp
 *
 *
 * Unit test for the class IdentPredBase.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "IdentPredBaseTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/IdentPredBase.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>

using namespace CppUnit;
using std::vector;
using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace papertwocompexample_identpredbasetest
{
  double timeStep = 0.25;

  double bolusAmount = 100.0;
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  PaperTwoCompExample_notIdentifiable_IdentPred
  //
  //
  // This nonidentifiable version of the two compartment system-
  // experiment model is based on Example 1 from the paper,
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // This class provides a PK-DES-ERROR block expression evaluator
  // that is specialized for determining the identifiability of
  // ordinary differential equation (ODE) based compartmental models.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following system-experiment model,
  //
  //     A1[t]  =  - k12 * A1 + k21 * A2 - Ve * A1 / (ke + A1) + b1 * u  ,
  // 
  //     A2[t]  =    k12 * A1 - k21 * A2 - k20 * A2  ,
  // 
  //     y      =     c1 * A1  ,
  // 
  // where
  // 
  //     k21 = THETA1  ,
  //
  //     k12 = THETA2  ,
  //
  //     Vm  = THETA3  ,
  //
  //     Km  = THETA4  ,
  //
  //     k02 = THETA5  ,
  //
  //     c1  = THETA6  ,
  //
  //     b1  = THETA7  ,
  //
  //     F1 = b1  ,
  //
  //     S1 = c1,
  //
  // and that correspond to additive weighting of the data
  //
  //     Y  =  F + ETA1  .
  //
  //**********************************************************************

  template<class Value>
  class PaperTwoCompExample_notIdentifiable_IdentPred : public IdentPredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    PaperTwoCompExample_notIdentifiable_IdentPred(
      int                        nY_iIn,
      int                        nThetaIn,
      int                        nEtaIn,
      bool                       isPkBlockAFuncOfTIn,
      int                        nCompIn,
      int                        defaultDoseCompIn,
      int                        defaultObservCompIn,
      const std::valarray<bool>& compInitialOffIn,
      const std::valarray<bool>& compNoOffIn,
      const std::valarray<bool>& compNoDoseIn )
    :
    IdentPredBase<Value> ( nThetaIn,
                           nEtaIn,
                           isPkBlockAFuncOfTIn,
                           nCompIn,
                           defaultDoseCompIn,
                           defaultObservCompIn,
                           compInitialOffIn,
                           compNoOffIn,
                           compNoDoseIn ),
    nY_i                 ( nY_iIn ),
    nComp                ( nCompIn ),
    S1                   ( this->getCompScaleParam( 0 ) ),
    F1                   ( this->getCompBioavailFrac( 0 ) ),
    Y                    ( this->getY() )
    {
    }

    ~PaperTwoCompExample_notIdentifiable_IdentPred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value k12;
    Value k21;
    Value Vm;
    Value Km;
    Value k02;
    Value c1;
    Value b1;

  
    //------------------------------------------------------------
    // Quantities usually declared in an OdePredBase subclass, i.e., OdePred. 
    //------------------------------------------------------------

  public:
    Value& F1;
    Value& S1;
    Value& Y;

    typename std::vector<Value>::const_iterator A;
    typename std::vector<Value>::const_iterator ETA;
    typename std::vector<Value>::const_iterator THETA; 

    typename std::vector<Value>::iterator DADT;


    //**********************************************************
    // 
    // Function: readDataRecord
    //
    //**********************************************************

  protected:
    void readDataRecord( int i, int j )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest ;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the type of event.
      if ( j == 0 )
      {
        //------------------------------------------------------
        // Instantaneous bolus dose events.
        //------------------------------------------------------

        // If this is the first data record, then set these flags to
        // indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the amount.
        this->setAMT( bolusAmount );

        // Set the compartment.
        this->setCMT( 1 );

        // Set the time.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( j * timeStep );
      }
    }


    //**********************************************************
    // 
    // Function: initUserEnv
    //
    //**********************************************************

    void initUserEnv(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int fOffset,     int fLen,
      int yOffset,     int yLen,
      int i,
      int j,
      const std::vector<Value>& indepVar,
      std::vector<Value>& depVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Do the initializations.
      //--------------------------------------------------------

       // Get non-const references.
       S1 = this->getCompScaleParam  (0);
       F1 = this->getCompBioavailFrac(0);
      
       // Get const iterators.
       A = this->getCompAmountIterator();
      
       // Get non-const iterators.
       DADT = this->getCompAmount_tIterator();

       THETA = indepVar.begin() + thetaOffset;
       typename std::vector<Value>::const_iterator THETA1 = indepVar.begin() + thetaOffset + 0;
       typename std::vector<Value>::const_iterator THETA2 = indepVar.begin() + thetaOffset + 1;
       typename std::vector<Value>::const_iterator THETA3 = indepVar.begin() + thetaOffset + 2;
       typename std::vector<Value>::const_iterator THETA4 = indepVar.begin() + thetaOffset + 3;
       typename std::vector<Value>::const_iterator THETA5 = indepVar.begin() + thetaOffset + 4;
       typename std::vector<Value>::const_iterator THETA6 = indepVar.begin() + thetaOffset + 5;
       typename std::vector<Value>::const_iterator THETA7 = indepVar.begin() + thetaOffset + 6;
       assert( thetaLen == 7 );

       ETA = indepVar.begin() + etaOffset;
       typename std::vector<Value>::const_iterator ETA1 = indepVar.begin() + etaOffset + 0;
       assert( etaLen == 1 );
    }


    //**********************************************************
    // 
    // Function: evalPk
    //
    //**********************************************************

    void evalPk(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      k21 = THETA[ ( 1 ) - 1 ];
      k12 = THETA[ ( 2 ) - 1 ];
      Vm  = THETA[ ( 3 ) - 1 ];
      Km  = THETA[ ( 4 ) - 1 ];
      k02 = THETA[ ( 5 ) - 1 ];
      c1  = THETA[ ( 6 ) - 1 ];
      b1  = THETA[ ( 7 ) - 1 ];
  
      F1 = b1;
      S1 = c1;
    }


    //**********************************************************
    // 
    // Function: evalDes
    //
    //**********************************************************

    void evalDes(
      int thetaOffset, int thetaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      DADT[ ( 1 ) - 1 ] = - k21*A[ (1) - 1 ] + k12*A[ (2) - 1 ] - Vm*A[ (1) - 1 ]/(Km + A[ (1) - 1 ]);
      DADT[ ( 2 ) - 1 ] =   k21*A[ (1) - 1 ] - k12*A[ (2) - 1 ] - k02*A[ (2) - 1 ];
    }


    //**********************************************************
    // 
    // Function: evalError
    //
    //**********************************************************

    void evalError(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Set the current value for the intra-individual error.
      //--------------------------------------------------------

      Y = S1*A[ (1) - 1 ] + ETA[ (1) - 1 ];
    }


    //**********************************************************
    // 
    // Function: getNRecords
    //
    //**********************************************************

  public:
    virtual int getNRecords( int i ) const
    {
      return 1 +           // Instantaneous bolus dose records.
             nY_i;         // Observation records.
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;         // Observation records.
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    PaperTwoCompExample_notIdentifiable_IdentPred(){}
    PaperTwoCompExample_notIdentifiable_IdentPred( const PaperTwoCompExample_notIdentifiable_IdentPred& ){}
    PaperTwoCompExample_notIdentifiable_IdentPred & operator=( const PaperTwoCompExample_notIdentifiable_IdentPred& ){}
  };


  //**********************************************************************
  //
  // Class:  PaperTwoCompExample_globallyIdentifiable_IdentPred
  //
  //
  // This globally identifiable version of the two compartment system-
  // experiment model is based on Example 1 from the paper,
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // The model is made globally identifiable by setting b1 equal to
  // one rather than being equal to THETA7.
  //
  // This class provides a PK-DES-ERROR block expression evaluator
  // that is specialized for determining the identifiability of
  // ordinary differential equation (ODE) based compartmental models.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following system-experiment model,
  //
  //     A1[t]  =  - k12 * A1 + k21 * A2 - Ve * A1 / (ke + A1) + b1 * u  ,
  // 
  //     A2[t]  =    k12 * A1 - k21 * A2 - k20 * A2  ,
  // 
  //     y      =     c1 * A1  ,
  // 
  // where
  // 
  //     k21 = THETA1  ,
  //
  //     k12 = THETA2  ,
  //
  //     Vm  = THETA3  ,
  //
  //     Km  = THETA4  ,
  //
  //     k02 = THETA5  ,
  //
  //     c1  = THETA6  ,
  //
  //     b1  = 1  ,
  //
  //     F1 = b1  ,
  //
  //     S1 = c1,
  //
  // and that correspond to additive weighting of the data
  //
  //     Y  =  F + ETA1  .
  //
  //**********************************************************************

  template<class Value>
  class PaperTwoCompExample_globallyIdentifiable_IdentPred : public IdentPredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    PaperTwoCompExample_globallyIdentifiable_IdentPred(
      int                        nY_iIn,
      int                        nThetaIn,
      int                        nEtaIn,
      bool                       isPkBlockAFuncOfTIn,
      int                        nCompIn,
      int                        defaultDoseCompIn,
      int                        defaultObservCompIn,
      const std::valarray<bool>& compInitialOffIn,
      const std::valarray<bool>& compNoOffIn,
      const std::valarray<bool>& compNoDoseIn )
    :
    IdentPredBase<Value> ( nThetaIn,
                           nEtaIn,
                           isPkBlockAFuncOfTIn,
                           nCompIn,
                           defaultDoseCompIn,
                           defaultObservCompIn,
                           compInitialOffIn,
                           compNoOffIn,
                           compNoDoseIn ),
    nY_i                 ( nY_iIn ),
    nComp                ( nCompIn ),
    S1                   ( this->getCompScaleParam( 0 ) ),
    F1                   ( this->getCompBioavailFrac( 0 ) ),
    Y                    ( this->getY() )
    {
    }

    ~PaperTwoCompExample_globallyIdentifiable_IdentPred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value k12;
    Value k21;
    Value Vm;
    Value Km;
    Value k02;
    Value c1;
    Value b1;

  
    //------------------------------------------------------------
    // Quantities usually declared in an OdePredBase subclass, i.e., OdePred. 
    //------------------------------------------------------------

  public:
    Value& F1;
    Value& S1;
    Value& Y;

    typename std::vector<Value>::const_iterator A;
    typename std::vector<Value>::const_iterator ETA;
    typename std::vector<Value>::const_iterator THETA; 

    typename std::vector<Value>::iterator DADT;


    //**********************************************************
    // 
    // Function: readDataRecord
    //
    //**********************************************************

  protected:
    void readDataRecord( int i, int j )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest ;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the type of event.
      if ( j == 0 )
      {
        //------------------------------------------------------
        // Instantaneous bolus dose events.
        //------------------------------------------------------

        // If this is the first data record, then set these flags to
        // indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the amount.
        this->setAMT( bolusAmount );

        // Set the compartment.
        this->setCMT( 1 );

        // Set the time.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( j * timeStep );
      }
    }


    //**********************************************************
    // 
    // Function: initUserEnv
    //
    //**********************************************************

    void initUserEnv(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int fOffset,     int fLen,
      int yOffset,     int yLen,
      int i,
      int j,
      const std::vector<Value>& indepVar,
      std::vector<Value>& depVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Do the initializations.
      //--------------------------------------------------------

       // Get non-const references.
       S1 = this->getCompScaleParam  (0);
       F1 = this->getCompBioavailFrac(0);
      
       // Get const iterators.
       A = this->getCompAmountIterator();
      
       // Get non-const iterators.
       DADT = this->getCompAmount_tIterator();

       THETA = indepVar.begin() + thetaOffset;
       typename std::vector<Value>::const_iterator THETA1 = indepVar.begin() + thetaOffset + 0;
       typename std::vector<Value>::const_iterator THETA2 = indepVar.begin() + thetaOffset + 1;
       typename std::vector<Value>::const_iterator THETA3 = indepVar.begin() + thetaOffset + 2;
       typename std::vector<Value>::const_iterator THETA4 = indepVar.begin() + thetaOffset + 3;
       typename std::vector<Value>::const_iterator THETA5 = indepVar.begin() + thetaOffset + 4;
       typename std::vector<Value>::const_iterator THETA6 = indepVar.begin() + thetaOffset + 5;
       assert( thetaLen == 6 );

       ETA = indepVar.begin() + etaOffset;
       typename std::vector<Value>::const_iterator ETA1 = indepVar.begin() + etaOffset + 0;
       assert( etaLen == 1 );
    }


    //**********************************************************
    // 
    // Function: evalPk
    //
    //**********************************************************

    void evalPk(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      k21 = THETA[ ( 1 ) - 1 ];
      k12 = THETA[ ( 2 ) - 1 ];
      Vm  = THETA[ ( 3 ) - 1 ];
      Km  = THETA[ ( 4 ) - 1 ];
      k02 = THETA[ ( 5 ) - 1 ];
      c1  = THETA[ ( 6 ) - 1 ];

      b1  = 1;
  
      F1 = b1;
      S1 = c1;
    }


    //**********************************************************
    // 
    // Function: evalDes
    //
    //**********************************************************

    void evalDes(
      int thetaOffset, int thetaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      DADT[ ( 1 ) - 1 ] = - k21*A[ (1) - 1 ] + k12*A[ (2) - 1 ] - Vm*A[ (1) - 1 ]/(Km + A[ (1) - 1 ]);
      DADT[ ( 2 ) - 1 ] =   k21*A[ (1) - 1 ] - k12*A[ (2) - 1 ] - k02*A[ (2) - 1 ];
    }


    //**********************************************************
    // 
    // Function: evalError
    //
    //**********************************************************

    void evalError(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace papertwocompexample_identpredbasetest;


      //--------------------------------------------------------
      // Set the current value for the intra-individual error.
      //--------------------------------------------------------

      Y = S1*A[ (1) - 1 ] + ETA[ (1) - 1 ];
    }


    //**********************************************************
    // 
    // Function: getNRecords
    //
    //**********************************************************

  public:
    virtual int getNRecords( int i ) const
    {
      return 1 +           // Instantaneous bolus dose records.
             nY_i;         // Observation records.
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;         // Observation records.
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    PaperTwoCompExample_globallyIdentifiable_IdentPred(){}
    PaperTwoCompExample_globallyIdentifiable_IdentPred( const PaperTwoCompExample_globallyIdentifiable_IdentPred& ){}
    PaperTwoCompExample_globallyIdentifiable_IdentPred & operator=( const PaperTwoCompExample_globallyIdentifiable_IdentPred& ){}
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void IdentPredBaseTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void IdentPredBaseTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* IdentPredBaseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IdentPredBaseTest" );

  suiteOfTests->addTest(new TestCaller<IdentPredBaseTest>(
    "PaperTwoCompExample_notIdentifiable_Test", 
    &IdentPredBaseTest::PaperTwoCompExample_notIdentifiable_Test ));

  suiteOfTests->addTest(new TestCaller<IdentPredBaseTest>(
    "PaperTwoCompExample_globallyIdentifiable_Test", 
    &IdentPredBaseTest::PaperTwoCompExample_globallyIdentifiable_Test ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: PaperTwoCompExample_notIdentifiable_Test
 *
 *
 * The goal of this test is to check that the PK-DES-ERROR block
 * expression evaluator that is specialized for identifiability works
 * for the case of
 *
 *     PaperTwoCompExample_notIdentifiable_IdentPred. 
 *
 *************************************************************************/

void IdentPredBaseTest::PaperTwoCompExample_notIdentifiable_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace papertwocompexample_identpredbasetest;

  int j;
  int k;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 7;

  // Set the seed to use for the random generator that will be used to
  // calculate a random value for THETA.
  int thetaSeed = 0;


  //------------------------------------------------------------
  // Prepare the rest of the parameters.
  //------------------------------------------------------------

  // Set the number of ETA elements.
  int nEta = 1;


  //------------------------------------------------------------
  // Prepare the PK-DES-ERROR block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 3;

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

  // Set the default compartments for doses and observations.
  int defaultDoseComp   = 1;
  int defaultObservComp = 1;

  // These flags indicate which compartments are initially off, cannot
  // be turned off, and cannot receive a dose.
  std::valarray<bool> compInitialOff( nComp );
  std::valarray<bool> compNoOff     ( nComp );
  std::valarray<bool> compNoDose    ( nComp );

  // Set the flags for compartment 1.
  compInitialOff[1 - 1] = false;
  compNoOff     [1 - 1] = false;
  compNoDose    [1 - 1] = false;

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3 (the output compartment).
  compInitialOff[3 - 1] = true;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = true;

  // Construct the identifiability PK-DES-ERROR block expression
  // evaluator.
  PaperTwoCompExample_notIdentifiable_IdentPred<GiNaC::ex> pkDesErrorEvaluator(
    nY_iKnown,
    nTheta,
    nEta,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number of observations for this individual.
  int i = 0;
  int nY_i = pkDesErrorEvaluator.getNObservs( i );


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the Groebner basis
  // are not printed.
  int level = 0;


  //------------------------------------------------------------
  // Check the identifiability of the individual's THETA parameters.
  //------------------------------------------------------------

  int nGroebnerBasisSoln;
  string identStatus;

  try
  {
    // Attempts to determine the identifiability of an individual's
    // THETA parameter using the system-experiment model that is defined
    // by the expressions from the PK, DES, and ERROR blocks and that is
    // also defined by the data records from the data file.
    nGroebnerBasisSoln = pkDesErrorEvaluator.checkIndParamIdent(
      i,
      level,
      nTheta,
      nEta,
      thetaSeed,
      identStatus );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkIndParamIdent failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkIndParamIdent failed for unknown reasons!", false);
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of Groebner basis solutions.
  //
  // This nonidentifiable version of the two compartment system-
  // experiment model is based on Example 1 from the paper,
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // This test has an infinite number of solutions as discussed in the
  // paragraph after Equation (23) of the paper.
  //
  // The value of -1 here indicates that the individual's THETA
  // parameter had an infinite number of solutions.
  int nGroebnerBasisSolnKnown = -1;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
  // The Groebner basis equations are not currently being solved using
  // a nonlinear systems of equations solver.
  //
  // Once the nonlinear solver has been added, it should be able to
  // determine that there are an infinite number of solutions which
  // should result in a value of -1 to indicate this error.
  //
  // For now, set the value equal to 0 here indicate that the
  // identifiability of the individual's THETA could not be determined
  // because the nonlinear solver is not currently available.
  nGroebnerBasisSolnKnown = 0;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  //------------------------------------------------------------
  // Compare the calculated and known number of solutions.
  //------------------------------------------------------------

  // Compare calculated and known number of solutions.
  if ( nGroebnerBasisSolnKnown != nGroebnerBasisSoln )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The calculated and known number of solutions did not agree.",
      false );
  }

}


/*************************************************************************
 *
 * Function: PaperTwoCompExample_globallyIdentifiable_Test
 *
 *
 * The goal of this test is to check that the PK-DES-ERROR block
 * expression evaluator that is specialized for identifiability works
 * for the case of
 *
 *     PaperTwoCompExample_globallyIdentifiable_IdentPred. 
 *
 *************************************************************************/

void IdentPredBaseTest::PaperTwoCompExample_globallyIdentifiable_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace papertwocompexample_identpredbasetest;

  int j;
  int k;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  //
  // Note that there is one less THETA element for this test as
  // compared to the nonidentifable version, which is what makes this
  // model be identifiable.
  int nTheta = 6;

  // Set the seed to use for the random generator that will be used to
  // calculate a random value for THETA.
  int thetaSeed = 0;


  //------------------------------------------------------------
  // Prepare the rest of the parameters.
  //------------------------------------------------------------

  // Set the number of ETA elements.
  int nEta = 1;


  //------------------------------------------------------------
  // Prepare the PK-DES-ERROR block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 3;

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

  // Set the default compartments for doses and observations.
  int defaultDoseComp   = 1;
  int defaultObservComp = 1;

  // These flags indicate which compartments are initially off, cannot
  // be turned off, and cannot receive a dose.
  std::valarray<bool> compInitialOff( nComp );
  std::valarray<bool> compNoOff     ( nComp );
  std::valarray<bool> compNoDose    ( nComp );

  // Set the flags for compartment 1.
  compInitialOff[1 - 1] = false;
  compNoOff     [1 - 1] = false;
  compNoDose    [1 - 1] = false;

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3 (the output compartment).
  compInitialOff[3 - 1] = true;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = true;

  // Construct the identifiability PK-DES-ERROR block expression
  // evaluator.
  PaperTwoCompExample_globallyIdentifiable_IdentPred<GiNaC::ex> pkDesErrorEvaluator(
    nY_iKnown,
    nTheta,
    nEta,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number of observations for this individual.
  int i = 0;
  int nY_i = pkDesErrorEvaluator.getNObservs( i );


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the Groebner basis
  // are not printed.
  int level = 0;


  //------------------------------------------------------------
  // Check the identifiability of the individual's THETA parameters.
  //------------------------------------------------------------

  int nGroebnerBasisSoln;
  string identStatus;

  try
  {
    // Attempts to determine the identifiability of an individual's
    // THETA parameter using the system-experiment model that is defined
    // by the expressions from the PK, DES, and ERROR blocks and that is
    // also defined by the data records from the data file.
    nGroebnerBasisSoln = pkDesErrorEvaluator.checkIndParamIdent(
      i,
      level,
      nTheta,
      nEta,
      thetaSeed,
      identStatus );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkIndParamIdent failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkIndParamIdent failed for unknown reasons!", false);
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of Groebner basis solutions.
  //
  // This globally identifiable version of the two compartment system-
  // experiment model is based on Example 1 from the paper,
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // The model is made globally identifiable by setting b1 equal to
  // one rather than being equal to THETA7.
  //
  // The value of 1 here indicates that the individual's THETA
  // parameter is globally identifiable.
  int nGroebnerBasisSolnKnown = 1;


  //------------------------------------------------------------
  // Compare the calculated and known number of solutions.
  //------------------------------------------------------------

  // Compare calculated and known number of solutions.
  if ( nGroebnerBasisSolnKnown != nGroebnerBasisSoln )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The calculated and known number of solutions did not agree.",
      false );
  }

}


