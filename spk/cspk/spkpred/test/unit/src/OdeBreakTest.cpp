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
 * File: OdeBreakTest.cpp
 *
 *
 * Unit test for the class OdeBreak.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "OdeBreakTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include <spkpred/OdeBreak.h>

// CppAD header files.
#include <CppAD/CppAD.h>
#include <CppAD/CppAD_vector.h>
#include <CppAD/NearEqual.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <vector>

using namespace CppUnit;
using std::vector;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void OdeBreakTest::setUp()
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

void OdeBreakTest::tearDown()
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

Test* OdeBreakTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "OdeBreakTest" );

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "FourBolus_OneInfus_Test", 
    &OdeBreakTest::FourBolus_OneInfus_Test ));

  return suiteOfTests;
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  FourBolus_OneInfus_Eval
  //
  //
  // The experimental design implemented by this Eval class includes
  // four instantaneous bolus doses and one regular infusion dose.
  //
  //**********************************************************************

  class FourBolus_OneInfus_Eval
  {
  private:
        size_t k;  // Current index set by Break method
  public:
        const size_t K;            // number of break points
        const size_t N;            // number of ordinary differential equations

        const double clearance;
        const double volume;

        std::vector<double> bolus;      // bolus amounts
        std::vector<double> rate;       // infusion rates

        FourBolus_OneInfus_Eval(void)
        :
        K        ( 6 ),
        N        ( 1 ),
        clearance( 10.0 ),
        volume   ( 25.0 ),
        bolus    ( K ),
        rate     ( K )
        {       // Set the bolus amounts.  Note that values equal to
                // zero mean there is not a bolus dose at the
                // beginning of the particular interval.
                bolus[0] = 1000.0;
                bolus[1] =    0.0;
                bolus[2] =  800.0;
                bolus[3] = 2300.0;
                bolus[4] =    0.0;
                bolus[5] =  650.0;

                // Set the infustion rates
                rate[0] =  0.0;
                rate[1] = 50.0;
                rate[2] = 50.0;
                rate[3] = 50.0;
                rate[4] =  0.0;
                rate[5] =  0.0;
        }

        void Break(size_t kIn, const std::vector<double> &concen, std::vector<double> &bolusOut)
        {       assert( kIn >= 0 && kIn <= K-1 );
                assert( bolusOut.size() == N );
                assert( concen  .size() == N );

                k = kIn;

                // Set the delta function multipliers, i.e., the bolus
                // amounts, at the beginning of this interval.
                bolusOut[0] = bolus[k];
        }

        void Ode(double t, const std::vector<double> &concen, std::vector<double> &concen_tOut)
        {       assert( concen     .size() == N );
                assert( concen_tOut.size() == N );

                // Set the derivatives with respect to time.
                concen_tOut[0] = rate[k] / volume - concen[0] * clearance / volume;
        }
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: FourBolus_OneInfus_Test
 *
 *
 * This test checks that OdeBreak works for the case of an
 * experimental design that includes four instantaneous bolus doses
 * and one regular infusion dose.
 *
 *************************************************************************/

void OdeBreakTest::FourBolus_OneInfus_Test()
{
        using namespace std;

        bool ok = true;

        // evaluation method
        FourBolus_OneInfus_Eval eval;

        // break point times
        size_t K = 6;
        std::vector<double> breakTime(K);
        breakTime[0] =  5.0;            // Bolus 1.
        breakTime[1] =  7.5;            // Infusion 1 start.
        breakTime[2] = 10.0;            // Bolus 2.
        breakTime[3] = 15.0;            // Bolus 3.
        breakTime[4] = 17.5;            // Infusion 1 end.
        breakTime[5] = 20.0;            // Bolus 4.

        // output grid
        size_t J = 15;
        std::vector<double> outputTime(J);
        size_t k;
        for(k = 0; k < K - 1; k++)
        {       outputTime[3 * k    ] = breakTime[k + 1] - 0.1;
                outputTime[3 * k + 1] = breakTime[k + 1];
                outputTime[3 * k + 2] = breakTime[k + 1] + 0.1;
        }

        // absolute error 
        size_t N = 1;
        std::vector<double> errorAbs(N);
        errorAbs[0] = 1e-6;

        // relative error
        double errorRel = 0.;

        // output values vector
        std::vector<double> concenOut(N * J);

        // numerical solution of differential equation
        OdeBreak(eval, breakTime, outputTime, errorAbs, errorRel, concenOut);

        // get the pharmacokinetic parameters
        double clearance = eval.clearance;
        double volume    = eval.volume;

        // check the output values
        double bolus;
        double rate;
        double concenKnown;
        double concenKnownAtBreak = 0.0;
        size_t n = 0;
        size_t j = 0;
        for( k = 0; k < K; k++ )
        {       bolus = eval.bolus[k];
                rate  = eval.rate[k];

                // Add the bolus at the beginning of this interval to
                // the current concentration.
                concenKnownAtBreak = concenKnownAtBreak + bolus;

                while( j < J )
                {        
                        concenKnown = rate / clearance + 
                                ( concenKnownAtBreak - rate / clearance ) *
                                std::exp( - clearance / volume * ( outputTime[j] - breakTime[k] ) );

                        CPPUNIT_ASSERT_DOUBLES_EQUAL(
                                concenOut[n + j * N],
                                concenKnown,
                                1e-6 );

                        j++;

                        // If this is not the last break point, then
                        // check to see if the current output value is
                        // in the next interval.
                        if( k != K - 1 )
                        {       if( outputTime[j] > breakTime[k + 1] )
                                {       break;
                                }
                        }
                }

                // If this is not the last break point, then set the
                // known value at the end of this interval.  The value
                // does not have to be recalculated here because every
                // break time for this test is also an output time.
                if( k != K - 1 )
                {       concenKnownAtBreak = concenKnown;
                        assert( outputTime[j - 1] == breakTime[k + 1] );
                }
        }  

}

