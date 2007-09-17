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
 * Author: Brad Bell
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "OdeBreakTest.h"

// SPK Pred library header files.
#include "../../../spkpred/OdeBreak.h"

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

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "OdeBreakOk_Test", 
    &OdeBreakTest::OdeBreakOk_Test ));

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "OneBreakOk_Test", 
    &OdeBreakTest::OneBreakOk_Test ));

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "ZeroBreakOk_Test", 
    &OdeBreakTest::ZeroBreakOk_Test ));

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "StiffBreakOk_Test", 
    &OdeBreakTest::StiffBreakOk_Test ));

  suiteOfTests->addTest(new TestCaller<OdeBreakTest>(
    "RightBreakOk_Test", 
    &OdeBreakTest::RightBreakOk_Test ));

  return suiteOfTests;
}


//------------------------------------------------------------------------
//
// Test Case: FourBolus_OneInfus
//
//
// This test checks that OdeBreak works for the case of an
// experimental design that includes four instantaneous bolus doses
// and one regular infusion dose.
//
//------------------------------------------------------------------------

/*************************************************************************
 *
 * Class:  FourBolus_OneInfus_Eval
 *
 *************************************************************************/

namespace // [Begin: unnamed namespace]
{
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
        void Ode(
		double                     t           ,
		const std::vector<double> &concen      , 
		std::vector<double>       &concen_tOut )
        {       assert( concen     .size() == N );
                assert( concen_tOut.size() == N );

                // Set the derivatives with respect to time.
                concen_tOut[0] = rate[k] / volume 
		               - concen[0] * clearance / volume;
        }
        void Ode_ind(
		double                     t           ,
		const std::vector<double> &concen      , 
		std::vector<double>       &concen_tOut_ind )
        {	assert(0); }
        void Ode_dep(
		double                     t           ,
		const std::vector<double> &concen      , 
		std::vector<double>       &concen_tOut_ind )
        {	assert(0); }
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: FourBolus_OneInfus_Test
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
	std::vector<bool>   outputLeft(J);
        size_t k;
	for(k = 0; k < J; k++)
		outputLeft[k] = true;
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
	std::string method = "Runge45";
        OdeBreak(eval, concenOut,
		method, breakTime, outputTime, outputLeft, errorAbs, errorRel);

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


//------------------------------------------------------------------------
//
// Test Case: OdeBreakOk
//
//------------------------------------------------------------------------

/*
$begin OdeBreakOk$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	oleft
	ind
	dep
	Runge
	std
	namespace
	std
	endl
	exp
	Dirac
	Cpp
	Eval
	const
	bool
	eabs
	erel
	vector vector
	xout
	btime
	Eval eval
	otime
$$

$section Example Using OdeBreak$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation:
$latex \[
\begin{array}{rcl}
x_0 ( 0 ) & = & 0.9 \\
x_1 ( 0 ) & = & 0. \\
x_0^\prime (t) & = & \left\{ \begin{array}{ll}
	- x_0 (t) + .6 * \delta( t - .7) + .8 & {\rm if} \; t \leq .7 \\
	- x_0 (t) + .6 * \delta( t - .7)     & {\rm otherwise} 
\end{array} \right.
\\
x_1^\prime (t) & = & \left\{ \begin{array}{ll}
	x_0 (t) - x_1 (t) & {\rm if} \; t \leq .7 \\
	- x_1 (t)         & {\rm otherwise}
\end{array} \right.
\end{array}
\] $$
where $latex .6 * \delta ( t - .7) $$ is the Dirac delta function 
representation of a step increase of .6 in the value of
$latex x_0 (t)$$ at $latex t = .7$$

$head Analytic Solution$$

$head X_0 (t)$$
For $latex 0 < t \leq .7$$, the function $latex X_0 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_0 ( 0 ) & = & 0.9 \\
X_0^\prime (t) & = & - X_0 (t) + .8 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_0 (t) = .1 * e^{-t} + .8 
\] $$ 
For $latex .7 < t \leq 1.$$, the function $latex X_0 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_0 ( .7 ) & = & .1 * e^{-.7} + 1.4 \\
X_0^\prime (t) & = & - X_0 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_0 (t) = ( .1 * e^{-.7} + 1.4) * e^{.7 - t} 
\] $$ 


$head X_1 (t)$$
For $latex 0 < t \leq .7$$, the function $latex X_1 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_1 ( 0 ) & = & 0.0 \\
X_1^\prime (t) 
& = & + X_0 (t) - X_1 (t) \\
& = & .1 * e^{-t} + .8 - X_1 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_1 (t) = .1 * t * e^{-t} + .8 * ( 1 - e^{-t} ) 
\] $$ 
For $latex .7 < t \leq 1.$$, the function $latex X_1 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_1 ( .7 ) & = & .07 * e^{-.7} + .8 * ( 1 - e^{-.7} )\\
X_1^\prime (t) & = & - X_1 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_1 (t) = [ .07 * e^{-.7} + .8 * (1 - e^{-.7}) ]  * e^{.7 - t} 
\] $$ 


$codep */

/*************************************************************************
 *
 * Class:  OdeBreakOk_Eval
 *
 *************************************************************************/

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

namespace { // Begin empty namespace

class OdeBreakOk_Eval {
private:
	size_t k;  // previous index passed to Break method
public:
	OdeBreakOk_Eval(void)
	{	k = 3; } // initialize as invalid break point index value
	void Break(size_t k_, const CppAD::vector<double> &x, CppAD::vector<double> &g)
	{	assert( g.size() == 2 );
		assert( x.size() == 2 );
		k = k_;
		g[0] = g[1] = 0.;
		switch(k)
		{	case 0:
			g[0] = .9;
			break;

			case 1:
			break;

			case 2:
			g[0] = .6;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f)
	{	assert( f.size() == 2 );
		assert( x.size() == 2 );
		switch(k)
		{	case 0:
			f[0] = - x[0] + .8;
			f[1] =   x[0] - x[1];
			break;

			case 1:
			// should never happen because btime[1] = btime[2]
			assert( 0 );

			case 2:
			f[0] = - x[0];
			f[1] = - x[1];
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode_ind(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_ind)
	{	assert(0); }
	void Ode_dep(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_dep)
	{	assert(0); }
};


} // End empty namespace


/*************************************************************************
 *
 * Function: OdeBreakOk_Test
 *
 *************************************************************************/

void OdeBreakTest::OdeBreakOk_Test()
{	bool ok = true;
	size_t K = 3;  // number of break point times
	size_t J = 10; // number of output point times

	// evaluation method
	OdeBreakOk_Eval eval;

	// break point times
	CppAD::vector<double> btime(K);
	btime[0] = 0.;
	btime[1] = .7;
	btime[2] = .7;

	// output grid
	CppAD::vector<double> otime(J);
	CppAD::vector<bool>   oleft(J);
	size_t j;
	for(j = 0; j < J; j++)
	{	otime[j] = double(j+1) / double(J);
		oleft[j] = true;
	}

	// absolute error 
	CppAD::vector<double> eabs(2);
	eabs[0] = 1e-6;
	eabs[1] = 1e-6;

	// relative error
	double erel = 0.;

	// results CppAD::vector
	CppAD::vector<double> xout(2 * J);

	// numerical solution of differential equation
	std::string method = "Runge45";
	OdeBreak(eval, xout, method, btime, otime, oleft, eabs, erel);

	// check the output values
	for(j = 0; j < J; j++)
	{	double t, x0, x1;
		t = otime[j];
		if( t <= .7 )
		{	x0 = .1 * exp(-t) + .8;
			x1 = .1 * t * exp(-t) + .8 * (1. - exp(-t));
		}
		else
		{	x0 = (.1 * exp(-.7) ) + 1.4;
			x0 *= exp(.7 - t);
			//
			x1 = .07 * exp(-.7) + .8 * (1 - exp(-.7));
			x1 *= exp(.7 - t);
		}
		ok &= CppAD::NearEqual(xout[0+j*2], x0, 1e-6, 1e-6);
		ok &= CppAD::NearEqual(xout[1+j*2], x1, 1e-6, 1e-6);
	}  

	CPPUNIT_ASSERT_MESSAGE( 
		"The calculated and known values for the ODE solution do not agree.",
		ok );

}

/* $$
$end
*/


//------------------------------------------------------------------------
//
// Test Case: OneBreakOk
//
//------------------------------------------------------------------------

/*
$begin OneBreakOk$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	oleft
	ind
	dep
	std
	Runge
	otime
	Eval eval
	eabs
	vector vector
	xout
	Cpp
	namespace
	const
	bool
	btime
	erel
$$

$section Test OdeBreak With One Break Point$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation
for $latex x : \R \rightarrow \R^n$$ :
$latex \[
\begin{array}{rcll}
	x ( 0 )        & = & 0              \\
	x_0^\prime (t) & = & 2 * t           \\
	x_i^\prime (t) & = & i \cdot x_{i-1} (t) & (i > 0)
\end{array}
\] $$

$head Analytic Solution$$
$latex \[
	x_i (t)  =  t^{i+1}
\] $$

$codep */

/*************************************************************************
 *
 * Class:  OneBreakOk_Eval
 *
 *************************************************************************/

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

namespace { // Begin empty namespace

class OneBreakOk_Eval {
private:
	size_t n;  // dimension of this problem
	size_t k;  // previous index passed to Break method
public:
	OneBreakOk_Eval(size_t n_)
	{	n = n_;	 // dimension of this problem
		k = 3;   // invalid index value 
	} 
	void Break(size_t k_, const CppAD::vector<double> &x, CppAD::vector<double> &g)
	{	assert( g.size() == n );
		assert( x.size() == n );
		k = k_;
		switch(k)
		{	size_t i;

			case 0:
			for(i = 0; i < n; i++)
				g[i] = 0.;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f)
	{	assert( f.size() == n );
		assert( x.size() == n );
		switch(k)
		{	size_t i;

			case 0:
			f[0] = 2. * t;
			for(i = 1; i < n; i++)
				f[i] = (2 + i) * x[i-1];
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode_ind(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_ind)
	{	assert(0); }
	void Ode_dep(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_dep)
	{	assert(0); }
};

} // End empty namespace


/*************************************************************************
 *
 * Function: OneBreakOk_Test
 *
 *************************************************************************/

void OdeBreakTest::OneBreakOk_Test()
{	bool ok = true;
	size_t K = 1; // number of break point times
	size_t J = 1; // number of output point times
	size_t n = 5; // number of components in x 

	// evaluation method
	OneBreakOk_Eval eval(n);

	// break point times
	CppAD::vector<double> btime(1);
	btime[0] = 0.;

	// output grid
	CppAD::vector<double> otime(J);
	CppAD::vector<bool>   oleft(J);
	size_t j;
	otime[0] = .5;
	oleft[0] = true;

	// absolute error 
	CppAD::vector<double> eabs(n);
	size_t i;
	for(i = 0; i < n; i++)
		eabs[i] = 0.;

	// relative error
	double erel = 1e-10;

	// results CppAD::vector
	CppAD::vector<double> xout(n);

	// numerical solution of differential equation
	std::string method = "Runge45";
	OdeBreak(eval, xout, method, btime, otime, oleft, eabs, erel);

	// check the output values
	double tip2 = otime[0];
	for(i = 0; i < n; i++)
	{	tip2 *= otime[0]; 
		ok &= CppAD::NearEqual(xout[i], tip2, erel, eabs[i]);
	}

	CPPUNIT_ASSERT_MESSAGE( 
		"The calculated and known values for the ODE solution do not agree.",
		ok );

}
/* $$
$end
*/


//------------------------------------------------------------------------
//
// Test Case: ZeroBreakOk
//
//------------------------------------------------------------------------

/*
$begin ZeroBreakOk$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	oleft
	ind
	dep
	std
	Runge
	otime
	Eval eval
	btime
	erel
	Cpp
	namespace
	const
	bool
	vector vector
	tpower
	xout
	eabs
$$

$section Test OdeBreak With an Identically Zero Component$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation
for $latex x : \R \rightarrow \R^n$$ :
$latex \[
\begin{array}{rcll}
	x ( 0 )        & = & 0             \\
	x_0^\prime (t) & = & 0             \\
	x_1^\prime (t) & = & 1             \\
	x_i^\prime (t) & = & i \cdot x_{i-1} (t)  \; ( i > 1 )
\end{array}
\] $$

$head Analytic Solution$$
$latex \[
\begin{array}{rcl}
	x_0      & = &  0            \\
	x_i (t)  & = & t^i
\end{array}
\] $$

$codep */

/*************************************************************************
 *
 * Class:  ZeroBreakOk_Eval
 *
 *************************************************************************/

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

namespace { // Begin empty namespace

class ZeroBreakOk_Eval {
private:
	size_t n;  // dimension of this problem
	size_t k;  // previous index passed to Break method
public:
	ZeroBreakOk_Eval(size_t n_)
	{	n = n_;	 // dimension of this problem
		k = 3;   // invalid index value 
	} 
	void Break(size_t k_, const CppAD::vector<double> &x, CppAD::vector<double> &g)
	{	assert( g.size() == n );
		assert( x.size() == n );
		k = k_;
		switch(k)
		{	size_t i;

			case 0:
			for(i = 0; i < n; i++)
				g[i] = 0.;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f)
	{	assert( f.size() == n );
		assert( x.size() == n );
		switch(k)
		{	size_t i;

			case 0:
			f[0] = 0.;
			f[1] = 1.;
			for(i = 2; i < n; i++)
				f[i] = i * x[i-1];
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode_ind(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_ind)
	{	assert(0); }
	void Ode_dep(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_dep)
	{	assert(0); }
};

} // End empty namespace


/*************************************************************************
 *
 * Function: ZeroBreakOk_Test
 *
 *************************************************************************/

void OdeBreakTest::ZeroBreakOk_Test()
{	bool ok = true;
	size_t K = 1; // number of break point times
	size_t J = 1; // number of output point times
	size_t n = 6; // number of components in x 

	// evaluation method
	ZeroBreakOk_Eval eval(n);

	// break point times
	CppAD::vector<double> btime(1);
	btime[0] = 0.;

	// output grid
	CppAD::vector<double> otime(J);
	CppAD::vector<bool>   oleft(J);
	size_t j;
	otime[0] = .5;
	oleft[0] = true;

	// absolute error 
	CppAD::vector<double> eabs(n);
	size_t i;
	for(i = 0; i < n; i++)
		eabs[i] = 0.;

	// relative error
	double erel = 1e-10;

	// results CppAD::vector
	CppAD::vector<double> xout(n);

	// numerical solution of differential equation
	std::string method = "Runge45";
	OdeBreak(eval, xout, method, btime, otime, oleft, eabs, erel);

	// check the output values
	double tpower = 0.;
	for(i = 0; i < n; i++)
	{	ok &= CppAD::NearEqual(xout[i], tpower, erel, eabs[i]);
		if( i == 0 )
			tpower = 1.;
		tpower *= otime[0];
	}

	CPPUNIT_ASSERT_MESSAGE( 
		"The calculated and known values for the ODE solution do not agree.",
		ok );

}
/* $$
$end
*/

//------------------------------------------------------------------------
//
// Test Case: StiffBreakOk
//
//------------------------------------------------------------------------

/*
$begin StiffBreakOk$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	oleft
	df
	dt
	ind
	dep
	std
	Runge
	Eval eval
	CppAD
	namespace
	const
	bool
	btime
	otime
	xout
	vector vector
	eabs
	erel
	exp
$$

$section Test OdeBreak With Stiff Equations$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation
with $latex a_0 \gg a_1 > 0$$:
$latex \[
\begin{array}{rcl}
x_0 ( 0 ) & = & 1. \\
x_1 ( 0 ) & = & 0. \\
x_0^\prime (t) & = &  - a_0  x_0 (t) \\
x_1^\prime (t) & = &  + a_0  x_0 (t) - a_1 x_1 (t) 
\end{array}
\] $$

$head Analytic Solution$$

$head x_0 (t)$$
The following $latex x (t)$$ satisfies
the ordinary differential equation above
$latex \[
\begin{array}{rcl}
x_0 ( t ) & = & \exp( -a_0 t ) \\
x_1 ( t ) & = & a_0 [ \exp( - a_1 t ) - \exp( - a_0 t )  ] / ( a_0 - a_1 )
\end{array}
\] $$
$codep */

/*************************************************************************
 *
 * Class:  StiffBreakOk_Eval
 *
 *************************************************************************/

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

namespace { // Begin empty namespace

class StiffBreakOk_Eval {
private:
	const CppAD::vector<double> a;
public:
	StiffBreakOk_Eval(const CppAD::vector<double> &a_) : a(a_) 
	{ } 
	void Break(size_t k, const CppAD::vector<double> &x, CppAD::vector<double> &g)
	{	assert( g.size() == 2 );
		assert( x.size() == 2 );
		assert( k == 0 );
		g[0] = 1.;
		g[1] = 0.;
		return;
	}

	// template so can use with Scalar equal to both double and AD<double>
	template <class Scalar>
	void Ode(Scalar t, const CppAD::vector<Scalar> &x, CppAD::vector<Scalar> &f)
	{	assert( f.size() == 2 );
		assert( x.size() == 2 );
		f[0] = - a[0] * x[0];
		f[1] = + a[0] * x[0] - a[1] * x[1];
		return;
	}

	// compute partial of f(t, x) with respect to t
	void Ode_ind(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_ind)
	{
		size_t n = x.size();
		CppAD::vector< CppAD::AD<double> > T(1);
		CppAD::vector< CppAD::AD<double> > X(n);
		CppAD::vector< CppAD::AD<double> > F(n);

		// set inputs to Ode_ind
		T[0] = t;
		size_t i;
		for(i = 0; i < n; i++)
			X[i] = x[i];

		// declare independent variables
		Independent(T);

		// compute f(t, x)
		this->Ode(T[0], X, F);

		// define AD function object
		CppAD::ADFun<double> Fun(T, F);

		// compute partial of f w.r.t. t
		CppAD::vector<double> dt(1);
		dt[0] = 1.;
		f_ind = Fun.Forward(1, dt);
	}

	// compute partial of f(t, x) with respect to x
	void Ode_dep(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_dep)
	{
		size_t n = x.size();
		CppAD::vector< CppAD::AD<double> > T(1);
		CppAD::vector< CppAD::AD<double> > X(n);
		CppAD::vector< CppAD::AD<double> > F(n);

		// set inputs to Ode_ind
		T[0] = t;
		size_t j;
		for(j = 0; j < n; j++)
			X[j] = x[j];

		// declare independent variables
		Independent(X);

		// compute f(t, x)
		this->Ode(T[0], X, F);

		// define AD function object
		CppAD::ADFun<double> Fun(X, F);

		// compute partial of f w.r.t. x
		CppAD::vector<double> dx(n);
		CppAD::vector<double> df(n);
		for(j = 0; j < n; j++)
			dx[j] = 0.;
		for(j = 0; j < n; j++)
		{	dx[j] = 1.;
			df    = Fun.Forward(1, dx);
			size_t i;
			for(i = 0; i < n; i++)
				f_dep[ i * n + j ] = df[i];
			dx[j] = 0.;
		}
	}
};

} // End empty namespace
void OdeBreakTest::StiffBreakOk_Test()
{	bool ok = true;
	size_t K = 1; // number of break point times
	size_t J = 2; // number of output point times

	// evaluation method
	CppAD::vector<double> a(2);
	a[0] = 1e1;
	a[1] = 1.;
	StiffBreakOk_Eval eval(a);

	// break point times
	CppAD::vector<double> btime(K);
	btime[0] = 0.;

	// output grid
	CppAD::vector<double> otime(J);
	CppAD::vector<bool>   oleft(J);
	otime[0] = 1. / a[0];
	otime[1] = 1. / a[1];
	oleft[0] = true;
	oleft[1] = true;

	// absolute error 
	CppAD::vector<double> eabs(2);
	eabs[0] = 0.;
	eabs[1] = 0.;

	// relative error
	double erel = 1e-3;

	// results vector
	CppAD::vector<double> xout(2 * J);

	// numerical solution of differential equation
	std::string method = "Runge45";
	OdeBreak(eval, xout, method, btime, otime, oleft, eabs, erel);

	// check the output values
	size_t j;
	for(j = 0; j < J; j++)
	{	double t, x0, x1;
		t  = otime[j];
		x0 = exp(-a[0] * t);
		x1 = a[0] * (exp(-a[1] * t) - exp(-a[0] * t)) / (a[0] - a[1]);
		ok &= CppAD::NearEqual(xout[0+j*2], x0, erel, erel);
		ok &= CppAD::NearEqual(xout[1+j*2], x1, erel, erel);
	}  

	CPPUNIT_ASSERT_MESSAGE( 
	"Numerical ODE solution does not agree with analytic solution.",
	ok
	);
}

/* $$
$end
*/

//------------------------------------------------------------------------
//
// Test Case: RightBreakOk
//
//------------------------------------------------------------------------

/*
$begin RightBreakOk$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	dep
	Eval eval
	vector vector
	Dirac
	oleft
	Eval
	CppAD
	namespace
	const
	ind
	bool
	btime
	otime
	eabs
	erel
	xout
	std
	Runge
$$

$section Example Using OdeBreak Right Continuous Solution$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation:
$latex \[
\begin{array}{rcl}
	x_0 ( 0 ) & = & 1                        \\
	x_1 ( 0 ) & = & 0.                       \\
	x_0^\prime (t) & = & 1 + \delta ( t - 1) \\
	x_1^\prime (t) & = & x_0 (t)
\end{array}
\] $$
where $latex \delta ( \cdot ) $$ is the Dirac delta function.

$head Analytic Solution$$

$subhead x_0 (t)$$
The function $latex x_0 (t)$$ is given by
$latex \[
x_0 (t) = \left\{ \begin{array}{ll}
	1 + t & {\rm if} \; t < 1  \\
	2 + t & {\rm if} \; 1 < t
\end{array} \right.
\] $$

$subhead x_1 (t)$$
The function $latex x_1 (t)$$ is given by
$latex \[
x_1 (t) = \left\{ \begin{array}{ll}
	t + t^2 / 2 & {\rm if} \; t < 1  \\
	1.5 + 2 (t - 1) + (t^2 - 1) / 2 & {\rm if} \; 1 < t
\end{array} \right.
\] $$

$codep */

/*************************************************************************
 *
 * Class:  RightBreakOk_Eval
 *
 *************************************************************************/

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

namespace { // Begin empty namespace

class RightBreakOk_Eval {
private:
	size_t k;  // previous index passed to Break method
public:
	RightBreakOk_Eval(void)
	{	k = 2; } // initialize as invalid break point index value
	void Break(size_t k_, const CppAD::vector<double> &x, CppAD::vector<double> &g)
	{	assert( g.size() == 2 );
		assert( x.size() == 2 );
		k = k_;
		g[0] = g[1] = 0.;
		switch(k)
		{	case 0:
			g[0] = 1.;
			break;

			case 1:
			g[0] = 1.;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f)
	{	assert( f.size() == 2 );
		assert( x.size() == 2 );
		switch(k)
		{	case 0:
			case 1:
			f[0] = 1;
			f[1] = x[0];
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode_ind(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_ind)
	{	assert(0); }
	void Ode_dep(double t, const CppAD::vector<double> &x, CppAD::vector<double> &f_dep)
	{	assert(0); }
};


} // End empty namespace


/*************************************************************************
 *
 * Function: RightBreakOk_Test
 *
 *************************************************************************/

void OdeBreakTest::RightBreakOk_Test()
{	bool ok = true;
	size_t K = 2;  // number of break point times
	size_t J = 5;  // number of output point times
	size_t j;      // temporary index

	// evaluation method
	RightBreakOk_Eval eval;

	// break point times
	CppAD::vector<double> btime(K);
	btime[0] = 0.;
	btime[1] = 1.;

	// output grid
	CppAD::vector<double> otime(J);
	otime[0] = 0.; otime[1] = 0.;
	otime[2] = 1.; otime[3] = 1.;
	otime[4] = 2.;

	// left or right continuous version of solution
	CppAD::vector<bool>   oleft(J);
	oleft[0] = true; oleft[1] = false;
	oleft[2] = true; oleft[3] = false;
	oleft[4] = true;

	// analytic solution
	CppAD::vector<double> solution(2 * J);

	// x_0 (t)        x_1 (t)
	solution[0] = 0.; solution[1] = 0.;   // left  continuous t = 0
	solution[2] = 1.; solution[3] = 0.;   // right continuous t = 0
	solution[4] = 2.; solution[5] = 1.5;  // left  continuous t = 1
	solution[6] = 3.; solution[7] = 1.5;  // right continuous t = 1
	solution[8] = 4.; solution[9] = 5.;   // left  continuous t = 2


	// absolute error 
	CppAD::vector<double> eabs(2);
	eabs[0] = 1e-6;
	eabs[1] = 1e-6;

	// relative error
	double erel = 0.;

	// results CppAD::vector
	CppAD::vector<double> xout(2 * J);

	// numerical solution of differential equation
	std::string method = "Runge45";
	OdeBreak(eval, xout, method, btime, otime, oleft, eabs, erel);

	// check the output values
	for(j = 0; j < 2 * J; j++)
		ok &= CppAD::NearEqual(xout[j], solution[j], 1e-6, 1e-6);

	CPPUNIT_ASSERT_MESSAGE( "RightBreakOk: "
		"calculated and analytic values for solution do not agree.",
		ok
	);

}

/* $$
$end
*/
