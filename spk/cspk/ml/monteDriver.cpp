/*
$begin monteDriver$$
$spell
	stderror
	Eval
	spkreport
	obj
	std
	xml
$$
$latex 
	\newcommand{\D}{ {\bf d} }
	\newcommand{\R}{ {\bf R} }
	\newcommand{\N}{ {\bf N} }
$$

$section Run Monte Carlo Integration With Respect to Random Effects$$

$index monte carlo, integrate$$
$index random effect, integrate$$
$index integrate, random effect$$
$index integrate, monte carlo$$

$table
$bold Syntax$$
$cnext
$syntax%monteDriver %method% %numberEval%
%$$
$tend

$fend 20$$

$head Method$$
The command line argument $italic method$$
specifies the method that will be used for the integration.
The possible values for this argument are

$table
$bold Method$$   $cnext $bold Description$$ $rnext
$code analytic$$ $cnext 
	Closed form solution only valid for $xref/LinearModel/$$  $rnext
$code grid$$     $cnext
	Approximation integral using evaluation on a uniform grid $rnext
$code monte$$    $cnext
	Monte-Carlo approximation for integral
$tend

$head Number Evaluations$$
The command line argument $italic numberEval$$
specifies the number of evaluations of the 
$xref/MapBay//Map Bayesian objective/$$ 
that will be used for each individual.
Each evaluation will correspond to a different
value for the random effects; i.e., $latex b$$
in the functions $latex f_i (b , \alpha )$$ and 
$latex R_i (b, \alpha )$$.
The value of $italic i$$ will be the
same for $italic numberEval$$ evaluations. 
Then it will changed to correspond to the next value.
The value of $italic \alpha$$ will be the same
for all the evaluations.
$pre

$$
If $italic method$$ is equal to $code analytic$$,
this argument has no effect.
$pre

$$
If $italic method$$ is equal to $code grid$$,
and the dimension of the random effects space is $latex n$$,
the actual number of evaluations used is $latex k^n$$ where
$latex k$$ is the largest integer such that
$latex k^n \leq numberEval$$.

$head Return Value$$
If the program succeeds, its return value is zero,
otherwise its return value is one.

$head Output File$$
$index output, file$$
$index file, output$$
If no message is written to standard error,
the following information is written to standard out:
$syntax%
	<spkreport>
		<error_message>
			%PCDATA%
		</error_message>
		<pop_monte_result>
			<pop_obj_estimate>
				<value>
					%PCDATA%
				</value>
			</pop_obj_estimate>
			<pop_obj_stderror>
				<value>
					%PCDATA%
				</value>
			</pop_obj_stderror>
			<pop_obj_seconds>
				<value>
					%PCDATA%
				</value>
			</pop_obj_seconds>
		</pop_monte_result>
	</spkreport>
%$$

$subhead error_message$$
$index error_message$$
A record of this type is present if and only if 
there is no $code pop_monte_result$$ record.
This record contains an error message that explains
why there is no $code pop_monte_result$$ record.

$subhead pop_monte_result$$
$index pop_monte_result$$
A record of this type is present if and only if 
there is no $code error_message$$ record.

$subhead pop_obj_estimate$$
$index pop_obj_estimate$$
A record of this type is present if and only if
a $code pop_monte_result$$ record is present. 
This record contains the estimate for the integral.

$subhead pop_obj_stderror$$
$index pop_obj_stderror$$
A record of this type is present if and only if
a $code pop_monte_result$$ record is present and 
$italic method$$ is $code monte$$. 
This record contains an estimate of the standard deviation
of the Monte-Carlo estimate for the integral.

$subhead pop_obj_seconds$$
is the number of seconds required to compute the result.

$contents%
	MonteSpkModel.omh%
	NonmemPars.omh%
	DataSet.omh%
	PopPredModel.omh%
	MontePopObj.cpp%
	MapMonte.cpp%
	MapBay.cpp%
	LinearModel.h%
	AnalyticIntegral.cpp%
	GridIntegral.cpp%
	NearEqual.cpp%
	WhatsNew.omh
%$$

$end
*/

# include <fstream>
# include <valarray>
# include <spkpred/PopPredModel.h>
# include <CppAD/CppAD.h>
# include <sys/time.h>
# include <spk/SpkException.h>
# include <cstdlib>

# include "AnalyticIntegral.h"
# include "GridIntegral.h"
# include "MontePopObj.h"
# include "MapBay.h"

# include "Pred.h"
# include "DataSet.h"
# include "NonmemPars.h"

# define monteDriverDebug 0

enum { ReturnSuccess = 0, ReturnFailure = 1 };


// locally defined Map Bayesian likelihood function
namespace {
	double ExpNegMapBay(double *x, size_t nB, void *parms)
	{	return exp( - MapBay(x, nB, parms) );
	}
	template <typename Type>
	void OutputValue(const char *name, Type value)
	{	using std::cout;
		using std::endl;

		cout << "<" << name << ">" << endl;
		cout <<  "<value>" << endl; 
		cout <<    value   << endl;
		cout << "</value>" << endl; 
		cout << "</" << name << ">" << endl;
	}
}

int main(int argc, const char *argv[])
{
	using std::string;
	using std::cerr;
	using std::cout;
	using std::endl;

	using namespace NonmemPars;

	// command line arguments
	size_t iarg       = 1;
	bool   analytic   = false;
	bool   grid       = false;
	bool   monte      = false;
	int    numberEval = 0;
	if( argc != 3 )
	{	cerr << "usage: monteDriver <method> <numberEval>" << endl;
		return ReturnFailure;
	}
	string arg = argv[1];
	analytic = (arg == "analytic");
	grid     = (arg == "grid");
	monte    = (arg == "monte");
	if( ! (analytic | grid | monte) )
	{	cerr << "monteDriver: ";
		cerr << "method is no analytic, grid, or monte" << endl;
		return ReturnFailure;
	}
	numberEval = atoi( argv[2] );
	if( numberEval <= 0 )
	{	cerr << "monteDriver: ";
		cerr << "numberEval is not greater than zero" << endl;
		return ReturnFailure;
	}

	// data set
	DataSet< CppAD::AD<double> > set;
	Pred< CppAD::AD<double> > mPred(&set);

	const size_t nPop = set.popSize;
	valarray<int> N(nPop);
	size_t i;
	for(i = 0; i < nPop; i++)
		N[i] = set.data[i]->DV.size();

	const int nY = N.sum();
	valarray<double> y(nY);
	size_t j;
	size_t k = 0;
	for(i= 0; i < nPop; i++)
	{	for(j = 0; j < N[i]; j++)
			y[k++] = Value( set.data[i]->DV[j] );
	}

	// model constructor
	PopPredModel model(
		mPred,
		nTheta,
		thetaLow,
		thetaUp,
		thetaIn,
		nEta,
		etaIn,
		nEps,
		omegaStruct,
		omegaIn,
		sigmaStruct,
		sigmaIn 
	);

	// get the input value for the fixed effects as a single vector
	const int nAlp = model.getNPopPar();
	valarray<double> alpIn (nAlp);
	model.getPopPar( alpIn );

	// get the limits on the random effects
	const int nB = model.getNIndPar();
	valarray<double> bLow( nB );
	valarray<double> bUp( nB );
	model.getIndParLimits( bLow, bUp );

	// start the output file
	cout << "<?xml version \"1.0\"?>" << endl;
	cout << "<report>" << endl;

	// start timing
	timeval timeBegin;
	gettimeofday( &timeBegin, NULL );
	double pop_obj_estimate = 0.;
	double pop_obj_stderr = 0.;

	// analytic integral
	if( analytic )
	{	for(i = 0; i < nPop; i++)
		{
			pop_obj_estimate += AnalyticIntegral(
				model               , 
				N                   ,
				y                   ,
				alpIn               ,
				bLow                ,
				bUp                 ,
				i
			);

		}
	}

	// grid integral approximation
	if( grid )
	{	double logNumberEval  = log( double(numberEval) );
		size_t  numberGrid    = static_cast<size_t>(
				exp( logNumberEval / double(nB) )
		);
		valarray<size_t> Ngrid(numberGrid, nB);

		size_t i;
		for(i = 0; i < nPop; i++)
		{
			// compute and check offset for this individuals data
			size_t offset = 0;
			size_t j;
			for(j = 0; j < i; j++)
			{	assert( N[j] >= 0 );
				offset += N[j];
			}
			assert( offset + N[i] <= y.size() );

			// extract data for this individual
			std::valarray<double> yi = 
				y[ std::slice(offset, N[i], 1) ];

			// pseudo constructor for this individual
			MapBaySet(&model, yi, alpIn, i, nB);

			void *null    = 0;
# if monteDriverDebug
			double *Mid   = new double[ nB ];
			for(j = 0; j < nB; j++)
				Mid[j] = (bLow[j] + bUp[j]) / 2.;
			double mapBay = MapBay(Mid, nB, null);
			delete [] Mid;
			cerr << "monteDriver: individual = " << i;
			cerr << ", MapBay(.5*(U + L)) = " << mapBay;
			cerr << ", y_i = ";
			for(j = 0; j < yi.size(); j++)
			{	cerr << yi[j];
				if( j + 1 < yi.size() )
					cerr << ", ";
			} 
			cerr << std::endl;
# endif
			pop_obj_estimate += GridIntegral(
				ExpNegMapBay,
				nB    , 
				null  ,
				Ngrid ,
				bLow  ,
				bUp
			);
		}
	}

	// Monte Carlo integral
	if( monte )
	{
		try
		{	MontePopObj(
				model,
				N,
				y,
				alpIn,
				bLow,
				bUp,
				numberEval,
				pop_obj_estimate,
				pop_obj_stderr
			);
		}
		catch( const SpkException& e )
		{	cout << "<error_message>" << endl;
			cout << e << endl;
			cout << "</error_message>" << endl;
			return ReturnSuccess;
		}
		catch( ... )
		{	cout << "<error_message>" << endl;
			cout << "Unknown exception occurred";
			cout << "</error_message>" << endl;
			return ReturnSuccess;
		}
	}
	timeval timeEnd;
	gettimeofday( &timeEnd, NULL );

	// report the Monte Carlo integration results
	OutputValue("pop_obj_estimate", pop_obj_estimate); 

	if( monte )
	{	// estimate of the standard error in pop_obj_estimate
		OutputValue("pop_obj_stderr", pop_obj_stderr); 
	}

	// report the time in seconds that Monte Carlo integration required
	double pop_obj_seconds = difftime(timeEnd.tv_sec, timeBegin.tv_sec );
	OutputValue("pop_obj_seconds", pop_obj_seconds); 

	// return from main program
	cout << "</report>" << endl; 
	return ReturnSuccess;
}
