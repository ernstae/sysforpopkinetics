/*
$begin monteDriver$$
$spell
	stderror
	Eval
	spkreport
	obj
	std
	xml
	elapsedtime
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
$syntax%monteDriver  > %outputFile%$$
$tend

$fend 20$$

$children%
	omh/MonteInput.omh%
	omh/Model.omh%
	omh/Subroutine.omh
%$$

$head Input$$
$tref MonteInput$$ 

$head Model$$
$tref Model$$

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
		<pop_monte_result elapsedtime="%PCDATA%">
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
It requires an attribute, $code elapsedtime$$, specifying
the number of seconds it took to compute the results.

$subhead pop_obj_estimate$$
$index pop_obj_estimate$$
A record of this type is present if and only if
a $code pop_monte_result$$ record is present. 
This record contains the estimate for the 
negative log likelihood as a function of the population parameters
(the individual parameters have been integrated out).

$subhead pop_obj_stderror$$
$index pop_obj_stderror$$
A record of this type is present if and only if
a $code pop_monte_result$$ record is present and 
$italic method$$ is $code monte$$. 
This record contains an estimate of the standard deviation
of the Monte-Carlo estimate for the integral.

$head Subroutine$$
$tref Subroutine$$

$end
*/

# include <cassert>
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
# include "MontePars.h"

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
	using std::valarray;

	using namespace NonmemPars;

	// method
	bool   analytic   = MontePars::method == MontePars::analytic;
	bool   grid       = MontePars::method == MontePars::grid;
	bool   monte      = (MontePars::method == MontePars::plain)
	                  | (MontePars::method == MontePars::miser);

	// numberEval
	valarray<int>  numberEval = MontePars::numberEval;

	if( ! (analytic | grid | monte) )
	{	cerr << "monteDriver: ";
		cerr << "method is no analytic, grid, plain, or miser" << endl;
		return ReturnFailure;
	}
	if( analytic && NonmemPars::nEta != 1 )
	{	cerr << "monteDriver: ";
		cerr << "method is analytic and nEta != 1" << endl;
		return ReturnFailure;
	}

	size_t i;
	for(i = 0; i < numberEval.size(); i++)
	{	if( numberEval[i] <= 0 )
		{	cerr << "monteDriver: ";
			cerr << "numberEval is not greater than zero" << endl;
			return ReturnFailure;
		}
	}

	// data set
	DataSet< CppAD::AD<double> > set;
	Pred< CppAD::AD<double> > mPred(&set);

	const int nPop = set.getPopSize();
	if( nPop <= 0 )
	{	cerr << "monteDriver: DataSet.getPopSize() <= 0 " << endl;
		return ReturnFailure;
	}
	valarray<int> N = set.getN();
	for(i = 0; i < nPop; i++)
	{	if( N[i] <= 0 )
		{	cerr << "monteDriver: DataSet.getN() <= 0" << endl;
			return ReturnFailure;
		}
	}
	valarray<double> y = set.getAllMeasurements();
	const int nY = N.sum();
	if( nY != y.size() )
	{	cerr << "monteDriver: y.size != N[0] + ... + N[M-1]" << endl;
		return ReturnFailure;
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
	cout << "<?xml version=\"1.0\"?>" << endl;
	cout << "<spkreport>" << endl;
        
	// start timing
	timeval timeBegin;
	gettimeofday( &timeBegin, NULL );
	double pop_obj_estimate = 0.;
	double pop_obj_stderr = 0.;

	// analytic integral
	if( analytic )
	{	for(i = 0; i < nPop; i++)
		{	pop_obj_estimate += - log( AnalyticIntegral(
				model               , 
				N                   ,
				y                   ,
				alpIn               ,
				bLow                ,
				bUp                 ,
				i
			) );

		}
	}

	// grid integral approximation
	if( grid )
	{	assert( numberEval.size() == nB );

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
			valarray<double> yi = 
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
			double estimate;
			double error;
			GridIntegral(
				ExpNegMapBay,
				nB          , 
				null        ,
				numberEval  ,
				bLow        ,
				bUp         ,
				estimate    ,
				error
			);
			pop_obj_estimate -= log( estimate ),
			pop_obj_stderr   += error / estimate;
		}
	}

	// Monte Carlo integral
	if( monte )
	{	assert( numberEval.size() == 1 );

		try
		{	MontePopObj(
				model,
				N,
				y,
				alpIn,
				bLow,
				bUp,
				numberEval[0],
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

	// report the time in seconds that Monte Carlo integration required
	double pop_obj_seconds = difftime(timeEnd.tv_sec, timeBegin.tv_sec );
	cout << "<pop_monte_result elapsedtime=\"" << pop_obj_seconds << "\">" << endl;
	
	// report the Monte Carlo integration results
	OutputValue( "pop_obj_estimate", pop_obj_estimate); 

	// estimate of the standard error in pop_obj_estimate
	OutputValue( "pop_obj_stderr", pop_obj_stderr); 

	// return from main program
        cout << "</pop_monte_result>" << endl;
	cout << "</spkreport>" << endl; 

	return ReturnSuccess;
}
