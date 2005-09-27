/*
$begin monteDriver$$
$spell
	elapsedtime
	Spk
	spkrecord
	nrows
	ncols
	stderror
	Eval
	spkreport
	obj
	std
	xml
	struct
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
If the program completes normally, regardless of whether computation
was successful or unsuccessful, an instance of SpkReportML is
written to standard output.  The most outer level record, <spkrecord>,
contains the following records.

$head error_list$$
$index error_list$$
This record contains a list of error messages accumulated during 
the execution of program.  If no error occurred, this record is still
present but as empty.

$head pop_monte_result$$
$index pop_monte_result$$
This record is present only when the estimation completed normally.
When it is present, this record contains the estimates for the 
negative log of the marginal likelihood as a function
of the fixed effects.
The attribute $code elapsedtime$$ specifies
the number of seconds it took to compute the results.

$head alpha_center$$
$index alpha_center$$
The record named $code alpha_center$$ contains
the central value for the fixed effects vector around
which all of the steps are taken.
The attribute $code nrows$$ for the corresponding record
is the number of fixed effects.

$head alpha_step$$
$index alpha_step$$
The record named $code alpha_step$$ contains
the step size for each of the components of the fixed effects.
The attribute $code nrows$$ for the corresponding record
is the number of fixed effects.

$head obj_value$$
The record named $code obj_value$$ contains a vector values of the 
negative log likelihood corresponding to varying one component
of the fixed points.
For $latex i = 0 , \ldots , nApl-1$$,
the row with index $italic i$$ contains the following vector
$latex \[
[ L( \alpha - s_i e^i ) , L( \alpha ) ,  L( \alpha + s_i e^i ) ] 
\] $$
where $latex nAlp$$ is the number of components in $xref/Alpha/$$,
$latex \alpha$$ is the vector in the $code alpha_center$$ record,
$latex s$$ is the vector in the  $code alpha_step$$ record, and
$latex e^i$$ is the unit vector in the 
$th i$$  component direction respectively.
There is one $code obj_value$$ row for each component
of $latex \alpha$$.


$head obj_std$$
The record named $code obj_std$$ contains the matrix of standard errors
for the corresponding values of the 
negative log likelihood in the record named $code obj_value$$
and in the same order as that record.

$head Format$$
$syntax%
<spkreport>
<error_list>
	%PCDATA%
</error_list>
</spkreport>
%$$
or
$syntax%
<spkreport>
<error_list>
</error_list>
<pop_monte_result elapsedtime="%PCDATA%" method="%PCDATA%" number_eval="%PCDATA%" >
	<column_major  name="alpha_center" nrows="%PCDATA%" ncols="1">
		<column>
			<value>
				%PCDATA%
			</value>
			%...%
		</column>
	</column_major>
	<column_major  name="alpha_step" nrows="%PCDATA%" ncols="1">
		<column>
			<value>
				%PCDATA%
			</value>
			%...%
		</column>
	</column_major>
	<row_major name="obj_value" nrows="%PCDATA%" ncols="3" >
		<row>
			<value>
				%PCDATA%
			</value>
			%...%
		</row>
		%...%
	</row_major>
	<row_major name="obj_std" nrows="%PCDATA%" ncols="3" >
		<row>
			<value>
				%PCDATA%
			</value>
			%...%
		</row>
		%...%
	</row_major>
</pop_monte_result>
</spkreport>
%$$

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
# include <gsl/gsl_errno.h>

# include "AnalyticIntegral.h"
# include "GridIntegral.h"
# include "MapMonte.h"
# include "MapBay.h"

# include "Pred.h"
# include "DataSet.h"
# include "NonmemPars.h"
# include "MontePars.h"
# include "Gsl2SpkError.h"

# define monteDriverDebug 0

enum { SUCCESSFUL             = 0,
       OTHER_KNOWN_ERROR      = 1,
       UNKNOWN_FAILURE        = 2,
       SYSTEM_ERROR           = 10,
       USER_INPUT_ERROR       = 14,
       SYSTEM_FAILURE         = 100,
       POST_OPT_ERROR         = 200,
       POST_OPT_FAILURE       = 300
     };


// locally defined functions
namespace {
	using std::valarray;

	// Map Bayesian objective
	double ExpNegMapBay(double *x, size_t nB, void *parms)
	{	return exp( - MapBay(x, nB, parms) );
	}
	void Indent(size_t indent)
	{	while(indent--)
			std::cout << " ";
	}
	template <typename Type>
	void OutputValue(size_t indent, Type value)
	{	using std::cout;
		using std::endl;

		Indent(indent);     cout <<  "<value>" << endl; 
		Indent(indent + 4); cout <<    value   << endl;
		Indent(indent);     cout << "</value>" << endl; 
	}
	void OutputColumnMajor(
		size_t                 indent,
		const valarray<double> &value, 
		const char              *name, 
		size_t                  nrows,
		size_t                  ncols 
	)
	{	using std::cout;
		using std::endl;

		Indent(indent); 
		cout << "<column_major ";
		cout << "name=\"" << name << "\" " ;
		cout << "nrows=\"" << nrows << "\" " ;
		cout << "ncols=\"" << ncols << "\" " ;
		cout << ">" << endl;

		size_t i;
		size_t j;
		for(j = 0; j < ncols; j++)
		{	Indent(indent + 4); 
			cout << "<column>" << endl;
			for(i = 0; i < nrows; i++)
				OutputValue(indent + 8, value[i * ncols + j]);
			Indent(indent + 4); 
			cout << "</column>" << endl;
		}
		Indent(indent); 
		cout << "</column_major>" << endl;;
	}
	void OutputRowMajor(
		size_t                 indent,
		const valarray<double> &value, 
		const char              *name, 
		size_t                  nrows,
		size_t                  ncols 
	)
	{	using std::cout;
		using std::endl;

		Indent(indent); 
		cout << "<row_major ";
		cout << "name=\"" << name << "\" " ;
		cout << "nrows=\"" << nrows << "\" " ;
		cout << "ncols=\"" << ncols << "\" " ;
		cout << ">" << endl;

		size_t i;
		size_t j;
		for(i = 0; i < nrows; i++)
		{	Indent(indent + 4);
			cout << "<row>" << endl;
			for(j = 0; j < ncols; j++)
				OutputValue(indent + 8, value[i * ncols + j]);
			Indent(indent + 4);
			cout << "</row>" << endl;
		}
		Indent(indent);
		cout << "</row_major>" << endl;;
	}
	// Analytic negative log marginal likelihood for entire data set
	void AnalyticIntegralAll(
		double           &pop_obj_estimate, 
		double           &pop_obj_stderror,
		PopPredModel     &model           ,
		const valarray<int>    &N         ,
		const valarray<double> &y         ,
		const valarray<double> &alp       ,
		const valarray<double> &bLow      ,
		const valarray<double> &bUp
	)
	{	pop_obj_estimate = 0.;
		pop_obj_stderror = 0.;
		size_t nPop      = N.size();
		size_t i;
		for(i = 0; i < nPop; i++)
		{	pop_obj_estimate += - log( AnalyticIntegral(
				model               , 
				N                   ,
				y                   ,
				alp                 ,
				bLow                ,
				bUp                 ,
				i
			) );
		}
	}
	// Grid approximation negative log marginal likelihood for entire data
	void GridIntegralAll(
		double           &pop_obj_estimate, 
		double           &pop_obj_stderror,
		PopPredModel     &model           ,
		const valarray<int>    &N         ,
		const valarray<double> &y         ,
		const valarray<double> &alp       ,
		const valarray<double> &bLow      ,
		const valarray<double> &bUp       ,
		const valarray<int>    &number_eval
	)
	{	pop_obj_estimate = 0.;
		pop_obj_stderror = 0.;
		size_t nPop      = N.size();
		size_t nB        = bLow.size();

		assert( bUp.size() == nB );
		assert( number_eval.size() == nB );

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
			MapBaySet(&model, yi, alp, i, nB);

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
			{	std::cerr << yi[j];
				if( j + 1 < yi.size() )
					std::cerr << ", ";
			} 
			std::cerr << std::endl;
# endif
			double estimate;
			double error;
			GridIntegral(
				ExpNegMapBay,
				nB          , 
				null        ,
				number_eval  ,
				bLow        ,
				bUp         ,
				estimate    ,
				error
			);
			pop_obj_estimate -= log( estimate ),
			pop_obj_stderror += error / estimate;
		}
	}
	// Monte Carlo approximation for negative log marginal likelihood 
	void MonteIntegralAll(
		double           &pop_obj_estimate, 
		double           &pop_obj_stderror,
		PopPredModel           &model     ,
		const valarray<int>    &N         ,
		const valarray<double> &y         ,
		const valarray<double> &alp       ,
		const valarray<double> &bLow      ,
		const valarray<double> &bUp       ,
		const valarray<int>    &number_eval
	)
	{	assert( number_eval.size() == 1 );

		pop_obj_estimate = 0.;
		pop_obj_stderror = 0.;
		size_t nPop      = N.size();
		size_t i;
		for(i = 0; i < nPop; i++)
		{	double estimate;
			double error;
			MapMonte(
				MontePars::method,
				model,
				N,
				y,
				alp,
				bLow,
				bUp,
				i,
				number_eval[0],
				estimate,
				error
			);
			pop_obj_estimate -= log( estimate ),
			pop_obj_stderror += error * error 
			                  / (estimate * estimate);
		}
		pop_obj_stderror = sqrt( pop_obj_stderror);
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

        // start the output file
        cout << "<?xml version=\"1.0\"?>" << endl;
        cout << "<spkreport>" << endl;
                                                                                                                     

	// number_eval
	valarray<int>  number_eval = MontePars::numberEval;

	// method
	std::string MethodName;
	bool analytic = false;
	bool grid     = false;
	bool monte    = false;
	switch( MontePars::method )
	{
		case MontePars::analytic:
		analytic   = true;
		MethodName = "analytic";
		break;

		case MontePars::grid:
		grid       = true;
		MethodName = "grid";
		break;

		case MontePars::plain:
		monte      = true;
		MethodName = "plain";
		break;

		case MontePars::miser:
		monte      = true;
		MethodName = "miser";
		break;

		case MontePars::vegas:
		monte      = true;
		MethodName = "vegas";
		break;

		default:
		cerr << "monteDriver: ";
		cerr << "method is not one of the following:" << endl;
		cerr << "analytic, grid, plain, miser, or vegas" << endl;
		return USER_INPUT_ERROR;
	}
	if( analytic && NonmemPars::nEta != 1 )
	{	cerr << "monteDriver: ";
		cerr << "method is analytic and nEta != 1" << endl;
		return USER_INPUT_ERROR;
	}

	size_t i;
	for(i = 0; i < number_eval.size(); i++)
	{	if( number_eval[i] <= 0 )
		{	cerr << "monteDriver: ";
			cerr << "number_eval is not greater than zero" << endl;
			return USER_INPUT_ERROR;
		}
	}

    try{
throw SpkException( SpkError::SPK_USER_INPUT_ERR, "test", __LINE__, __FILE__ );
	// data set
	DataSet< CppAD::AD<double> > set;
	Pred< CppAD::AD<double> > mPred(&set);

	const int nPop = set.getPopSize();
	if( nPop <= 0 )
	{	cerr << "monteDriver: DataSet.getPopSize() <= 0 " << endl;
		return USER_INPUT_ERROR;
	}
	valarray<int> N = set.getN();
	for(i = 0; i < nPop; i++)
	{	if( N[i] <= 0 )
		{	cerr << "monteDriver: DataSet.getN() <= 0" << endl;
			return USER_INPUT_ERROR;
		}
	}
	valarray<double> y = set.getAllMeasurements();
	const int nY = N.sum();
	if( nY != y.size() )
	{	cerr << "monteDriver: y.size != N[0] + ... + N[M-1]" << endl;
		return USER_INPUT_ERROR;
	}

	// model constructor
       PopPredModel *model = 0;
       try {   // model constructor
               model = new PopPredModel(
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
       }
       catch( const SpkException& e )
       {       cout << "<error_list>" << endl;
               cout << "Known exception in model contructor:" << std::endl;
               cout << e << endl;
               cout << "</error_list>" << endl;
               cout << "</spkreport>" << endl;
               return OTHER_KNOWN_ERROR;
       }
       catch( ... )
       {       cout << "<error_list>" << endl;
               cout << "Unknown exception in model contructor:" << std::endl;
               cout << "</error_list>" << endl;
               cout << "</spkreport>" << endl;
               return UNKNOWN_FAILURE;
       }

	// get the input value for the fixed effects as a single vector
	const int nAlp = model->getNPopPar();
	valarray<double> alpIn (nAlp);
	model->getPopPar( alpIn );

	// get the limits on the fixed effects
	valarray<double> alpLow(nAlp);
	valarray<double> alpUp(nAlp);
	model->getPopParLimits(alpLow, alpUp);

	// step size in fixed effects
	valarray<double> alpStep(nAlp);
	alpStep = 2e-2 * (alpUp - alpLow);

	// get the limits on the random effects
	const int nB = model->getNIndPar();
	valarray<double> bLow( nB );
	valarray<double> bUp( nB );
	model->getIndParLimits( bLow, bUp );

	// start timing
	timeval timeBegin;
	gettimeofday( &timeBegin, NULL );

	// replace the default GSL error handler with an SPK exception thow
	gsl_set_error_handler(Gsl2SpkError);

	// space used to hold the results
	double pop_obj_estimate;
	double pop_obj_stderror;
	valarray<double> obj_value(nAlp * 3);
	valarray<double> obj_std(nAlp * 3);
	try
	{	// loop over indices in fixed effects vector
		size_t index;
		valarray<double> alp (nAlp);
		for(i = 0; i < nAlp; i++) 
		{	int m;
			for(m = 0; m < 3; m++) if( i == 0 || m != 1 ) 
			{	double step = (m-1) * alpStep[i];
				alp         = alpIn;
				alp[i]      = alp[i] + step;

				// analytic integral
				if( analytic ) AnalyticIntegralAll(
					pop_obj_estimate, 
					pop_obj_stderror,
					*model           ,
					N               ,
					y               ,
					alp             ,
					bLow            ,
					bUp
				);
				// grid integral approximation
				if( grid ) GridIntegralAll(
					pop_obj_estimate, 
					pop_obj_stderror,
					*model           ,
					N               ,
					y               ,
					alp             ,
					bLow            ,
					bUp             ,
					number_eval
				);
				// Monte Carlo integral approximation
				if( monte ) MonteIntegralAll(
					pop_obj_estimate, 
					pop_obj_stderror,
					*model           ,
					N               ,
					y               ,
					alp             ,
					bLow            ,
					bUp             ,
					number_eval
				);
				// save results
				index            = i * 3 + m;
				obj_value[index] = pop_obj_estimate;
				obj_std[index]   = pop_obj_stderror;
			}
			// if m == 1 then result is the same for all i
			index            = i * 3 + 1;
			obj_value[index] = obj_value[1];
			obj_std[index]   = obj_std[1];
		}
	}
	catch( const SpkException& e )
	{	cout << "<error_list>" << endl;
		cout << e << endl;
		cout << "</error_list>" << endl;
	        cout << "</spkreport>" << endl; 
                if( e.find( SpkError::SPK_USER_INPUT_ERR ) >= 0 )
                   return USER_INPUT_ERROR;
                else
		   return POST_OPT_ERROR;
	}
	catch( ... )
	{	cout << "<error_list>" << endl;
		cout << "Unknown exception occurred";
		cout << "</error_list>" << endl;
	        cout << "</spkreport>" << endl; 
		return POST_OPT_FAILURE;
	}

        // Estimates completed successfully.  Print out emtpy <error_list>.
        cout << "<error_list/>" << endl;

	timeval timeEnd;
	gettimeofday( &timeEnd, NULL );

	// report the time in seconds that Monte Carlo integration required
	double pop_obj_seconds = difftime(timeEnd.tv_sec, timeBegin.tv_sec );
	cout << "<pop_monte_result elapsedtime=\"" << pop_obj_seconds 
	     << "\" method=\"" << MethodName 
	     << "\" number_eval=\"" << number_eval[0];
	for(i = 1; i < number_eval.size(); i++)
		cout << ", " << number_eval[i];
	cout << "\" >" << endl;

	size_t indent = 4;
	size_t nrows = nAlp;
	size_t ncols  = 1;
	OutputColumnMajor(indent, alpIn,    "alpha_center", nrows, ncols);
	OutputColumnMajor(indent, alpStep,  "alpha_step",   nrows, ncols);
	nrows = nAlp;
	ncols  = 3;
	OutputRowMajor(indent, obj_value,   "obj_value",  nrows, ncols);
	OutputRowMajor(indent, obj_std,     "obj_std",    nrows, ncols);


	// return from main program
        cout << "</pop_monte_result>" << endl;
	cout << "</spkreport>" << endl; 

        // Clean up
        delete model;
   }
   catch( const SpkException& e )
   {
           cout << "<error_list>" << endl;
           cout << e << endl;
           cout << "</error_list>" << endl;
           cout << "</spkreport>" << endl;
           if( e.find( SpkError::SPK_USER_INPUT_ERR ) >= 0 )
              return USER_INPUT_ERROR;
           else
              return POST_OPT_ERROR;
   }
   catch( ... )
   {       
           cout << "<error_list>" << endl;
           cout << "Unknown exception occurred";
           cout << "</error_list>" << endl;
           cout << "</spkreport>" << endl;
           return POST_OPT_FAILURE;
   }
   return SUCCESSFUL;
}
