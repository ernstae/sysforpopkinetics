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
written to a file result.xml.  The most outer level record, <spkrecord>,
contains the following records.

$head error_list$$
$index error_list$$
This record contains a list of error messages accumulated during 
the execution of program.  If no error occurred, this record is still
present but it is empty.

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

# include <stdio.h>
# include <fstream>
# include <pvm3.h>
# include <unistd.h>
# include <memory>
# include <cassert>
# include <fstream>
# include <valarray>
# include "IndData.h"
# include "DataSet.h"
# include <spkpred/PopPredModel.h>
# include <CppAD/CppAD.h>
# include <sys/time.h>
# include <spk/SpkException.h>
# include <spk/spkpvm.h>
# include <cstdlib>
# include <gsl/gsl_errno.h>
# include <pthread.h>
# include "GridIntegral.h"
# include "AdaptIntegral.h"
# include "MapMonte.h"
# include "MapBay.h"
# include "f2c.h"

#ifdef ODEPRED
	# include "OdePred.h"
#else
	# include "Pred.h"
#endif

# include "NonmemPars.h"
# include "MontePars.h"
# include "Gsl2SpkError.h"

# define monteDriverDebug 0

enum { SUCCESSFUL             = 0,
	UNKNOWN_ERROR          = 1,
	UNKNOWN_FAILURE        = 2,
	PVM_FAILURE            = 3,
	USER_ABORT             = 4,
	FILE_ACCESS_ERROR      = 10,
	USER_INPUT_ERROR       = 14,
	FILE_ACCESS_FAILURE    = 100
};

using namespace std;
static ofstream fout;
static char task[100];
static int num_tasks;
static int num_objectives;
static int parent_tid;
static std::vector<int> alp_tid;
static std::vector<int> alp_aid;
static std::vector<int> host_tid;
static std::vector<int> exit_val;
static int nPvmTasks = 0;
static const char* working_dir;

// locally defined functions
namespace {
	using std::valarray;

	// Map Bayesian objective
	double ExpNegMapBayGrid(double *x, size_t nB, void *parms)
	{	return exp( - MapBay(x, nB, parms) );
	}
	extern "C" doublereal ExpNegMapBayAdapt(integer *ndim, doublereal *x)
	{	void *parms = 0;
		return static_cast<doublereal>(exp(-MapBay(x, *ndim, parms)));
	}
	void Indent(size_t indent)
	{	while(indent--)
			fout << " ";
	}
	template <typename Type>
	void OutputValue(size_t indent, Type value)
	{	
		Indent(indent);     fout <<  "<value>" << endl; 
		Indent(indent + 4); fout <<    value   << endl;
		Indent(indent);     fout << "</value>" << endl; 
	}
	void OutputColumnMajor(
		size_t                 indent,
		const valarray<double> &value, 
		const char              *name, 
		size_t                  nrows,
		size_t                  ncols 
	)
	{	
		Indent(indent); 
		fout << "<column_major ";
		fout << "name=\"" << name << "\" " ;
		fout << "nrows=\"" << nrows << "\" " ;
		fout << "ncols=\"" << ncols << "\" " ;
		fout << ">" << endl;

		size_t i;
		size_t j;
		for(j = 0; j < ncols; j++)
		{	Indent(indent + 4); 
			fout << "<column>" << endl;
			for(i = 0; i < nrows; i++)
				OutputValue(indent + 8, value[i * ncols + j]);
			Indent(indent + 4); 
			fout << "</column>" << endl;
		}
		Indent(indent); 
		fout << "</column_major>" << endl;;
	}
	void OutputRowMajor(
		size_t                 indent,
		const valarray<double> &value, 
		const char              *name, 
		size_t                  nrows,
		size_t                  ncols 
	)
	{	
		Indent(indent); 
		fout << "<row_major ";
		fout << "name=\"" << name << "\" " ;
		fout << "nrows=\"" << nrows << "\" " ;
		fout << "ncols=\"" << ncols << "\" " ;
		fout << ">" << endl;

		size_t i;
		size_t j;
		for(i = 0; i < nrows; i++)
		{	Indent(indent + 4);
			fout << "<row>" << endl;
			for(j = 0; j < ncols; j++)
				OutputValue(indent + 8, value[i * ncols + j]);
			Indent(indent + 4);
			fout << "</row>" << endl;
		}
		Indent(indent);
		fout << "</row_major>" << endl;;
	}
	// Numerical approximation of integral for entire data
	void NumericIntegralAll(
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
			double estimate;
			double error;
			if( MontePars::method == MontePars::grid )
			{	if( number_eval.size() != nB )
				{	throw SpkException (
                                	SpkError::SPK_USER_INPUT_ERR,
                                	"grid: length of number eval is not "
					"equal number of random effects"     ,
                                	__LINE__                             ,
                                	__FILE__
                        		);
				}
				GridIntegral(
					ExpNegMapBayGrid,
					nB              , 
					null            ,
					number_eval     ,
					bLow            ,
					bUp             ,
					estimate        ,
					error
				);
			}
			else if( MontePars::method == MontePars::adapt )
			{	if( number_eval.size() != 1 )
				{	throw SpkException (
                                	SpkError::SPK_USER_INPUT_ERR,
                                	"adapt: length of number eval is not "
					"equal to one"                      ,
                                	__LINE__                            ,
                                	__FILE__
                        		);
				}
				AdaptIntegral(
					ExpNegMapBayAdapt,
					nB              , 
					number_eval[0]  ,
					bLow            ,
					bUp             ,
					estimate        ,
					error
				);
			}
			else	assert(0);
			pop_obj_estimate -= log( estimate ),
			pop_obj_stderror += error / estimate;

			// Check parent
			if(nPvmTasks > 0)
			{
				int bufid = 0;
				if((bufid = pvm_nrecv(-1, PvmTaskExit)) > 0)
				{
					pvm_exit();
					exit(UNKNOWN_ERROR);
				}
			}
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

                        // Check parent
			if(nPvmTasks > 0)
			{
				int bufid = 0;
				if((bufid = pvm_nrecv(-1, PvmTaskExit)) > 0)
				{
					pvm_exit();
					exit(UNKNOWN_ERROR);
				}
			}
		}
		pop_obj_stderror = sqrt( pop_obj_stderror);
	}

	void OutputErrorMsg(const char *msg)
	{	
		fout << "<error_list>"  << endl;
		fout << "monteDriver: " << msg << endl;
		fout << "</error_list>" << endl;
		fout << "</spkreport>"  << endl;
	}

	void OutputSpkException(const SpkException &e)
	{	
		fout << "<error_list>"  << endl;
		fout << e               << endl;
		fout << "</error_list>" << endl;
		fout << "</spkreport>"  << endl;
	}

	void finish(int exit_value)               // for using pvm
	{
		fout.close();
		pvm_initsend(PvmDataDefault);
		pvm_pkint(&exit_value, 1, 1);
		pvm_send(parent_tid, SpkPvmExitValue);
		pvm_exit();
	}

	void stop(char* message, int exit_value)  // for parallel
	{
		OutputErrorMsg(message);
		for(int i = 0; i < num_tasks; i++)
			pvm_kill(alp_tid[i]);
		finish(exit_value);
	}

	void spawnAlp(int j)
	{
		int id = j;
		if(j >= num_tasks) id = j % num_tasks;
		int i = 0;
                int m = 0;
		if(j == 1) m = 1;
		if(j > 1)
		{
			i = (j - 1) / 2;
			m = (j - 1) % 2 * 2;
		}
		char* arg[4];
		int tid;
		int rval;
		arg[0] = const_cast<char*>(working_dir); 
		char ibuf[100];
		char mbuf[100];
		sprintf(ibuf, "%d", i);
		sprintf(mbuf, "%d", m);
		arg[1] = ibuf;
		arg[2] = mbuf;
		arg[3] = NULL;
		rval = pvm_spawn(task, arg, 0, NULL, 1, &tid);
		if(rval != 1)
		{
			stop("could not spawn alpha level", PVM_FAILURE);
                        exit(PVM_FAILURE);
		}
		int host = pvm_tidtohost(tid);
		if(j < num_tasks)
		{
			alp_tid.push_back(tid);
                        alp_aid.push_back(j);
			host_tid.push_back(host);
			exit_val.push_back(SpkPvmUnreported);
		}
		else
		{
			alp_tid[id]  = tid;
                        alp_aid[id]  = j;
			host_tid[id] = host;
			exit_val[id] = SpkPvmUnreported;
		}

		// Establish notification of deletion of the host of the task we have just
		// spawned, if we haven't already asked to be notified for this host.
		// WARNING! Do not change the order of these notifications.
		bool host_notified = false;
                for(int k = 0; k < num_tasks; k++)
		{
			if(k != id && host_tid[k] == host_tid[id])
			{
				host_notified = true;
				break;
			}
		}
		if(!host_notified && pvm_notify(PvmHostDelete, PvmHostDelete, 1, &host) < 0)
		{
			stop("pvm_notify failed for PvmHostDelete", PVM_FAILURE);
                        exit(PVM_FAILURE);
		}

		// Establish notification for task exit of the task we have just spawned
		if(pvm_notify(PvmTaskExit, PvmTaskExit, 1, &tid) < 0)
		{
			stop("pvm_notify failed for PvmTaskExit", PVM_FAILURE);
                        exit(PVM_FAILURE);
		}
	}

	int respawn(int host)
	{
		int n = 0;
		for(int i = 0; i < num_tasks; i++)
		{
			if(host_tid[i] == host)
			{
				host_tid[i] = 0;
				spawnAlp(alp_aid[i]);
                                n++;
			}
		}
		return n;		
	}

	int tid2id(int tid)
	{
		for(int i = 0; i < num_tasks; i++)
			if(alp_tid[i] == tid)
				return i;
		return -1;
	}
}

int main(int argc, const char *argv[])
{
	if(argc > 1)
	{
		nPvmTasks = 1;
                working_dir = argv[1];
		pvm_mytid();
		parent_tid = pvm_parent();
		pvm_setopt(PvmRoute, PvmDontRoute);
		pvm_notify(PvmTaskExit, PvmTaskExit, 1, &parent_tid);
     
		if(chdir(working_dir) != 0)
		{
			OutputErrorMsg( "could not change working directory" );
			finish(FILE_ACCESS_ERROR );
			return FILE_ACCESS_ERROR;
		}
	}
	if(argc > 2)
	{
		nPvmTasks = atoi(argv[2]);
	}
	const char* stderrFileName = "software_error";
	freopen( stderrFileName, "a", stderr );

	using namespace NonmemPars;

	const char *msg;

	// start the output file
        fout.open("result.xml");
	fout << "<?xml version=\"1.0\"?>" << endl;
	fout << "<spkreport>" << endl;

	// number_eval
	valarray<int>  number_eval = MontePars::numberEval;

	// method
	std::string MethodName;
	bool numeric = false;
	bool monte   = false;
	switch( MontePars::method )
	{
		case MontePars::adapt:
		numeric    = true;
		MethodName = "adapt";
		if( NonmemPars::nEta < 2 )
		{	msg = "monteDriver\n"
		      	"Method is adapt and nEta < 2";
			OutputErrorMsg( msg );
			if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
			fout.close();
			fclose(stderr);
			return USER_INPUT_ERROR;
		}
		break;

		case MontePars::grid:
		numeric    = true;
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
		msg = "monteDriver\n"
		      "Method is not one of the following:\n"
		      "grid, adapt, plain, miser, or vegas";
		OutputErrorMsg( msg );
		if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
		fout.close();
		fclose( stderr );
		return USER_INPUT_ERROR;
	}

	size_t i;
	for(i = 0; i < number_eval.size(); i++)
	{	if( number_eval[i] <= 0 )
		{	msg = "monteDriver\n"
			       "number_eval is not greater than zero";
			OutputErrorMsg( msg );
			if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
			fout.close();
			fclose(stderr);
			return USER_INPUT_ERROR;
		}
	}

	// pointers to objects created inside of try block
	std::auto_ptr< DataSet< double > > set; 
	std::auto_ptr< DataSet< CppAD::AD<double> > > setAD; 
	std::auto_ptr< DataSet< CppAD::AD< CppAD::AD<double> > > > setADAD; 
#ifdef ODEPRED
	std::auto_ptr< OdePred< double > > mPred;
	std::auto_ptr< OdePred< CppAD::AD<double> > > mPredAD;
	std::auto_ptr< OdePred< CppAD::AD< CppAD::AD<double> > > > mPredADAD;
#else
	std::auto_ptr< Pred< double > > mPred;
	std::auto_ptr< Pred< CppAD::AD<double> > > mPredAD;
	std::auto_ptr< Pred< CppAD::AD< CppAD::AD<double> > > > mPredADAD;
#endif
	try { 
		// data set
		set.reset( new DataSet< double > );
		setAD.reset( new DataSet< CppAD::AD<double> > );
		setADAD.reset( new DataSet< CppAD::AD< CppAD::AD<double> > > );
#ifdef ODEPRED
		mPred.reset( new OdePred< double >(set.get(),
			NonmemPars::nIndividuals,
			NonmemPars::isPkFunctionOfT,
			NonmemPars::nCompartments,
			NonmemPars::nParameters,
			NonmemPars::defaultDoseComp,
			NonmemPars::defaultObservationComp,
			NonmemPars::initialOff,
			NonmemPars::noOff,
			NonmemPars::noDose,
			NonmemPars::relTol 
		) );

		mPredAD.reset( new OdePred< CppAD::AD<double> >(setAD.get(),
			NonmemPars::nIndividuals,
			NonmemPars::isPkFunctionOfT,
			NonmemPars::nCompartments,
			NonmemPars::nParameters,
			NonmemPars::defaultDoseComp,
			NonmemPars::defaultObservationComp,
			NonmemPars::initialOff,
			NonmemPars::noOff,
			NonmemPars::noDose,
			NonmemPars::relTol 
		) );

		mPredADAD.reset( new OdePred< CppAD::AD< CppAD::AD<double> > >(setADAD.get(),
			NonmemPars::nIndividuals,
			NonmemPars::isPkFunctionOfT,
			NonmemPars::nCompartments,
			NonmemPars::nParameters,
			NonmemPars::defaultDoseComp,
			NonmemPars::defaultObservationComp,
			NonmemPars::initialOff,
			NonmemPars::noOff,
			NonmemPars::noDose,
			NonmemPars::relTol 
		) );
#else
		mPred.reset( new Pred< double >(set.get()) );
		mPredAD.reset( new Pred< CppAD::AD<double> >(setAD.get()) );
		mPredADAD.reset( new Pred< CppAD::AD< CppAD::AD<double> > >(setADAD.get()) );
#endif
	}
	catch( SpkException& e )
	{       e.push(SpkError::SPK_USER_INPUT_ERR,
			"DataSet or Pred constructor",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
	catch( const std::exception& stde )
	{       SpkException e( stde,
			"DataSet or Pred constructor",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose( stderr );
		return UNKNOWN_ERROR;
	}
	catch( ... )
	{	OutputErrorMsg("DataSet or Pred constructor");
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose( stderr );
		return UNKNOWN_ERROR;
	}

	const int nPop = set->getPopSize();
	if( nPop <= 0 )
	{	msg = "monteDriver\n"
		      "DataSet.getPopSize() is less than or equal 0";
		OutputErrorMsg(msg);
		if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
		fout.close();
		fclose(stderr);
		return USER_INPUT_ERROR;
	}
	valarray<int> N = set->getN();
	for(i = 0; i < nPop; i++)
	{	if( N[i] <= 0 )
		{	msg = "monteDriver\n"
			      "DataSet.getN() is less than or equal 0";
			OutputErrorMsg(msg);
			if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
			fout.close();
			fclose(stderr);
			return USER_INPUT_ERROR;
		}
	}
	valarray<double> y = set->getAllMeasurements();
	const int nY = N.sum();
	if( nY != y.size() )
	{	msg = "monteDriver\n"
		      "y.size != N[0] + ... + N[M-1]";
		OutputErrorMsg(msg);
		if(nPvmTasks > 0) finish( USER_INPUT_ERROR );
		fout.close();
		fclose(stderr);
		return USER_INPUT_ERROR;
	}

	// pointers to object created inside of try block
	std::auto_ptr< PopPredModel > model;

	try {   // model constructor
		model.reset( new PopPredModel(
			*mPred,
			*mPredAD,
			*mPredADAD,
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
		) );
	}
	catch( SpkException& e )
	{       e.push(SpkError::SPK_USER_INPUT_ERR,
			"Model constructor",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
	catch( const std::exception& stde )
	{       SpkException e( stde,
			"Model constructor",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
	catch( ... )
	{	OutputErrorMsg("Model constructor");
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
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

    if(nPvmTasks > 1)
    {
        // loop over indices in fixed effects vector
        size_t index;
        sprintf(task, "%s/alpDriver", argv[1]);
        num_objectives = nAlp * 2 + 1;
        num_tasks = num_objectives;
        if(num_objectives > nPvmTasks) num_tasks = nPvmTasks;

        for(int j = 0; j < num_objectives; j++)
        {
            // spawn a PVM task
            spawnAlp(j);
            alp_aid[j % num_tasks] = j;
        
            if((j + 1) % num_tasks == 0 || j + 1 == num_objectives)
            {
                // receive results
                int ndone = 0;
                if(j + 1 == num_objectives && num_objectives % num_tasks != 0)
                    ndone = num_tasks - num_objectives % num_tasks;
                int ip[2];
                double dp[2];
                int i, m, bufid, bytes, msgtag, source, exit_tid, aid, exit_value;
                while(ndone < num_tasks && (bufid = pvm_recv(-1, -1)) > 0)
                {
                    pvm_bufinfo(bufid, &bytes, &msgtag, &source);
                    if(msgtag == PvmHostDelete)
                    {
                        pvm_upkint(&exit_tid, 1, 1);
                        respawn(exit_tid);
                    }
                    if(msgtag == PvmTaskExit)
                    {
                        pvm_upkint(&exit_tid, 1, 1);
                        if(exit_tid == parent_tid)
                        {
                            stop("user abort job", USER_ABORT);
                            fclose(stderr);
                            return USER_ABORT;
                        }
                        if((aid = tid2id(exit_tid)) >= 0)
                        {
                            if(exit_val[aid] == SpkPvmUnreported)
                            {
                                stop("an alpha task exited without an exit value", UNKNOWN_ERROR);
                                fclose(stderr);
                                return UNKNOWN_ERROR;
                            }
                            ndone++;
                        }
                    }
                    if(msgtag == SpkPvmResult)
                    {
                        pvm_upkint(ip, 2, 1);
                        i = ip[0];
                        m = ip[1];
                        index = i * 3 + m;
                        pvm_upkdouble(dp, 2, 1);
	                obj_value[index] = dp[0];
	                obj_std[index]   = dp[1];
                    }
                    if(msgtag == SpkPvmExitValue)
                    {
                        if((aid = tid2id(source)) >= 0)
                        {
                            pvm_upkint(&exit_value, 1, 1);
                            if((exit_val[aid] = exit_value) != SUCCESSFUL)
                            {
                                char* error = new char[bytes];
                                pvm_upkstr(error);
                                stop(error, exit_value);
                                delete [] error;
                                fclose(stderr);
                                return exit_value;
                            }
                        }
                    }
                }
            }
        }
        for(i = 0; i < nAlp; i++)
        {
            index = i * 3 + 1;
            obj_value[index] = obj_value[1];
            obj_std[index]   = obj_std[1]; 
        }
    }
    else
    {
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

				// numerical integral approximation
				if( numeric ) NumericIntegralAll(
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
	catch( SpkException& e )
	{       e.push(SpkError::SPK_USER_INPUT_ERR,
			"Monte Carlo or numerical integration",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
	catch( const std::exception& stde )
	{       SpkException e( stde,
			"Monte Carlo or numerical integration",
			__LINE__,
			__FILE__
		);
		OutputSpkException( e );
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
	catch( ... )
	{	OutputErrorMsg("Monte Carlo or numerical integration");
		if(nPvmTasks > 0) finish( UNKNOWN_ERROR );
		fout.close();
		fclose(stderr);
		return UNKNOWN_ERROR;
	}
    }

	// Estimates completed successfully.  Print out emtpy <error_list>.
	fout << "<error_list>"  << endl;
	fout << "</error_list>" << endl;

	timeval timeEnd;
	gettimeofday( &timeEnd, NULL );

	// report the time in seconds that Monte Carlo integration required
	double pop_obj_seconds = difftime(timeEnd.tv_sec, timeBegin.tv_sec );
	fout << "<pop_monte_result elapsedtime=\"" << pop_obj_seconds 
	     << "\" method=\"" << MethodName 
	     << "\" number_eval=\"" << number_eval[0];
	for(i = 1; i < number_eval.size(); i++)
		fout << ", " << number_eval[i];
	fout << "\" >" << endl;

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
	fout << "</pop_monte_result>" << endl;
	fout << "</spkreport>" << endl;
	fout.close();

	if(nPvmTasks > 0) finish( SUCCESSFUL );
	fclose(stderr);
	// automatic cleanup by auto_ptr
	return SUCCESSFUL;
}
