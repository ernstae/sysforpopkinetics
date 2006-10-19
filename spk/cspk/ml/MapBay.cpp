/*
$begin MapBay$$
$spell
	Spk
	yi
	const
	std
	valarray
$$

$index map, Bayesian$$
$index Bayesian, map$$
$index MapBay$$

$section The Map Bayesian Objective function$$

$table
$bold Syntax$$ $cnext
$syntax%double MapBay(double *%b%, size_t %n%, void *%p%)%$$
$rnext $cnext
$syntax%void MapBaySet(
	SpkModel<double>             *%model%     ,
	const std::valarray<double>  &%yi%        ,
	const std::valarray<double>  &%alpha%     , 
	size_t                        %individual%,
	size_t                        %n%         )%$$
$tend

$fend 25$$

$head Description$$
We are given a mixed effects 
$xref/MonteSpkModel/$$.
This routine $code MapBay$$ evaluates 
the Map Bayesian objective function which we define by
$latex \[
\begin{array}{rcl}
G( b ) & = &
\frac{1}{2} \log \det [ 2  \pi  D( \alpha ) ]
+
\frac{1}{2} b^T D( \alpha )^{-1} b
+
\frac{1}{2} \log \det [ 2  \pi  R_i ( b , \alpha ) ]
\\
& + &
\frac{1}{2} 
[ y_i - f_i ( b , \alpha )]^T 
	R_i ( b , \alpha )^{-1} 
		[ y_i - f_i ( b , \alpha )] 
\end{array}
\] $$
where the equations above uses the 
$xref/MonteSpkModel/Notation/Monte-Carlo model notation/$$.

$head MapBaySet$$
This function call determines all of the parameters
in the function $latex G(b)$$ as follows:

$subhead model$$
is a $xref/MonteSpkModel/$$ object that is used to 
obtain values for the functions 
$latex D$$, $latex f_i$$ and $italic R_i$$.

$subhead yi$$
is a vector that contains all of the measurements
for the $th i$$ individual and in the same order
as returned by the $code doDataMean$$ and $code doDataVariance$$
member functions of $italic model$$; i.e.,
it specifies the value for $latex y_i$$
in the definition of $latex G(b)$$.

$subhead alpha$$
is a vector containing one component for each fixed effect
in the model; i.e., it specifies the value for $latex \alpha$$
in the definition of $latex G(b)$$.

$subhead individual$$
specifies the individual of interest; i.e., the value
of $italic i$$ 
in the definition of $latex G(b)$$.

$subhead n$$
is the number of random effects in the model.

$head MapBay$$
The return value from $code MapBay$$ is $latex G(b)$$.

$subhead b$$
is a vector with $italic n$$ elements 
containing the value of the random effects 
in the definition of $latex G(b)$$.

$subhead n$$
is the number of random effects in the model; i.e.,
the length of the vector $italic b$$

$subhead p$$
currently this pointer is not used.

$end
------------------------------------------------------------------------------
*/

# include <valarray>
# include <cmath>
# include <CppAD/CppAD.h>
# include <spk/SpkModel.h>

# include "MapBay.h"

namespace {
	// constants
	const double Pi = 4. * std::atan(1.);

	// values for this individual
	std::valarray<double>      Y;
	SpkModel<double>          *Model;
	CppADvector<double>        DInv;
	double                     Dlogdet;

	// allocated memory for this individual
	std::valarray<double>      B;
	std::valarray<double>      F;
	std::valarray<double>      R;
}

void MapBaySet(
	SpkModel<double>             *model     ,
	const std::valarray<double>  &y         ,
	const std::valarray<double>  &alpha     , 
	size_t                        individual,
	size_t                        n         )
{
	using std::valarray;

	size_t i, j;

	// copy data to local variable
	Y.resize( y.size() );
	Y = y;

	// pointer to model
	Model = model;

	// set value of the fixed effects
	Model->setPopPar(alpha);

	// variance of random effects given fixed effects
	valarray<double> D(n * n);
	Model->indParVariance(D);

	// compute the inverse of the variance of the random effects
	CppADvector<double> Matrix(n * n);
	CppADvector<double> I(n * n);
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
		{	Matrix[ i * n + j] = D[i * n + j];
			I[i * n + j]     = 0.;
		}
		I[i * n + i] = 1.;
	}
	DInv.resize(n * n);
	int signdet = CppAD::LuSolve(n, n, Matrix, I, DInv, Dlogdet);
	assert( signdet == 1 );

	// set the individual index
	Model->selectIndividual(individual);

	// set to proper dimension
	B.resize(n);
	F.resize(Y.size());
	R.resize(Y.size() * Y.size());

	return;
}


// This function is made to have the type gsl_monte_function
extern double MapBay(double *x, size_t dim, void *params)
{	size_t i, j;

	assert( DInv.size() == (B.size() * B.size()) );
	assert( R.size()    == (Y.size() * Y.size()) );

	// set the value of the random effects
	for(i = 0; i < B.size(); i++)
		B[i] = x[i];
	Model->setIndPar(B);

	// get the mean measurement for this individual
	Model->dataMean(F);

	// get the measurement covariance
	Model->dataVariance(R);

	// check that the variance is diagonal
	for(i = 0; i < Y.size(); i++)
	{	for(j = 0; j < Y.size(); j++)
		{	assert( (i == j) | (R[ i * Y.size() + j ] == 0.) );
			assert( (i != j) | (R[ i * Y.size() + j ]  > 0.) );
		}
	}

	// b^T * D^{-1} * b
	double sum = 0.;
	for(i = 0; i < B.size(); i++)
	{	for(j = 0; j < B.size(); j++)
			sum += B[i] * DInv[i * B.size() + j] * B[j];
	}

	// (Y-F)^T * R^{-1} * (Y-F)
	double Rlogdet = 0.;
	for(i = 0; i < Y.size(); i++)
	{	sum += (Y[i] - F[i]) * (Y[i] - F[i]) / R[i * Y.size() + i];
		Rlogdet += log( R[i * Y.size() + i] );
	}

	// logdet(2 * pi * D)
	sum += B.size() * log(2. * Pi) + Dlogdet;

	// logdet(2 * pi * R)
	sum += Y.size() * log(2. * Pi) + Rlogdet;

	return .5 * sum;
}
