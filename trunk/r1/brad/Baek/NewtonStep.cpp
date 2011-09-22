# include "NewtonStep.h"
# include <boost/numeric/ublas/matrix.hpp>

void SetMatrix(const double *x,  boost::numeric::ublas::matrix<double> &X)
{	size_t M = X.size1();
	size_t N = X.size2();
	size_t i, j;
	for(i = 0; i < M; i++)
		for(j = 0; j < N; j++)
			X(i, j) = x[ i  + j * M ];
	return;
}

void SetVector(const boost::numeric::ublas::matrix<double> &X, double *x)
{	size_t M = X.size1();
	size_t N = X.size2();
	size_t i, j;
	for(i = 0; i < M; i++)
		for(j = 0; j < N; j++)
			x[ i  + j * M ] = X(i, j);
	return;
}
 

extern "C" void NewtonStep(
	int           MaxStatus   , 
	char         *status      , 
	double       *lambdaStep  , 
	double       *wStep       ,
	int           M           ,
	int           N           ,
	const double *psi         ,
	const double *lambda      , 
	const double *w           , 
	const double *t
)
{	using boost::numeric::ublas::matrix;
	
	matrix<double> W(M, 1), WStep(M, 1);
	matrix<double> Lambda(N, 1), LambdaStep(N, 1);
	matrix<double> Psi(M, N);

	SetMatrix(w, W);
	SetMatrix(lambda, Lambda);
	SetMatrix(psi, Psi);

	LambdaStep = Lambda;
	WStep      = W;

	SetVector(LambdaStep, lambdaStep);
	SetVector(WStep, wStep);

	return;
}
