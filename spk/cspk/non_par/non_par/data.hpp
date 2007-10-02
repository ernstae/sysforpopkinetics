# ifndef NON_PAR_DATA_INCLUDED
# define NON_PAR_DATA_INCLUDED

# include <mat2cpp.hpp>

namespace non_par {  // Begin non_par namespace

extern mat2cpp::matrix<double> data_simulate(
	size_t m                      ,
	mat2cpp::matrix<double> &beta );

template <class Type>
mat2cpp::matrix<Type> data_likelihood(
	const mat2cpp::matrix<Type>   &beta ,
	const mat2cpp::matrix<double> &y    )
{	using namespace mat2cpp;
	size_t i, j, k;

	// number of random effect vectors
	size_t n = beta.size1();

	// number of measurement vectors
	size_t m = y.size1();

	// length of each measurement vector
	size_t q = y.size2();

	// standard deviation of the measurement noise
	double sigma = .2;

	// a universal constant
	double pi = 4. * std::atan(1.);

	// constant term in the negative log likelihood
	double term = q * ( std::log(2. * pi) / 2. + std::log(sigma) ); 

	// place where we store the likelihood values
	matrix<Type> Psi(m, n);

	// for each measurement vector
	for(i = 0; i < m; i++)
	{	// for each random effects vector
		for(j = 0; j < n; j++)
		{	// initialize sum of negative log likelihoods
	  		Type negloglike = 0.;

			// for each measurement
			for(k = 0; k < q; k++)
			{	double time = 2. * k / double(q);
				Type   mean = exp( - beta(i, 0) * time );
				Type   res  = (y(i, k) - mean) / sigma;
				negloglike += res * res / 2.;
			}
			negloglike += term;

			// value of likelihood
			Psi(i, j) = exp( - negloglike);
		}
	}
 	return Psi;
}

} // END non_par namespace

# endif
