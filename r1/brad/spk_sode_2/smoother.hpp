# include <cppad/cppad.hpp>
# include <cassert>

# define DEBUG_GAUSS_NEWTON  0
# define Q_AT_INDEX_ZERO     1
# define R_AT_INDEX_ZERO     0

// solve the deterministic ODE for one time step
// Scalar can be either double, AD<double>, or AD< AD<double> >
template <class Scalar>
Scalar r_of_dt(Scalar dt, Scalar a, Scalar b, Scalar q0, Scalar r0)
{	using CppAD::abs;
	bool ok = abs(a - b) > Scalar(1e-4) * (abs(a) + abs(b));
	Scalar term;
	if( ok )
		term = ( Scalar(1) - exp(-(a-b) * dt) ) / (a - b);
	else	term = dt - (a - b) * dt * dt / Scalar(2.);
	Scalar r = (r0 + a * q0 * term) * exp(-b * dt);
	return r;
}

// Given q[0] and r[0], solve stochastic difference equation for q, r
// Scalar can be either double, AD<double>, or AD< AD<double> >
template <class Scalar>
void qr_of_tab(
	const CppAD::vector<Scalar> &t         ,
	const CppAD::vector<Scalar> &a         , 
	const CppAD::vector<Scalar> &b         , 
	CppAD::vector<Scalar>       &q         ,
	CppAD::vector<Scalar>       &r         )
{
	size_t Ni = t.size();
	assert( q.size() == Ni );
	assert( r.size() == Ni );
	assert( a.size() == Ni );
	assert( b.size() == Ni );
	size_t j;
	for(j = 1; j < Ni; j++)
	{	Scalar dt = (t[j] - t[j-1]);
		q[j]    = q[j-1] * exp( - a[j-1] * dt );
		r[j]    = r_of_dt(dt, a[j-1], b[j-1], q[j-1], r[j-1]);
	}
	return;
}

template <class Scalar>
Scalar weighted_sumsq(
	const Scalar                &log_var_epsilon  ,
	const Scalar                &log_var_gamma    ,
	const Scalar                &log_var_delta    ,
	const CppAD::vector<Scalar> &t                ,
	const CppAD::vector<Scalar> &y                ,
	const CppAD::vector<Scalar> &a                ,
	const CppAD::vector<Scalar> &b                )
{	size_t Ni = t.size();
	assert( y.size() == Ni );
	assert( a.size() == Ni );
	assert( b.size() == Ni );

	// determine q, r
	CppAD::vector<Scalar> q(Ni);
	CppAD::vector<Scalar> r(Ni);
	q[0] = Scalar(Q_AT_INDEX_ZERO);
	r[0] = Scalar(R_AT_INDEX_ZERO);
	qr_of_tab(t, a, b, q, r);

	Scalar var_epsilon = exp(log_var_epsilon);
	Scalar var_gamma   = exp(log_var_gamma);
	Scalar var_delta   = exp(log_var_delta);

	Scalar residual = (r[0] - y[0]);
	Scalar sumsq    =  residual * residual / var_epsilon; 
	size_t j;
	for(j = 1; j < Ni; j++)
	{	sumsq  += (r[j] - y[j]) * (r[j] - y[j]) / var_epsilon;
		Scalar gamma = a[j] - a[j-1];
		sumsq  += gamma * gamma / var_gamma;
		Scalar delta = b[j] - b[j-1];
		sumsq  += delta * delta / var_delta;
	}
	return sumsq;
}

// Solve for the optimal a_j, b_j and corresponding q_j, r_j
// Scalar can be either double, AD<double>
// The value y[0] is ignored
template <class Scalar>
void abqr_of_initial(
	const CppAD::vector<Scalar> &eta   ,
	const CppAD::vector<Scalar> &alpha ,
	const CppAD::vector<Scalar> &t     ,
	const CppAD::vector<Scalar> &y     ,
	CppAD::vector<Scalar>       &a     , 
	CppAD::vector<Scalar>       &b     , 
	CppAD::vector<Scalar>       &q     ,
	CppAD::vector<Scalar>       &r     )
{	using CppAD::vector;
	using CppAD::AD;

	assert( eta.size() == 2 );
	assert( alpha.size() == 7 );
	size_t Ni = t.size();
	size_t Ni2 = 2 * (Ni - 1);
	assert( y.size() == Ni );
	assert( q.size() == Ni );
	assert( r.size() == Ni );
	assert( a.size() == Ni );
	assert( b.size() == Ni );

	// variances
	Scalar var_epsilon = exp( alpha[4] );
	Scalar var_gamma   = exp( alpha[5] );
	Scalar var_delta   = exp( alpha[6] );
	Scalar eps_gamma   = var_epsilon / var_gamma;
	Scalar eps_delta   = var_epsilon / var_delta;

	// initial conditions
	q[0] = Scalar(Q_AT_INDEX_ZERO);
	r[0] = Scalar(R_AT_INDEX_ZERO);
	a[0] = alpha[0] * exp( eta[0] );
	b[0] = alpha[1] * exp( eta[1] );

	// Initialize the sequences
	size_t j;
	vector<Scalar>       gd(Ni2);
	for(j = 1; j < Ni; j++)
	{	a[j]           = a[j-1];
		b[j]           = b[j-1];
		q[j]           = q[j-1];
		r[j]           = r[j-1];
		gd[j-1]        = 0.;
		gd[j + Ni - 2] = 0.;
	}

	// Now use Gauss-Newton method to solve for a_j, b_j
	size_t itr;
	for(itr = 0; itr < 4; itr++)
	{	// declare independent variables as gamma, delta
		vector< AD<Scalar> > GD( Ni2 );
		for(j = 0; j < Ni2; j++)
			GD[j] = gd[j];
		CppAD::Independent(GD);

		// compute the R as a function of gamma, delta
		vector< AD<Scalar> > T(Ni);
		vector< AD<Scalar> > Q(Ni);
		vector< AD<Scalar> > R(Ni);
		vector< AD<Scalar> > A(Ni);
		vector< AD<Scalar> > B(Ni);
		T[0] = t[0];
		A[0] = a[0];
		B[0] = b[0];
		Q[0] = q[0];
		R[0] = r[0];
		for(j = 1; j < Ni; j++)
		{	T[j] = t[j];
			A[j] = A[j-1] + GD[j-1];
			B[j] = B[j-1] + GD[j + Ni - 2];
		}
		qr_of_tab(T, A, B, Q, R);

# if DEBUG_GAUSS_NEWTON
		// compute objective for tracing 
		vector< AD<Scalar> > Y(Ni);
		for(j = 0; j < Ni; j++)
			Y[j] = y[j];
		AD<Scalar> Alpha_4( alpha[4] );
		AD<Scalar> Alpha_5( alpha[5] );
		AD<Scalar> Alpha_6( alpha[6] );
		AD<Scalar> Sumsq = weighted_sumsq(
			Alpha_4, Alpha_5, Alpha_6, T, Y, A, B
		);
		std::cout << "Gauss-Newton: itr = " 
		          << itr << ", sumsq = " 
		          << Sumsq << std::endl;
# endif

		// differentiate R as a function of A, B
		CppAD::ADFun<Scalar> F(GD, R);
		vector<Scalar> jac = F.Jacobian(gd);

		// linear least squares equation D * gd_new = z
		// jac                             * gd_new = y - R + jac * gd 
		// (sigma_epsilon/sigma_gamma)     * g_new  = 0
		// (sigma_epsilon/ sigma_delta)    * d_new  = 0
		// solution is gdnew = (D^T * D)^{-1} * D^T z
		vector<Scalar> DT_D(Ni2 * Ni2), DT_z(Ni2), jac_gd(Ni);
		size_t k1, k2;
		Scalar sum;
		for(j = 0; j < Ni; j++)
		{	sum = Scalar(0);
			for(k2 = 0; k2 < Ni2; k2++)
				sum += jac[j * Ni2 + k2] * gd[k2];
			jac_gd[j] = sum;
		}
		for(k1 = 0; k1 < Ni2; k1++)
		{	sum = Scalar(0);
			for(j = 0; j < Ni; j++)
			{	sum = sum + jac[j * Ni2 + k1] * 
					(y[j] - Value(R[j]) + jac_gd[j]);
			}
			DT_z[k1] = sum;
		}
		for(k1 = 0; k1 < Ni2; k1++)
		{	for(k2 = 0; k2 < Ni2; k2++)
			{	sum = Scalar(0); 
				for(j = 0; j < Ni; j++)
					sum += jac[j * Ni2 + k1] *
					       jac[j * Ni2 + k2];
				DT_D[k1 * Ni2 + k2] = sum;
			}
		}
		for(j = 1; j < Ni; j++)
		{	DT_D[(j-1) * Ni2 + (j-1)]       += eps_gamma;
			DT_D[(j+Ni-2) * Ni2 + (j+Ni-2)] += eps_delta;
		}
		// note solve for gd
		Scalar logdet;
		int signdet = CppAD::LuSolve(
			Ni2, 1, DT_D, DT_z, gd, logdet
		);
	}

	// compute the q, r, q, b as a function of gamma, delta
	for(j = 1; j < Ni; j++)
	{	a[j] = a[j-1] + gd[j-1];
		b[j] = b[j-1] + gd[j + Ni - 2];
	}
	qr_of_tab(t, a, b, q, r);

	return;
}
