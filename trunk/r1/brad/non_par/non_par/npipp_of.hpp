# ifndef NPIPP_OF_INCLUDED
# define NPIPP_OF_INCLUDED

# include <mat2cpp.hpp>

extern void npipp_of( 
	const mat2cpp::matrix<double> &Psi     ,
	const mat2cpp::matrix<double> &option  ,
	mat2cpp::matrix<double>       &lam     ,
	mat2cpp::matrix<double>       &w       ,
	mat2cpp::matrix<double>       &info    );

# endif
