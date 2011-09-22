# ifndef NON_PAR_RELAXED_INCLUDED
# define NON_PAR_RELAXED_INCLUDED

# include <mat2cpp.hpp>

namespace non_par {
	extern double relaxed( 
		const mat2cpp::matrix<double> &Psi     ,
		double                         t       ,
		const mat2cpp::matrix<double> &option  ,
		mat2cpp::matrix<double>       &lam     ,
		mat2cpp::matrix<double>       &w       ,
		mat2cpp::matrix<double>       &info    );
}

# endif
