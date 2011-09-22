# ifndef MapBayIncluded
# define MapBayIncluded

# include <valarray>
# include <spk/SpkModel.h>

extern void MapBaySet(
	SpkModel                     *model     ,
	const std::valarray<double>  &y         ,
	const std::valarray<double>  &alpha     , 
	size_t                        individual,
	size_t                        n         
);

// This function is made to have the type gsl_monte_function
extern double MapBay(double *x, size_t dim, void *params);

# endif

