# ifndef AnalyticIntegralIncluded
# define AnalyticIntegralIncluded

# include <spk/SpkModel.h>

# include <cstddef>
# include <valarray>

double AnalyticIntegral(
	SpkModel<double>            &model           ,
	const std::valarray<int>    &N               , 
	const std::valarray<double> &y               ,
	const std::valarray<double> &alpha           ,
	const std::valarray<double> &L               ,
	const std::valarray<double> &U               ,
	size_t                       individual
); 

# endif
