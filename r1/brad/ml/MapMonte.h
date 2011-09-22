# ifndef MapMonteIncluded
# define MapMonteIncluded

# include <spk/SpkModel.h>

# include <cstddef>
# include <valarray>

void MapMonte(
	SpkModel                    &model           ,
	const std::valarray<int>    &N               , 
	const std::valarray<double> &y               ,
	const std::valarray<double> &alpha           ,
	const std::valarray<double> &L               ,
	const std::valarray<double> &U               ,
	size_t                       individual      ,
	size_t                       numberEval      ,
	//
	double                      &integralEstimate,
	double                      &estimateStd     
); 

# endif
