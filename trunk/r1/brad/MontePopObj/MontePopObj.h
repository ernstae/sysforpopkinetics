# ifndef MontePopObjIncluded
# define MontePopObjIncluded

# include <valarray>
# include <spk/SpkModel.h>

extern void MontePopObj(
	SpkModel                    &model           ,
	const std::valarray<int>    &N               , 
	const std::valarray<double> &y               ,
	const std::valarray<double> &alpha           ,
	const std::valarray<double> &L               ,
	const std::valarray<double> &U               ,
	size_t                       numberEval      ,
	//
	double                      &integralEstimate,
	double                      &estimateStd
); 

# endif
