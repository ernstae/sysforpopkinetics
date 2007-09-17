# ifndef MontePopObjIncluded
# define MontePopObjIncluded

# include <valarray>
# include <spk/SpkModel.h>
# include "MontePars.h"

extern void MontePopObj(
	enum MontePars::METHOD       method          ,
	SpkModel<double>            &model           ,
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
