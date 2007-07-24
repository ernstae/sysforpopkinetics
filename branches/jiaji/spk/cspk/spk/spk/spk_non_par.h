# ifndef SPK_NON_PAR_H
# define SPK_NON_PAR_H

# include <CppAD/CppAD.h>
# include "DoubleMatrix.h"
# include "SpkModel.h"

extern void spk_non_par(
	size_t                             level       ,
	SpkModel< CppAD::AD<double> >     &admodel     ,
	SpkModel<double>                  &model       ,
	const DoubleMatrix                &N           ,
	const DoubleMatrix                &y           ,
	const DoubleMatrix                &max_itr     ,
	const DoubleMatrix                &epsilon     ,
	const DoubleMatrix                &blow        ,
	const DoubleMatrix                &bup         ,
	const DoubleMatrix                &Bin         ,
	DoubleMatrix                      &Bout        ,
	DoubleMatrix                      &lamout      ,
	DoubleMatrix                      &pout 
);

# endif
