/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
#ifndef FOMODEL_H
#define FOMODEL_H

#include <functional>

#include "SpkValarray.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"

class FoModel : public SpkModel
{
private:
    SpkModel* _pmodel;
    SPK_VA::valarray<double> _b, _bStep;
    SPK_VA::valarray<double> _F0, _F0_a, _F0_b, _F0_b_a, _R0, _R0_a, _D0, _D0_a;

	int _nA, _nB, _nY;

public:
	FoModel( SpkModel* pmodel, 
      const SPK_VA::valarray<double>& alp,
      const SPK_VA::valarray<double>& bStep, 
	  const int nY );

	~FoModel();

private:

	void doSelectIndividual( int inx );
    void doSetPopPar( const SPK_VA::valarray<double>& popPar );
    void doSetIndPar( const SPK_VA::valarray<double>& indPar );
	void doDataMean( SPK_VA::valarray<double>& ret ) const;
    bool doDataMean_popPar( SPK_VA::valarray<double>& ret ) const;
	bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const;
    void doDataVariance( SPK_VA::valarray<double>& ret ) const;
    bool doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const;
    bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const;
	void doIndParVariance( SPK_VA::valarray<double>& ret ) const;
	bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const;
    void doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const;
    bool doIndParVarianceInv_popPar( SPK_VA::valarray<double> & ret ) const;
    
    class FoModelFunction : public std::unary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double> >
	{
	    SpkModel* pModel;
		bool isF_alpOut;
        mutable SPK_VA::valarray<double> ret;

	public:
		FoModelFunction( SpkModel* p, bool s );
		~FoModelFunction();
		FoModelFunction( const FoModelFunction& right );
		const SPK_VA::valarray<double> operator()( const SPK_VA::valarray<double>& var ) const;
	};
    
};

#endif