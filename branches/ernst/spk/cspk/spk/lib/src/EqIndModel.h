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

/*************************************************************************
 *
 * File: EqIndModel.h
 *
 *
 * This class is constructed from population SpkModel class.
 * Objects of this class are used by function firstOrderOpt. 
 *
 * Author: Jiaji Du
 *
 * Reviewed by Sachiko Honda, 09/25/2002
 *
 *************************************************************************/

#ifndef EQINDMODEL_H
#define EQINDMODEL_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <functional>
#include "rvec.h"
#include "SpkValarray.h"
#include "SpkModel.h"

/*------------------------------------------------------------------------
 * Equivalent individual model class definition
 *------------------------------------------------------------------------*/

class EqIndModel : public SpkModel
{
private:
	SpkModel* _pModel;
	SPK_VA::valarray<int> _N;
    SPK_VA::valarray<double> _bStep; // This step size is for the "b" in the conventional sense.  
                                     // It'll be used for approximating for fi_b(alp,0).
	int _nB, _nb, _nInd, _nY;

    SPK_VA::valarray<double> _b0;
	SPK_VA::valarray<double> _B;

    mutable SPK_VA::valarray<double> cachedR;
    mutable SPK_VA::valarray<double> cachedRInv;
    mutable bool isCachedRValid;
    mutable bool isCachedRInvValid;

public:
	EqIndModel( SpkModel* pModel, const SPK_VA::valarray<int>& N, 
		        const SPK_VA::valarray<double>& xStep, int nA );

    ~EqIndModel(){}

//
// [ Comment by Sachiko, 09/25/2002 ]
// Error.  These virtual functions, doXXX(), are "private".
//
protected:
	void doSetIndPar( const SPK_VA::valarray<double>& x );
	void doDataMean( SPK_VA::valarray<double>& f ) const;
    bool doDataMean_indPar( SPK_VA::valarray<double>& foF_B ) const;

	class EqIndModelFunction : public std::unary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double> >
	{
	    SpkModel* pModel;
	public:
		EqIndModelFunction( SpkModel* p ) : pModel( p )
        {}
		~EqIndModelFunction()
        {}
		EqIndModelFunction(const EqIndModelFunction& right) : pModel(right.pModel)
        {}
		const SPK_VA::valarray<double> operator()( const SPK_VA::valarray<double>& b ) const
		{
            SPK_VA::valarray<double> f;
            pModel->setIndPar( b );

			pModel->dataMean(f);
            return f;
		}
	};
	class EqIndModelDerivative : public std::unary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double> >
	{
	    SpkModel* pModel;
        const int nA;
	public:
		EqIndModelDerivative( SpkModel* p, int nAIn ) 
          : pModel( p ), nA( nAIn )
        {}
		~EqIndModelDerivative()
        {}
		EqIndModelDerivative(const EqIndModelDerivative& right) 
          : pModel( right.pModel ), nA( right.nA )
        
        {}
		const SPK_VA::valarray<double> operator()( const SPK_VA::valarray<double>& b ) const
		{
            SPK_VA::valarray<double> f_a;
            pModel->setIndPar( b );
			pModel->dataMean_popPar( f_a );
            DoubleMatrix DMf_a( f_a, nA );
            DMf_a = rvec( DMf_a );
            f_a = DMf_a.toValarray();
            return f_a;
		}
	};

    void doDataVariance( SPK_VA::valarray<double>& R ) const;
    bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const;
	void doDataVarianceInv( SPK_VA::valarray<double>& ret ) const;
};

#endif
