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
#ifndef NAIVE_FOMODEL_H
#define NAIVE_FOMODEL_H

#include <functional>

#include "SpkValarray.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"

class NaiveFoModel : public SpkModel<double>
{
private:
    SpkModel<double>* _pmodel;
    SPK_VA::valarray<double> _alp, _b, _bStep, _bZero;

	int _nA, _nB;

public:
	NaiveFoModel( SpkModel<double>* pmodel, 
      const SPK_VA::valarray<double>& bStep
      );
	~NaiveFoModel();

private:

	virtual void doSelectIndividual( int who );
    virtual void doSetPopPar( const SPK_VA::valarray<double>& popPar );
    virtual void doSetIndPar( const SPK_VA::valarray<double>& indPar );

	virtual void doDataMean(              SPK_VA::valarray<double>& foFiOut     ) const;
    virtual bool doDataMean_popPar(       SPK_VA::valarray<double>& foFi_alpOut ) const;
	virtual bool doDataMean_indPar(       SPK_VA::valarray<double>& foFi_bOut   ) const;
    virtual void doDataVariance(          SPK_VA::valarray<double>& foRiOut     ) const;
    virtual bool doDataVariance_popPar(   SPK_VA::valarray<double>& foRi_alpOut ) const;
    virtual bool doDataVariance_indPar(   SPK_VA::valarray<double>& foRi_bOut   ) const;
	virtual void doIndParVariance(        SPK_VA::valarray<double>& DOut        ) const;
	virtual bool doIndParVariance_popPar( SPK_VA::valarray<double>& DOut        ) const;
    
    //
    // NaiveFoModelFunction class is an instance of the unary function class.
    // It provides a unary operator( x ) that evalutes f(b).
    //
    class NaiveFoModelFunction 
      : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
	{
	  SpkModel<double>* pModel;
      mutable SPK_VA::valarray<double> ret;

	public:
	  NaiveFoModelFunction( SpkModel<double>* p );
      NaiveFoModelFunction( const NaiveFoModelFunction& right );
	  ~NaiveFoModelFunction();
	  const SPK_VA::valarray<double> operator()
        ( 
          const SPK_VA::valarray<double>& alp,
          const SPK_VA::valarray<double>& b 
        ) const;

    private:
      NaiveFoModelFunction& operator=( const NaiveFoModelFunction& right );
    };

    //
    // NaiveFoModelDeriv_popPar class is an instance of the unary function class.
    // It provides a unary operator( x ) that evalutes f_alp(b).
    //
    class NaiveFoModelDeriv_popPar 
      : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
	{
	  SpkModel<double>* pModel;
      mutable SPK_VA::valarray<double> ret;

	public:
	  NaiveFoModelDeriv_popPar( SpkModel<double>* p );
      NaiveFoModelDeriv_popPar( const NaiveFoModelDeriv_popPar& right );
	  ~NaiveFoModelDeriv_popPar();
	  const SPK_VA::valarray<double> operator()
        ( 
          const SPK_VA::valarray<double>& alp,
          const SPK_VA::valarray<double>& b
        ) const;

    private:
      NaiveFoModelDeriv_popPar& operator=( const NaiveFoModelDeriv_popPar& right );

    };
};

#endif
