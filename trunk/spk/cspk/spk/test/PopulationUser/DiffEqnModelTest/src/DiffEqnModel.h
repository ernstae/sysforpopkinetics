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
#ifndef DIFFEQNMODELH_H
#define DIFFEQNMODELH_H

// This source code is stored in the file TmodelH.h
#include <spk/SpkModel.h>
#include <spk/SpkValarray.h>


class DiffEqnModel : public SpkModel {
public:

    enum PK_TYPE   { PK_ADDITIVE, PK_PROPORTIONAL, PK_EXPONENTIAL };
    enum ERROR_TYPE{ ER_ADDITIVE, ER_MODEL_BASED,  ER_DATA_BASED  };
    enum OMEGA_TYPE{ BLOCK, DIAGONAL };
    
    static const SPK_VA::valarray<double> Dval( int Q, const SPK_VA::valarray<double> &alp, const enum OMEGA_TYPE &omega = BLOCK );

    const SPK_VA::valarray<int> _N;

    DiffEqnModel( 
      int P, 
      int Q, 
      double odeStep, 
      const SPK_VA::valarray<int>    &N,
      const SPK_VA::valarray<double> &gamma,
      const SPK_VA::valarray<double> &w,
      const SPK_VA::valarray<double> &t
      )
        //
        // Setting constant properties
        //
     :  _P                ( P ), 
        _Q                ( Q ), 
        _odeStep          ( odeStep ),
        _N                ( N ), 
        _gamma            ( gamma ), 
        _w                ( w ), 
        _t                ( t ),
		_y                ( 0 ),
		_exp              ( true ),

        //
        // Initializing temporary state holders
        //
        _alp              ( 0 ), 
        _b                ( 0 ),
        _who              ( -1 ),
        _Ni               ( -1 ),
        _ti               ( 0 ),
        _pk               ( PK_ADDITIVE ),
        _er               ( ER_ADDITIVE ),
        _omega            ( BLOCK )
    { 
    };
    DiffEqnModel( 
      int P, 
      int Q, 
      double odeStep, 
      const SPK_VA::valarray<int>    &N,
      const SPK_VA::valarray<double> &gamma,
      const SPK_VA::valarray<double> &w,
      const SPK_VA::valarray<double> &t,
	  const SPK_VA::valarray<double> &y, 
      enum PK_TYPE      pk,
      enum ERROR_TYPE   er,
      enum OMEGA_TYPE   omega,
	  const bool exp
      )
        //
        // Setting constant properties
        //
     :  _P                ( P ), 
        _Q                ( Q ), 
        _odeStep          ( odeStep ),
        _N                ( N ), 
        _gamma            ( gamma ), 
        _w                ( w ), 
        _t                ( t ),
		_y                ( y ),
		_exp              ( exp ),

        //
        // Initializing temporary state holders
        //
        _alp              ( 0 ), 
        _b                ( 0 ),
        _who              ( -1 ),
        _Ni               ( -1 ),
        _ti               ( 0 ),
        _pk               ( pk ),
        _er               ( er ),
        _omega            ( omega )
    { 
    };

private:
    static void G  ( void *info, const double z[], double g[] );
    static void dG ( void *info, const double z[], double g[] );

    const int _P; 
    const int _Q; 
    const double _odeStep;
	const bool _exp;  
    const SPK_VA::valarray<double> _gamma; 
    const SPK_VA::valarray<double> _w; 
    const SPK_VA::valarray<double> _t;
	const SPK_VA::valarray<double> _y;
    const enum PK_TYPE    _pk;
    const enum ERROR_TYPE _er;
    const enum OMEGA_TYPE _omega;

    int _who; 
    SPK_VA::valarray<double> _alp; 
    SPK_VA::valarray<double> _b; 
    int _Ni; 
    int _ti;

    void doSelectIndividual     ( int i );
    void doSetPopPar            ( const SPK_VA::valarray<double>& alp );
    void doSetIndPar            ( const SPK_VA::valarray<double>& b );
    void doIndParVariance       ( SPK_VA::valarray<double>& ret ) const; 
    bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const;
    void doIndParVarianceInv    ( SPK_VA::valarray<double>& ret ) const; 
    void doDataMean             ( SPK_VA::valarray<double>& ret ) const; 
    bool doDataMean_popPar      ( SPK_VA::valarray<double>& ret ) const; 
    bool doDataMean_indPar      ( SPK_VA::valarray<double>& ret ) const;
    void doDataVariance         ( SPK_VA::valarray<double>& ret ) const; 
    bool doDataVariance_popPar  ( SPK_VA::valarray<double>& ret ) const; 
    bool doDataVariance_indPar  ( SPK_VA::valarray<double>& ret ) const;
    void doDataVarianceInv      ( SPK_VA::valarray<double>& ret ) const;

    mutable SPK_VA::valarray<double> cachedFi;
    mutable SPK_VA::valarray<double> cachedFia;
    mutable SPK_VA::valarray<double> cachedFib;
    mutable SPK_VA::valarray<double> cachedRi;
    mutable SPK_VA::valarray<double> cachedD;
    mutable SPK_VA::valarray<double> cachedRia;
    mutable SPK_VA::valarray<double> cachedRib;
    mutable SPK_VA::valarray<double> cachedDa;
    mutable SPK_VA::valarray<double> cachedRiInv;
    mutable SPK_VA::valarray<double> cachedDInv;

    mutable bool isCachedFiValid;
    mutable bool isCachedFiaValid;
    mutable bool isCachedFibValid;
    mutable bool isCachedRiValid;
    mutable bool isCachedDValid;
    mutable bool isCachedRiaValid;
    mutable bool isCachedRibValid;
    mutable bool isCachedDaValid;
    mutable bool isCachedRiInvValid;
    mutable bool isCachedDInvValid;

    inline void invalidateCachedValues() const
    {
        isCachedFiValid    = false;
        isCachedFiaValid   = false;
        isCachedFibValid   = false;
        isCachedRiValid    = false;
        isCachedRiaValid   = false;
        isCachedRibValid   = false;
        isCachedRiInvValid = false;
        isCachedDValid     = false;
        isCachedDaValid    = false;
        isCachedDInvValid  = false;
    }

    //
    // Prohibited operations.
    //
    DiffEqnModel( const DiffEqnModel& right );
    DiffEqnModel& operator=( const DiffEqnModel& right );
};  

#endif
