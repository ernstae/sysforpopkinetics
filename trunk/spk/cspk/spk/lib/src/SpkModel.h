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
/*
 *****************************************************************************
 *
 * SpkModel.h
 *
 *****************************************************************************
 */
#ifndef SPKMODEL_H
#define SPKMODEL_H

#include "DoubleMatrix.h"
#include "Covariance.h"
#include "SpkException.h"
#include "FullDataCovariance.h"
#include "FullIndParCovariance.h"
/*
 *****************************************************************************
 *
 * SpkModel declaration
 *
 *****************************************************************************
 */
class SpkModel
{
public:
	void selectIndividual(int base0);
    void setPopPar(const SPK_VA::valarray<double> &popPar);
    void setIndPar(const SPK_VA::valarray<double> &indPar);

    void dataMean( SPK_VA::valarray<double>& ret ) const;
    bool dataMean_indPar( SPK_VA::valarray<double>& ret ) const;
    bool dataMean_popPar( SPK_VA::valarray<double>& ret ) const;

    void dataVariance( SPK_VA::valarray<double>& ret ) const;
    bool dataVariance_indPar( SPK_VA::valarray<double>& ret ) const;
    bool dataVariance_popPar( SPK_VA::valarray<double>& ret ) const;

    void dataVarianceInv( SPK_VA::valarray<double>& ret ) const;
    bool dataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;
    bool dataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;

    void indParVariance( SPK_VA::valarray<double>& ret ) const;
    bool indParVariance_popPar( SPK_VA::valarray<double>& ret ) const;

    void indParVarianceInv( SPK_VA::valarray<double>& ret ) const;
    bool indParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;

private:
	virtual void doSelectIndividual ( int base0 );
    virtual void doSetPopPar( const SPK_VA::valarray<double>& inVA );
    virtual void doSetIndPar( const SPK_VA::valarray<double>& inVA ) = 0;

    virtual void doDataMean( SPK_VA::valarray<double>& ret ) const = 0;
    virtual bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const = 0;
    virtual bool doDataMean_popPar( SPK_VA::valarray<double>& ret ) const;

    virtual void doDataVariance( SPK_VA::valarray<double>& ret ) const = 0;
    virtual bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const = 0;
    virtual bool doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const;

    virtual void doDataVarianceInv( SPK_VA::valarray<double>& ret ) const;
    virtual bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;
    virtual bool doDataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;

    virtual void doIndParVariance( SPK_VA::valarray<double>& ret ) const;
    virtual bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const;
    virtual void doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const;
    virtual bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;

protected:
    // These are protected (rather than public) because this is an 
    // abstract class and they should not be called directly.
    // These are protected (rather than private) because subclasses
    // of this class may need to call these functions.
    SpkModel();
    SpkModel( const SpkModel &right );
    SpkModel& operator=(const SpkModel &right);

public:
    // This destructor is public so that it can be called directly.   
    // The destructor must be virtual for an abstract class.
    virtual ~SpkModel();

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Backward Compatable SpkModel Code - Mitch]
    // This code is temporary and should be deleted once all of 
    // the old SpkModel remnants are gone.
    //
public:
    const Covariance& getDataCovariance()   const;
    const Covariance& getIndParCovariance() const;
    //
private:
    FullDataCovariance* pDataCovariance;
    FullIndParCovariance* pIndParCovariance;
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

};

#endif
