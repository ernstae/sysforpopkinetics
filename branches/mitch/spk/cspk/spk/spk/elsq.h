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
 * elsq.h
 *
 * Returns 0.5 * logdet[2*pi*Q(x)] + 0.5*[z-h(x)]^T * Q(x)^-1 * [z-h(x)]
 *
 * Author: Sachiko Honda
 */
#ifndef ELSQ_H
#define ELSQ_H

#include <iostream>
#include <functional>

#include "SpkValarray.h"
#include "DoubleMatrix.h"

double elsq(const DoubleMatrix &dvecZ, 
			const DoubleMatrix &dvecH, 
			const DoubleMatrix &dmatQ, 
			const DoubleMatrix &dmatInvQ);

double elsq(const DoubleMatrix &dvecR, // z-h 
			const DoubleMatrix &dmatQ, 
			const DoubleMatrix &dmatInvQ);

double elsq(
            const SPK_VA::valarray<double> &z,
            const SPK_VA::valarray<double> &h,
            const SPK_VA::valarray<double> &Q,
            const SPK_VA::valarray<double> &invQ 
            );

//
// function object that evaluates elsq
//
#include "subtract.h"
template <class Model>
class Elsq : public std::unary_function<DoubleMatrix, DoubleMatrix>
{
    private:
        const DoubleMatrix _dvecY;
        Model *model;
        int _nY;

    public:
        Elsq( Model *m, const DoubleMatrix& yi ) : model(m), _dvecY(yi), _nY(0)
        {
          _nY = _dvecY.nr();
        };
        ~Elsq() throw() {};
        Elsq(const Elsq& right) : _nY(right._nY) {};
    const DoubleMatrix operator()(const DoubleMatrix& dvecB) const
    {
        SPK_VA::valarray<double> b = dvecB.toValarray();
        model->setIndPar(b);

        SPK_VA::valarray<double> h;
        model->dataMean(h);
        DoubleMatrix dvecH( h, 1 );

        SPK_VA::valarray<double> Q;
        model->dataVariance(Q);
        DoubleMatrix dmatQ( Q, _nY );

        SPK_VA::valarray<double> Qinv;
        model->dataVarianceInv(Qinv);
        DoubleMatrix dmatQinv( Qinv, _nY );

        DoubleMatrix dvecResidual = subtract(_dvecY, dvecH);

        DoubleMatrix s = elsq(dvecResidual, dmatQ, dmatQinv);
        return s;
    }
};
/*
template <class Model>
class ElsqValarray : public std::unary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double> >
{
    private:
        const SPK_VA::valarray<double> _y;
        Model *model;
        mutable SPK_VA::valarray<double> h;
        mutable SPK_VA::valarray<double> Q;
        mutable SPK_VA::valarray<double> Qinv;
        mutable SPK_VA::valarray<double> r;

    public:
        ElsqValarray( Model *m, const SPK_VA::valarray<double>& yi ) : model(m), _y(yi) {};
        ~ElsqValarray() throw() {};
        ElsqValarray(const Elsq& right) : _y(right._y) {};
        const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& b) const
    {
        int nH = h.size();
        DoubleMatrix rDM;
        DoubleMatrix QDM;
        DoubleMatrix QinvDM;

        model->setIndPar(b);
        model->dataMean(h);
        model->dataVariance(QDM);
        model->dataVarianceInv(QinvDM);
        rDM = DoubleMatrix(_y - h);


        DoubleMatrix s( elsq(rDM, QDM, QinvDM) );
        return s.toValarray();
    }
};
*/
#endif
