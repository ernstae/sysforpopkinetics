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
#ifndef EXPECTEDHESSIAN_H
#define EXPECTEDHESSIAN_H

#include <functional>

#include "SpkValarray.h"
#include "DoubleMatrix.h"
#include "SpkModel.h"

void expectedHessian(
         SpkModel& model, 
         const DoubleMatrix & alp,
         const DoubleMatrix & b,
         const DoubleMatrix & bStep,
         DoubleMatrix * expectedHessianTilde,
         DoubleMatrix * expectedHessianTilde_alp,
         DoubleMatrix * expectedHessianTilde_b
         );
class ExpectedHessian : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>
{
    private:
        SpkModel *_model;
        DoubleMatrix _bStep;

    public:
        ExpectedHessian(SpkModel *m, const DoubleMatrix& bStep)
            : _model(m),  _bStep(bStep)
        {}
        ~ExpectedHessian() throw() 
        {}
        ExpectedHessian(const ExpectedHessian& right)
            : _model(right._model), _bStep(right._bStep)

        {}
        const DoubleMatrix operator()(const DoubleMatrix& dvecAlp, const DoubleMatrix& dvecB) const
        {
            const int nB = dvecB.nr();
            const SPK_VA::valarray<double> alp = dvecAlp.toValarray();
            const SPK_VA::valarray<double> b   = dvecB.toValarray();

            DoubleMatrix expectedHessianTilde(nB, nB);
            _model->setPopPar(alp);
            _model->setIndPar(b);
            expectedHessian(*_model, dvecAlp, dvecB, _bStep, &expectedHessianTilde, 0, 0 );
            return expectedHessianTilde;
        }
};
class ExpectedHessianValarray : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
{
    private:
        SpkModel *_model;
        SPK_VA::valarray<double> _bStep;

    public:
        ExpectedHessianValarray(SpkModel *m, const SPK_VA::valarray<double>& bStep)
            : _model(m),  _bStep(bStep)
        {}
        ~ExpectedHessianValarray() throw() 
        {}
        ExpectedHessianValarray(const ExpectedHessianValarray& right)
            : _model(right._model), _bStep(right._bStep)

        {}
        const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& a, const SPK_VA::valarray<double>& b) const
        {
            int nB = b.size();

            DoubleMatrix expectedHessianTilde( nB, nB );
            _model->setPopPar(a);
            _model->setIndPar(b);
            expectedHessian(*_model, DoubleMatrix( a ), DoubleMatrix( b ), DoubleMatrix( _bStep ), &expectedHessianTilde, 0, 0 );
            return expectedHessianTilde.toValarray();
        }
};
#endif
