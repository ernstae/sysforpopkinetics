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
 * File: lambda.h
 *
 *
 * Evaluates the negative logarithm of the integrand that appears 
 * in the population expected determinant optimal design criterion.  
 *
 * Note that the implementation of lambda has been modified for
 * use in the population optimal design system.  The interface for
 * lambda has not changed, however, which means that the code portion
 * of this file is a copy of the original version of lambda.h.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef LAMBDA_H
#define LAMBDA_H

#include <functional>
#include "SpkModel.h"
#include "DoubleMatrix.h"

double             lambda(SpkModel& model,
                          const DoubleMatrix &y,
                          const DoubleMatrix &alp,
                          const DoubleMatrix &b,
                          bool  withD=true);
const DoubleMatrix lambda_alp(SpkModel& model,
                          const DoubleMatrix &y,
                          const DoubleMatrix &alp,
                          const DoubleMatrix &b,
                          bool  withD=true);
const DoubleMatrix lambda_b(SpkModel& model,
                          const DoubleMatrix &y,
                          const DoubleMatrix &alp,
                          const DoubleMatrix &b,
                          bool  withD=true);

class Lambda : public std::binary_function<DoubleMatrix, DoubleMatrix, double>
{
    private:
        SpkModel *model;
        const DoubleMatrix y;
        const bool includeD;

    public:
        Lambda(SpkModel *m, const DoubleMatrix& yi, bool withD)
            : model(m), y(yi), includeD(withD)
        {}
        ~Lambda() throw() 
        {}
        Lambda(const Lambda& right)
            : model(right.model), y(right.y), includeD(right.includeD)
        {}
        const double operator()(const DoubleMatrix& alp, const DoubleMatrix& b) const
        {
            return lambda(*model, y, alp, b, includeD);
        }
};

/*
class LambdaValarray : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
{
    private:
        SpkModel *model;
        const SPK_VA::valarray<double> y;
        const bool includeD;
        mutable DoubleMatrix dvecY;
        mutable DoubleMatrix dvecB;
        mutable DoubleMatrix dvecAlp;

    public:
        LambdaValarray(SpkModel *m, const SPK_VA::valarray<double>& yi, bool withD)
            : model(m), y(yi), includeD(withD)
        {
        }
        ~LambdaValarray() throw() 
        {}
        LambdaValarray(const LambdaValarray& right)
            : model(right.model), y(right.y), includeD(right.includeD)
        {}
        const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& alp, const SPK_VA::valarray<double>& b) const
        {
          dvecAlp.fromValarray(alp);
          dvecB.fromValarray(b);
          dvecY.fromValarray(y);

          return SPK_VA::valarray<double>( lambda(*model, dvecY, dvecAlp, dvecB, includeD), 1 );
        }
};
*/

class Lambda_alp : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>
{
    private:
        SpkModel *model;
        const DoubleMatrix y;
        const bool includeD;

    public:
        Lambda_alp(SpkModel *m, const DoubleMatrix& yi, bool withD)
            : model(m), y(yi), includeD(withD)
        {}
        ~Lambda_alp() throw() {}
        Lambda_alp(const Lambda_alp& right)
            : model(right.model), y(right.y), includeD(right.includeD)
        {}
        const DoubleMatrix operator()(const DoubleMatrix& a, const DoubleMatrix& b) const
        {
            // returns nA dimensional row vector
            return lambda_alp(*model, y, a, b, includeD);
        }
};
class Lambda_b : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>
{
    private:
        SpkModel *model;
        const DoubleMatrix y;
        const bool includeD;

    public:
        Lambda_b(SpkModel *m, const DoubleMatrix& yi, bool withD)
            : model(m), y(yi), includeD(withD)
        {}
        ~Lambda_b() throw() {}
        Lambda_b(const Lambda_b& right)
            : model(right.model), y(right.y), includeD(right.includeD)
        {}
        const DoubleMatrix operator()(const DoubleMatrix& a, const DoubleMatrix& b) const
        {
            // returns nB dimensional row vector
            return lambda_b(*model, y, a, b, includeD);
        }
};

#endif
