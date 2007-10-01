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
 * File: Function.h
 *
 *
 * Template Function classes for SpkModel member functions.  
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: ModelFunction class
 *
 *************************************************************************/
#ifndef FUNCTIONOBJECT_H
#define FUNCTIONOBJECT_H

#include <cassert>
#include <functional>

#include "SpkValarray.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"

class ModelFunction : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>
{
public:
    typedef void (SpkModel<double>::*model_proto)( DoubleMatrix& ) const;
private:
    model_proto fun;    // fun is a pointer to a SpkModel member function
    SpkModel<double>  *model;  // a pointer to the SpkModel object
    mutable DoubleMatrix ret;
public:

    ModelFunction(const model_proto f, SpkModel<double>* m);
    ~ModelFunction() throw();
    ModelFunction(const ModelFunction& right);
    const DoubleMatrix operator()(const DoubleMatrix& X1, const DoubleMatrix& X2) const;
};
class ModelFunctionValarray : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
{
public:
    typedef void (SpkModel<double>::*model_proto)( SPK_VA::valarray<double>& ) const;
private:
    model_proto fun;    // fun is a pointer to a SpkModel member function
    SpkModel<double>  *model;  // a pointer to the SpkModel object
    mutable SPK_VA::valarray<double> ret;
public:

    ModelFunctionValarray(const model_proto f, SpkModel<double>* m);
    ~ModelFunctionValarray() throw();
    ModelFunctionValarray(const ModelFunctionValarray& right);
    const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& x1, const SPK_VA::valarray<double>& x2) const;
};


class ModelDerivative : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>
{
public:
    typedef bool (SpkModel<double>::*model_proto)( DoubleMatrix& ) const;
private:
    model_proto fun;    // fun is a pointer to a SpkModel member function
    SpkModel<double>  *model;  // a pointer to the SpkModel object
    mutable DoubleMatrix ret;
public:

    ModelDerivative(const model_proto f, SpkModel<double>* m);
    ~ModelDerivative() throw();
    ModelDerivative(const ModelDerivative& right);
    const DoubleMatrix operator()(const DoubleMatrix& X1, const DoubleMatrix& X2) const;
};
class ModelDerivativeValarray : public std::binary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double>, SPK_VA::valarray<double> >
{
public:
    typedef bool (SpkModel<double>::*model_proto)( SPK_VA::valarray<double>& ) const;
private:
    model_proto fun;    // fun is a pointer to a SpkModel member function
    SpkModel<double>  *model;  // a pointer to the SpkModel object
    mutable SPK_VA::valarray<double> ret;
public:

    ModelDerivativeValarray(const model_proto f, SpkModel<double>* m);
    ~ModelDerivativeValarray() throw();
    ModelDerivativeValarray(const ModelDerivativeValarray& right);
    const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& x1, const SPK_VA::valarray<double>& x2) const;
};

#endif
