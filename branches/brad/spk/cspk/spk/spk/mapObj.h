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
#ifndef MAPOBJ_H
#define MAPOBJ_H

#include <functional>
#include <typeinfo>

#include "SpkValarray.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"

void mapObj(  SpkModel<double> &model, 
              const DoubleMatrix &dmatY, 
              const DoubleMatrix &dvecB, 
              double *mapObjOut,
              DoubleMatrix *mapObj_bOut,
              bool withD,
              bool isFo,
              const DoubleMatrix* pdvecN = NULL,
              const DoubleMatrix* pdvecBMean = NULL
           );

template <class ElemType>
class MapObj : public std::unary_function<ElemType, ElemType>
{
    private:
        SpkModel<double> *model;
        const ElemType y;
        const ElemType *pN;
        const ElemType *pBMean;
        const bool includeD;
        const bool isFo;

    public:
        MapObj( SpkModel<double> *m, const ElemType& yi, bool withD, bool fo, const ElemType* NforAll = NULL, const ElemType* pBMeanIn = NULL )
            : model(m), y(yi), includeD(withD), isFo(fo), pN(NforAll), pBMean(pBMeanIn)
        {
        }
        ~MapObj() throw() {}
        MapObj(const MapObj& right)
            : model(right.model), y(right.y), includeD(right.includeD), pN(right.pN), pBMean(right.pBMean)
        {}
        const ElemType operator()(const ElemType& b) const
        {
            double mapObjOut;
            mapObj(*model, y, b, &mapObjOut, 0, includeD, isFo, pN, pBMean);
            return ElemType(mapObjOut);
        }
};

static inline int  size(   const SPK_VA::valarray<double> & v )        { return v.size(); }
static inline int  size(   const DoubleMatrix             & V )        { return V.nr() * V.nc(); }
static inline void resize( SPK_VA::valarray<double> & v, int m, int n ){ v.resize(m * n); }
static inline void resize( DoubleMatrix             & V, int m, int n ){ V.resize(m, n); }
template <class ElemType>
class MapObj_b : public std::unary_function<ElemType, ElemType>
{
    private:
        SpkModel<double> *model;
        const ElemType y;
        const bool includeD;
        const bool isFo;
        const ElemType* pN;
        const ElemType* pBMean;

    public:
        MapObj_b(SpkModel<double> *m, const ElemType& yi, bool withD, bool fo, const ElemType* NforAll = NULL, const ElemType* pBMeanIn = NULL )
            : model(m), y(yi), includeD(withD), isFo(fo), pN(NforAll), pBMean(pBMeanIn)
        {
        }
        ~MapObj_b() throw() {}
        MapObj_b(const MapObj_b& right)
            : model(right.model), y(right.y), includeD(right.includeD), 
        isFo(right.isFo), pN(right.pN), pBMean(right.pBMean)
        {}
        const ElemType operator()(const ElemType& b) const
        {
            //
            // [ Comment by Sachiko, 10/10/2002 ]
            // mapObj_bOut needs to know the size of resulting row vector.
            // Since this class is a template class and
            // DoubleMatrix and valarray do not have the same interface
            // for a member function that returns the size of the vector/matrix
            // object, this seems the only way at this point to get
            // the size.
            //
            ElemType mapObj_bOut;
            resize( mapObj_bOut, 1, size( b ) );

            mapObj(*model, y, b, 0, &mapObj_bOut, includeD, isFo, pN, pBMean);
            return mapObj_bOut;
        }
};
#endif
