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
 * File: PopConstVals.h
 *
 * Declares class PopConstVals
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#ifndef POPCONSTVALS_H
#define POPCONSTVALS_H

#include <iostream>

#include "DoubleMatrix.h"
#include "Optimizer.h"

class PopConstVals
{
public:
    PopConstVals();
    PopConstVals(int size, Optimizer& optimizer, int objective,
        const DoubleMatrix& low, const DoubleMatrix& up, const DoubleMatrix& step);
    PopConstVals(const PopConstVals& right);
    ~PopConstVals();
    
    PopConstVals&   operator=(const PopConstVals& right);
    bool   operator!=(const PopConstVals& right) const;
    friend std::ostream& operator<<(std::ostream&, const PopConstVals&);
    friend std::istream& operator>>(std::istream&, PopConstVals&);

    int        getSize() const;
    Optimizer&  getOptimizer() const;
    void       setOptimizer( const Optimizer optimizer );
    int        getObjective() const;
    const      DoubleMatrix getLow() const;
    const      DoubleMatrix getUp() const;
    const      DoubleMatrix getStep() const;

private:
    int          _size;
    Optimizer&   _optimizer;
    int          _objective;
    DoubleMatrix _indLow;
    DoubleMatrix _indUp;
    DoubleMatrix _indStep;
};
#endif
