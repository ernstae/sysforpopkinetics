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
#ifndef INDRESULTS_H
#define INDRESULTS_H

#include <iostream>
#include "DoubleMatrix.h"

class IndResults
{
public:
    IndResults();
    IndResults(const IndResults&);
    IndResults(int inx, const DoubleMatrix& bh, const DoubleMatrix& bt, const DoubleMatrix& bt_pop);
    IndResults(int inx, const DoubleMatrix& bh, const DoubleMatrix& bt, const DoubleMatrix& bt_pop,
               double dLambda, double dLogdetLambda2diff);
    ~IndResults();

    IndResults&     operator=(const IndResults&);
    bool   operator!=(const IndResults&) const;
    friend std::ostream& operator<<(std::ostream&, const IndResults&);
    friend std::istream& operator>>(std::istream&, IndResults&);

    int   getIndex() const;
    const DoubleMatrix getHat() const;
    const DoubleMatrix getTilde() const;
    const DoubleMatrix getTilde_pop() const;
    const double getLambda() const;
    const double getLogdetLambda2diff() const;

private:
    int          _index;
    DoubleMatrix _indHat;
    DoubleMatrix _indTilde;
    DoubleMatrix _indTilde_pop;
    double       _indLambda;
    double       _indLogdetLambda2diff;
};
#endif
