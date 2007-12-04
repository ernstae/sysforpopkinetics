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
#ifndef INDVARS_H
#define INDVARS_H

#include <iostream>
#include "DoubleMatrix.h"

class IndVars
{
public:
    IndVars();
    IndVars(int, const DoubleMatrix&, const DoubleMatrix&);
    IndVars(const IndVars&);
    ~IndVars();

    IndVars& operator=(const IndVars&);
    bool   operator!=(const IndVars&) const;
    friend std::ostream& operator<<(std::ostream&, const IndVars&);
    friend std::istream& operator>>(std::istream&, IndVars&);

    const DoubleMatrix getIn() const;
    const DoubleMatrix getData() const;
    int who() const;

private:
    DoubleMatrix _indIn;
    DoubleMatrix _indData;
    int          _who;
};
#endif
