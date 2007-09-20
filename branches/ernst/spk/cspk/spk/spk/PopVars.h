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
#ifndef POPVARS_H
#define POPVARS_H

#include <iostream>
#include "DoubleMatrix.h"

class PopVars
{
public:
    PopVars();
    ~PopVars();
    PopVars(const PopVars&);
    PopVars(const DoubleMatrix& cur_pop, bool, bool, bool);

    PopVars& operator=(const PopVars&);
    bool operator!=(const PopVars&) const;
    friend std::ostream& operator<<(std::ostream&, const PopVars&);
    friend std::istream& operator>>(std::istream& stream, PopVars& right);

    const DoubleMatrix getPop() const;
    bool  isHat() const;
    bool  isTilde() const;
    bool  isTilde_pop() const;

private:
    DoubleMatrix _pop;
    bool _isIndHat;
    bool _isIndTilde;
    bool _isIndTilde_pop;
};
#endif
