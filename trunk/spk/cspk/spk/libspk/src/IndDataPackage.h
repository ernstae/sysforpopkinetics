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
#ifndef INDDATAPACKAGE_H
#define INDDATAPACKAGE_H

#include <iostream>
#include "PopConstVals.h"
#include "PopVars.h"
#include "IndVars.h"
#include "IndResults.h"

class IndDataPackage
{
public:
    IndDataPackage() : isEmpty(true), index(-1){};
    virtual ~IndDataPackage(){};
    inline bool     empty(){ return isEmpty; };
protected:
    bool     isEmpty;
    int      index;
};


class IndInputDataPackage : public IndDataPackage
{
public:
    PopConstVals popConstVals;
    PopVars      popVars;
    IndVars      indVars;
    int          popItr;

    IndInputDataPackage();
    IndInputDataPackage(const IndInputDataPackage&);
    IndInputDataPackage(
        int   curPopItr,
        const PopConstVals& popconst, 
        const PopVars& popvars, 
        const IndVars& indvars);
    ~IndInputDataPackage();
    
    bool empty() const;
    IndInputDataPackage& operator=(const IndInputDataPackage&);
    bool   operator!=(const IndInputDataPackage&) const;
    friend std::ostream& operator<<(std::ostream&, const IndInputDataPackage&);
    friend std::istream& operator>>(std::istream&, IndInputDataPackage&);
};


class IndOutputDataPackage : public IndDataPackage
{
public:
    IndResults indResults;
    int        popItr;

    IndOutputDataPackage();
    IndOutputDataPackage(const IndOutputDataPackage&);
    IndOutputDataPackage(int curPopItr, const IndResults& results);
    ~IndOutputDataPackage();

    bool empty() const;
    IndOutputDataPackage& operator=(const IndOutputDataPackage&);
    bool   operator!=(const IndOutputDataPackage&) const;
    friend std::ostream& operator<<(std::ostream&, const IndOutputDataPackage&);
    friend std::istream& operator>>(std::istream&, IndOutputDataPackage&);
};

#endif
