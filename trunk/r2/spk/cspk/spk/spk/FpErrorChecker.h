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
#ifndef FPERRORCHECKER_H
#define FPERRORCHECKER_H

#ifdef WIN32
    #include <cfloat>
#endif

class FpErrorChecker
{
    static const unsigned int _default_control;
    static const unsigned int _default_mask;
    static int   _cnt;
    static unsigned int _preserved_control;
    static unsigned int _preserved_status;

    inline static unsigned int SpkControl(unsigned int control, unsigned int mask)
    {
#ifdef WIN32
        return _controlfp(control, mask);
#endif
    }
    inline static unsigned int SpkStatus()
    {
#ifdef WIN32
        return _statusfp();
#endif
    }
    inline static unsigned int SpkClear()
    {
#ifdef WIN32
        return _clearfp();
#endif
    }

public:
    FpErrorChecker();
    ~FpErrorChecker();

    static unsigned int check(unsigned int line, const char* filename);
    static unsigned int clear();
    static void print();
};

#endif
