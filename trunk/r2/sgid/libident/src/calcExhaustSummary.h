/*************************************************************************
 *//**
 * @file: calcExhaustSummary.h
 *
 *
 * Declares calcExhaustSummary() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CALCEXHAUSTSUMMARY_H
#define CALCEXHAUSTSUMMARY_H

extern "C"
{

int calcExhaustSummary( int         level,
                        int         nTheta,
                        char**      thetaName,
                        int         thetaSeed,
                        int         nIdentComp,
                        int         nObservType,
                        int         nDoseType,
                        const char* sysExpModelRegChainIn,
                        const char* naturalOrderingIn,
                        const char* charSetOrderingIn,
                        char***     exhaustSummaryPolyOut );

}

#endif
