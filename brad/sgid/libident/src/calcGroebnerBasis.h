/*************************************************************************
 *//**
 * @file: calcGroebnerBasis.h
 *
 *
 * Declares calcGroebnerBasis() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CALCGROEBNERBASIS_H
#define CALCGROEBNERBASIS_H

extern "C"
{

int calcGroebnerBasis( int         level,
                       int         nTheta,
                       char**      thetaName,
                       int         thetaSeed,
                       int         nIdentComp,
                       int         nObservType,
                       int         nDoseType,
                       const char* sysExpModelRegChainIn,
                       const char* naturalOrderingIn,
                       const char* charSetOrderingIn,
                       int**       nGroebnerBasisPolyEachOut,
                       int*        nGroebnerBasisPolyTotalOut,
                       char***     groebnerBasisPolyAllOut );

}

#endif

