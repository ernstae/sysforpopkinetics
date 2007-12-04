/*************************************************************************
 *//**
 * @file checkParamIdent.h
 * 
 * 
 * Declares checkParamIdent() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CHECKPARAMIDENT_H
#define CHECKPARAMIDENT_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Standard library header files.
#include <string>
#include <vector>

int checkParamIdent( int                                level,
                     int                                nTheta,
                     const std::vector< std::string >&  thetaName,
                     int                                thetaSeed,
                     int                                nIdentComp,
                     int                                nObservType,
                     int                                nDoseType,
                     const std::vector< std::string >&  compOde,
                     const std::vector< std::string >&  observEqn,
                     std::string&                       identStatus );

#endif
