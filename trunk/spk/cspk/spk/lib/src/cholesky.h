/*************************************************************************
 * 
 * From:   Resource Facility for Population Kinetics
 *         Department of Bioengineering Box 352255
 *         University of Washington
 *         Seattle, WA 98195-2255
 *
 * Copyright (C) 2002, University of Washington,
 * Resource Facility for Population Kinetics. All Rights Reserved.
 *
 * This software was developed with support from NIH grant RR-12609.
 * Please cite this grant in any publication for which this software
 * is used and send a notification to the address given above.
 *
 * Check for updates and notices at:
 * http://www.rfpk.washington.edu
 *
 *************************************************************************/
#ifndef CHOLESKY_H
#define CHOLESKY_H

/*************************************************************************
 *
 * File: cholesky.h
 *
 * Decompose a symmetric positive definite matrix, A, to C * C = A
 *
 * Author: Sachiko Honda, based on Mitch's inverse()
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: cholesky
 *
 *************************************************************************/
#include <valarray>

const std::valarray<double> cholesky( const std::valarray<double>& A, int n );
#endif