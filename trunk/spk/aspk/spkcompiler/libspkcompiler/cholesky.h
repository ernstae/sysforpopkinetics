#ifndef CHOLESKY_H
#define CHOLESKY_H


/**
 * @file cholesky.h
 * Declare cholesky() function.
 */
/**
 * Perform the Cholesky factorization on symmetric positive definite
 * matrix, A.
 *
 * @return n by n matrix, C, such that A = C*C^t.  The lower triangle
 * contains the values for C.  The upper triangle is filled with zeros.
 *
 * @param A n by n matrix to be factored. Only the lower triangle is referenced.
 * @param n The order of A.
 */
#include <valarray>

const std::valarray<double> cholesky( const std::valarray<double>& A, int n );

#endif
