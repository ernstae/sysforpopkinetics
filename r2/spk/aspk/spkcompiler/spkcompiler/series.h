#ifndef SERIES_H
#define SERIES_H

/**
 * @file series.h
 * Declare the series() function.
 */

/**
 * Calculate a + (a+d) + (a+2d) + ... + {a+(n-l)d},
 * where l = a + (n-1)d as the last term.
 * @param    a The base (start) natural number.
 * @param    d The incremental (natural) number.
 * @param    n The number of repetition.
 * @return   n(a+l)/2
 */
int series( int a, int d, int n);

#endif
