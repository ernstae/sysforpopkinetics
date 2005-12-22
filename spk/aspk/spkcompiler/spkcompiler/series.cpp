/**
 * @file series.cpp
 * Define series().
 */
#include "series.h"

int series( int a, int d, int n )
{
   int l = a + (n-1)*d;
   return n * (a+l) / 2;
}
