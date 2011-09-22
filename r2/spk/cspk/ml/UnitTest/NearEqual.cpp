// This is a rouintine that used to be part of the CppAD package. 
// a copy of this routine is included in the RFPK data base (but it is not 
// owned by RFPK)

// BEGIN SHORT COPYRIGHT
/* -----------------------------------------------------------------------
CppAD Copyright (C) 2003 Bradley M. Bell

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
------------------------------------------------------------------------ */
// END SHORT COPYRIGHT

/*
$begin NearEqual$$
$spell
	std
	Cpp
	namespace
	const
	bool
$$

$mindex NearEqual complex error absolute relative$$
$section Determine if Two AD<double> Values Are Nearly Equal$$

$table
$bold Syntax$$ $cnext
$syntax%bool NearEqual(
	const double &%x%, const double &%y%, double r, double a)%$$
$syntax%bool NearEqual(
	const AD<double> &%x%, const AD<double> &%y%, double r, double a)%$$
$syntax%bool NearEqual(
	const std::complex<double> &%x%, 
	const std::complex<double> &%y%, 
	double r, 
	double a)%$$
$syntax%bool NearEqual(
	const AD< std::complex<double> > &%x%, 
	const AD< std::complex<double> > &%y%, 
	double r, 
	double a)%$$
$tend

$fend 20$$

$head Description$$
Returns true if 
$latex \[
	\frac{ | x - y | } { |x| + |y| } \leq r
	\; {\rm or} \;
	| x - y | \leq a
\] $$
Otherwise, it return false.
Note that $italic a$$ is an absolute error bound
and $italic r$$ is a relative error bound.
(This function is defined within the $code CppAD$$ namespace.)

$end

*/

// ------------------------------------------------------------------------
bool NearEqual(double x, double y, double r, double a)
{
	if( x == y )
		return true;

	double ax   = x;
	double ay   = y;
	double ad   = x - y;

	if( ax < 0. )
		ax = - ax;
	if( ay < 0. )
		ay = - ay;
	if( ad < 0. )
		ad = - ad;

	if( ad < a )
		return true;

	if( ad / (ax + ay) < r )
		return true;

	return false;
}
