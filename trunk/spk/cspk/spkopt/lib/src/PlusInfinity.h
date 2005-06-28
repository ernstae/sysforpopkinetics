# ifndef PlusInfinityIncluded
# define PlusInfinityIncluded

/*
-----------------------------------------------------------------------
From:   Resource Facility for Population Kinetics
          Department of Bioengineering Box 352255
          University of Washington
          Seattle, WA 98195-2255

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2005, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Software:   Brad Bell (brad@apl.washington.edu)

$begin PlusInfinity$$
$spell
	const
$$

$index infinity, plus$$
$index PlusInfinity$$
$index ieee, plus infinity$$
$index ieee, infinity$$

$section Returns the IEEE Floating Point Value Plus Infinity$$

$table
$bold Syntax$$ $cnext $syntax%PlusInfinity(const <%Type%> &%zero%)%$$
$tend

$fend 20$$

$head Description$$
This routine
returns the ieee value $latex + \infty$$ with the 
same type as $italic zero$$
(where $italic zero$$ is equal to zero).
Note that passing in $italic zero$$ as an argument avoids
having the compiler detect division by zero 
and warn or generate an error in response.

$head Type$$
Conversion from integer to this type must be supported.

$end
--------------------------------------------------------------------------------
*/
template <typename Type>
Type PlusInfinity(Type zero)
{	return Type(1) / zero; } 

# endif
