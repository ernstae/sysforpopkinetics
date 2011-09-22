# ifndef MemoryIncluded
# define MemoryIncluded
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

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Author: Brad Bell (brad@apl.washington.edu)

*/

/*
$begin Memory$$
$spell
	ntot
	subblocks
$$

$section A Fast and Simple Memory Management Template Class$$

$table
$bold Syntax$$ 
$cnext $syntax%Memory<%Type%> %buffer%(size_t %ntot%)%$$  $rnext
$cnext $syntax%%Type% *%buffer%(size_t %m%)%$$  $rnext
$tend

$fend 25$$

$head Constructor$$
The operation
$syntax%
	Memory<%Type%> %buffer%(size_t %ntot%)
%$$ 
allocates $italic ntot$$ $italic Type$$ objects with the
corresponding default constructor for each object.
Subblocks of this array can be accessed through $italic buffer$$.

$head Subblocks$$
The operation
$syntax%
	%Type% *%buffer%(size_t %m%)
%$$
returns a pointer to an array of $italic m$$ object of
type $italic Type$$.
Between the constructor for $italic buffer$$ and
its destructor,
the total sum of the arguments $italic m$$ used with
a specific $italic buffer$$ must equal the $italic ntot$$
used to construct $italic buffer$$ (no more and no less or
an program $code assert$$ will occur).

$head Destructor$$
The pointers returned by $syntax%%buffer%(%m%)%$$ are valid
until the destructor for the variable $italic buffer$$ is called.

$end
*/

# include <cstddef>
# include <cassert>

template <class Type>
class Memory {
public:
	// constructor
	explicit inline Memory(size_t n) : p(new Type[n + 2]) , ntot(n + 2), next(1)
	{	// set up an electric fence to check for memory overwrite	
		first = (unsigned char *) p;
		last  = (unsigned char *)  (p + n + 1);

		*first = 255;
		*last  = 254;
       	}

	// destructor
	~Memory(void)
	{	// check the total amount of allocated memory
		assert(next+1 == ntot);

		// check for a memory overwrite
		assert( *first == 255 );
		assert( *last  == 254 );

		delete [] p; 
	}

	Type * operator()(size_t m)
	{	// allocate the next chunck of memory
		next = next + m;
		assert(next < ntot);
		return p + next - m;
	}

private:
	Type * const p;
	size_t ntot; 
	size_t next;
	unsigned char *first;
	unsigned char *last;
};

# endif
