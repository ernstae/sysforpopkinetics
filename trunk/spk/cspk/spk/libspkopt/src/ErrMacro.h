# ifndef CppADErrMacroIncluded
# define CppADErrMacroIncluded

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
-------------------------------------------------------------------------------
$begin ErrMacro$$
$spell
	Cpp
	exp
	bool
	ifdef
	std
	cerr
	std::endl
	endif
$$

$mindex error assert ErrMacro$$
$section User Definable Error Macro$$

$table
$bold File$$
$cnext $code CppAD/include/ErrMacro.h$$ $rnext
$bold Syntax$$
$cnext $syntax%CppADTapeOverflow(%exp%)%$$            $rnext
$cnext $syntax%CppADInternalAssert(%exp%)%$$          $rnext
$cnext $syntax%CppADExternalAssert(%exp%, %text%)%$$ 
$tend

$fend 25$$

$head Type of Error$$
Three types of errors are checked for by the 
$code AD$$ template class:
$list number$$
If the $code /Create/CppADCreateTape/TapeLength/TapeLength/1/$$
is not large enough for the calculations being recorded,
$code CppADTapeOverflow$$ is invoked.
$lnext 
If there is a user violation of the specifications 
in this $code /CppAD/$$ documentation
(external to the CppAD source code),
$code CppADExternalAssert$$ is invoked.
$lnext
If an error internal to CppAD source code is detected,
$code CppADInternalAssert$$ is invoked.
$lend

$head CppADExternalAssert$$
This macro detects and handles external errors.
The argument $italic exp$$ is a C++ source code expression that
results in a $code bool$$ value.
It should be true.
If it evaluates to false, an external error has occurred.
The argument $italic text$$ is a $code '\0'$$ terminated
character string that contains a description of the error
in the case where $italic exp$$ is false.

$subhead Example$$
Below is the default CppAD external error handler.
It is located in the file $code CppAD/include/ErrMacro.h$$.
It can be changed
so long as it meets the specifications above.
$codep */

# ifdef NDEBUG
# define CppADExternalAssert(exp, text)  // do nothing
# else
# define CppADExternalAssert(exp, text)                                 \
        if( ! (exp) )                                                   \
                std::cerr << "Error in usage of CppAD " << CppADVersion \
                          << ":\n" << text << std::endl;                \
        assert(exp)
# endif

/* $$

$head CppADTapeOverflow$$
This macro detects and handles tape overflow cases.
The argument $italic exp$$ is a C++ source code expression that
results in a $code bool$$ value.
It should be false.
If it evaluates to true, a tape overflow has occurred.

$subhead Example$$
Below is the default CppAD external error handler.
It is located in the file $code CppAD/include/ErrMacro.h$$.
It can be changed
so long as it meets the specifications above.
$codep */

# ifdef NDEBUG
# define CppADTapeOverflow(exp)                                     \
        if( exp )                                                   \
        {       std::cerr << "CppAD tape overflow, program abort";  \
                std::cerr << std::endl;                             \
                exit(1);                                            \
        }
# else
# define CppADTapeOverflow(exp)                                     \
        CppADExternalAssert( ! (exp) , "CppAD tape overflow" );
# endif

/* $$


$head CppADInternalAssert$$
This macro detects and handles internal errors.
The argument $italic exp$$ is a C++ source code expression that
evaluates to a $code bool$$ value.
It should be true.
If it evaluates to false, an internal error has occurred.

$subhead Example$$
Below is the default CppAD internal error handler.
It is located in the file $code CppAD/include/ErrMacro.h$$.
It can be changed
so long as it meets the specifications above.
$codep */

# ifdef NDEBUG
# define CppADInternalAssert(exp)        // do nothing
# else
# define CppADInternalAssert(exp)                                        \
        if( ! (exp) )                                                    \
                std::cerr << "Error internal to CppAD "  << CppADVersion \
                          << ":\n" << std::endl;                         \
        assert(exp)
# endif

/* $$

$end
-------------------------------------------------------------------------------
*/

# endif
