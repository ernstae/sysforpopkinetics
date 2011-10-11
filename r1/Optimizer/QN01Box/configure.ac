dnl From:   Resource Facility for Population Kinetics
dnl           Department of Bioengineering Box 352255
dnl           University of Washington, Seattle, WA 98195-2255
dnl 
dnl This file is part of the System for Population Kinetics (SPK), which
dnl was developed with support from NIH grants RR-12609 and P41-
dnl EB001975. Please cite these grants in any publication for which this
dnl software is used and send a notification to the address given above.
dnl 
dnl SPK is Copyright (C) 1998-2005, by the University of Washington,
dnl Resource Facility for Population Kinetics, and is made available as
dnl free open source software under the terms of the University of
dnl Washington Free-Fork License as a public service.  A copy of the
dnl License can be found in the COPYING file in the root directory of this
dnl distribution.
dnl -----------------------------------------------------------------------
dnl Software:   Brad Bell (brad@apl.washington.edu)
dnl Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)

dnl I (Brad Bell) hereby grant the University of Washington the right to use
dnl this a modified version of CppAD-05-10-12/configure.ac in the sense of the
dnl COPYING file mentioned above.

dnl     package,          version,  bugreport
AC_INIT(QN01Box, 11-10-11, bradbell@apl.washington.edu)

dnl The postfix directiory corresponding to this installation of QN01Box --
AC_MSG_CHECKING( [POSTFIX_DIR] )
AC_SUBST(POSTFIX_DIR)
AM_CONDITIONAL(QN01Box_POSTFIX_DIR, test "$POSTFIX_DIR" != "" )
if test "$POSTFIX_DIR" != "" ; then
        AC_MSG_RESULT( [POSTFIX_DIR=$POSTFIX_DIR] )
else
        AC_MSG_RESULT( [no] )
fi

dnl The prefix corresponding to the CppAD installation ------------------
AC_MSG_CHECKING( [CPPAD_PREFIX_DIR] )
AC_SUBST(CPPAD_PREFIX_DIR)
AM_CONDITIONAL(QN01Box_CPPAD_PREFIX_DIR, test "$CPPAD_PREFIX_DIR" != "" )
if test "$CPPAD_PREFIX_DIR" != "" ; then
	AC_MSG_RESULT( [CPPAD_PREFIX_DIR=$CPPAD_PREFIX_DIR] ) 
else
	AC_MSG_RESULT( [no] )
fi 

dnl The suffix corresponding to the CppAD installation ------------------
AC_MSG_CHECKING( [CPPAD_POSTFIX_DIR] )
AC_SUBST(CPPAD_POSTFIX_DIR)
AM_CONDITIONAL(QN01Box_CPPAD_POSTFIX_DIR, test "$CPPAD_POSTFIX_DIR" != "" )
if test "$CPPAD_POSTFIX_DIR" != "" ; then
	AC_MSG_RESULT( [CPPAD_POSTFIX_DIR=$CPPAD_POSTFIX_DIR] ) 
else
	AC_MSG_RESULT( [no] )
fi 


dnl The C++ error and warning flags for every compliation ---------------
AC_MSG_CHECKING( [COMPILE_FLAGS] )
AC_SUBST(COMPILE_FLAGS)
AM_CONDITIONAL(QN01Box_COMPILE_FLAGS, test "$COMPILE_FLAGS" != "" )
if test "$COMPILE_FLAGS" != "" ; then
	AC_MSG_RESULT( [COMPILE_FLAGS=$COMPILE_FLAGS] ) 
else
	AC_MSG_RESULT( [no] )
fi 
dnl Set the default install prefix to the installers home directory
AC_PREFIX_DEFAULT(${HOME})

dnl Run any macros required for proper operation of generated Makefiles
dnl nostdinc: Do not define standard include directories in generated Makefiles
AM_INIT_AUTOMAKE(nostdinc)

dnl Determine if ranlib is present and where it is if present (set RANLIB)
AC_PROG_RANLIB

dnl AC_PROG_CC([compiler-search-list]) outputs CC as C compiler to use
AC_PROG_CC

dnl AC_PROG_F77([compiler-search-list]) outputs F77 as Fortran compiler to use
AC_PROG_F77

dnl AC_PROG_CXX([compiler-search-list]) outputs CXX as C++ compiler to use
AC_PROG_CXX

dnl No default setting for compiler flags
AC_SUBST(CXXFLAGS, "")
AC_SUBST(CFLAGS, "")
AC_SUBST(FFLAGS, "")

dnl Have configure check distribution directory for QN01Box/QuasiNewton01Box.h 
AC_CONFIG_SRCDIR([QN01Box/QuasiNewton01Box.h])

dnl Use config.h instead of make file command line arguments
AM_CONFIG_HEADER([QN01Box/config.h])

dnl AC_PROG_CP outputs CPP as command to run C preprocessor
AC_PROG_CPP

dnl Do nothing if complier supports inline, otherwise try to find a subsitute
dnl definition or, if cannot, define inline as empty
AC_C_INLINE

dnl AC_CONFIG_FILES(file-list) for each file in the list, configure will 
dnl read file.in, do its substitutions, and create file
AC_CONFIG_FILES([ 
	Makefile
	lib/Makefile 
	Test/Makefile 
])


dnl create all the requested output files
AC_OUTPUT
