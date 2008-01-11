#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(QN01Box, 08-01-10, bradbell@apl.washington.edu)
AC_CONFIG_SRCDIR([header.h])
AC_CONFIG_HEADER([config.h])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_F77
AC_PROG_RANLIB

# Checks for libraries.
# FIXME: Replace `main' with a function in `-llapack':
AC_CHECK_LIB([lapack], [main])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([float.h stddef.h stdlib.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

AC_PREFIX_DEFAULT([/usr/local/spk])

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

# Checks for library functions.
AC_CHECK_FUNCS([pow sqrt])

AC_CONFIG_FILES([Makefile
                 Test/Makefile
                 lib/Makefile])
AC_OUTPUT


