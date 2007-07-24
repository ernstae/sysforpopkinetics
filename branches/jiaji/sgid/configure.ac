#*************************************************************************
#
# File: configure.ac
#
#
# Process this file with autoconf to produce a configure script.
#
# The initial version of the file was created with autoscan.
#
# Author: Mitch Watrous
#
#*************************************************************************

AC_PREREQ(2.59)
AC_INIT(identifiabily, 1.0, watrous@u.washington.edu)

AC_CONFIG_SRCDIR([libident/src/calcGroebnerBasis.c])
AC_CONFIG_HEADER([config.h])

# Macros for automake.
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_INSTALL

# Checks for libraries.
AC_CHECK_LIB(cln, ginac, bad, bap, bav, ba0)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Clear the default setting for the C compiler flags.
AC_SUBST(CFLAGS,   "")

# Clear the default setting for the C++ compiler flags.
AC_SUBST(CXXFLAGS, "")

# This will check to see if the user used the --enable-debug=(yes|no)
# option when they called configure.
AC_ARG_ENABLE(debug,
[  --enable-debug    Turn on debugging],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
esac],[debug=false])
AM_CONDITIONAL(DEBUG, test x$debug = xtrue)

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([floor pow])

# Generate these Makefiles.
AC_OUTPUT(Makefile 
          libident/Makefile
          libident/src/Makefile
          libident/test/Makefile
          check_ident/Makefile
          check_ident/src/Makefile
          check_ident/test/Makefile)

