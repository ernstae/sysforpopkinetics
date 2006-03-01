dnl     package  version                    bug report
AC_INIT( MonteTest, 06-01-27,  bradbell@apl.washington.edu)

dnl Run any macros requried for automake to generate Makefiles
dnl nostdinc: Do not define standard include directories in Makefiles
AM_INIT_AUTOMAKE(nostdinc)

dnl determine the C compiler
AC_PROG_CC

dnl determine the C++ compiler
AC_PROG_CXX

dnl no default setting for compiler flags
AC_SUBST(CXXFLAGS, "")

dnl use config.h instead of make file command line arugments
AC_CONFIG_HEADER(config.h)

dnl create file.in for each of the following
AC_CONFIG_FILES([Makefile])

dnl create all the requested output file
AC_OUTPUT
