dnl     package,          version,  bugreport
AC_INIT(non_par, 07-10-02, bradbell@apl.washington.edu)

dnl Set the default install prefix to the installers home directory
AC_PREFIX_DEFAULT(${HOME})

#dnl The directory corresponding to the installation of boost ------------
AC_MSG_CHECKING( [BOOST_DIR] )
AC_SUBST(BOOST_DIR)
if test "$BOOST_DIR" != "" ; then
        AC_MSG_RESULT( [ BOOST_DIR=$BOOST_DIR ] )
else
        AC_MSG_WARN( [no] )
fi

dnl The prefix corresponding to the installation of mat2cpp ------------
AC_MSG_CHECKING( [MAT2CPP_PREFIX] )
AC_SUBST(MAT2CPP_PREFIX)
if test "$MAT2CPP_PREFIX" != "" ; then
        AC_MSG_RESULT( [ MAT2CPP_PREFIX=$MAT2CPP_PREFIX ] )
else
        AC_MSG_WARN( [no] )
fi

dnl The prefix corresponding to the installation of mat2cpp ------------
AC_MSG_CHECKING( [QN01BOX_PREFIX] )
AC_SUBST(QN01BOX_PREFIX)
if test "$QN01BOX_PREFIX" != "" ; then
        AC_MSG_RESULT( [ QN01BOX_PREFIX=$QN01BOX_PREFIX ] )
else
        AC_MSG_WARN( [no] )
fi

dnl The postfix directiory corresponding to this installation --
AC_MSG_CHECKING( [POSTFIX_DIR] )
AC_SUBST(POSTFIX_DIR)
AM_CONDITIONAL(non_par_POSTFIX_DIR, test "$POSTFIX_DIR" != "" )
if test "$POSTFIX_DIR" != "" ; then
        AC_MSG_RESULT( [POSTFIX_DIR=$POSTFIX_DIR] )
else
        AC_MSG_RESULT( [no] )
fi

dnl The compiler flags --------------------------------------------------
AC_MSG_CHECKING( [COMPILE_FLAGS] )
AC_SUBST(COMPILE_FLAGS)
if test "$COMPILE_FLAGS" != "" ; then
        AC_MSG_RESULT( [ COMPILE_FLAGS=$COMPILE_FLAGS ] )
else
        AC_MSG_WARN( [no] )
fi

dnl Run any macros required for proper operation of generated Makefiles
dnl nostdinc: Do not define standard include directories in generated Makefiles
AM_INIT_AUTOMAKE(nostdinc)

dnl Use config.h instead of make file command line arguments
AM_CONFIG_HEADER( config.h )

dnl AC_PROG_CXX([compiler-search-list]) outputs CXX as C++ compiler to use
AC_PROG_CXX

dnl No default setting for compiler flags
AC_SUBST(CXXFLAGS, "")

dnl Determine if ranlib is present and where it is if present (set RANLIB)
AC_PROG_RANLIB

dnl Have configure check distribution directory 
AC_CONFIG_SRCDIR([matlab/npipp_of.m])

AC_CHECK_LIB([boost], [main], [],[
echo "
------------------------------------------------------------
This package requires the boost library, which can be 
installed on most systems using RPM packages.  Try running
the command:
   yum install boost boost-devel
if you are on a Fedora/Red Hat/CentOS/Scientific Linux
system.
------------------------------------------------------------
"
])

AC_CHECK_LIB([mat2cpp],[main],[],[
echo "
------------------------------------------------------------ 
You cannot build this library properly without the presence of
the mat2cpp library.  This library can be downloaded from
http://toronto.rfpk.washington.edu/doc/mat2cpp and must be
installed before you continue installing the non_par library.
------------------------------------------------------------
"
])

AC_CHECK_LIB([QN01Box], [main], [], [
echo "
------------------------------------------------------------
This package requires QN01Box to be installed before you 
can continue.  You can install this from the SPK source
which is available via:

  http://www.rfpk.washington.edu
------------------------------------------------------------
"
])

dnl AC_CONFIG_FILES(file-list) for each file in the list, configure will 
dnl read file.in, do its substitutions, and create file
AC_CONFIG_FILES([ 
	Makefile
	cpp/Makefile
])

dnl create all the requested output files
AC_OUTPUT
