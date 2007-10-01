#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(spkpred, 0.99, watrous@u.washington.edu)
AC_CONFIG_SRCDIR([spkpred/OdePredBase.h])
AC_CONFIG_HEADER([config.h])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LIBTOOL

# set the default install prefix
AC_PREFIX_DEFAULT([/usr/local/spk])

# Set an extra command line option "--enable-release-build" to 'configure'.
# if the user specifies this option, a variable "release_build" gets set to TRUE
AC_ARG_ENABLE(release_build,
	AC_HELP_STRING( [--enable-release-build], [optimize SPK Library for speed] ),
	[release_build=TRUE], 
	[release_build=FALSE]
)

# Define a macro SPK_RELEASE so that Makefile can switch its behavior and
# build an optimized version of SPK.
AM_CONDITIONAL(SPK_RELEASE, test $release_build = TRUE)

# Display "checking for build mode..." message on screen when configure is run.
AC_MSG_CHECKING( [for build mode] )

# if $release_build is set to yes, then define SPK_RELEASE to be 1 otherwise 0
if test $release_build = TRUE; then
   	AC_DEFINE(SPK_RELEASE, 1, "build release version of SPK(optimized)")
	AC_MSG_RESULT( [release] )
else
	AC_DEFINE(SPK_RELEASE, 0, "building debugging version of SPK")
	AC_MSG_RESULT( [debug] )
fi




# Checks for libraries.



use_atlas=yes
AC_CHECK_LIB([atlas],[main],[LIBS=$LIBS -latlas],[use_atlas=no])
if test x$use_atlas = xyes; then
AC_CHECK_LIB([f77blas],[main],[LIBS="$LIBS -lf77blas"],[use_atlas=no])
fi
if test x$use_atlas = xyes; then
AC_CHECK_LIB([cblas],[main],[LIBS="$LIBS -lcblas"],[use_atlas=no])
fi
if test x$use_atlas = xyes; then
AC_CHECK_LIB([lapack],[main],[LIBS="$LIBS -llapack"],[use_atlas=no])
fi
if test x$use_atlas = xyes; then
AC_CHECK_LIB([lapack_atlas],[main],[LIBS="$LIBS -llapack_atlas"], [use_atlas=no])
fi
if test x$use_atlas = xno; then
AC_CHECK_LIB([gslcblas],[main],[LIBS="$LIBS -lgslcblas"],[
echo \
"------------------------------------------------------------------------
ERROR: Could not find a CBLAS implementation.
Tried both ATLAS cblas and gslcblas.
------------------------------------------------------------------------"
exit
])
fi


# FIXME: Replace `main' with a function in `-lQN01Box':
AC_CHECK_LIB([QN01Box], [main])
# FIXME: Replace `main' with a function in `-latlas':
AC_CHECK_LIB([atlas], [main],, AC_MSG_WARN([no atlas]))

# Check for BLAD libraries
AC_CHECK_LIB([ba0], [main])
AC_CHECK_LIB([bad], [main])
AC_CHECK_LIB([bap], [main])
AC_CHECK_LIB([bav], [main])

# FIXME: Replace `main' with a function in `-lcblas':
#AC_CHECK_LIB([cblas], [main])


AC_CHECK_LIB([gsl],[main],[LIBS="$LIBS -lgsl"],[
echo \
"------------------------------------------------------------------------
ERROR: libgsl appears to be missing. You cannot sanely try to build
SPK without libgsl, the GNU Scientific Library. It is available from
http://www.gnu.org/ and compiles readily on most systems.
------------------------------------------------------------------------"
exit
],[-lgslcblas])

dnl Checks for header files.
AC_CHECK_HEADERS([gsl/gsl_version.h],[],[
echo \
"------------------------------------------------------------------------
ERROR: libgsl is present, but the header files appear not to have been
installed. If the GNU Scientific Library was installed as an RPM or
similar, make sure you also include the -devel package. Otherwise, you
can obtain the GNU Scientific Library from http://www.gnu.org.
------------------------------------------------------------------------"
exit
])

# FIXME: Replace `main' with a function in `-lcppunit':
AM_PATH_CPPUNIT(1.8.0,,[
    AC_MSG_WARN([])
    AC_MSG_WARN([CppUnit not found])
    AC_MSG_WARN([Please install CppUnit from http://cppunit.sourceforge.net])
    AC_MSG_WARN([])
    AC_MSG_ERROR([Sorry, I can't continue without CppUnit])
])

# FIXME: Replace `main' with a function in `-ldl':
AC_CHECK_LIB([dl], [main])

# FIXME: Replace `main' with a function in `-lginac':
AC_CHECK_LIB([ginac], [main], [LIBS="$LIBS -lginac"], [
echo \
"------------------------------------------------------------------------
ERROR: libginac.so appears to be missing.  You cannot sanely try to build
SPK without libginac.  It is available from http://www.ginac.de/ and 
compiles readily on most systems.  If ginac is installed as an RPM or
similar package, make sure you also include the -devel package.
-------------------------------------------------------------------------"
exit
])
		    
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [main], [LIBS="$LIBS -lm"], AC_MSG_ERROR(libm not found))

# FIXME: Replace `main' with a function in `-lpthread':
AC_CHECK_LIB([pthread], [main], [LIBS="$LIBS -lpthread"], AC_MSG_ERROR(libpthread not found))

AC_CHECK_HEADER($PVM_ROOT/include/pvm3.h,
[
 	        PVM_INCLUDES='-I${PVM_ROOT}/include'
 	        PVM_XTRA_LIBS="`sed -n '/^ARCHLIB/s/[[^-]]*//p' ${PVM_ROOT}/conf/${PVM_ARCH}.def`"
	        echo "PVM_XTRA_LIBS=$PVM_XTRA_LIBS"
	        PVM_LIBS='-L${PVM_ROOT}/lib/${PVM_ARCH} -lpvm3'
 	        tPVM_LIBS="-L${PVM_ROOT}/lib/${PVM_ARCH} -lpvm3 ${PVM_XTRA_LIBS}"
 	        LIBS="$tPVM_LIBS $LIBS"

AC_CHECK_LIB(pvm3,pvm_parent,[
 	                AC_CHECK_LIB(pvm3,pvm_pkmesg,[])
 	        ],[
 	                echo "There is a pvm3.h but I can't link with -lpvm3"
 	                echo "So not using pvm afterall."
 	                PVM_INCLUDES=""
 	                PVM_LIBS=""
 	                tPVM_LIBS=""
		   ])
 	        LIBS="$xLIBS"
 ])


# FIXME: Replace `main' with a function in `-lcln':
#AC_CHECK_LIB([cln], [main], [LIBS="$LIBS -lcln"], AC_MSG_ERROR(libcln not found)))

# FIXME: Replace `main' with a function in `-lnon_par':
#AC_CHECK_LIB([non_par], [main], [LIBS="$LIBS -lnon_par"], AC_MSG_ERROR(libnon_par not found))

# FIXME: Replace `main' with a function in `-lmat2cpp':
#AC_CHECK_LIB([mat2cpp], [main], [LIBS="$LIBS -lmat2cpp"], AC_MSG_ERROR(libmat2cpp not found))

# FIXME: Replace `main' with a function in `-lspk':
#AC_CHECK_LIB([spk], [main], [LIBS="$LIBS -lspk"], AC_MSG_ERROR([libspk not found]))

EXTRA_LIBS="$EXTRA_LIBS $LIBS"

AC_SUBST(LIBS)
AC_SUBST(EXTRA_LIBS)
AC_SUBST(PVM_LIBS)
AC_SUBST(PVM_INCLUDES)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([floor pow sqrt])

AC_CONFIG_FILES([Makefile
                 spkpred/Makefile
                 test/Makefile
                 test/unit/Makefile
                 test/unit/src/Makefile])
AC_OUTPUT
