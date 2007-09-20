#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(spk-pvm-driver, 1.0, rfpksoft@u.washington.edu)
AC_CONFIG_SRCDIR([midDriver.cpp])
AC_CONFIG_HEADER([config.h])
AC_PROG_LIBTOOL
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lspk':
AC_CHECK_LIB([spk], [_ZZ10elsq_xDiagRK12DoubleMatrixS1_S1_S1_S1_E19__PRETTY_FUNCTION__], [echo "Found libspk " $LIBS], [echo "did not find libspk"])

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
 	        LIBS="$xLIBS $LIBS"
 ])


AC_SUBST(LIBS)
AC_SUBST(PVM_LIBS)
AC_SUBST(PVM_INCLUDES)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST

# Checks for library functions.
AC_CHECK_FUNCS([getcwd])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
