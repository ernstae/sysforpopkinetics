dnl
dnl <AC_INIT>
dnl
dnl Process any command-line arguments and perform various 
dnl initialization and verifications.
dnl
dnl    AC_INIT( package, version, bug-report, tarname )
dnl       package    - package name
dnl       version    - version number
dnl       bug-report - email address to which users should send bug reports
dnl       tarname    - distribution tarball name
dnl
dnl The following macros and names are defined by AC_INIT.
dnl    
dnl    AC_PACKAGE_NAME, PACKAGE_NAME
dnl       exactly "package"
dnl    AC_PACKAGE_TARNAME, PACKAGE_TARNAME
dnl       exactly "tarname"
dnl    AC_PACKAGE_VERSION, PACKAGE_VERSION
dnl       exactly "version"
dnl    AC_PACKAGE_STRING, PACKAGE_STRING
dnl       exactly "package version"
dnl    AC_PACKAGE_BUGREPORT, PACKAGE_BUG_REPORT
dnl       exactly "bug-report"
dnl
AC_INIT(SPK Compiler, 0.1, honda99@u.washington.edu, spkcompiler )

dnl
dnl <AC_CONFIG_SRCDIR>
dnl
dnl    AC_CONFIG_SRCDIR( unique-file-in-source-dir )
dnl       unique-file-in-source-dir - a file that is in the package's source directory.
dnl
dnl configure checks for this file's existence to make sure that the directory
dnl that it is told contains the source code in fact does.
dnl
AC_CONFIG_SRCDIR(spkcompiler/main.cpp)

dnl
dnl <AM_INIT_AUTOMAKE>
dnl
dnl Runs many macros required for proper operation of the generated Makefiles.
dnl
AM_INIT_AUTOMAKE

dnl
dnl <AM_CONFIG_HEADER>
dnl
dnl Automake will generate rules to automatically regenerate the config header.
dnl
AM_CONFIG_HEADER(config.h)

dnl 
dnl <AC_PROG_CC>
dnl
dnl Checks for C compiler and load the package when it's found.
dnl 
AC_PROG_CC

dnl
dnl <AC_PROG_CXX>
dnl
dnl Checks for C++ compiler and load the package when it's found.
dnl
AC_PROG_CXX

dnl
dnl <AM_PROG_LEX>
dnl
dnl Checks for LEX (ex. flex) and load the package when it's found.
dnl
AM_PROG_LEX

dnl
dnl <AC_PROG_YACC>
dnl
dnl Checks for YACC (ex. bison) and load the package when it's found.
dnl
AC_PROG_YACC

dnl
dnl <AC_PROG_INSTALL>
dnl
AC_PROG_INSTALL

dnl
dnl <AC_PROG_LIBTOOL>
dnl
dnl Checks for LibTool (i.e. the tool needed to build dynamic link libraries)
dnl and load the packeage when it's found.
dnl
AC_PROG_LIBTOOL

dnl
dnl Null out default settings for C++ compiler flags
dnl
AC_SUBST(CXXFLAGS, "")

dnl
dnl Set an extra command line option "--enable-release-build" to 'configure'.
dnl If the user specifies this option, a variable "release_build" gets
dnl set to TRUE.  An important note here is that the variable name
dnl must exactly match the option string after "enable_" with dashes 
dnl replaces by underscores.
dnl
AC_ARG_ENABLE( release_build,
       AC_HELP_STRING( [--enable-release-build], [optimize SPK Compiler for speed] ),
       [release_build=TRUE],
       [release_build=FALSE]
)

dnl
dnl <AM_CONDITIONAL>
dnl
dnl    AM_CONDITIONAL(conditional, condition)
dnl
dnl Checks if $release_build is set to "yes".
dnl If yes, define a macro SPK_RELEASE so that
dnl Makefile's can switch its behavior and
dnl build the SPK Library with compiler optimization
dnl flags.
dnl
AM_CONDITIONAL(SPK_RELEASE, test $release_build = TRUE)

dnl
dnl Display "checking for build mode ..." message on screen when 
dnl "configure" is run.
dnl
AC_MSG_CHECKING( [for build mode] )

dnl
dnl If $release_build is set to "yes",
dnl define SPK_RELEASE to be 1; otherwise to be 0.
dnl Display a text, "release_build", followed after the 
dnl "checking for build mode..." screen message.
dnl
if test $release_build = TRUE; then
 	AC_DEFINE(SPK_RELEASE, 1, "build release")
        AC_MSG_RESULT( [release] )
else
	AC_DEFINE(SPK_RELEASE, 0, "build debug")
        AC_MSG_RESULT( [debug] )
fi

dnl
dnl <AC_CONFIG_FILES>
dnl
dnl Make AC_OUTPUT create each 'file' by copying an input file (by default 'file.in')
dnl substituing the output variable values.  
dnl This macro creates the directory that the file is in if it doesn't exist.
dnl Usually, 'Makefile's are created this way, but other files such as '.gdbinit'
dnl can be specified as well.
dnl
dnl    AC_CONFIG_FILES(file..., [cmds], [init-cmds])
dnl       file
dnl 
AC_CONFIG_FILES( [Makefile 
		  doc/Makefile
	          spkcompiler/Makefile
	          spkcompiler/nonmem/Makefile
                  tests/Makefile
		  tests/nonmem/Makefile
                  tests/common/Makefile] )

dnl
dnl <AC_OUTPUT>
dnl
dnl Generate 'config.status' and launch it.  Call this macro once at the end
dnl of 'configure.ac'.
dnl 'config.status' will perform all the configuration actions;
dnl all the output files, header files, comands, links, subdirectories
dnl to configure are hornored.
dnl
dnl The location of AC_OUTPUT invocation is the exact point where
dnl configuration actions are taken; any code afterwards will be executed
dnl by configure once config.status was run.  
dnl
AC_OUTPUT


