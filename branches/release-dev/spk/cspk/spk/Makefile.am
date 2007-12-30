if SPK_POSTFIX
  mybindir = ${bindir}/${spk_postfixdir}
  mylibdir = ${libdir}/${spk_postfixdir}
  myincludedir = $(includedir)/$(spk_postfixdir)
  INST_BASE = $(libdir)
else
  INST_BASE=$(prefix)
endif

# Set C++ compiler flags
if SPK_RELEASE
AM_CXXFLAGS = -O3 -Dspk_release -DNDEBUG 
else
AM_CXXFLAGS = -g  -Dspk_debug 
endif                                                                                                

#if SPK_PROFILE
#AM_CXXFLAGS += -pg -Dspk_profiling
#endif

#Set F77 compiler flags                                                                              
AM_FFLAGS = -g  

AUTOMAKE_OPTIONS = gnu
SUBDIRS = spk test doc 
pkginclude_HEADERS = config.h
