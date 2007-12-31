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
SUBDIRS = spk doc 
pkginclude_HEADERS = config.h
