lib_LTLIBRARIES = libspkpred.la libspkpred_release.la

CXXFLAGS   =
CXXCOMPILE =
CFLAGS     =

libspkpred_la_CXXFLAGS           = -g
libspkpred_la_CXXCOMPILE         = -g
libspkpred_release_la_CXXFLAGS   = -O3
libspkpred_release_la_CXXCOMPILE = -O3

pkgincludedir = @includedir@/spkpred

libspkpred_la_SOURCES = \
	Cov.cpp \
	DiagCov.cpp \
	isEqual.cpp \
	Cov.h \
	DiagCov.h \
	isEqual.h

libspkpred_release_la_SOURCES = \
	Cov.cpp \
	DiagCov.cpp \
	isEqual.cpp \
	Cov.h \
	DiagCov.h \
	isEqual.h \
	PredBase.h

INCLUDES = -I@top_srcdir@/
pkginclude_HEADERS = \
	Cov.h \
	DiagCov.h \
	isEqual.h \
	PredBase.h

