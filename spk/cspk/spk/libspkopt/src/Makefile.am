lib_LTLIBRARIES = libspkopt.la libspkopt_release.la

CXXFLAGS   =
CXXCOMPILE =
CFLAGS     =

libspkopt_la_CXXFLAGS           = -g
libspkopt_la_LDFLAGS            = -llapack -lg2c
libspkopt_release_la_CXXFLAGS   = -O3
libspkopt_release_la_LDFLAGS    = -llapack -lg2c

FFLAGS =
F77COMPILE = -g

libspkopt_la_SOURCES = \
	Bfgs.cpp \
	c2dsyev.f \
	Delta.cpp \
	MaxAbs.cpp \
	NearEqual.cpp \
	Next.cpp \
	QuadBox.cpp \
	QuadTrust.cpp \
	QuasiNewton01Box.cpp \
	Residual.cpp \
        ADDefine.h \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
        ErrMacro.h \
        LuSolve.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	QuadBox.h \
	QuadTrust.h \
	QuasiNewton01Box.h \
	Residual.h \
        vector.h

libspkopt_release_la_SOURCES = \
	Bfgs.cpp \
	c2dsyev.f \
	Delta.cpp \
	MaxAbs.cpp \
	NearEqual.cpp \
	Next.cpp \
	QuadBox.cpp \
	QuadTrust.cpp \
	QuasiNewton01Box.cpp \
	Residual.cpp \
        ADDefine.h \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
        ErrMacro.h \
        LuSolve.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	QuadBox.h \
	QuadTrust.h \
	QuasiNewton01Box.h \
	Residual.h \
        vector.h

INCLUDES = -I@top_srcdir@/
pkginclude_HEADERS = \
        ADDefine.h \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
        ErrMacro.h \
        LuSolve.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	QuadBox.h \
	QuadTrust.h \
	QuasiNewton01Box.h \
	Residual.h \
        vector.h
