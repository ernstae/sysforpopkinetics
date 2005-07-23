lib_LTLIBRARIES = libspkopt.la libspkopt_release.la

INST_BASE = /usr/local

libdir        = $(INST_BASE)/lib/spktest
pkgincludedir = $(INST_BASE)/include/spktest/spkopt

AM_CPPFLAGS = -I$(INST_BASE)/include/spktest -I$(INST_BASE)/include/spktest/CppAD
AM_LDFLAGS  = -llapack -lg2c

CXXFLAGS   =
CXXCOMPILE =
CFLAGS     =

libspkopt_la_CXXFLAGS         = -g
libspkopt_release_la_CXXFLAGS = -O3

FFLAGS =
F77COMPILE = -g

libspkopt_la_SOURCES = \
	Bfgs.cpp \
	c2dsyev.f \
	Delta.cpp \
	MaxAbs.cpp \
	NearEqual.cpp \
	Next.cpp \
	PlusInfinity.cpp \
	QuadBox.cpp \
	QuadFixed.cpp \
	QuasiNewton01Box.cpp \
	Residual.cpp \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	PlusInfinity.h \
	QuadBox.h \
	QuadFixed.h \
	QuasiNewton01Box.h \
	Residual.h 

libspkopt_release_la_SOURCES = \
	Bfgs.cpp \
	c2dsyev.f \
	Delta.cpp \
	MaxAbs.cpp \
	NearEqual.cpp \
	Next.cpp \
	PlusInfinity.cpp \
	QuadBox.cpp \
	QuadFixed.cpp \
	QuasiNewton01Box.cpp \
	Residual.cpp \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	PlusInfinity.h \
	QuadBox.h \
	QuadFixed.h \
	QuasiNewton01Box.h \
	Residual.h 

pkginclude_HEADERS = \
	Bfgs.h \
	c2dsyev.h \
        CppADUtil.h \
	Delta.h \
	MaxAbs.h \
        max.h \
	Memory.h \
	NearEqual.h \
        min.h \
	Next.h \
	PlusInfinity.h \
	QuadBox.h \
	QuadFixed.h \
	QuasiNewton01Box.h \
	Residual.h 
