# From:   Resource Facility for Population Kinetics
#           Department of Bioengineering Box 352255
#           University of Washington, Seattle, WA 98195-2255
# 
# This file is part of the System for Population Kinetics (SPK), which
# was developed with support from NIH grants RR-12609 and P41-
# EB001975. Please cite these grants in any publication for which this
# software is used and send a notification to the address given above.
# 
# SPK is Copyright (C) 1998-2005, by the University of Washington,
# Resource Facility for Population Kinetics, and is made available as
# free open source software under the terms of the University of
# Washington Free-Fork License as a public service.  A copy of the
# License can be found in the COPYING file in the root directory of this
# distribution.
# -----------------------------------------------------------------------
# Software:   Brad Bell (brad@apl.washington.edu)
# Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)
#
# I (Brad Bell) hereby grant the University of Washington the right to use
# this a modified version of CppAD-05-10-12/lib/Makefile.am in the sense of the
# COPYING file mentioned above.
#
if QN01Box_POSTFIX_DIR
mylibdir = $(libdir)/$(POSTFIX_DIR)
else
mylibdir = $(libdir)
endif
#
if QN01Box_CPPAD_PREFIX_DIR
CppADprefix = $(CPPAD_PREFIX_DIR)
else
CppADprefix = $(HOME)
endif

if QN01Box_CPPAD_POSTFIX_DIR
CppADpostfix = $(CPPAD_POSTFIX_DIR)
else
CppADpostfix = 
endif

AM_CPPFLAGS = \
	-I. \
	-I$(top_builddir) \
	-I$(CppADprefix)/include/$(CppADpostfix)
AM_CXXFLAGS = \
	$(COMPILE_FLAGS)
AM_CFLAGS = \
	$(COMPILE_FLAGS)
AM_FFLAGS = \
	-O2
#
# libQN01Box
#
mylib_LIBRARIES   = libQN01Box.a 
libQN01Box_a_SOURCES    =  \
	Bfgs.cpp \
	c2dsyev.f \
	Delta.cpp \
	Error.cpp \
	is_symmetric.cpp \
	MaxAbs.cpp \
	Next.cpp \
	PositiveMatrix.cpp \
	QuadBox.cpp \
	QuasiNewton01Box.cpp \
	Residual.cpp \
	ScaleProjGrad.cpp \
	SumAbs.cpp 
