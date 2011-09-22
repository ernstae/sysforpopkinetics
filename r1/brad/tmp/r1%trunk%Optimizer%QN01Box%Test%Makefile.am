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
noinst_PROGRAMS   = RunTest 
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
AM_LDFLAGS  = \
	-L$(top_builddir)/lib \
	-L$(CppADprefix)/lib/$(CppADpostfix) 
LDADD       = $(top_builddir)/lib/libQN01Box.a -llapack
#
RunTest_SOURCES   = \
	Bennet5.cpp \
	DanWood.cpp \
	Degenerate.cpp \
	Degenerate2.cpp \
	ExpLeastSquares.cpp \
	LinearObjective.cpp \
	MemoryError.cpp \
	PlusInfinity.cpp \
	QuasiNewton01Box.cpp \
	ResidualIncrease.cpp \
	RunTest.cpp \
	SingularUpdate.cpp \
	Test_04_02_18.cpp \
	TwoCompartment.cpp  \
	zero_one_scale.cpp
