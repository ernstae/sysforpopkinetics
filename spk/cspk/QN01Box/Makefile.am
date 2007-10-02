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
if QN01Box_POSTFIX_DIR
myincludedir = $(includedir)/$(POSTFIX_DIR)
else
myincludedir = $(includedir)
endif
#
nobase_myinclude_HEADERS = \
	QN01Box/Bfgs.h \
	QN01Box/c2dsyev.h \
	QN01Box/Delta.h \
	QN01Box/Error.h \
	QN01Box/is_symmetric.h \
	QN01Box/MaxAbs.h \
	QN01Box/max.h \
	QN01Box/Memory.h \
	QN01Box/min.h \
	QN01Box/Next.h \
	QN01Box/PlusInfinity.h \
	QN01Box/PositiveMatrix.h \
	QN01Box/QuadBox.h \
	QN01Box/QuasiNewton01Box.h \
	QN01Box/Residual.h \
	QN01Box/ScaleProjGrad.h \
	QN01Box/SumAbs.h  \
	QN01Box/zero_one_scale.h
#
SUBDIRS = \
	lib \
	Test
#
EXTRA_DIST = \
	QN01Box.omh \
	Build.sh \
	RunOMhelp.sh \
	Doc  \
	omh
#
dist-hook:
	rm -rf `find $(distdir)/omh -name CVS`
