#!/bin/bash

#************************************************************************
#                                                                       *
#  From:   Resource Facility for Population Kinetics                    *
#          Department of Bioengineering Box 352255                      *
#          University of Washington                                     *
#          Seattle, WA 98195-2255                                       *
#                                                                       *
#  Copyright (C) 2002, University of Washington,                        *
#  Resource Facility for Population Kinetics. All Rights Reserved.      *
#                                                                       *
#  This software was developed with support from NIH grant RR-12609.    *
#  Please cite this grant in any publication for which this software    *
#  is used and send a notification to the address given above.          *
#                                                                       *
#  Check for updates and notices at:                                    *
#  http://www.rfpk.washington.edu                                       *
#                                                                       *
#************************************************************************

# -----------------------------------------------------------------
#
#  This shell script makes the following assumptions:
#
#    (1.) the directory htm is a subdirectory of the current directory,
#    (2.) the Omhelp files are located in the current directory, and
#    (3.) the HTML files should be created in the htm directory.
#
# -----------------------------------------------------------------
T = spkLib
HTML_DIR = htm
DEST_HTML_DIR =  
WEB = $(WEBDIR)/v0.1/specs/$T

all : html

html:
	make -C omh

clean:
	rm -f -r $(HTML_DIR)
