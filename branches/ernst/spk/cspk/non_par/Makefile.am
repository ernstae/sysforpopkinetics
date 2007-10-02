if non_par_POSTFIX_DIR
myincludedir = $(includedir)/$(POSTFIX_DIR)
else
myincludedir = $(includedir)
endif
SUBDIRS = \
	cpp
#
nobase_myinclude_HEADERS = \
	non_par/bender.hpp \
	non_par/data.hpp \
	non_par/npipp_of.hpp \
	non_par/opt_measure.hpp \
	non_par/relaxed.hpp
#
EXTRA_DIST = \
	build.sh \
	run_omhelp.sh \
	matlab/run_omhelp.sh \
	matlab \
	doc
#
dist-hook:
	rm -rf `find $(distdir) -name *.CVS`
