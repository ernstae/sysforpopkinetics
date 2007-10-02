if non_par_POSTFIX_DIR
mylibdir          = $(libdir)/$(POSTFIX_DIR)
QN01BOX_LINK      = $(QN01BOX_PREFIX)/lib/$(POSTFIX_DIR)
QN01BOX_INCLUDE   = $(QN01BOX_PREFIX)/include/$(POSTFIX_DIR)
else
mylibdir          = $(libdir)
QN01BOX_LINK      = $(QN01BOX_PREFIX)/lib
QN01BOX_INCLUDE   = $(QN01BOX_PREFIX)/include
endif
AM_CXXFLAGS       = $(COMPILE_FLAGS) \
	-I$(top_builddir) \
	-I$(BOOST_DIR) \
	-I$(MAT2CPP_PREFIX)/include \
	-I$(QN01BOX_INCLUDE)
LDADD             = libnon_par.a -lQN01Box -lmat2cpp -llapack  \
	-L$(MAT2CPP_PREFIX)/lib \
	-L$(QN01BOX_LINK) 
#
mylib_LIBRARIES        = libnon_par.a
libnon_par_a_SOURCES = \
	data.cpp \
	npipp_of.cpp \
	relaxed.cpp 
#
noinst_PROGRAMS   = all_ok 
#
all_ok_SOURCES    = \
	all_ok.cpp \
	data_ok.cpp \
	bender_ok.cpp \
	npipp_ok.cpp \
	opt_measure_ok.cpp \
	relaxed_ok.cpp 
