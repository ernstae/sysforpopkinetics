bin_PROGRAMS = genericDriver

genericDriver_SOURCES = \
	convertStatisticsToNonmem.cpp \
	convertStatisticsToNonmem.h \
	DiffEqnModel.h \
	DiffEqnModelD.cpp \
	DiffEqnModelF_Analytic.cpp \
	DiffEqnModelF_x_Analytic.cpp \
	DiffEqnModelR.cpp \
	DiffEqnModelS.cpp \
	genericDriver.cpp \
	getdata.cpp \
	getdata.h \
	gval.cpp \
	gval.h \
	nonmemEqvContainers.cpp \
	nonmemEqvContainers.h \
	ode4rk.h \
	readNonmemTheophylline.cpp \
	readNonmemTheophylline.h \
	../config.h

genericDriver_LDFLAGS = -L/usr/local/lib/spktest -lspk -lspkopt -latlas_lapack -lcblas -latlas -lpthread -lm

INCLUDES = -I@top_srcdir@/ \
	   -I/usr/local/include/spktest

pkginclude_HEADERS = \
	convertStatisticsToNonmem.h \
	DiffEqnModel.h \
	getdata.h \
	gval.h \
	nonmemEqvContainers.h \
	ode4rk.h \
	readNonmemTheophylline.h
