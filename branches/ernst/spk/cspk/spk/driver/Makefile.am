bin_PROGRAMS = midDriver jobDriver

INCLUDES = -I$(top_srcdir) -I$(includedir) $(PVM_INCLUDES)

midDriver_SOURCES = midDriver.cpp
jobDriver_SOURCES = jobDriver.cpp

midDriver_LDADD = -L../spk $(LIBS) $(PVM_LIBS)
jobDriver_LDADD = -L../spk $(LIBS) $(PVM_LIBS)

