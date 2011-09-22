#*************************************************************************
#
# File: makefile
#
#
# Make file for the bench2 test that comes with the uBLAS installation
# from Boost.
#
# Note that this test comes from Boost with a Jamfile that works with 
# Boost Jam, which is a make like tool.  This file was created as a 
# replacement for that file so that the test could be built using make. 
#
# Author: Mitch Watrous
#
#*************************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Original Boost Jam file
# -----------------------
#
# subproject libs/numeric/ublas/bench2 ;
#
# SOURCES = bench2 bench21 bench22 bench23 ;
#
# exe bench2
#     : $(SOURCES).cpp
#     : <include>$(BOOST_ROOT)
#       <borland><*><cxxflags>"-w-8026 -w-8027 -w-8057 -w-8084 -w-8092"
#       <kylix><*><cxxflags>"-w-8026 -w-8027 -w-8057 -w-8084 -w-8092"
#     ;
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------
# Preliminaries.
#-------------------------------------------------------------------------

# Boost library information (uBLAS is part of Boost). 
BOOST_PATH = /home/watrous/Boost/boost_1_30_0
UBLAS_PATH = $(BOOST_PATH)/numeric/ublas

# ATLAS generated BLAS library information.
ATLAS_BLAS_ROOT = /home/watrous/Atlas/ATLAS
ATLAS_BLAS_LIB  = $(ATLAS_BLAS_ROOT)/lib/Linux_P4SSE2
ATLAS_BLAS_INC  = $(ATLAS_BLAS_ROOT)/include
ATLAS_BLAS_NAME = atlas
ATLAS_BLAS_C_INTERFACE = cblas

# Fortran BLAS library information.
FORTRAN_BLAS_PATH = /home/watrous/BLAS
FORTRAN_BLAS_NAME = blas_linux

# Compiler.
CC = g++

# Include flags.
INC_FLAG  = -I$(BOOST_PATH) \
            -I$(UBLAS_PATH) \
            -I$(ATLAS_BLAS_INC) 

# Library flags.
LIB_FLAG = -L$(ATLAS_BLAS_LIB) \
           -l$(ATLAS_BLAS_C_INTERFACE) \
           -l$(ATLAS_BLAS_NAME)

# Object files.
OBJECTS = bench2.o \
          bench21.o \
          bench22.o \
          bench23.o

# Header files.
COMMON_HEADERS  = $(BOOST_PATH)/timer.hpp \
                  $(UBLAS_PATH)/vector.hpp \
                  $(UBLAS_PATH)/matrix.hpp \
                  $(UBLAS_PATH)/config.hpp \
                  $(ATLAS_BLAS_INC)/cblas.h

# Release or debug mode (comment out mode not being used).
#MODE_FLAG = -g -DBOOST_UBLAS_TYPE_CHECK   # Debug mode.
MODE_FLAG = -O2 -DNDEBUG                  # Release mode.


#-------------------------------------------------------------------------
# Build the executable.
#-------------------------------------------------------------------------

bench2: $(OBJECTS) $(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_NAME).a \
                   $(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_C_INTERFACE).a
	$(CC) -o bench2 $(OBJECTS) $(LIB_FLAG)

bench2.o : bench2.cpp $(COMMON_HEADERS) bench2.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench2.cpp

bench21.o : bench21.cpp $(COMMON_HEADERS) bench21.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench21.cpp

bench22.o : bench22.cpp $(COMMON_HEADERS) bench22.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench22.cpp

bench23.o : bench23.cpp $(COMMON_HEADERS) bench23.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench23.cpp

# Do nothing for the ATLAS generated library since it is already built.
$(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_NAME).a :
$(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_C_INTERFACE).a :

# Do nothing for header files with .hpp extensions.
$(BOOST_PATH)/timer.hpp :
$(UBLAS_PATH)/vector.hpp :
$(UBLAS_PATH)/matrix.hpp :
$(UBLAS_PATH)/config.hpp :
bench2.hpp :	
bench21.hpp :	
bench22.hpp :	
bench23.hpp :	

