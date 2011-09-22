#*************************************************************************
#
# File: makefile
#
#
# Make file for the bench1 test that comes with the uBLAS installation
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
# subproject libs/numeric/ublas/bench1 ;
#
# SOURCES = bench1 bench11 bench12 bench13 ;
#
# exe bench1
#     : $(SOURCES).cpp 
#     : <include>$(BOOST_ROOT)
#       <debug><define>BOOST_UBLAS_TYPE_CHECK
#       <borland><*><cxxflags>"-w-8026 -w-8027 -w-8057 -w-8084 -w-8092"
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
OBJECTS = bench1.o \
          bench11.o \
          bench12.o \
          bench13.o

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

bench1: $(OBJECTS) $(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_NAME).a \
                   $(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_C_INTERFACE).a
	$(CC) -o bench1 $(OBJECTS) $(LIB_FLAG)

bench1.o : bench1.cpp $(COMMON_HEADERS) bench1.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench1.cpp

bench11.o : bench11.cpp $(COMMON_HEADERS) bench11.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench11.cpp

bench12.o : bench12.cpp $(COMMON_HEADERS) bench12.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench12.cpp

bench13.o : bench13.cpp $(COMMON_HEADERS) bench13.hpp
	$(CC) -c $(MODE_FLAG) $(INC_FLAG) bench13.cpp

# Do nothing for the ATLAS generated library since it is already built.
$(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_NAME).a :
$(ATLAS_BLAS_LIB)/lib$(ATLAS_BLAS_C_INTERFACE).a :

# Do nothing for header files with .hpp extensions.
$(BOOST_PATH)/timer.hpp :
$(UBLAS_PATH)/vector.hpp :
$(UBLAS_PATH)/matrix.hpp :
$(UBLAS_PATH)/config.hpp :
bench1.hpp :	
bench11.hpp :	
bench12.hpp :	
bench13.hpp :	

