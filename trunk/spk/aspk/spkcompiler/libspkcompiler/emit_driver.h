#ifndef EMIT_DRIVER_H
#define EMIT_DRIVER_H

#include <cstdlib>
#include "SpkParameters.h"

void emit_driver( 
		 FILE * out, 
		 int nIndividuals, 
		 const char SpkModel_name[], 
		 struct SpkParameters & spk 
		 );

#endif
