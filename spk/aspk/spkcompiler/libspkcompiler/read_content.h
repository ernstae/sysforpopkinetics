#ifndef READ_CONTENT_H
#define READ_CONTENT_H

#include <string>
#include <xercesc/dom/DOM.hpp>
#include "client.h"
#include "SpkParameters.h"

bool read_content( xercesc::DOMDocument* tree, 
		   std::string & spkml_verOut, 
		   enum client::type & clientOut, 
		   enum SpkParameters::Analysis& analysisOut  );

#endif
