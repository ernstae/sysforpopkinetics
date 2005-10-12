#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "countStrInLhs.h"
#include "../upper.h"
#include "../lower.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

void NonmemTranslator::parseIndAnalysis( DOMElement* ind_analysis )
{
  //---------------------------------------------------------------------------------------
  // Parse <simulate> if exists.  There's a chance in which only data simulation
  // is requested but not estimation.
  //---------------------------------------------------------------------------------------
  myIsSimulate = false;
  mySeed = 0;
  DOMNodeList * simulations = ind_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      if( simulations->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Multiple <%s> elements found.",
		    C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( simulations->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s> element.",
		    C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( X_SEED ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.",
		    C_SIMULATION, C_SEED );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    C_SIMULATION, C_SEED, XMLString::transcode( xml_seed ) );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
   
      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
	{
	  const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
	  if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			C_SIMULATION, C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
    }

  //---------------------------------------------------------------------------------------
  // <pop_analysis> Required attributes
  //---------------------------------------------------------------------------------------
  // * is_estimation = {yes, no}
  if( !ind_analysis->hasAttribute( X_IS_ESTIMATION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.", 
		C_IND_ANALYSIS, C_IS_ESTIMATION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  const XMLCh * xml_is_estimation = ind_analysis->getAttribute( X_IS_ESTIMATION );
  if( XMLString::equals( xml_is_estimation, X_YES ) )
    myIsEstimate = true;
  else if ( XMLString::equals( xml_is_estimation, X_NO ) )
    myIsEstimate = false;
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_IND_ANALYSIS, 
	       C_IS_ESTIMATION, 
	       XMLString::transcode( xml_is_estimation ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  setPopSize( 1 );

  myIndTraceLevel = 1;
  myPopTraceLevel = 1;

  //---------------------------------------------------------------------------------------
  // Optional attributes
  //---------------------------------------------------------------------------------------
  // * mitr   --- required when is_estimation == "yes"
  // * is_restart = {yes, "no"}
  // * sig_digits = 3
  myIndMitr   = 0;
  mySigDigits = 3;

  const XMLCh * xml_is_restart;
  if( ind_analysis->hasAttribute( X_SIG_DIGITS ) )
    {
      xml_is_restart = ind_analysis->getAttribute( X_IS_RESTART );
      myIsRestart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
    }
  if( myIsEstimate )
    {
      const XMLCh* xml_mitr;
      if( ind_analysis->hasAttribute( X_MITR ) )
	{
	  xml_mitr = ind_analysis->getAttribute( X_MITR );
	  if( !XMLString::textToBin( xml_mitr, myIndMitr ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			C_IND_ANALYSIS, C_MITR, XMLString::transcode(xml_mitr) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
      const XMLCh* xml_sig_digits;
      if( ind_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = ind_analysis->getAttribute( X_SIG_DIGITS );
	  if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			C_IND_ANALYSIS, C_SIG_DIGITS, XMLString::transcode(xml_sig_digits) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  if( !( mySigDigits > 0 && mySigDigits < 9 ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".  Valid values: (1-8).", 
			C_IND_ANALYSIS, C_SIG_DIGITS, mySigDigits );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	}
    }
  
  //---------------------------------------------------------------------------------------
  // Required elements
  //---------------------------------------------------------------------------------------
  // <data_labels>
  // <theta>
  // <omega>+
  DOMNodeList * data_labels_list = ind_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found under <%s>.",
		C_DATA_LABELS, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( data_labels_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element under <%s>.",
		C_DATA_LABELS, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {

    DOMNodeList * labels = data_labels->getElementsByTagName( X_LABEL );
    int nLabels = labels->getLength();
    if( nLabels < 1 )
      {
        char mess[ SpkCompilerError::maxMessageLen() ];
        snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_LABEL, C_DATA_LABELS );
        SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
        throw e;
      }
    for( int i=0; i<nLabels; i++ )
      {
	DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );
	// <label> is an empty element

	// required attribute
	// * name
	if( !xml_label->hasAttribute( X_NAME ) )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            snprintf( mess, 
		      SpkCompilerError::maxMessageLen(),
		      "Missing <%s::%s> attribute.",
		      C_LABEL, C_NAME );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	char * c_name = XMLString::transcode( xml_name );

	// optional attribute
	// * synonym
	const XMLCh* xml_synonym;
	char * c_synonym = NULL;
	if( xml_label->hasAttribute( X_SYNONYM ) )
	  {
	    xml_synonym = xml_label->getAttribute( X_SYNONYM );
	    c_synonym = XMLString::transcode( xml_synonym );
	  }

	Symbol * name = table->findi( c_name );

	// "name" may not be one of the official data item labels.
	// For example, "DV" may be used as an official data label
	// in the data set (ie. dataML) but used as an alias
	// to "CP" so that "CP" appears as the item title in
	// the display table/scatterplot.
	// Check if this <label> has label::synonym attribute.
	// If it does, check if it exists in the symbol table.
	if( name == Symbol::empty() )
	  {
	    if( c_synonym == NULL )
	      {
                char mess[ SpkCompilerError::maxMessageLen() ];
                snprintf( mess, 
			  SpkCompilerError::maxMessageLen(),
			  "\"%s\" is not found in the symbol table as either the name or the alias.", c_name );
                SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, 
                                        __LINE__, __FILE__ );
                throw e;
	      }
	    Symbol * synonym = table->findi( c_synonym );
	    if( synonym == Symbol::empty() )
	      {
                char mess[ SpkCompilerError::maxMessageLen() ];
                snprintf( mess, 
			  SpkCompilerError::maxMessageLen(),
			  "\"%s\" is not found in the symbol table as either the name or the alias.", c_name );
                SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                        __LINE__, __FILE__ );
                throw e;
	      }
	    synonym->synonym = c_name;
	  }

	else if( c_synonym != NULL )
	  {
	    // register the synonym to the symbol table
	    name->synonym = c_synonym;
	  }
	delete c_name;
	delete c_synonym;
      }
  }

  // THETA
  DOMNodeList * theta_list = ind_analysis->getElementsByTagName( X_THETA );
  if( theta_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found under <%s>.",
		C_THETA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( theta_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element under <%s>.",
		C_THETA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		C_THETA, C_LENGTH );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\"", 
		C_THETA, C_LENGTH, XMLString::transcode( xml_theta_len ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertVector( DefaultStr.THETA, myThetaLen, Symbol::SYSTEM, Symbol::READONLY );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> element found under <%s>.",
		  C_THETA, C_IN );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_THETA, C_IN );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements under <%s> does not match with the <%s::%s> attribute value.",
		  C_VALUE, C_IN, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
        bool isFixed = false;
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
        const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
	if( XMLString::stringLen( xml_fixed ) != 0 )
	  {
	    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	  }
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->initial[0][i] = str_val;
	sym_theta->fixed[0][i]   = isFixed;
      }
    //<low>
    DOMNodeList * theta_low_list = theta->getElementsByTagName( X_LOW );
    if( theta_low_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.",
		  C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_low_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element  under <%s>.",
		  C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements under <%s> tag does not match with the <%s::%s> attribute value.",
		  C_VALUE, C_LOW, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->lower[0][i] = str_val;
      }

    //<up>
    DOMNodeList * theta_up_list = theta->getElementsByTagName( X_UP );
    if( theta_up_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.",
		  C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_up_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements under <%s> tag does not match with the <%s::%s> attribute value.",
		  C_VALUE, C_UP, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	const XMLCh* xml_val = value_list->item(i)->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->upper[0][i] = str_val;
      }

    // step values
    for( int i=0; i<myThetaLen; i++ )
      {
	double tmp_dbl = fabs( ( atof( sym_theta->upper[0][i].c_str() ) - atof( sym_theta->lower[0][i].c_str() ) ) ) / 1000.0;
	char tmp_char[256];
	snprintf( tmp_char, 256, "%f", tmp_dbl );
	sym_theta->step[0][i] = string( tmp_char );
      }
  }

  // OMEGA
  DOMNodeList * omega_list = ind_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  if( nOmegaSpecs > 1 )
    {
      // v0.1 supports only one Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found under <%s>.",
		C_OMEGA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( nOmegaSpecs < 1 )
    {
      // v0.1 supports only one Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> elements under <%s>.",
		C_OMEGA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  if( !omega->hasAttribute( X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		C_OMEGA, C_DIMENSION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		C_OMEGA, C_DIMENSION, XMLString::transcode(xml_omega_dim) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !omega->hasAttribute( X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		C_OMEGA, C_STRUCT );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  // In Individual analysis, Omega is diagonal only.
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  if( !XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"For the individual analysis, Omega can be only diagonal.  %s is invalid.",
		XMLString::transcode( xml_omega_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  myOmegaStruct = Symbol::DIAGONAL;
  myOmegaOrder = myOmegaDim;

  Symbol * sym_omega = table->insertSymmetricMatrix( DefaultStr.OMEGA, myOmegaStruct, myOmegaDim, Symbol::SYSTEM, Symbol::READONLY );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    if( omega_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.",
		  C_IN, C_OMEGA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( omega_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_IN, C_OMEGA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );
    //
    // Omega specification contains the minimal representation of the matrix.
    //
    //     /                 \
      //     |  a11  a12  a13  |
      // A = |  a21  a22  a23  |
      //     |  a31  a32  a33  |
      //     \                 /
      //
      // For full, the list contains the LOWER half in the row major order.
      // For diagonal, only the diagonal elements.
      //
      // If A is full, the user-given list will contain elements in the following order:
      // A' = { a11, a21, a22, a31, a32, a33 }
      //
      // xxxPredModel's constructor expects the list containing elements of
      // the UPPER half in the row major order.
      //
      // Thus, A has to be reorganized and stored in an internal array in the following order:
      // A" = { a11, a21, a31, a22, a32, a33 }
      //
      char valueDefault[] = "0.0";
      DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
      if( myOmegaOrder != value_list->getLength() )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		   "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		   C_VALUE, C_OMEGA, C_LENGTH );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( myOmegaStruct == Symbol::TRIANGLE )
	{
	  // First construct a full n by n matrix.
	  valarray<string> omega_in_full ( myOmegaDim * myOmegaDim );
	  valarray<bool> omega_fix_full( myOmegaDim * myOmegaDim );
	  for( int i=0, cnt=0; i<myOmegaDim; i++ )
	    {
	      for( int j=0; j<=i; j++, cnt++ )
		{
		  char str_val[128];
		  bool isFixed = false;
		  DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		  if( value->hasAttribute( X_FIXED ) )
		    {
		      const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		      isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
		    }
		  const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

		  if( XMLString::stringLen( xml_val ) == 0 )
		    strcpy( str_val, valueDefault );
		  else
		    {
		      char * tmp_c_val = XMLString::transcode( xml_val );
		      strcpy( str_val, tmp_c_val );
		      delete tmp_c_val;
		    }
		  //omega_in_full[ j + i*dim ] = a[cnt]; // filling a lower triangle element
		  omega_in_full [ i + j*myOmegaDim ] = str_val; // filling a upper triangle element
		  omega_fix_full[ i + j*myOmegaDim ] = isFixed;
		}
	    }
	  // Then, extract only the upper half in the row major order.
	  for( int i=0, cnt=0; i<myOmegaDim; i++ )
	    {
	      for( int j=i; j<myOmegaDim; j++, cnt++ )
		{
		  sym_omega->initial[0][cnt] = omega_in_full [ j + i * myOmegaDim ];
		  sym_omega->fixed  [0][cnt] = omega_fix_full[ j + i * myOmegaDim ];
		}
	    }
	}
      else // diagonal case
	{
	  for( int i=0; i<myOmegaDim; i++ )
	    {
	      char str_val[128];
	      bool isFixed = false;
	      DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	      if( value->hasAttribute( X_FIXED ) )
		{
		  const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		  isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
		}
	      const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

	      if( XMLString::stringLen( xml_val ) == 0 )
		strcpy( str_val, valueDefault );
	      else
		{
		  char * tmp_c_val = XMLString::transcode( xml_val );
		  strcpy( str_val, tmp_c_val );
		  delete tmp_c_val;
		}
	      sym_omega->initial[0][i] = str_val;
	      sym_omega->fixed[0][i]   = isFixed;
	    }
	}
  }

  // ETA
  // Eta plays the same role as EPS as in the population analysis.
  // Variance of data?
  char etaDefault[] = "0.0";
  //  myEtaLen = myOmegaOrder;
  myEtaLen = myOmegaDim;
  Symbol * sym_eta = table->insertVector( DefaultStr.ETA, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;
  
  //---------------------------------------------------------------------------------------
  // Optional elements
  //---------------------------------------------------------------------------------------
  // <description>  --- ignore!
  // <ind_stat>
  // <pop_stat>
  DOMNodeList * descriptions = ind_analysis->getElementsByTagName( X_DESCRIPTION );
  myDescription = new char[ 128 ];
  strcpy( myDescription, "--- This file is generated by SPK Compiler ---" );

  if( descriptions->getLength() > 0 )
    {
      const XMLCh* description = descriptions->item(0)->getFirstChild()->getNodeValue();
      if( XMLString::stringLen( description ) > 0 )
	{
	  delete [] myDescription;
	  myDescription = XMLString::transcode( description );
	}
    }

  DOMNodeList * ind_stat_list = ind_analysis->getElementsByTagName( X_IND_STAT );

  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( ind_stat_list->getLength() > 0 && myIsEstimate )
    {
      if( ind_stat_list->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Multiple <%s> elements found.",
		    C_IND_STAT );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( ind_stat_list->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s> element.",
		    C_IND_STAT );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      DOMElement * ind_stat = dynamic_cast<DOMElement*>( ind_stat_list->item(0) );
      const XMLCh* xml_stderr = ind_stat->getAttribute( X_IS_ERR_OUT );
      const XMLCh* cov_form = X_COV_H;
      if( ind_stat->hasAttribute( X_COVARIANCE_FORM ) )
	cov_form = ind_stat->getAttribute( X_COVARIANCE_FORM );
      if( XMLString::equals( cov_form, X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, X_COV_RSR ) )
	myCovForm = "RSR";
      else if( XMLString::equals( cov_form, X_COV_R ) )
	myCovForm = "R";
      else if( XMLString::equals( cov_form, X_COV_HSH ) )
	myCovForm = "HSH";
      else if( XMLString::equals( cov_form, X_COV_H ) )
	myCovForm = "H";
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		     SpkCompilerError::maxMessageLen(),
		     "Invalid <%s::%s> attribute value: \"%s\".", 
		     C_IND_STAT, C_COVARIANCE_FORM, XMLString::transcode( cov_form )  );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( XMLString::stringLen( xml_stderr ) > 0 )
	{
	  myIsStderr = (XMLString::equals( xml_stderr, X_YES )? true : false );
	}
      const XMLCh* xml_correlation = ind_stat->getAttribute( X_IS_CORR_OUT );
      if( XMLString::stringLen( xml_correlation ) > 0 )
	{
	  myIsCorrelation = (XMLString::equals( xml_correlation, X_YES )? true : false );
	}
      const XMLCh* xml_cov = ind_stat->getAttribute( X_IS_COV_OUT );
      if( XMLString::stringLen( xml_cov ) > 0 )
	{
	  myIsCov = (XMLString::equals( xml_cov, X_YES )? true : false );
	}
      const XMLCh* xml_inv_cov = ind_stat->getAttribute( X_IS_INV_COV_OUT );
      if( XMLString::stringLen( xml_inv_cov ) > 0 )
	{
	  myIsInvCov = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
	}
      const XMLCh* xml_conf = ind_stat->getAttribute( X_IS_CONF_OUT );
      if( XMLString::stringLen( xml_conf ) > 0 )
	{
	  myIsConfidence = (XMLString::equals( xml_conf, X_YES )? true : false );
	}
      const XMLCh* xml_coef = ind_stat->getAttribute( X_IS_COEF_OUT );
      if( XMLString::stringLen( xml_coef ) > 0 )
	{
	  myIsCoefficient = (XMLString::equals( xml_coef, X_YES )? true : false );
	}
    }
  else
    {
      myIsStat = false;
      myIsStderr = false;
      myIsCorrelation = false;
      myIsCov = false;
      myIsInvCov = false;
      myIsConfidence = false;
      myIsCoefficient = false;
    }

  myIsStat = myIsStderr 
    || myIsCorrelation 
    || myIsCov 
    || myIsInvCov 
    || myIsConfidence 
    || myIsCoefficient;

  return;
}
