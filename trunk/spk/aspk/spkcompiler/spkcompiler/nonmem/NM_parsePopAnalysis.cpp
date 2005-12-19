#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

void NonmemTranslator::parsePopAnalysis( DOMElement* pop_analysis )
{
  
  //---------------------------------------------------------------------------------------
  // <pop_analysis>: Attributes required when "is_estimation=yes".
  //---------------------------------------------------------------------------------------
  // * approximation = {fo, foce, laplace}
  // * pop_size
  // * is_estimation = {yes, no}
  //
  // Finding out if parameter estimation is requested.
  //
  if( !pop_analysis->hasAttribute( X_IS_ESTIMATION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.", C_POP_ANALYSIS, C_IS_ESTIMATION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }
  const XMLCh * xml_is_estimation = pop_analysis->getAttribute( X_IS_ESTIMATION );
  myIsEstimate = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );


  if( myIsEstimate )
    {
      //
      // Finding out the approximation method
      //
      if( !pop_analysis->hasAttribute( X_APPROXIMATION ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.", C_POP_ANALYSIS, C_APPROXIMATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_approx = pop_analysis->getAttribute( X_APPROXIMATION );
      
      if( XMLString::equals( xml_approx, X_FO ) )
	setApproximation( FO );
      else if( XMLString::equals( xml_approx, X_FOCE ) )
	setApproximation( FOCE );
      else if( XMLString::equals( xml_approx, X_LAPLACE ) )
	setApproximation( LAPLACE );
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    C_POP_ANALYSIS, 
		    C_APPROXIMATION, 
		   XMLString::transcode(xml_approx) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
    }

  myIndTraceLevel = 0;
  myPopTraceLevel = 1;

  //---------------------------------------------------------------------------------------
  // Optional attributes
  //---------------------------------------------------------------------------------------
  // * is_eta_out = {yes, "no"}
  // * is_restart = {yes, "no"}
  if( myIsEstimate )
    {
      const XMLCh * xml_is_eta_out;
      if( pop_analysis->hasAttribute( X_IS_ETA_OUT ) )
	{
	  xml_is_eta_out = pop_analysis->getAttribute( X_IS_ETA_OUT );
	  myIsPosthoc = ( XMLString::equals( xml_is_eta_out, X_YES )? true : false );
	}
      
      const XMLCh * xml_is_restart;
      if( pop_analysis->hasAttribute( X_IS_RESTART ) )
	{
	  xml_is_restart = pop_analysis->getAttribute( X_IS_RESTART );
	  myIsRestart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
	}
      if( pop_analysis->hasAttribute( X_MITR ) )
	{
	  const XMLCh* xml_mitr = pop_analysis->getAttribute( X_MITR );
	  if( !XMLString::textToBin( xml_mitr, myPopMitr ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			C_POP_ANALYSIS, C_MITR, XMLString::transcode(xml_mitr) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
      /*
	else{
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
	SpkCompilerError::maxMessageLen(),
	"Missing <%s::%s> attribute.", 
	C_POP_ANALYSIS, C_MITR );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
	__LINE__, __FILE__ );
	throw e;
	}
      */
      const XMLCh* xml_sig_digits;
      if( pop_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = pop_analysis->getAttribute( X_SIG_DIGITS );
	  if( XMLString::stringLen( xml_sig_digits ) > 0 )
	    {
	      if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  snprintf( mess, 
			    SpkCompilerError::maxMessageLen(),
			    "Invalid <%s::%s> attribute value: \"%s\".", 
			    C_POP_ANALYSIS, 
			    C_SIG_DIGITS, 
			   XMLString::transcode( xml_sig_digits ) );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
		  throw e;
              
		}
	      if( !( mySigDigits > 0 && mySigDigits < 9 ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  snprintf( mess, 
			    SpkCompilerError::maxMessageLen(),
			    "Invalid <%s::%s> attribute value: \"%s\".  Valid range: (1-8)", 
			    C_POP_ANALYSIS, C_SIG_DIGITS, XMLString::transcode( xml_sig_digits )  );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
		  throw e;
		}
	      myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	      myPopEpsilon = myIndEpsilon;
	    }
	}
    }

  //---------------------------------------------------------------------------------------
  // Required elements
  //---------------------------------------------------------------------------------------
  // <data_labels>
  // <theta>
  // <omega>+
  // <sigma>+
  DOMNodeList * data_labels_list = pop_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found in the sourceML document.", 
		C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }
  if( data_labels_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", 
		C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
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
		  "Missing <%s> element.",  C_LABEL );
        SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
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
		      "Missing <%s::%s> attribute for the %d-th <%s>.", C_LABEL, C_NAME,
		      i, C_LABEL );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	char * c_name = XMLString::transcode( xml_name );
	Symbol * name = table->find( c_name );

	if( name == Symbol::empty() )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            snprintf( mess, 
		      SpkCompilerError::maxMessageLen(),
		      "\"%s\" is not registered in the symbol table.", c_name );
            SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	delete c_name;

	// optional attribute
	// * synonym
	if( xml_label->hasAttribute( X_SYNONYM ) )
	  {
	    const XMLCh* xml_synonym = xml_label->getAttribute( X_SYNONYM );
	    char * c_synonym = XMLString::transcode( xml_synonym );
	    // register the synonym to the symbol table
	    name->synonym = string( c_synonym );
	    delete c_synonym;
	  }
      }
  }

  char valueDefault[] = "0.0";

  DOMNodeList * theta_list = pop_analysis->getElementsByTagName( X_THETA );
  if( theta_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found in the sourceML document.", C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( theta_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute .", C_THETA, C_LENGTH );
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
	       "Invalid <%s::%s> attribute value: %s", 
	       C_THETA, C_LENGTH, XMLString::transcode( xml_theta_len ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertVector( NMKey.THETA, myThetaLen, Symbol::SYSTEM, Symbol::READONLY );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> child elements found under <%s>.", 
		  C_IN, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> child under <%s>.", 
		  C_IN, C_THETA );
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
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
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
		  "Missing <%s> element under <%s>.", 
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
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, valueDefault );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	if( sym_theta->fixed[0][i] )
	  sym_theta->lower[0][i] = sym_theta->initial[0][i];
        else
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
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	const XMLCh* xml_val = value_list->item(i)->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, valueDefault );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
        if( sym_theta->fixed[0][i] )
	  sym_theta->upper[0][i] = sym_theta->initial[0][i];
        else
	  sym_theta->upper[0][i] = str_val;
      }

    // step values
    for( int i=0; i<myThetaLen; i++ )
      {
	if( sym_theta->fixed[0][i] )
	  sym_theta->step[0][i] = "0.0";
	else
	  {
	    double tmp_dbl = fabs( atof( sym_theta->upper[0][i].c_str() ) 
				   - atof( sym_theta->lower[0][i].c_str() ) ) / 1000.0;
	    char tmp_char[256];
	    snprintf( tmp_char, 256, "%f", tmp_dbl );
	    sym_theta->step[0][i] = string( tmp_char );
	  }
      }
  }

  DOMNodeList * omega_list = pop_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  if( nOmegaSpecs > 1 )
    {
      // v0.1 supports only one (full) Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found.",
	       C_OMEGA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }  
  if( nOmegaSpecs < 1 )
    {
      // v0.1 supports only one (full) Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.",
		C_OMEGA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
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
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		C_OMEGA, C_DIMENSION, XMLString::transcode( xml_omega_dim ) );
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
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  if( XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      myOmegaStruct = Symbol::DIAGONAL;
      myOmegaOrder = myOmegaDim;
    }
  else if( XMLString::equals( xml_omega_struct, X_BLOCK ) )
    {
      myOmegaStruct = Symbol::TRIANGLE;
      myOmegaOrder = series( 1, 1, myOmegaDim );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		C_OMEGA, C_STRUCT, XMLString::transcode( xml_omega_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_omega = table->insertSymmetricMatrix( NMKey.OMEGA, myOmegaStruct, myOmegaDim, Symbol::SYSTEM, Symbol::READONLY );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    if( omega_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.",
		  C_OMEGA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( omega_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_OMEGA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
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

  DOMNodeList * sigma_list = pop_analysis->getElementsByTagName( X_SIGMA );
  int nSigmaSpecs = sigma_list->getLength();
  if( nSigmaSpecs > 1 )
    { 
      // v0.1 supports only one (full) Sigma specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found.",
		C_SIGMA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( nSigmaSpecs < 1 )
    { 
      // v0.1 supports only one (full) Sigma specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.",
		C_SIGMA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * sigma = dynamic_cast<DOMElement*>( sigma_list->item(0) );
  if( !sigma->hasAttribute( X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		C_SIGMA, C_DIMENSION );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_dim = sigma->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_sigma_dim, mySigmaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		C_OMEGA, C_DIMENSION, XMLString::transcode( xml_sigma_dim ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !sigma->hasAttribute( X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		C_SIGMA, C_STRUCT );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_struct = sigma->getAttribute( X_STRUCT );
  if( XMLString::equals( xml_sigma_struct, X_DIAGONAL ) )
    {
      mySigmaStruct = Symbol::DIAGONAL;
      mySigmaOrder = mySigmaDim;
    }
  else if( XMLString::equals( xml_sigma_struct, X_BLOCK ) )
    {
      mySigmaStruct = Symbol::TRIANGLE;
      mySigmaOrder = series( 1, 1, mySigmaDim );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		C_SIGMA, C_STRUCT, XMLString::transcode( xml_sigma_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  Symbol * sym_sigma = table->insertSymmetricMatrix( NMKey.SIGMA, mySigmaStruct, mySigmaDim, Symbol::SYSTEM, Symbol::READONLY ); 
  {
    //<in>
    DOMNodeList * sigma_in_list = sigma->getElementsByTagName( X_IN );
    if( sigma_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.",
		  C_SIGMA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( sigma_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  C_SIGMA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(0) );

    //
    // Sigma specification contains the minimal representation of the matrix.
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
      DOMNodeList * value_list = sigma_in->getElementsByTagName( X_VALUE );
      if( mySigmaOrder != value_list->getLength() )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		    C_VALUE, C_SIGMA, C_LENGTH );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( mySigmaStruct == Symbol::TRIANGLE )
	{
	  // First construct a full n by n matrix.
	  valarray<string> sigma_in_full ( mySigmaDim * mySigmaDim );
	  valarray<bool>   sigma_fix_full( mySigmaDim * mySigmaDim );
	  for( int i=0, cnt=0; i<mySigmaDim; i++ )
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
		  //sigma_in_full[ j + i*dim ] = str_val; // filling a lower triangle element
		  sigma_in_full [ i + j*mySigmaDim ] = str_val; // filling a upper triangle element
		  sigma_fix_full[ i + j*mySigmaDim ] = isFixed;
		}
	    }
	  // Then, extract only the upper half in the row major order.
	  for( int i=0, cnt=0; i<mySigmaDim; i++ )
	    {
	      for( int j=i; j<mySigmaDim; j++, cnt++ )
		{
		  sym_sigma->initial[0][cnt] = sigma_in_full [ j + i * mySigmaDim ];
		  sym_sigma->fixed  [0][cnt] = sigma_fix_full[ j + i * mySigmaDim ];
		}
	    }
	}
      else // diagonal case
	{
	  for( int i=0; i<mySigmaDim; i++ )
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
	      sym_sigma->initial[0][i] = str_val;
	      sym_sigma->fixed[0][i]   = isFixed;
	    }
	}
  }
  
  //---------------------------------------------------------------------------------------
  // eta
  // NOTE: eta is not given by the user.  
  // eta's initial estimate is set to 0.0 automatically.
  //
  //---------------------------------------------------------------------------------------
  myEtaLen = myOmegaDim;
  char etaDefault[] = "0.0";
  Symbol * sym_eta = table->insertVector( NMKey.ETA, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;

  //---------------------------------------------------------------------------------------
  // Sigma 
  // Sigma is the covariance of EPS: thus, 
  // the order of Sigma is the length of EPS vector.
  //---------------------------------------------------------------------------------------
  myEpsLen = mySigmaDim;
  char epsDefault[] = "0.0";
  Symbol * sym_eps = table->insertVector( NMKey.EPS, myEpsLen, Symbol::SYSTEM, Symbol::READONLY );
  for( int i=0; i<myEpsLen; i++ ) sym_eps->initial[0][i] = epsDefault;
  sym_eta->fixed[0] = false;

  //---------------------------------------------------------------------------------------
  // (Optional) Statistics elements
  //---------------------------------------------------------------------------------------
  // <description>
  // <simulation>
  // <pop_stat>
  DOMNodeList * descriptions = pop_analysis->getElementsByTagName( X_DESCRIPTION );
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

  myIsSimulate = false;
  mySeed       = 0;
  DOMNodeList * simulations = pop_analysis->getElementsByTagName( X_SIMULATION );
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
		    C_SIMULATION, C_SEED, XMLString::transcode(xml_seed) );
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

  DOMNodeList * pop_stat_list = pop_analysis->getElementsByTagName( X_POP_STAT );

  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( pop_stat_list->getLength() > 0 && myIsEstimate )
    {
      if( pop_stat_list->getLength() > 1 )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Multiple <%s> elements found.", 
		    C_POP_STAT );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      if( !pop_stat->hasAttribute( X_COVARIANCE_FORM ) && myIsStat )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.", 
		    C_POP_STAT, C_COVARIANCE_FORM );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      const XMLCh* cov_form = pop_stat->getAttribute( X_COVARIANCE_FORM ); // r, rsr, s
      if( XMLString::equals( cov_form, X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, X_COV_RSR ) )
	myCovForm = "RSR";
      else if( XMLString::equals( cov_form, X_COV_R ) )
	myCovForm = "R";
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    C_POP_STAT, C_COVARIANCE_FORM, XMLString::transcode( cov_form )  );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      if( pop_stat->hasAttribute( X_IS_ERR_OUT ) )
	{
	  const XMLCh* xml_stderr = pop_stat->getAttribute( X_IS_ERR_OUT );
	  myIsStderr = (XMLString::equals( xml_stderr, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CORR_OUT ) )
	{
	  const XMLCh* xml_correlation = pop_stat->getAttribute( X_IS_CORR_OUT );
	  myIsCorrelation = (XMLString::equals( xml_correlation, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COV_OUT ) )
	{
	  const XMLCh* xml_cov = pop_stat->getAttribute( X_IS_COV_OUT );
	  myIsCov = (XMLString::equals( xml_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_INV_COV_OUT ) )
	{
	  const XMLCh* xml_inv_cov = pop_stat->getAttribute( X_IS_INV_COV_OUT );
	  myIsInvCov = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CONF_OUT ) )
	{
	  const XMLCh* xml_conf = pop_stat->getAttribute( X_IS_CONF_OUT );
	  myIsConfidence = (XMLString::equals( xml_conf, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COEF_OUT ) )
	{
	  const XMLCh* xml_coef = pop_stat->getAttribute( X_IS_COEF_OUT );
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
