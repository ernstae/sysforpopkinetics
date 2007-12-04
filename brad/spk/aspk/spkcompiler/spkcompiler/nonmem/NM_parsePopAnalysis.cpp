/**
 * @file NM_parsePopAnalysis.cpp
 * Define NonmemTranslator::parsePopAnalysis().
 */
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

/*
 *****************************************************************************************
 *
 * Parsing <pop_analysis>
 *
 *****************************************************************************************
 */
void NonmemTranslator::parsePopAnalysis( const DOMElement* pop_analysis )
{
  //--------------------------------------------------------------------------------------
  // Determine the value of <pop_analysis::is_estimation>.
  // If "yes", it requires certain other attributes to be given as well.
  //--------------------------------------------------------------------------------------
  if( !pop_analysis->hasAttribute( XML.X_IS_ESTIMATION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.", XML.C_POP_ANALYSIS, XML.C_IS_ESTIMATION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }
  const XMLCh * xml_is_estimation = pop_analysis->getAttribute( XML.X_IS_ESTIMATION );
  myIsEstimate = ( XMLString::equals( xml_is_estimation, XML.X_YES )? true : false );


  //--------------------------------------------------------------------------------------
  // Attributes that are required when <pop_analysis::is_estimation> is "yes".
  //
  // * approximation = {fo, foce, laplace, std_two_stage, global_two_stage,
  // *                  iterative_two_stage, nonparametric}
  // * pop_size
  // * is_estimation = {yes, no}
  //--------------------------------------------------------------------------------------
  if( myIsEstimate )
    {
      //
      // Finding out the approximation method.
      //
      if( !pop_analysis->hasAttribute( XML.X_APPROXIMATION ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.", XML.C_POP_ANALYSIS, XML.C_APPROXIMATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_approx = pop_analysis->getAttribute( XML.X_APPROXIMATION );
      
      if( XMLString::equals( xml_approx, XML.X_FO ) )
	setApproximation( FO );
      else if( XMLString::equals( xml_approx, XML.X_FOCE ) )
	setApproximation( FOCE );
      else if( XMLString::equals( xml_approx, XML.X_LAPLACE ) )
	setApproximation( LAPLACE );
      else if( XMLString::equals( xml_approx, XML.X_STD_TWO_STAGE ) )
	setApproximation( STD_TWO_STAGE );
      else if( XMLString::equals( xml_approx, XML.X_GLOBAL_TWO_STAGE ) )
	setApproximation( GLOBAL_TWO_STAGE );
      else if( XMLString::equals( xml_approx, XML.X_ITERATIVE_TWO_STAGE ) )
	setApproximation( ITERATIVE_TWO_STAGE );
      else if( XMLString::equals( xml_approx, XML.X_NONPARAMETRIC ) )
	myIsNonparam = true;
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    XML.C_POP_ANALYSIS, 
		    XML.C_APPROXIMATION, 
		   XMLString::transcode(xml_approx) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
    }

  //---------------------------------------------------------------------------------------
  // Optional attributes that matter when <pop_analysis::is_estimation> is "yes".
  //
  // * is_eta_out = {yes, "no"}
  // * is_restart = {yes, "no"}
  // * mitr
  // * sig_digits
  //---------------------------------------------------------------------------------------
  if( myIsEstimate )
    {
      const XMLCh * xml_is_eta_out;
      if( pop_analysis->hasAttribute( XML.X_IS_ETA_OUT ) )
	{
	  xml_is_eta_out = pop_analysis->getAttribute( XML.X_IS_ETA_OUT );
	  myIsPosthoc = ( XMLString::equals( xml_is_eta_out, XML.X_YES )? true : false );
	}
      
      const XMLCh * xml_is_restart;
      if( pop_analysis->hasAttribute( XML.X_IS_RESTART ) )
	{
	  xml_is_restart = pop_analysis->getAttribute( XML.X_IS_RESTART );
	  myIsRestart = ( XMLString::equals( xml_is_restart, XML.X_YES )? true : false );
	}
      if( pop_analysis->hasAttribute( XML.X_MITR ) )
	{
	  const XMLCh* xml_mitr = pop_analysis->getAttribute( XML.X_MITR );
	  if( !XMLString::textToBin( xml_mitr, myPopMitr ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			XML.C_POP_ANALYSIS, XML.C_MITR, XMLString::transcode(xml_mitr) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
				      __LINE__, __FILE__ );
	      throw e;
	    }
	}
      const XMLCh* xml_sig_digits;
      if( pop_analysis->hasAttribute( XML.X_SIG_DIGITS ) )
	{
	  xml_sig_digits = pop_analysis->getAttribute( XML.X_SIG_DIGITS );
	  if( XMLString::stringLen( xml_sig_digits ) > 0 )
	    {
	      if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  snprintf( mess, 
			    SpkCompilerError::maxMessageLen(),
			    "Invalid <%s::%s> attribute value: \"%s\".", 
			    XML.C_POP_ANALYSIS, 
			    XML.C_SIG_DIGITS, 
			   XMLString::transcode( xml_sig_digits ) );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
					  __LINE__, __FILE__);
		  throw e;
              
		}
	      if( !( mySigDigits > 0 && mySigDigits < 9 ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  snprintf( mess, 
			    SpkCompilerError::maxMessageLen(),
			    "Invalid <%s::%s> attribute value: \"%s\".  Valid range: (1-8)", 
			    XML.C_POP_ANALYSIS, XML.C_SIG_DIGITS, XMLString::transcode( xml_sig_digits )  );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
					  __LINE__, __FILE__);
		  throw e;
		}
	      myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	      myPopEpsilon = myIndEpsilon;
	    }
	}
    }

  //---------------------------------------------------------------------------------------
  // Elements that are required when <pop_analysis::approximation> is "nonparametric".
  //
  // * nonparametric_info
  //
  // Elements that are required in <nonparametric_info>
  //
  // * measure_points_in
  //
  // Elements that are required in <measure_points_in>
  //
  // * auto_generate_method = {grid, random_uniform}
  //
  // Attributes that are required when <auto_generate_method> = "grid"
  //
  // * points_per_dimension
  //
  // Attributes that are required when <auto_generate_method> = "random_uniform"
  //
  // * number_of_points
  // * seed
  //---------------------------------------------------------------------------------------
  if( myIsNonparam )
    {
      DOMNodeList * nonparametric_info_list = pop_analysis->getElementsByTagName( XML.X_NONPARAMETRIC_INFO );
      if( nonparametric_info_list->getLength() > 1 )
        {
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess, 
                    SpkCompilerError::maxMessageLen(),
                    "Multiple <%s> elements found in the sourceML document.", XML.C_NONPARAMETRIC_INFO );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
        }
      if( nonparametric_info_list->getLength() < 1 )
        {
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess, 
                    SpkCompilerError::maxMessageLen(),
                    "Missing <%s> element.", XML.C_NONPARAMETRIC_INFO );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
        }

      DOMElement * nonparametric_info = dynamic_cast<DOMElement*>( nonparametric_info_list->item(0) );
      DOMNodeList * measure_points_in_list = nonparametric_info->getElementsByTagName( XML.X_MEASURE_POINTS_IN );
      if( measure_points_in_list->getLength() > 1 )
        {
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
                    SpkCompilerError::maxMessageLen(),
                    "Multiple <%s> child elements found under <%s>.", 
                    XML.C_MEASURE_POINTS_IN, XML.C_NONPARAMETRIC_INFO );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
        }
      if( measure_points_in_list->getLength() < 1 )
        {
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
                    SpkCompilerError::maxMessageLen(),
                    "Missing <%s> child under <%s>.", 
                    XML.C_MEASURE_POINTS_IN, XML.C_NONPARAMETRIC_INFO );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
        }
      DOMElement * measure_points_in = dynamic_cast<DOMElement*>( measure_points_in_list->item(0) );

      if( measure_points_in->hasAttribute( XML.X_AUTO_GENERATE_METHOD ) )
        {
          const XMLCh* xml_auto_generate_method = measure_points_in->getAttribute( XML.X_AUTO_GENERATE_METHOD );
          if( XMLString::equals( xml_auto_generate_method, XML.X_GRID ) )
            {
              setApproximation( NONPARAM_GRID );

              const XMLCh* xml_points_per_dimension;
              if( measure_points_in->hasAttribute( XML.X_POINTS_PER_DIMENSION ) )
                {
                  xml_points_per_dimension = measure_points_in->getAttribute( XML.X_POINTS_PER_DIMENSION );
                  if( XMLString::stringLen( xml_points_per_dimension ) > 0 )
                    {
                      if( !XMLString::textToBin( xml_points_per_dimension, myNonparamGridMeasurePointPerSideIn ) )
                        {
                          char mess[ SpkCompilerError::maxMessageLen() ];
                          snprintf( mess, 
                                    SpkCompilerError::maxMessageLen(),
                                    "Invalid <%s::%s> attribute value: \"%s\".", 
                                    XML.C_MEASURE_POINTS_IN, 
                                    XML.C_POINTS_PER_DIMENSION, 
                                   XMLString::transcode( xml_points_per_dimension ) );
                          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                                  __LINE__, __FILE__);
                          throw e;
                        }
                      if( myNonparamGridMeasurePointPerSideIn < 1 )
                        {
                          char mess[ SpkCompilerError::maxMessageLen() ];
                          snprintf( mess, 
                                    SpkCompilerError::maxMessageLen(),
                                    "Invalid <%s::%s> attribute value: \"%s\".  Value must be greater than zero.", 
                                    XML.C_MEASURE_POINTS_IN, XML.C_POINTS_PER_DIMENSION, XMLString::transcode( xml_points_per_dimension )  );
                          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                                  __LINE__, __FILE__);
                          throw e;
                        }
        	    }
        	}
            }
          else
            {
              setApproximation( NONPARAM_RANDOM_UNIFORM );

              const XMLCh* xml_number_of_points;
              if( measure_points_in->hasAttribute( XML.X_NUMBER_OF_POINTS ) )
                {
                  xml_number_of_points = measure_points_in->getAttribute( XML.X_NUMBER_OF_POINTS );
                  if( XMLString::stringLen( xml_number_of_points ) > 0 )
                    {
                      if( !XMLString::textToBin( xml_number_of_points, myNonparamRandomMeasurePointIn ) )
                        {
                          char mess[ SpkCompilerError::maxMessageLen() ];
                          snprintf( mess, 
                                    SpkCompilerError::maxMessageLen(),
                                    "Invalid <%s::%s> attribute value: \"%s\".", 
                                    XML.C_MEASURE_POINTS_IN, 
                                    XML.C_NUMBER_OF_POINTS, 
                                   XMLString::transcode( xml_number_of_points ) );
                          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                                  __LINE__, __FILE__);
                          throw e;
                        }
                      if( myNonparamRandomMeasurePointIn < 1 )
                        {
                          char mess[ SpkCompilerError::maxMessageLen() ];
                          snprintf( mess, 
                                    SpkCompilerError::maxMessageLen(),
                                    "Invalid <%s::%s> attribute value: \"%s\".  Value must be greater than zero.", 
                                    XML.C_MEASURE_POINTS_IN, XML.C_NUMBER_OF_POINTS, XMLString::transcode( xml_number_of_points )  );
                          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                                  __LINE__, __FILE__);
                          throw e;
                        }
        	    }
        	}

              const XMLCh* xml_seed;
              if( measure_points_in->hasAttribute( XML.X_SEED ) )
                {
                  xml_seed = measure_points_in->getAttribute( XML.X_SEED );
                  if( XMLString::stringLen( xml_seed ) > 0 )
                    {
                      if( !XMLString::textToBin( xml_seed, mySeed ) )
                        {
                          char mess[ SpkCompilerError::maxMessageLen() ];
                          snprintf( mess, 
                                    SpkCompilerError::maxMessageLen(),
                                    "Invalid <%s::%s> attribute value: \"%s\".", 
                                    XML.C_MEASURE_POINTS_IN, 
                                    XML.C_SEED, 
                                   XMLString::transcode( xml_seed ) );
                          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                                  __LINE__, __FILE__);
                          throw e;
                        }
        	    }
        	}
            }
        }
      else
        {
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
                    SpkCompilerError::maxMessageLen(),
                    "Missing <%s> child under <%s>.", 
                    XML.C_AUTO_GENERATE_METHOD, XML.C_MEASURE_POINTS_IN );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
        }
    }

  //---------------------------------------------------------------------------------------
  // Required elements
  //
  // <data_labels>
  // <theta>
  // <omega>+
  // <sigma>+
  //---------------------------------------------------------------------------------------

  //-------------------------------------------
  // <data_labels>
  //-------------------------------------------
  // Get the list of <data_labels> elements from SourceML.
  DOMNodeList * data_labels_list = pop_analysis->getElementsByTagName( XML.X_DATA_LABELS );
  if( data_labels_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found in the sourceML document.", 
		XML.C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }

  // Check the number of <data_labels>'s.  There must be at least one <data_labels>.
  if( data_labels_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", 
		XML.C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }

  // Pick the first <data_labels> from the list of <data_labels>'s.
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {
    // Get the list of <label>s.  It cannot be empty.
    DOMNodeList * labels = data_labels->getElementsByTagName( XML.X_LABEL );
    int nLabels = labels->getLength();
    if( nLabels < 1 ) 
      {

        char mess[ SpkCompilerError::maxMessageLen() ];
        snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element.",  XML.C_LABEL );
        SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
        throw e;
      }

    // Iterate though the list of <label>s.
    // Check if the specification of labels here matches with that of
    // labels in the data set (SpkDataML).
    for( int i=0; i<nLabels; i++ )
      {
	DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );

	// <label::name> is an required attribute.
	if( !xml_label->hasAttribute( XML.X_NAME ) )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            snprintf( mess, 
		      SpkCompilerError::maxMessageLen(),
		      "Missing <%s::%s> attribute for the %d-th <%s>.", XML.C_LABEL, XML.C_NAME,
		      i, XML.C_LABEL );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	const XMLCh* xml_name = xml_label->getAttribute( XML.X_NAME );
	char * c_name = XMLString::transcode( xml_name );

	// The label name should have been already registered in the symbol table
        // during parseData(), or throw an exception.
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
	// The synonym is an optional attribute.
	// Register the value into the symbol table if it is given.
	if( xml_label->hasAttribute( XML.X_SYNONYM ) )
	  {
	    const XMLCh* xml_synonym = xml_label->getAttribute( XML.X_SYNONYM );
	    char * c_synonym = XMLString::transcode( xml_synonym );
	    name->synonym = string( c_synonym );
	    delete c_synonym;
	  }
	delete c_name;
      }
  }

  //-------------------------------------------
  // <theta>
  //-------------------------------------------
  char valueDefault[] = "0.0";

  DOMNodeList * theta_list = pop_analysis->getElementsByTagName( XML.X_THETA );
  if( theta_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements found in the sourceML document.", XML.C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( theta_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", XML.C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( XML.X_LENGTH ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute .", XML.C_THETA, XML.C_LENGTH );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_theta_len = theta->getAttribute( XML.X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
	       "Invalid <%s::%s> attribute value: %s", 
	       XML.C_THETA, XML.C_LENGTH, XMLString::transcode( xml_theta_len ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertVector( nonmem::THETA, myThetaLen, Symbol::SYSTEM, Symbol::READONLY );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( XML.X_IN );
    if( theta_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> child elements found under <%s>.", 
		  XML.C_IN, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> child under <%s>.", 
		  XML.C_IN, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( XML.X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  XML.C_VALUE, XML.C_THETA, XML.C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
        bool isFixed = false;
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	if( value->hasAttribute( XML.X_FIXED ) )
	  {
	    const XMLCh* xml_fixed = value->getAttribute( XML.X_FIXED );
	    isFixed = (XMLString::equals( xml_fixed, XML.X_YES )? true : false );
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
    DOMNodeList * theta_low_list = theta->getElementsByTagName( XML.X_LOW );
    if( theta_low_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.", 
		  XML.C_LOW, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_low_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.", 
		  XML.C_LOW, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( XML.X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  XML.C_VALUE, XML.C_THETA, XML.C_LENGTH );
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
    DOMNodeList * theta_up_list = theta->getElementsByTagName( XML.X_UP );
    if( theta_up_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Multiple <%s> elements found under <%s>.", 
		  XML.C_UP, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_up_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.", 
		  XML.C_UP, XML.C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( XML.X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess,
		  SpkCompilerError::maxMessageLen(),
		  "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		  XML.C_VALUE, XML.C_THETA, XML.C_LENGTH );
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

  //-------------------------------------------
  // <omega>
  //-------------------------------------------
  DOMNodeList * omega_list = pop_analysis->getElementsByTagName( XML.X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  //if( nOmegaSpecs > 1 )
  //{
  //    // v0.1 supports only one (full) Omega specification
  //    char mess[ SpkCompilerError::maxMessageLen() ];
  //    snprintf( mess, 
  //		SpkCompilerError::maxMessageLen(),
  //		"Multiple <%s> elements found.",
  //	       XML.C_OMEGA );
  //    SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
  //    throw e;
      //}  
  if( nOmegaSpecs < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.",
		XML.C_OMEGA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  myOmegaDim.resize( nOmegaSpecs );
  myOmegaOrder.resize( nOmegaSpecs );
  myOmegaStruct.resize( nOmegaSpecs );
  myOmegaSameAsPrev.resize( nOmegaSpecs );
  
  DOMElement * omega;
  for (int ii = 0; ii < nOmegaSpecs; ii++)    // loop through blocks
  {
     omega = dynamic_cast<DOMElement*>( omega_list->item(ii) );
     if( !omega->hasAttribute( XML.X_DIMENSION ) )
       {
	 char mess[ SpkCompilerError::maxMessageLen() ];
	 snprintf( mess, 
		   SpkCompilerError::maxMessageLen(),
		   "Missing <%s::%s> attribute.",
		   XML.C_OMEGA, XML.C_DIMENSION );
	 SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	 throw e;
       }
     const XMLCh* xml_omega_dim = omega->getAttribute( XML.X_DIMENSION );
     if( !XMLString::textToBin( xml_omega_dim, myOmegaDim[ii] ) )
       {
	 char mess[ SpkCompilerError::maxMessageLen() ];
	 snprintf( mess,
		   SpkCompilerError::maxMessageLen(),
		   "Invalid <%s::%s> attribute value: \"%s\".", 
		   XML.C_OMEGA, XML.C_DIMENSION, XMLString::transcode( xml_omega_dim ) );
	 SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	 throw e;
       }
      if( omega->hasAttribute( XML.X_SAME_AS_PREVIOUS ) )
       {
	 const XMLCh* xml_omega_sameAsPrev = omega->getAttribute( XML.X_SAME_AS_PREVIOUS );
	 myOmegaSameAsPrev[ii] = (XMLString::equals( xml_omega_sameAsPrev, XML.X_YES )? true : false );
       }
      if( !omega->hasAttribute( XML.X_STRUCT ) )
       {
	 char mess[ SpkCompilerError::maxMessageLen() ];
	 snprintf( mess, 
		   SpkCompilerError::maxMessageLen(),
		   "Missing <%s::%s> attribute.",
		   XML.C_OMEGA, XML.C_STRUCT );
	 SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	 throw e;
       }
     const XMLCh* xml_omega_struct = omega->getAttribute( XML.X_STRUCT );
     if( XMLString::equals( xml_omega_struct, XML.X_DIAGONAL ) )
       {
	 myOmegaStruct[ii] = Symbol::DIAGONAL;
	 myOmegaOrder[ii] = myOmegaDim[ii];
       }
     else if( XMLString::equals( xml_omega_struct, XML.X_BLOCK ) )
       {
	 myOmegaStruct[ii] = Symbol::TRIANGLE;
	 myOmegaOrder[ii] = series( 1, 1, myOmegaDim[ii] );
       }
     else
       {
	 char mess[ SpkCompilerError::maxMessageLen() ];
	 snprintf( mess,
		   SpkCompilerError::maxMessageLen(),
		   "Invalid <%s::%s> attribute value: \"%s\".", 
		   XML.C_OMEGA, XML.C_STRUCT, XMLString::transcode( xml_omega_struct ) );
	 SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	 throw e;
       }
  }  
  // end of first "loop through blocks"


  Symbol * sym_omega = table->insertSymmetricMatrix( nonmem::OMEGA, myOmegaStruct, myOmegaDim, Symbol::SYSTEM, Symbol::READONLY );
 

  DOMElement * omega_in;
  DOMNodeList * value_list;
  for (int ii = 0; ii < nOmegaSpecs; ii++)    // loop through blocks
  {
     omega = dynamic_cast<DOMElement*>( omega_list->item(ii) );
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( XML.X_IN );
    //if( omega_in_list->getLength() > 1 )
    //  {
    //	char mess[ SpkCompilerError::maxMessageLen() ];
    //	snprintf( mess, 
    //		  SpkCompilerError::maxMessageLen(),
    //		  "Multiple <%s> elements found under <%s>.",
    //		  XML.C_OMEGA, XML.C_IN );
    //	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
    //	throw e;
    //  }
    if( omega_in_list->getLength() < 1 )
      {
    	char mess[ SpkCompilerError::maxMessageLen() ];
    	snprintf( mess, 
    		  SpkCompilerError::maxMessageLen(),
    		  "Missing <%s> element under <%s>.",
    		  XML.C_OMEGA, XML.C_IN );
    	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
    	throw e;
     }

    omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );  //there is only one set of items (value)
 
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
      value_list = omega_in->getElementsByTagName( XML.X_VALUE );

      if( myOmegaOrder[ii] != value_list->getLength() )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		    XML.C_VALUE, XML.C_OMEGA, XML.C_LENGTH );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      DOMElement * value;
      if( myOmegaStruct[ii] == Symbol::TRIANGLE )
	{
	  // First construct a full n by n matrix.
	  valarray<string> omega_in_full ( myOmegaDim[ii] * myOmegaDim[ii] );
	  valarray<bool> omega_fix_full( myOmegaDim[ii] * myOmegaDim[ii] );
	  for( int i=0, cnt=0; i<myOmegaDim[ii]; i++ )
	    {
	      for( int j=0; j<=i; j++, cnt++ )
		{
		  char str_val[128];
		  bool isFixed = false;
		  value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		  if( value->hasAttribute( XML.X_FIXED ) )
		    {
		      const XMLCh* xml_fixed = value->getAttribute( XML.X_FIXED );
		      isFixed = (XMLString::equals( xml_fixed, XML.X_YES )? true : false );
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
		  omega_in_full [ i + j*myOmegaDim[ii] ] = str_val; // filling a upper triangle element
		  omega_fix_full[ i + j*myOmegaDim[ii] ] = isFixed;
		}
	    }
	  // Then, extract only the upper half in the row major order.
	  for( int i=0, cnt=0; i<myOmegaDim[ii]; i++ )
	    {
	      for( int j=i; j<myOmegaDim[ii]; j++, cnt++ )
		{
		  sym_omega->initial[ii][cnt] = omega_in_full [ j + i * myOmegaDim[ii] ];
		  sym_omega->fixed  [ii][cnt] = omega_fix_full[ j + i * myOmegaDim[ii] ];
		}
	    }
	}
      else // diagonal case
	{
	  for( int i=0; i<myOmegaDim[ii]; i++ )
	    {
	      char str_val[128];
	      bool isFixed = false;
	      value = dynamic_cast<DOMElement*>( value_list->item(i) );
	      if( value->hasAttribute( XML.X_FIXED ) )
		{
		  const XMLCh* xml_fixed = value->getAttribute( XML.X_FIXED );
		  isFixed = (XMLString::equals( xml_fixed, XML.X_YES )? true : false );
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
	      sym_omega->initial[ii][i] = str_val;
	      sym_omega->fixed[ii][i]   = isFixed;
	    }
	}
  }

  //-------------------------------------------
  // <sigma>
  //-------------------------------------------
  DOMNodeList * sigma_list = pop_analysis->getElementsByTagName( XML.X_SIGMA );
  int nSigmaSpecs = sigma_list->getLength();
  //if( nSigmaSpecs > 1 )
  //  { 
  //    // v0.1 supports only one (full) Sigma specification
  //    char mess[ SpkCompilerError::maxMessageLen() ];
  //    snprintf( mess, 
  //		SpkCompilerError::maxMessageLen(),
  //		"Multiple <%s> elements found.",
  //		XML.C_SIGMA );
  //    SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
  //    throw e;
  //  }
  if( nSigmaSpecs < 1 )
    { 
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.",
		XML.C_SIGMA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  mySigmaDim.resize( nSigmaSpecs );
  mySigmaOrder.resize( nSigmaSpecs  );
  mySigmaStruct.resize( nSigmaSpecs );
  mySigmaSameAsPrev.resize( nSigmaSpecs );

  DOMElement * sigma;
  for (int ii = 0; ii < nSigmaSpecs; ii++)    // loop through blocks
    {
  sigma = dynamic_cast<DOMElement*>( sigma_list->item(ii) );
  if( !sigma->hasAttribute( XML.X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		XML.C_SIGMA, XML.C_DIMENSION );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_dim = sigma->getAttribute( XML.X_DIMENSION );
  if( !XMLString::textToBin( xml_sigma_dim, mySigmaDim[ii] ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		XML.C_OMEGA, XML.C_DIMENSION, XMLString::transcode( xml_sigma_dim ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  // SAME_AS_PREVIOUS  ---------------------------------------------
  if( sigma->hasAttribute( XML.X_SAME_AS_PREVIOUS ) )
    {
      const XMLCh* xml_sigma_sameAsPrev = sigma->getAttribute( XML.X_SAME_AS_PREVIOUS );
      mySigmaSameAsPrev[ii] = (XMLString::equals( xml_sigma_sameAsPrev, XML.X_YES )? true : false );
    }
  // --------------------------------------------------------------
  if( !sigma->hasAttribute( XML.X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.",
		XML.C_SIGMA, XML.C_STRUCT );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_struct = sigma->getAttribute( XML.X_STRUCT );
  if( XMLString::equals( xml_sigma_struct, XML.X_DIAGONAL ) )
    {
      mySigmaStruct[ii] = Symbol::DIAGONAL;
      mySigmaOrder[ii] = mySigmaDim[ii];
    }
  else if( XMLString::equals( xml_sigma_struct, XML.X_BLOCK ) )
    {
      mySigmaStruct[ii] = Symbol::TRIANGLE;
      mySigmaOrder[ii] = series( 1, 1, mySigmaDim[ii] );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess,
		SpkCompilerError::maxMessageLen(),
		"Invalid <%s::%s> attribute value: \"%s\".", 
		XML.C_SIGMA, XML.C_STRUCT, XMLString::transcode( xml_sigma_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  }  // end of first "loop through blocks"

  Symbol * sym_sigma = table->insertSymmetricMatrix( nonmem::SIGMA, mySigmaStruct, mySigmaDim, Symbol::SYSTEM, Symbol::READONLY );


  DOMNodeList * sigma_in_list;
  DOMElement * sigma_in;
  for (int ii = 0; ii < nSigmaSpecs; ii++)    // loop through blocks
    {
      sigma = dynamic_cast<DOMElement*>( sigma_list->item(ii) );
      //<in>
      sigma_in_list = sigma->getElementsByTagName( XML.X_IN );
    //if( sigma_in_list->getLength() > 1 )
    //  {
    //	char mess[ SpkCompilerError::maxMessageLen() ];
    //	snprintf( mess, 
    //		  SpkCompilerError::maxMessageLen(),
    //		  "Multiple <%s> elements found under <%s>.",
    //		  XML.C_SIGMA, XML.C_IN );
    //	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
    //	throw e;
    //  }
    if( sigma_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	snprintf( mess, 
		  SpkCompilerError::maxMessageLen(),
		  "Missing <%s> element under <%s>.",
		  XML.C_SIGMA, XML.C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
 

    sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(ii) );

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
      DOMNodeList * value_list = sigma_in->getElementsByTagName( XML.X_VALUE );
      if( mySigmaOrder[ii] != value_list->getLength() )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		    XML.C_VALUE, XML.C_SIGMA, XML.C_LENGTH );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( mySigmaStruct[ii] == Symbol::TRIANGLE )
	{
	  // First construct a full n by n matrix.
	  valarray<string> sigma_in_full ( mySigmaDim[ii] * mySigmaDim[ii] );
	  valarray<bool>   sigma_fix_full( mySigmaDim[ii] * mySigmaDim[ii] );
	  for( int i=0, cnt=0; i<mySigmaDim[ii]; i++ )
	    {
	      for( int j=0; j<=i; j++, cnt++ )
		{
		  char str_val[128];
		  bool isFixed = false;
		  DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		  if( value->hasAttribute( XML.X_FIXED ) )
		    {
		      const XMLCh* xml_fixed = value->getAttribute( XML.X_FIXED );
		      isFixed = (XMLString::equals( xml_fixed, XML.X_YES )? true : false );
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
		  sigma_in_full [ i + j*mySigmaDim[ii] ] = str_val; // filling a upper triangle element
		  sigma_fix_full[ i + j*mySigmaDim[ii] ] = isFixed;
		}
	    }
	  // Then, extract only the upper half in the row major order.
	  for( int i=0, cnt=0; i<mySigmaDim[ii]; i++ )
	    {
	      for( int j=i; j<mySigmaDim[ii]; j++, cnt++ )
		{
		  sym_sigma->initial[ii][cnt] = sigma_in_full [ j + i * mySigmaDim[ii] ];
		  sym_sigma->fixed  [ii][cnt] = sigma_fix_full[ j + i * mySigmaDim[ii] ];
		}
	    }
	}
      else // diagonal case
	{
	  for( int i=0; i<mySigmaDim[ii]; i++ )
	    {
	      char str_val[128];
	      bool isFixed = false;
	      DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	      if( value->hasAttribute( XML.X_FIXED ) )
		{
		  const XMLCh* xml_fixed = value->getAttribute( XML.X_FIXED );
		  isFixed = (XMLString::equals( xml_fixed, XML.X_YES )? true : false );
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
	      sym_sigma->initial[ii][i] = str_val;
	      sym_sigma->fixed[ii][i]   = isFixed;
	    }
	}
  }
  
  //-------------------------------------------
  // <eta>
  //-------------------------------------------
  // eta
  // NOTE: eta is not given by the user.  
  // eta's initial estimate is initialized to
  // 0.0 here.
  //-------------------------------------------
  myEtaLen = myOmegaDim.sum();
  char etaDefault[] = "0.0";
  Symbol * sym_eta = table->insertVector( nonmem::ETA, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;

  //-------------------------------------------
  // <eps> 
  //-------------------------------------------
  myEpsLen = mySigmaDim.sum();
  char epsDefault[] = "0.0";
  Symbol * sym_eps = table->insertVector( nonmem::EPS, myEpsLen, Symbol::SYSTEM, Symbol::READONLY );
  for( int i=0; i<myEpsLen; i++ ) sym_eps->initial[0][i] = epsDefault;
  sym_eta->fixed[0] = false;

  //---------------------------------------------------------------------------------------
  // (Optional) Statistics elements
  //
  // <description>
  // <simulation>
  // <pop_stat>
  //---------------------------------------------------------------------------------------

  //-------------------------------------------
  // <description>
  //-------------------------------------------
  DOMNodeList * descriptions = pop_analysis->getElementsByTagName( XML.X_DESCRIPTION );
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

  //-------------------------------------------
  // <simulation>
  //-------------------------------------------
  myIsSimulate = false;
  DOMNodeList * simulations = pop_analysis->getElementsByTagName( XML.X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      if( simulations->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Multiple <%s> elements found.",
		    XML.C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      myIsSimulate = true;
      mySeed       = 0;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( XML.X_SEED ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.",
		    XML.C_SIMULATION, XML.C_SEED );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      const XMLCh* xml_seed = simulation->getAttribute( XML.X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    XML.C_SIMULATION, XML.C_SEED, XMLString::transcode(xml_seed) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      if( simulation->hasAttribute( XML.X_SUBPROBLEMS ) )
	{
	  const XMLCh* xml_subproblems = simulation->getAttribute( XML.X_SUBPROBLEMS );
	  if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"Invalid <%s::%s> attribute value: \"%s\".", 
			XML.C_SIMULATION, XML.C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
    }

  //-------------------------------------------
  // <pop_stat>
  //-------------------------------------------
  DOMNodeList * pop_stat_list = pop_analysis->getElementsByTagName( XML.X_POP_STAT );

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
		    XML.C_POP_STAT );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      if( !pop_stat->hasAttribute( XML.X_COVARIANCE_FORM ) && myIsStat )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <%s::%s> attribute.", 
		    XML.C_POP_STAT, XML.C_COVARIANCE_FORM );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      const XMLCh* cov_form = pop_stat->getAttribute( XML.X_COVARIANCE_FORM ); // r, rsr, s
      if( XMLString::equals( cov_form, XML.X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, XML.X_COV_RSR ) )
	myCovForm = "RSR";
      else if( XMLString::equals( cov_form, XML.X_COV_R ) )
	myCovForm = "R";
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\".", 
		    XML.C_POP_STAT, XML.C_COVARIANCE_FORM, XMLString::transcode( cov_form )  );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      if( pop_stat->hasAttribute( XML.X_IS_ERR_OUT ) )
	{
	  const XMLCh* xml_stderr = pop_stat->getAttribute( XML.X_IS_ERR_OUT );
	  myIsStderr = (XMLString::equals( xml_stderr, XML.X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( XML.X_IS_CORR_OUT ) )
	{
	  const XMLCh* xml_correlation = pop_stat->getAttribute( XML.X_IS_CORR_OUT );
	  myIsCorrelation = (XMLString::equals( xml_correlation, XML.X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( XML.X_IS_COV_OUT ) )
	{
	  const XMLCh* xml_cov = pop_stat->getAttribute( XML.X_IS_COV_OUT );
	  myIsCov = (XMLString::equals( xml_cov, XML.X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( XML.X_IS_INV_COV_OUT ) )
	{
	  const XMLCh* xml_inv_cov = pop_stat->getAttribute( XML.X_IS_INV_COV_OUT );
	  myIsInvCov = (XMLString::equals( xml_inv_cov, XML.X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( XML.X_IS_CONF_OUT ) )
	{
	  const XMLCh* xml_conf = pop_stat->getAttribute( XML.X_IS_CONF_OUT );
	  myIsConfidence = (XMLString::equals( xml_conf, XML.X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( XML.X_IS_COEF_OUT ) )
	{
	  const XMLCh* xml_coef = pop_stat->getAttribute( XML.X_IS_COEF_OUT );
	  myIsCoefficient = (XMLString::equals( xml_coef, XML.X_YES )? true : false );
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

  //--------------------------------------------------------------------------------------
  // Set trace levels.
  //--------------------------------------------------------------------------------------
  myIndTraceLevel = 0;
  myPopTraceLevel = 1;

  return;
}
