/**
 * @file NM_parseSource.cpp
 * Define NonmemTranslator::parseSource().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "../SpkCompilerException.h"
#include "explang.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  parseSource()
//  Analizes source.xml further in details.
//
//  Pre-conditions
//
//  * ClientTranslator::ourTarget is set to either POP or IND
//  * ClientTranslator::ourPopSize is set to the number of subjects.
// 
//  Post-conditions
//
/////////////////////////////////////////////////////////////////////////////////////////////
void NonmemTranslator::parseSource()
{
  //---------------------------------------------------------------------------------------
  // <nonmem>
  //---------------------------------------------------------------------------------------
  DOMElement  * spksouce = getSourceTree()->getDocumentElement();
  DOMNodeList * nonmems  = spksouce->getElementsByTagName( XML.X_NONMEM );
  if( !nonmems->getLength() > 0 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", XML.C_NONMEM );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  //---------------------------------------------------------------------------------------
  // <constraint>
  //---------------------------------------------------------------------------------------
  DOMNodeList * constraints = nonmem->getElementsByTagName( XML.X_CONSTRAINT );
  if( constraints->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen()];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", XML.C_CONSTRAINT );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * constraint = dynamic_cast<DOMElement*>( constraints->item(0) );
  if(  !constraint->hasChildNodes() )
    {
      char mess[ SpkCompilerError::maxMessageLen()];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> or <%s> element.", 
		XML.C_POP_ANALYSIS, 
		XML.C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * analysis;
  bool isAnalysisDone = false;
  if( getTarget() == POP )
    {
      DOMNodeList * pop_analysises = constraint->getElementsByTagName( XML.X_POP_ANALYSIS );
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      parsePopAnalysis( analysis );
    }
  else //if( getTarget() == IND )
    {
      DOMNodeList * ind_analysises = constraint->getElementsByTagName( XML.X_IND_ANALYSIS );
      analysis = dynamic_cast<DOMElement*>( ind_analysises->item(0) );
      parseIndAnalysis( analysis );
    }
  isAnalysisDone = true;

  //---------------------------------------------------------------------------------------
  // <model>
  // NOTE: <pred> and the combination of {<pk>, <diffeqn>, <error>} are allowed.
  //       Default to <pred>.
  //---------------------------------------------------------------------------------------
  bool myIsModelDone = false;

  myModelSpec = detModelType();

  // PRED - computed within Pred::eval() or OdePred::eval()
  table->insertScalar( nonmem::PRED, Symbol::SYSTEM, Symbol::READONLY );

  // RES - computed within Pred::eval() or OdePred::eval()
  table->insertScalar( nonmem::RES, Symbol::SYSTEM, Symbol::READONLY );

  // WRES
  table->insertScalar( nonmem::WRES, Symbol::SYSTEM, Symbol::READONLY );

  // IPRED
  table->insertScalar( nonmem::IPRED, Symbol::SYSTEM, Symbol::READONLY );

  // IRES
  table->insertScalar( nonmem::IRES, Symbol::SYSTEM, Symbol::READONLY );

  // IWRES
  table->insertScalar( nonmem::IWRES, Symbol::SYSTEM, Symbol::READONLY );

  if( getTarget() == POP )
    {
      // ETARES
      table->insertVector( nonmem::ETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // WETARES
      table->insertVector( nonmem::WETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // IETARES
      table->insertVector( nonmem::IETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // IWETARES
      table->insertVector( nonmem::IWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // PPRED
      table->insertScalar( nonmem::PPRED, Symbol::SYSTEM, Symbol::READONLY );

      // PRES
      table->insertScalar( nonmem::PRES, Symbol::SYSTEM, Symbol::READONLY );
      
      // PWRES
      table->insertScalar( nonmem::PWRES, Symbol::SYSTEM, Symbol::READONLY );

      // PETARES
      table->insertVector( nonmem::PETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // PWETARES
      table->insertVector( nonmem::PWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // CPRED
      table->insertScalar( nonmem::CPRED, Symbol::SYSTEM, Symbol::READONLY );
      
      // CRES
      table->insertScalar( nonmem::CRES, Symbol::SYSTEM, Symbol::READONLY );
      
      // CWRES
      table->insertScalar( nonmem::CWRES, Symbol::SYSTEM, Symbol::READONLY );

      // CETARES
      table->insertVector( nonmem::CETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );

      // CWETARES
      table->insertVector( nonmem::CWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
    } 
  DOMNodeList * models = nonmem->getElementsByTagName( XML.X_MODEL );
  if( models->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", XML.C_MODEL );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement  * model = dynamic_cast<DOMElement*>( models->item(0) );

  unsigned int advan, trans;

  //
  // All ADVANs fit to the compartmental modeling framework.
  //
  if( myModelSpec != PRED )
    {
      XMLString::textToBin( model->getAttribute( XML.X_ADVAN ), advan );
      assert( advan > 0 );

      assert( myTrans == TRANS1 );  // default TRANS value
      if( model->hasAttribute( XML.X_TRANS ) )
	{
	  XMLString::textToBin( model->getAttribute( XML.X_TRANS ), trans );
	  myTrans = static_cast<TRANS>( trans );
	}
      parseAdvan( myModelSpec, myTrans, model );
    }
  else
    {
      DOMNodeList * preds   = model->getElementsByTagName( XML.X_PRED );
      if( preds->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing <pred>!" );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
	}
      DOMElement  * pred = dynamic_cast<DOMElement*>( preds->item(0) );
      parsePred( pred ); 
    }

  myIsModelDone = true;
 
  //---------------------------------------------------------------------------------------
  // <monte_carlo>
  //---------------------------------------------------------------------------------------
  DOMNodeList * monte_carlos = nonmem->getElementsByTagName( XML.X_MONTE_CARLO );
  DOMElement * monte_carlo;
  if( monte_carlos->getLength() > 0 )
    {
      myIsMonte = true;


      //
      // REVISIT - Sachiko - 09/09/2004
      // estimation and MC are mutually exclusive.  Currently (as of 9/9/04)
      // MDA allows both to be true, so for now set myIsEstimate=false.
      //
      if( myIsEstimate )
	myIsEstimate = false;
      //
      // if( myIsEstimate )
      // {
      //    char mess[ SpkCompilerError::maxMessageLen() ];
      //    snprintf( mess, 
      //              SpkCompilerError::maxMessageLen(),
      //              "The parameter estimation and the post-interation requests are mutually exclusive." );
      //    SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
      //        mess, __LINE__, __FILE__ );
      //    throw e;
      // }

      if( getTarget() != POP )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "Integral methods are only valid for the population analysis results.");
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
	}
      monte_carlo = dynamic_cast<DOMElement*>( monte_carlos->item(0) );
      parseMonte( monte_carlo );
    }

  //---------------------------------------------------------------------------------------
  //<presentation>
  //---------------------------------------------------------------------------------------
  //PRED parsing and <xxx_analysis> parsing must have been completed so
  //that the symbol table contains entries for the user defined
  //variables and THETA/OMEGA/SIGMA/ETA.
  if( !myIsModelDone )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"<%s> must be parsed before parsing <%s>.", 
		XML.C_PRED, XML.C_PRESENTATION);
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( !isAnalysisDone )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"<%s> must be parsed before parsing <%s>", 
		(getTarget()==POP? XML.C_POP_ANALYSIS : XML.C_IND_ANALYSIS ), 
		XML.C_PRESENTATION );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( XML.X_PRESENTATION );
  if( presentations->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements are found.", XML.C_PRESENTATION ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  myRecordNums.resize( getPopSize() );
  Symbol * id = table->find( nonmem::ID );
  if( id == NULL || id == Symbol::empty() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"\"%s\" data item seems missing from the data set.", 
		nonmem::ID.c_str() ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  // Determine the number of data records for each subject.
  for( int i=0; i<getPopSize(); i++ )
    {
      myRecordNums[i] = id->initial[i].size();
    }

  //---------------------------------------------------------------------------------------
  //
  // Check predefined words and register in the symbol table if appropriate
  // before generating C++ source code files.
  //
  //---------------------------------------------------------------------------------------
  assert( isAnalysisDone );
  Symbol * p;

  //+++++++++++++++++++++++++++++++++++++
  // Ones that are definitely going in.
  //+++++++++++++++++++++++++++++++++++++

  // ORGDV is a placeholder for the original data set.
  // The original data set should be copied into this placeholder
  // in case a new data set is simulated and replaces the oridinary DV field.
  if( ( p = table->find( nonmem::ORGDV )) == Symbol::empty() )
    {  
      table->insertScalar( nonmem::ORGDV, Symbol::SYSTEM, Symbol::READONLY );
      assert( table->find( nonmem::ORGDV ) != Symbol::empty() );
    }
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "ORGDV should NOT have been defined yet!",
				  __LINE__,
				  __FILE__ );
    }


  //+++++++++++++++++++++++++++++++++++++
  // Ones that may have been appeared 
  // in the model definition.
  //+++++++++++++++++++++++++++++++++++++
  // CMT
  if( (p = table->find( nonmem::CMT ))  != Symbol::empty() )
    myIsMissingCmt = false;

  // PCMT
  if( (p = table->find( nonmem::PCMT )) != Symbol::empty() )
    myIsMissingPcmt = false;

  // RATE
  if( (p = table->find( nonmem::RATE )) != Symbol::empty() )
    myIsMissingRate = false;

  if( myModelSpec != PRED )
    {
      if( (p = table->find( nonmem::MDV ))  == Symbol::empty() )
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR,
				      "MDV data item is missing",
				      __LINE__, __FILE__ );
	}
      if( (p = table->find( nonmem::AMT ))  == Symbol::empty() )
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR,
				      "AMT data item is missing",
				      __LINE__, __FILE__ );
	}
      if( (p = table->find( nonmem::EVID ))  == Symbol::empty() )
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR,
				      "EVID data item is missing",
				      __LINE__, __FILE__ );
	}

    }



  //+++++++++++++++++++++++++++++++++++++
  // Ones that should have been 
  // registered by now. 
  // Thus, not going in!
  //+++++++++++++++++++++++++++++++++++++

  // ID is required in a data set.
  // The field should have been inserted even if
  // the original data set lacked it.
  if( (p = table->find( nonmem::ID )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "ID should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // DV is always required in a data set and
  // should have appeared in the model definition as well.
  if( (p = table->find( nonmem::DV )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "DV should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // REVISIT by Sachiko - 08/08/2005
  // F is required by the current implementation of SPK, 
  // although it is not by NONMEM.
  if( (p = table->find( nonmem::F )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, 
				  "F was missing in the user's model!",
				  __LINE__,
				  __FILE__ );
    }
  
  // F should have appeared in the model definition.
  // If not, it's a user input error.
  if( (p = table->find( nonmem::Y )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, 
				  "Y was missing in the user's model!",
				  __LINE__,
				  __FILE__ );
    }


  // THETA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->find( nonmem::THETA )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "THETA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // OMEGA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->find( nonmem::OMEGA )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "OMEGA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // ETA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->find( nonmem::ETA )) == Symbol::empty() )
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "ETA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // Predefined words required only for POPULATION analysis.
  if( getTarget() == POP )
    {
      // SIGMA
      if( (p = table->find( nonmem::SIGMA )) == Symbol::empty() )
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				      "SIGMA should have been defined by now!",
				      __LINE__,
				      __FILE__ );
	}
      // EPS
      if( (p = table->find( nonmem::EPS )) == Symbol::empty() )
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				      "EPS should have been defined by now!",
				      __LINE__,
				      __FILE__ );
	}
    }

  //---------------------------------------------------------------------------------------
  //
  // Generate source code files
  //
  //---------------------------------------------------------------------------------------

  //*************************************
  // Generate the headers and definition 
  // files for IndData class and
  // DataSet class.
  //
  // The symbol table (ie. the order of 
  // data labels in the list) must not 
  // change between the following two
  // calls
  //*************************************
  generateDataSet();
  generateIndData();


  //*************************************
  // Generate model-related source.
  //*************************************
  if( myModelSpec == PRED )
    {
      generatePred( fPredEqn_cpp );
    }
  else if( myModelSpec == ADVAN6 )
    {
      generateOdePred( fPkEqn_cpp, fDiffEqn_cpp, fErrorEqn_cpp, myIsIdent );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Invalid model specification!." );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess,
			      __LINE__, __FILE__ );
      throw e;
    }
  
  //*************************************
  // Generate namespace files.
  //*************************************
  generateNonmemParsNamespace();
  if( myIsMonte )
    generateMonteParsNamespace();
  
  if( myIsIdent )
    {
// [Revisit - Identifiability Files Should not be Included in the Release - Mitch]
//      generateIdentDriver();
// [Revisit - Identifiability Files Should not be Included in the Release - Mitch]
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
        "The identifiability capabilities have been removed from SPK.",
        __LINE__,
        __FILE__ );
    }
  else if( myIsEstimate || myIsSimulate )
    {
      if( getTarget() == POP )
        generatePopDriver();
      else 
        generateIndDriver();
    }
  //*************************************
  // Generate Makefile.
  //*************************************
  generateMakefile();
}
