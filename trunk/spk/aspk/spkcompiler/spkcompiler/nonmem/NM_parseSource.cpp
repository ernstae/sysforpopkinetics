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
  DOMNodeList * nonmems  = spksouce->getElementsByTagName( X_NONMEM );
  if( !nonmems->getLength() > 0 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", C_NONMEM );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  //---------------------------------------------------------------------------------------
  // <constraint>
  //---------------------------------------------------------------------------------------
  DOMNodeList * constraints = nonmem->getElementsByTagName( X_CONSTRAINT );
  if( constraints->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen()];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", C_CONSTRAINT );
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
		C_POP_ANALYSIS, 
		C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * analysis;
  bool isAnalysisDone = false;
  if( getTarget() == POP )
    {
      DOMNodeList * pop_analysises = constraint->getElementsByTagName( X_POP_ANALYSIS );
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      parsePopAnalysis( analysis );
    }
  else //if( getTarget() == IND )
    {
      DOMNodeList * ind_analysises = constraint->getElementsByTagName( X_IND_ANALYSIS );
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

  DOMNodeList * models = nonmem->getElementsByTagName( X_MODEL );
  if( models->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element.", C_MODEL );
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
      XMLString::textToBin( model->getAttribute( X_ADVAN ), advan );
      assert( advan > 0 );

      assert( myTrans == TRANS1 );  // default TRANS value
      if( model->hasAttribute( X_TRANS ) )
	{
	  XMLString::textToBin( model->getAttribute( X_TRANS ), trans );
	  myTrans = static_cast<TRANS>( trans );
	}
      parseAdvan( myModelSpec, myTrans, model );
    }
  else
    {
      DOMNodeList * preds   = model->getElementsByTagName( X_PRED );
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
  DOMNodeList * monte_carlos = nonmem->getElementsByTagName( X_MONTE_CARLO );
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
		C_PRED, C_PRESENTATION);
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( !isAnalysisDone )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"<%s> must be parsed before parsing <%s>", 
		(getTarget()==POP? C_POP_ANALYSIS : C_IND_ANALYSIS ), 
		C_PRESENTATION );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( X_PRESENTATION );
  if( presentations->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Multiple <%s> elements are found.", C_PRESENTATION ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  myRecordNums.resize( getPopSize() );
  Symbol * id = table->findi( KeyStr.ID );
  if( id == NULL || id == Symbol::empty() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"\"%s\" data item seems missing from the data set.", 
	       DefaultStr.ID.c_str() ); 
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
  if( ( p = table->findi( KeyStr.ORGDV )) == Symbol::empty() )
    {  
      table->insertScalar( DefaultStr.ORGDV, Symbol::SYSTEM, Symbol::READONLY );
      assert( table->findi( KeyStr.ORGDV ) != Symbol::empty() );
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

  // MDV
  if( (p = table->findi( KeyStr.MDV ))  != Symbol::empty() )
    myIsMissingMdv = false;

  // EVID
  if( (p = table->findi( KeyStr.EVID )) != Symbol::empty() )
    myIsMissingEvid = false;

  // CMT
  if( (p = table->findi( KeyStr.CMT ))  != Symbol::empty() )
    myIsMissingCmt = false;

  // PCMT
  if( (p = table->findi( KeyStr.PCMT )) != Symbol::empty() )
    myIsMissingPcmt = false;

  // RATE
  if( (p = table->findi( KeyStr.RATE )) != Symbol::empty() )
    myIsMissingRate = false;

  // PRED
  if( (p = table->findi( KeyStr.PRED )) != Symbol::empty() )
    UserStr.PRED = p->name;
  else
    {
      table->insertScalar( DefaultStr.PRED, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.PRED = DefaultStr.PRED;
    }

  // RES
  if( (p = table->findi( KeyStr.RES )) != Symbol::empty() )
    UserStr.RES = p->name;
  else
    {
      table->insertScalar( DefaultStr.RES, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.RES = DefaultStr.RES;
    }

  // WRES
  if( (p = table->findi( KeyStr.WRES )) != Symbol::empty() )
    UserStr.WRES = p->name;
  else
    {
      table->insertScalar( DefaultStr.WRES, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.WRES = DefaultStr.WRES;
    }

  // IPRED
  if( (p = table->findi( KeyStr.IPRED )) != Symbol::empty() )
    UserStr.IPRED = p->name;
  else
    {
      table->insertScalar( DefaultStr.IPRED, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.IPRED = DefaultStr.IPRED;
    }

  // IRES
  if( (p = table->findi( KeyStr.IRES )) != Symbol::empty() )
    UserStr.IRES = p->name;
  else
    {
      table->insertScalar( DefaultStr.IRES, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.IRES = DefaultStr.IRES;
    }

  // IWRES
  if( (p = table->findi( KeyStr.IWRES )) != Symbol::empty() )
    UserStr.IWRES = p->name;
  else
    {
      table->insertScalar( DefaultStr.IWRES, Symbol::SYSTEM, Symbol::READONLY );
      UserStr.IWRES = DefaultStr.IWRES;
    }

  if( getTarget() == POP )
    {
      // ETARES
      if( (p = table->findi( KeyStr.ETARES )) != Symbol::empty() )
	UserStr.ETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.ETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.ETARES = DefaultStr.ETARES;
	}

      // WETARES
      if( (p = table->findi( KeyStr.WETARES )) != Symbol::empty() )
	UserStr.WETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.WETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.WETARES = DefaultStr.WETARES;
	}

      // IETARES
      if( (p = table->findi( KeyStr.IETARES )) != Symbol::empty() )
	UserStr.IETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.IETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.IETARES = DefaultStr.IETARES;
	}

      // IWETARES
      if( (p = table->findi( KeyStr.IWETARES )) != Symbol::empty() )
	UserStr.IWETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.IWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.IWETARES = DefaultStr.IWETARES;
	}
      // PPRED
      if( (p = table->findi( KeyStr.PPRED )) != Symbol::empty() )
	UserStr.PPRED = p->name;
      else
	{
	  table->insertScalar( DefaultStr.PPRED, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.PPRED = DefaultStr.PPRED;
	}
      
      // PRES
      if( (p = table->findi( KeyStr.PRES )) != Symbol::empty() )
	UserStr.PRES = p->name;
      else
	{
	  table->insertScalar( DefaultStr.PRES, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.PRES = DefaultStr.PRES;
	}
      
      // PWRES
      if( (p = table->findi( KeyStr.PWRES )) != Symbol::empty() )
	UserStr.PWRES = p->name;
      else
	{
	  table->insertScalar( DefaultStr.PWRES, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.PWRES = DefaultStr.PWRES;
	}

      // PETARES
      if( (p = table->findi( KeyStr.PETARES )) != Symbol::empty() )
	UserStr.PETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.PETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.PETARES = DefaultStr.PETARES;
	}

      // PWETARES
      if( (p = table->findi( KeyStr.PWETARES )) != Symbol::empty() )
	UserStr.PWETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.PWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.PWETARES = DefaultStr.PWETARES;
	}

      // CPRED
      if( (p = table->findi( KeyStr.CPRED )) != Symbol::empty() )
	UserStr.CPRED = p->name;
      else
	{
	  table->insertScalar( DefaultStr.CPRED, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.CPRED = DefaultStr.CPRED;
	}
      
      // CRES
      if( (p = table->findi( KeyStr.CRES )) != Symbol::empty() )
	UserStr.CRES = p->name;
      else
	{
	  table->insertScalar( DefaultStr.CRES, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.CRES = DefaultStr.CRES;
	}
      
      // CWRES
      if( (p = table->findi( KeyStr.CWRES )) != Symbol::empty() )
	UserStr.CWRES = p->name;
      else
	{
	  table->insertScalar( DefaultStr.CWRES, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.CWRES = DefaultStr.CWRES;
	}

      // CETARES
      if( (p = table->findi( KeyStr.CETARES )) != Symbol::empty() )
	UserStr.CETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.CETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.CETARES = DefaultStr.CETARES;
	}

      // CWETARES
      if( (p = table->findi( KeyStr.CWETARES )) != Symbol::empty() )
	UserStr.CWETARES = p->name;
      else
	{
	  table->insertVector( DefaultStr.CWETARES, myEtaLen, Symbol::SYSTEM, Symbol::READONLY );
	  UserStr.CWETARES = DefaultStr.CWETARES;
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
  if( (p = table->findi( KeyStr.ID )) != Symbol::empty() )
    UserStr.ID = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "ID should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  /*
  // MDV is required in a data set.
  // The field should have been inserted even if 
  // the original data set lacked it.

  if( (p = table->findi( KeyStr.MDV )) != Symbol::empty() )
    UserStr.MDV = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "MDV should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // EVID is required in a data set.
  // The field should have been inserted even if 
  // the original data set lacked it.

  if( (p = table->findi( KeyStr.EVID )) != Symbol::empty() )
    UserStr.EVID = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "EVID should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }
  */

  // DV is always required in a data set and
  // should have appeared in the model definition as well.
  if( (p = table->findi( KeyStr.DV )) != Symbol::empty() )
    UserStr.DV = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "DV should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // REVISIT by Sachiko - 08/08/2005
  // F is required by the current implementation of SPK, 
  // although it is not by NONMEM.
  if( (p = table->findi( KeyStr.F )) != Symbol::empty() )
    {
      UserStr.F = p->name;
    }
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, 
				  "F was missing in the user's model!",
				  __LINE__,
				  __FILE__ );
    }
  
  // F should have appeared in the model definition.
  // If not, it's a user input error.
  if( (p = table->findi( KeyStr.Y )) != Symbol::empty() )
    {
       UserStr.Y = p->name;
    }
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, 
				  "Y was missing in the user's model!",
				  __LINE__,
				  __FILE__ );
    }


  // THETA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->findi( KeyStr.THETA )) != Symbol::empty() )
    UserStr.THETA = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "THETA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // OMEGA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->findi( KeyStr.OMEGA )) != Symbol::empty() )
    UserStr.OMEGA = p->name;
  else
    {
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "OMEGA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // ETA is always required.
  // It should have been registered while parsing <pop_analysis> or <ind_analysis>.
  if( (p = table->findi( KeyStr.ETA )) != Symbol::empty() )
    UserStr.ETA = p->name;
  else
    {
      UserStr.ETA = DefaultStr.ETA;
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				  "ETA should have been defined by now!",
				  __LINE__,
				  __FILE__ );
    }

  // Predefined words required only for POPULATION analysis.
  if( getTarget() == POP )
    {
      // SIGMA
      if( (p = table->findi( KeyStr.SIGMA )) != Symbol::empty() )
	UserStr.SIGMA = p->name;
      else
	{
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, 
				      "SIGMA should have been defined by now!",
				      __LINE__,
				      __FILE__ );
	}
      // EPS
      if( (p = table->findi( KeyStr.EPS )) != Symbol::empty() )
	UserStr.EPS = p->name;
      else
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
      generateOdePred( fPkEqn_cpp, fDiffEqn_cpp, fErrorEqn_cpp );
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
  
  if( myIsEstimate || myIsSimulate )
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
