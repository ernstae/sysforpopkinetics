#include <iostream>
#include <set>
#include <map>
#include <xercesc/dom/DOM.hpp>

#include "../libcommon/SpkCompilerUtil.h"
#include "../libnonmem/NonmemCompiler.h"
#include "../libnonmem/explang.tab.h"

using namespace std;
using namespace xercesc;


static const char* trim( const XMLCh* source )
{
  XMLCh* target = XMLString::replicate( source );
  XMLString::trim( target );
  return C( target );
}
static const char* tolower( char* buf, const char* mixed )
{
  int len = strlen( mixed );
  for( int i=0; i<len; i++ )
    buf[i] = tolower( mixed[i] );
  buf[len] = 0;
  return buf;
}
////////////////////////////////////////////////////////////////////////////////////
// REVISIT: 06/03/03
// This routine should be replaced by something more rigorous.
////////////////////////////////////////////////////////////////////////////////////
static int errors = 0;
static int error( const char * message )
{
  fprintf( stderr, "!!! ERROR !!! %s (%d: %s)\n", message, __LINE__, __FILE__);
  ++errors;
  return 1;
}
NonmemCompiler::NonmemCompiler( const char* filename )
  : SpkCompiler( client::NONMEM, filename ),
    isCannedModelUsed( false ),
    baseModel( NONE )
{
  gSpkExpTreeGenerator = new ExpTreeGenerator;
  gSpkExpSymbolTable   = getTable();
  gSpkExpTree = gSpkExpTreeGenerator->getRoot();
}

NonmemCompiler::~NonmemCompiler( )
{
  delete gSpkExpTreeGenerator;
}

const set<string> NonmemCompiler::emit()
{
  set<string> files;
  //emitModel();
  //emitDriver();
  return files;
}
void NonmemCompiler::interpret()
{
  interpretContent();

  // data set must be parsed first 
  // so that data item names are registered in the symbol table
  // before they're referenced in driver or model section.
  interpretData();

  interpretDriver();
  interpretModel();

  //interpretOutput();
}
void NonmemCompiler::interpretContent()
{
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  DOMDocument * doc = getDOMDoc();
  assert( doc->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node = dynamic_cast<DOMElement*>(doc->getElementsByTagName( X("content") )->item(0));
  assert( content_node != NULL );

  //
  // Verify SpkInML version specification
  //
  const char* c_spkml_ver = trim( content_node->getAttribute( X("spkinml_ver") ) );
  if( strcmp( c_spkml_ver, "1.0" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "SpkInML version mismatch!  Only \"1.0\" supported!  The version you gave me says %s.\n", c_spkml_ver );
    exit( error( buf ) );
  }
  
  //
  // Verify client specification
  //
  const char * c_client = trim( content_node->getAttribute( X("client") ) );
  if( strcmp( c_client, "nonmem" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Anything besides \"nonmem\" does not make sense in this context!  \
                  You gave me %s (case sensitive).\n", c_client );
    exit( error( buf ) );
  }

  //
  // analysis level
  //
  const char * c_analysis = trim( content_node->getAttribute( X("analysis") ) );
  if( strcmp( c_analysis, "population" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Currently only \"population\" is supported!  You gave me %s (case sensitive).\n",
	     c_analysis );
    exit( error( buf ) );
  }
}
void NonmemCompiler::interpretDriver()
{
  //
  // Get the root of "driver" subtree.  Since there's one and only one
  // <driver> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.
  //
  DOMDocument * doc = getDOMDoc();
  assert( doc->getElementsByTagName( X("driver") ) != NULL );
  DOMNode * driverTree = doc->getElementsByTagName( X("driver") )->item(0);
  assert( driverTree != NULL );

  //
  // Get a pointer to the symbol table.  In this <driver> section parsing, 
  // the components in FitParameters data structure held in the table
  // get filled with actual values.
  // The objectof the data structure in the table is called "spkSymbols".
  //
  SymbolTable * table = getTable();
  

  //
  // Declare an integer placeholder for #of individuals.
  // 
  int nIndividuals = 0;

  //
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMTreeWalker * walker = doc->createTreeWalker( driverTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  DOMNode *categoryNode = walker->firstChild();

  while( categoryNode != NULL )
    {
      const char * nodeName = C( categoryNode->getNodeName() );

      //
      // <theta length="xxx">
      //        <in fixed=(yes|no)>
      //            <value>xxx</value>
      //            <value>xxx</value>
      //            ...
      //        </in>
      //        <up>
      //            <value>xxx</value>
      //            <value>xxx</value>
      //            ...
      //        </up>
      //        <low>
      //            <value>xxx</value>
      //            <value>xxx</value>
      //            ...
      //        </low>
      // </theta>
      // 
      if( strcmp( nodeName, "theta" ) == 0 )
	{
	  //
	  // <theta length CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) );
	  assert( len > 0 );
	  
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );

	  while( inNode != NULL )
	    {
	      if( strcmp( C( inNode->getNodeName() ), "in" ) == 0 )
		{	  
		  //
		  // <in (value)+>
		  // <value fixed (yes|no) #FIXED "no">
		  //
		  thetaIn.resize( len );
		  thetaFixed.resize( len );

		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      bool isFixed 
			= strcmp( C( dynamic_cast<DOMElement*>
				     (valueNode)->getAttribute( X( "fixed" ) ) ), "yes" )==0? 
				  true : false;
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      thetaIn[cnt]    = value;
		      thetaFixed[cnt] = isFixed;
		      valueNode = walker->nextSibling();
		    }	  
		  assert( cnt == len );
		  walker->parentNode();// back to <in>
		}
	      //
	      // <low (value)+>
	      // <value>
	      //
	      else if( strcmp( C( inNode->getNodeName() ), "low" ) == 0 )
		{
		  thetaLow.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      thetaLow[cnt] = value;
		      valueNode = walker->nextSibling();
		    }	
		  assert( cnt == len );
		  walker->parentNode();// back to <low>
		}
	      //
	      // <up (value)+>
	      // <value>
	      //
	      else if( strcmp( C( inNode->getNodeName() ), "up" ) == 0 )
		{
		  thetaUp.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      thetaUp[cnt] = value;
		      valueNode = walker->nextSibling();
		    }	
		  assert( cnt == len );
		  walker->parentNode();// back to <up>
		}
	      else
		{
		  char buf[128];
		  sprintf( buf, "Unknown tag <%s>! Note that tag names are case sensitive.\n",
			   nodeName );
		  exit( error( buf ) );
		}
	      inNode = walker->nextSibling();
	    }
	  walker->parentNode(); // back to <theta>
	}
      //
      // <omega span="xxx" struct(diagonal|block)>
      //        <in fixed=(yes|no)>
      //            <value>xxx</value>
      //            <value>xxx</value>
      //            ...
      //        </in>
      else if( strcmp( nodeName, "omega" ) == 0 )
	{
	  //
	  // <omega span CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  //
	  // <omega struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  omegaIn.resize( dimensions );
          omegaFixed.resize( dimensions );

	  //
	  // <in (value)+>
	  // <value fixed (yes|no) #FIXED "no">
	  //
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
	    {
	      bool isFixed 
		= strcmp( C( dynamic_cast<DOMElement*>
			     (valueNode)->getAttribute( X( "fixed" ) ) ), "yes" )==0? 
		true : false;
	      double value 
		= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      omegaIn[cnt]    = value;
              omegaFixed[cnt] = isFixed;
	      valueNode = walker->nextSibling();
	    }	
	  assert( cnt == dimensions );
 	  walker->parentNode();// back to <in>
	  walker->parentNode();// back to <omega>
	}
      //
      // <sigma span="xxx" struct=(diagonal|block)>
      //    <in fixed=(yes|no)>
      //       <value>xxx</value>
      //       <value>xxx</value>
      //    </in>
      // </sigma>
      //
      else if( strcmp( nodeName, "sigma" ) == 0 )
	{
	  //
	  // <simga span CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  //
	  // <sigma struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  sigmaIn.resize( dimensions );
          sigmaFixed.resize( dimensions );

	  //
	  // <in (value)+>
	  // <value fixed (yes|no) #FIXED "no">
	  //
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
	    {
	      bool isFixed 
		= strcmp( C( dynamic_cast<DOMElement*>
			     (valueNode)->getAttribute( X( "fixed" ) ) ), "yes" )==0? 
		true : false;
	     double value 
	       = atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      sigmaIn[cnt]    = value;
              sigmaFixed[cnt] = isFixed;
	      valueNode = walker->nextSibling();
	    }	  
	  assert( cnt == dimensions );
 	  walker->parentNode();// back to <in>
	  walker->parentNode();// back to <sigma>
	}
      //
      // Visit <eta>.
      // Though the initial value for eta is always 0.0,
      // we expect that value to be given in the document just
      // to maintain simplicity in parsing.
      //
      // <eta length="xxx">
      //    <in fixed=(yes|no)>
      //       <value>xxx</value>
      //       <vlaue>xxx</value>
      //       ...
      //    </in>
      // </eta>
      //
      else if( strcmp( nodeName, "eta" ) == 0 )
	{
	  //
	  // <eta length CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) );
	  assert( len > 0 );
	  
	  etaIn.resize( len );
          etaFixed.resize( len );

	  //
	  // <in (value)+>
	  // <value fixed (yes|no) #FIXED "no">...</value>
	  //
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
	    {
	      bool isFixed 
		= strcmp( C( dynamic_cast<DOMElement*>
			     (valueNode)->getAttribute( X( "fixed" ) ) ), "yes" )==0? 
		true : false;
	      double value 
		= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      etaIn[cnt]    = value;
              etaFixed[cnt] = isFixed; 
	      valueNode = walker->nextSibling();
	    }	  
	  assert( cnt == len );
 	  walker->parentNode();// back to <in>
	  walker->parentNode();// back to <eta>
	}
      //
      // <pop_opt EMPTY>
      // <pop_opt approximation (fo|foce|laplace) #REQUIRED>
      // <pop_opt pop_size   CDATA    #REQUIRED>
      // <pop_opt epsilon    CDATA    #REQUIRED>
      // <pop_opt mitr       CDATA    #REQUIRED>
      // <pop_opt trace      CDATA    #REQUIRED>
      // <pop_opt restart    (yes|no) #REQUIRED>
      // <pop_opt par_out    (yes|no) #REQUIRED>
      // <pop_opt obj_out    (yes|no) #REQUIRED>
      // <pop_opt deriv1_out (yes|no) #REQUIRED>
      // <pop_opt deriv2_out (yes|no) #REQUIRED>
      //
      else if( strcmp( nodeName, "pop_opt" ) == 0 )
	{
	  // approximation (fo|foce|laplace)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("approximation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("approximation") ) ) > 0 );
	  const char *approx = C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("approximation") ) );
	  if( strcmp( approx, "fo" ) == 0 )
	    table->spkSymbols->objective = FO;
	  else if( strcmp( approx, "foce" ) == 0 )
	    table->spkSymbols->objective = FOCE;
	  else if( strcmp( approx, "laplace" ) == 0 )
	    table->spkSymbols->objective = LAPLACE;
	  else
	    {
	      char buf[128];
	      sprintf( buf, "Unknown objective (%s)\n", approx );
	      exit( error( buf ) );
	    }
	  
	  // pop_size
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) ) > 0 );
	  nIndividuals = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) ) );
	  assert( nIndividuals > 0 );
	  table->spkSymbols->nIndividuals = nIndividuals;

	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon
	    = atof( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  table->spkSymbols->popEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  table->spkSymbols->popMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  table->spkSymbols->popTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isPopObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopObjOut = isPopObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isPopObj_popParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopObj_popParOut = isPopObj_popParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isPopObj_popPar_popParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopObj_popPar_popParOut = isPopObj_popPar_popParOut;
	}
      //
      // Visit <ind_opt>.
      //
      else if( strcmp( nodeName, "ind_opt" ) == 0 )
	{
	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon 
	    = atof( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  table->spkSymbols->indEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  table->spkSymbols->indMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  table->spkSymbols->indTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isIndObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndObjOut = isIndObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isIndObj_indParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndObj_indParOut = isIndObj_indParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isIndObj_indPar_indParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndObj_indPar_indParOut = isIndObj_indPar_indParOut;
	}
      //
      // Visit <pop_stat>.
      //
      else if( strcmp( nodeName, "pop_stat" ) == 0 )
	{
	  // stderror (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) ) > 0 );
	  bool isStderrorOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  table->spkSymbols->isPopConfidenceOut = isConfidenceOut;
	}
      //
      // Visit <ind_stat>.
      //
      else if( strcmp( nodeName, "ind_stat" ) == 0 )
	{
	  // stderror (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) ) > 0 );
	  bool isStderrorOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("stderror") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  table->spkSymbols->isIndConfidenceOut = isConfidenceOut;
	}

      //
      // Unknown elements.
      //
      else
	{
	  char buf[128];
	  sprintf( buf, "Unknown tag <%s>! Note that tag names are case sensitive.\n",
		   nodeName );
	  exit( error( buf ) );
	}
      categoryNode = walker->nextSibling();
    }
    return;
}
void NonmemCompiler::interpretData()
{
  //
  // Get a pointer to the root of "data" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  DOMDocument * doc = getDOMDoc();
  assert( doc->getElementsByTagName( X("data") ) != NULL );
  DOMNode * dataTree = doc->getElementsByTagName( X("data") )->item(0);
  assert( dataTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  SymbolTable *table = getTable();
  assert( table != NULL );

  Symbol dose( "dose", Symbol::VECTOR, Symbol::DOUBLE, false );
  table->insert( dose );

  Symbol wt( "wt", Symbol::VECTOR, Symbol::DOUBLE, false );
  table->insert( wt );

  Symbol w( "w", Symbol::VECTOR, Symbol::DOUBLE, false );
  table->insert( w );

  Symbol time( "time", Symbol::VECTOR, Symbol::DOUBLE, false );
  table->insert( time );

  Symbol ds( "ds", Symbol::VECTOR, Symbol::DOUBLE, false );
  table->insert( ds );

  assert( gSpkExpSymbolTable->spkSymbols->nIndividuals > 0 );
}
void NonmemCompiler::interpretModel()
{
  //
  // Get a pointer to the root of "model" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  DOMDocument * doc = getDOMDoc();
  assert( doc->getElementsByTagName( X("model") ) != NULL );
  DOMNode * modelTree = doc->getElementsByTagName( X("model") )->item(0);
  assert( modelTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  assert( gSpkExpSymbolTable == getTable() );
  assert( gSpkExpSymbolTable != NULL );

  DOMTreeWalker * walker = doc->createTreeWalker( modelTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  
  // 
  // Determine if a canned model is requested.
  //
  const char* c_baseModel = C( dynamic_cast<DOMElement*>(modelTree)->getAttribute( X("base") ) );
  if( c_baseModel == NULL || XMLString::stringLen( c_baseModel ) == 0 )
    {
      //
      // If no canned model is used, <pred> has to follow.
      //
      isCannedModelUsed = false;

      DOMNode * pred = walker->firstChild();
      assert( XMLString::equals( pred->getNodeName(), X("pred") ) );
      setCannedModel( "none" );
    }
  else
    {
      //
      // When a canned model is used, either (comp_model, diffeqn) or (comp_model?, (pk, error), diffeqn?)
      // combination must follow.
      //
      isCannedModelUsed = true;

      bool isPkGiven        = false;
      bool isErrorGiven     = false;
      bool isCompModelGiven = false;
      bool isDiffEqnGiven   = false;
      DOMNode * model = walker->firstChild();

      while( model != NULL )
	{
	  const char * name = C( model->getNodeName() );
	  if( strcmp( name, "pk" ) == 0 )
	    {
	      isPkGiven = true;
              
	      FILE * fo = fopen( "pk", "w" );	      
	      const char * mixed = trim( model->getFirstChild()->getNodeValue() );
              const int  len = strlen( mixed );
	      char buf[ strlen( mixed ) + 1 ];

	      fprintf( fo, "%s\n", tolower( buf, mixed ) );
	      fclose( fo );
	      yyin = fopen( "pk", "r" );
              yydebug = 0;
	      yyparse();
	      //	      gSpkExpTreeGenerator->printToStdout();
	    }
	  else if( strcmp( name, "error" ) == 0 )
	    {
	      isErrorGiven = true;
	    }
	  else if( strcmp( name, "comp_model" ) == 0 )
	    {
	      isCompModelGiven = true;
	    }
	  else if( strcmp( name, "diffeqn" ) == 0 )
	    {
	      isDiffEqnGiven = true;
	    }
	  else
	    {
	      char buf[128];
	      sprintf( buf, "Unknown model <%s>.\n", name );
	      exit( error( buf ) );
	    }
	  model = walker->nextSibling();
	}
      assert( isPkGiven && isErrorGiven );

      setCannedModel( c_baseModel );
    }
}
const set<string> NonmemCompiler::emitModel()
{
}
const set<string> NonmemCompiler::emitDriver()
{
}
const set<string> NonmemCompiler::getFileNames() const
{
  return fileNames;
}
const string NonmemCompiler::getDriverFileName() const
{
  return driverFileName;
}
const valarray<bool> NonmemCompiler::getThetaFixed() const
{
  return thetaFixed;
}
const valarray<double> NonmemCompiler::getThetaIn() const
{
  return thetaIn;
}
const valarray<double> NonmemCompiler::getThetaLow() const
{
  return thetaLow;
}
const valarray<double> NonmemCompiler::getThetaUp() const
{
  return thetaUp;
}

const valarray<bool> NonmemCompiler::getOmegaFixed() const
{
  return omegaFixed;
}
const valarray<double> NonmemCompiler::getOmegaIn() const
{
  return omegaIn;
}
const valarray<bool> NonmemCompiler::getSigmaFixed() const
{
  return sigmaFixed;
}
const valarray<double> NonmemCompiler::getSigmaIn() const
{
  return sigmaIn;
}
const valarray<bool> NonmemCompiler::getEtaFixed() const
{
  return etaFixed;
}
const valarray<double> NonmemCompiler::getEtaIn() const
{
  return etaIn;
}
enum NonmemCompiler::BaseModel NonmemCompiler::setCannedModel( const char* c_model )
{
  if( strcmp( c_model, "none" ) == 0 || c_model == NULL )
    baseModel = NONE;
  else if( strcmp( c_model, "advan1" ) == 0 )
    baseModel = ADVAN1;
  else if( strcmp( c_model, "advan2" ) == 0 )
    baseModel = ADVAN2;
  else if( strcmp( c_model, "advan3" ) == 0 )
    baseModel = ADVAN3;
  else if( strcmp( c_model, "advan4" ) == 0 )
    baseModel = ADVAN4;
  else if( strcmp( c_model, "advan5" ) == 0 )
    baseModel = ADVAN5;
  else if( strcmp( c_model, "advan6" ) == 0 )
    baseModel = ADVAN6;
  else if( strcmp( c_model, "advan7" ) == 0 )
    baseModel = ADVAN7;
  else if( strcmp( c_model, "advan8" ) == 0 )
    baseModel = ADVAN8;
  else if( strcmp( c_model, "advan9" ) == 0 )
    baseModel = ADVAN9;
  else if( strcmp( c_model, "advan10" ) == 0 )
    baseModel = ADVAN10;
  else if( strcmp( c_model, "advan11" ) == 0 )
    baseModel = ADVAN11;
  else if( strcmp( c_model, "advan12" ) == 0 )
    baseModel = ADVAN12;
  else
    {
      char mess[128];
      sprintf( "Unknown canned model <%s>!\n", c_model );
      exit( error( mess ) );
    }
  return baseModel;
}
const char* NonmemCompiler::whichCannedModel() const
{
  if( !isCannedModelUsed || baseModel == NONE )
    return NULL;
  else if( baseModel == ADVAN1 )
    return "advan1";
  else if( baseModel == ADVAN2 )
    return "advan2";
  else if( baseModel == ADVAN3 )
    return "advan3";
  else if( baseModel == ADVAN4 )
    return "advan4";
  else if( baseModel == ADVAN5 )
    return "advan5";
  else if( baseModel == ADVAN6 )
    return "advan6";
  else if( baseModel == ADVAN7 )
    return "advan7";
  else if( baseModel == ADVAN8 )
    return "advan8";
  else if( baseModel == ADVAN9 )
    return "advan9";
  else if( baseModel == ADVAN10 )
    return "advan10";
  else if( baseModel == ADVAN11 )
    return "advan11";
  else if( baseModel == ADVAN12 )
    return "advan12";
  else
    return NULL;
}
//====================================================================
//
// Protected members
//
//====================================================================
NonmemCompiler::NonmemCompiler()
  : SpkCompiler()
{
}
NonmemCompiler::NonmemCompiler( const NonmemCompiler & right )
  : SpkCompiler( right )
{
}
NonmemCompiler& NonmemCompiler::operator=( const NonmemCompiler & )
{
}
