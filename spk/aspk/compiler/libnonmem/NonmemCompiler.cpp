#include <iostream>
#include <set>
#include <map>
#include <xercesc/dom/DOM.hpp>

#include "../libcommon/SpkCompilerUtil.h"
#include "../libnonmem/NonmemCompiler.h"
#include "../libnonmem/nmabb.tab.h"

using namespace std;
using namespace xercesc;

extern int                gSpkExpLines;
extern int                gSpkExpErrors;
extern ExpTreeGenerator * gSpkExpTreeGenerator;
extern DOMDocument      * gSpkExpTree;
extern SymbolTable      * gSpkExpSymbolTable;
extern FILE             * yyin;
extern int                yydebug;

static const char* trim( const XMLCh* source )
{
  XMLCh* target = XMLString::replicate( source );
  XMLString::trim( target );
  return C( target );
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
  gSpkExpTree          = gSpkExpTreeGenerator->root();
  gSpkExpSymbolTable   = new SymbolTable( client::NONMEM );
}

NonmemCompiler::~NonmemCompiler( )
{
  delete gSpkExpTreeGenerator;
  delete gSpkExpSymbolTable;
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
  interpretDriver();
  interpretModel();
  interpretData();
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
    sprintf( buf, "SpkInML version mismatch!  Only \"1.0\" supported! You gave me %s.\n", c_spkml_ver );
    exit( error( buf ) );
  }
  
  //
  // Verify client specification
  //
  const char * c_client = trim( content_node->getAttribute( X("client") ) );
  if( strcmp( c_client, "nonmem" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Anything besides \"nonmem\" does not make sense!  You gave me %s.\n", c_client );
    exit( error( buf ) );
  }

  //
  // analysis level
  //
  const char * c_analysis = trim( content_node->getAttribute( X("analysis") ) );
  if( strcmp( c_analysis, "population" ) )
  {
    char buf[128];
    sprintf( buf, "For NONMEM, only \"population\" or \"individual\" are supported (case sensitive)!  You gave me %s.\n",
	     c_analysis );
    exit( error( buf ) );
  }
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
  SymbolTable * table = getTable();

  assert( table->spkSymbols->nIndividuals > 0 );
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
      // Visit <pop_opt>.
      //
      if( strcmp( nodeName, "pop_opt" ) == 0 )
	{
	  // pop_size
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) ) > 0 );
	  nIndividuals = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("pop_size") ) ) );
	  assert( nIndividuals > 0 );
	  table->spkSymbols->nIndividuals = nIndividuals;

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
      // Visit <theta>.
      //
      else if( strcmp( nodeName, "theta" ) == 0 )
	{
	  // length
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) );
	  assert( len > 0 );
	  
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );

	  while( inNode != NULL )
	    {
	      // theta->in 
	      if( strcmp( C( inNode->getNodeName() ), "in" ) == 0 )
		{	  
		  thetaIn.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      const char * value 
			= trim( valueNode->getFirstChild()->getNodeValue() );
		      
		      thetaIn[cnt] = atof(value);
		      valueNode = walker->nextSibling();
		    }	  
		  assert( cnt == len );
		  walker->parentNode();// back to <in>
		}
	      // theta->low
	      else if( strcmp( C( inNode->getNodeName() ), "low" ) == 0 )
		{
		  thetaLow.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      const char * value 
			= trim( valueNode->getFirstChild()->getNodeValue() );
		      
		      thetaLow[cnt] = atof(value);
		      valueNode = walker->nextSibling();
		    }	
		  assert( cnt == len );
		  walker->parentNode();// back to <low>
		}
	      // theta->up
	      else if( strcmp( C( inNode->getNodeName() ), "up" ) == 0 )
		{
		  thetaUp.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      const char * value 
			= trim( valueNode->getFirstChild()->getNodeValue() );
		      
		      thetaUp[cnt] = atof(value);
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
      // Visit <omega>.
      //
      else if( strcmp( nodeName, "omega" ) == 0 )
	{
	  // span
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  // structure
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  omegaIn.resize( dimensions );

	  // omega->in
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
	    {
	      const char * value 
		= trim( valueNode->getFirstChild()->getNodeValue() );
	      
	      omegaIn[cnt] = atof(value);
	      valueNode = walker->nextSibling();
	    }	
	  assert( cnt == dimensions );
 	  walker->parentNode();// back to <in>
	  walker->parentNode();// back to <omega>
	}
      //
      // Visit <sigma>.
      //
      else if( strcmp( nodeName, "sigma" ) == 0 )
	{
	  // span
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  sigmaIn.resize( dimensions );

	  // sigma->in
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
	    {
	      const char * value 
		= trim( valueNode->getFirstChild()->getNodeValue() );
	      
	      sigmaIn[cnt] = atof(value);
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
      else if( strcmp( nodeName, "eta" ) == 0 )
	{
	  // length
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "length" ) ) ) );
	  assert( len > 0 );
	  
	  etaIn.resize( len );

	  // eta->in 
	  DOMNode * inNode = walker->firstChild();
	  assert( inNode != NULL );
	  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
	  
	  DOMNode * valueNode = walker->firstChild();
	  int cnt;
	  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
	    {
	      const char * value 
		= trim( valueNode->getFirstChild()->getNodeValue() );
	      
	      etaIn[cnt] = atof(value);
	      valueNode = walker->nextSibling();
	    }	  
	  assert( cnt == len );
 	  walker->parentNode();// back to <in>
	  walker->parentNode();// back to <eta>
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
  SymbolTable * table = getTable();

  DOMTreeWalker * walker = doc->createTreeWalker( modelTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  // 
  // Determine if a canned model is requested.
  //
  // If a canned model is requested, then <pk> and <error> are required.
  // Otherwise, <pred> is required.
  //
  const char* c_baseModel = C( dynamic_cast<DOMElement*>(modelTree)->getAttribute( X("base") ) );
  if( c_baseModel == NULL || XMLString::stringLen( c_baseModel ) == 0 )
    {
      isCannedModelUsed = false;
      DOMNode * pred = walker->firstChild();
      assert( XMLString::equals( pred->getNodeName(), X("pred") ) );
      setCannedModel( "none" );
    }
  else
    {
      isCannedModelUsed = true;

      bool isPkGiven    = false;
      bool isErrorGiven = false;
      DOMNode * model = walker->firstChild();

      while( model != NULL )
	{
	  const char * name = C( model->getNodeName() );
	  if( strcmp( name, "pk" ) == 0 )
	    {
	      isPkGiven = true;
	      FILE * fo = fopen( "junk", "w" );	      
	      fprintf( fo, "a = 1.0\n" );
	      fclose( fo );
	      yyin = fopen( "junk", "r" );
              yydebug = 1;
	      yyparse();
	      gSpkExpTreeGenerator->printToStdout();

	    }
	  else if( strcmp( name, "error" ) == 0 )
	    {
	      isErrorGiven = true;
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

const valarray<double> NonmemCompiler::getOmegaIn() const
{
  return omegaIn;
}
const valarray<double> NonmemCompiler::getSigmaIn() const
{
  return sigmaIn;
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
