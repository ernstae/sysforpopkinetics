#include "NonmemSpkMLToCpp.h"

#include <xercesc/dom/DOM.hpp>

#include <iostream>

using namespace xercesc;
using namespace std;

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

NonmemSpkMLToCpp::NonmemSpkMLToCpp( DOMDocument* treeIn )
  : tree( treeIn ), canned_model( NONE ), isCannedModelUsed( false )
{
  gSpkExpSymbolTable = new SymbolTable;
  gSpkExpTree        = expTreeUtils.createTree( "unit" );

  assert( tree->getElementsByTagName( X("model" ) ) != NULL );
  DOMElement * modelTag 
    = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("model" ) )->item(0));
  assert( modelTag != NULL );
  const char * advan = C( modelTag->getAttribute( X("base") ) );
  if( advan != NULL )
    {
      if( strcmp( advan, "advan2" )==0 )
	{
	  //symbol_checker = NULL;//new NonmemAdvan2Translator;
	}
      else
	{
	  char buf[126];
	  sprintf( buf, "\"%s\" is not supported! (%d, %s)\n", advan, __LINE__, __FILE__ );
	  fprintf( stderr, buf );
	  exit(-1);
	}
      canned_model = ADVAN2;
      isCannedModelUsed = true;
    }
  else
    {
      canned_model = NONE;
      //symbol_checker = NULL; //new NonmemPredTranslator;
    }
}
NonmemSpkMLToCpp::~NonmemSpkMLToCpp( )
{
  //delete symbol_checker;
  delete gSpkExpSymbolTable;
}

void NonmemSpkMLToCpp::assemble( DOMDocument* tree )
{
  interpretContent();

  // <driver> must be parsed before <data> so that
  // the size of population is discovered first.
  interpretDriver();

  // <data> must be parsed before <model> so that
  // symbols are registered into the symbol table.
  interpretData();
  interpretModel();

  //interpretOutput();

}
void NonmemSpkMLToCpp::emit( DOMDocument* tree )
{
  //emitModel();
  //emitDriver();
}
void NonmemSpkMLToCpp::initSymbolTable( SymbolTable& )
{
}
void NonmemSpkMLToCpp::interpretContent()
{
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  assert( tree != NULL );

  //////////////////////////////////////////////////////////////////////////
  // START FROM HERE!!! 7/11/03 (FRI)
  //
  // Run in debugger.  Segmentation error happens at the immediately
  // following line.  tree is surely not NULL.  A same kind of operation
  // done in the constructor goes successfully.
  //////////////////////////////////////////////////////////////////////////
  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
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
void NonmemSpkMLToCpp::interpretDriver()
{
  //
  // Get the root of "driver" subtree.  Since there's one and only one
  // <driver> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.
  //
  assert( tree->getElementsByTagName( X("driver") ) != NULL );
  DOMNode * driverTree = tree->getElementsByTagName( X("driver") )->item(0);
  assert( driverTree != NULL );

  //
  // Get a pointer to the symbol table.  In this <driver> section parsing, 
  // the components in FitParameters data structure held in the table
  // get filled with actual values.
  // The objectof the data structure in the table is called "spkSymbols".
  //
  SymbolTable * table = gSpkExpSymbolTable;
  

  //
  // Declare an integer placeholder for #of individuals.
  // 
  int nIndividuals = 0;

  //
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker 
    = doc->createTreeWalker( driverTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
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
	  
	  Symbol theta( "theta", Symbol::VECTOR, Symbol::DOUBLE, true );
	  theta.size( len );
	  table->insert( theta );

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
		  nonmem.thetaIn.resize( len );
		  nonmem.thetaFixed.resize( len );

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
		      
		      nonmem.thetaIn[cnt]    = value;
		      nonmem.thetaFixed[cnt] = isFixed;
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
		  nonmem.thetaLow.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmem.thetaLow[cnt] = value;
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
		  nonmem.thetaUp.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmem.thetaUp[cnt] = value;
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

	  Symbol omega( "omega", Symbol::MATRIX, Symbol::DOUBLE, true );
	  omega.dim( span, span );
	  table->insert( omega );
	  //
	  // <omega struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmem.omegaIn.resize( dimensions );
          nonmem.omegaFixed.resize( dimensions );

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
	      
	      nonmem.omegaIn[cnt]    = value;
              nonmem.omegaFixed[cnt] = isFixed;
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

	  Symbol sigma( "sigma", Symbol::MATRIX, Symbol::DOUBLE, true );
	  sigma.dim( span, span );
	  table->insert( sigma );

	  //
	  // <sigma struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmem.sigmaIn.resize( dimensions );
          nonmem.sigmaFixed.resize( dimensions );

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
	      
	      nonmem.sigmaIn[cnt]    = value;
              nonmem.sigmaFixed[cnt] = isFixed;
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
	  
	  Symbol eta( "eta", Symbol::VECTOR, Symbol::DOUBLE, true );
	  eta.size( len );

	  nonmem.etaIn.resize( len );
          nonmem.etaFixed.resize( len );

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
	      
	      nonmem.etaIn[cnt]    = value;
              nonmem.etaFixed[cnt] = isFixed; 
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
	    spk.objective = FO;
	  else if( strcmp( approx, "foce" ) == 0 )
	    spk.objective = FOCE;
	  else if( strcmp( approx, "laplace" ) == 0 )
	    spk.objective = LAPLACE;
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
	  spk.nIndividuals = nIndividuals;

	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon
	    = atof( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  spk.popEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spk.popMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spk.popTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spk.isPopWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spk.isPopParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isPopObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spk.isPopObjOut = isPopObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isPopObj_popParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spk.isPopObj_popParOut = isPopObj_popParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isPopObj_popPar_popParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spk.isPopObj_popPar_popParOut = isPopObj_popPar_popParOut;
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
	  spk.indEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spk.indMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spk.indTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spk.isIndWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spk.isIndParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isIndObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spk.isIndObjOut = isIndObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isIndObj_indParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spk.isIndObj_indParOut = isIndObj_indParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isIndObj_indPar_indParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spk.isIndObj_indPar_indParOut = isIndObj_indPar_indParOut;
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
	  spk.isPopStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spk.isPopCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spk.isPopCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spk.isPopCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spk.isPopConfidenceOut = isConfidenceOut;
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
	  spk.isIndStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spk.isIndCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spk.isIndCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spk.isIndCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spk.isIndConfidenceOut = isConfidenceOut;
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

void NonmemSpkMLToCpp::interpretData()
{
  //
  // Get a pointer to the root of "data" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMNode * dataTree = tree->getElementsByTagName( X("data") )->item(0);
  assert( dataTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  SymbolTable *table = gSpkExpSymbolTable;
  assert( table != NULL );

  //
  // Discover the number of data columns per individual set.
  // The number must be the same for all individuals.
  //
  int ind_cnt=0;
  int num_columns_previous = 0;
  int num_columns_current  = 0;

  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker = doc->createTreeWalker( dataTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  DOMElement * individual = dynamic_cast< DOMElement* >(walker->firstChild());
  while( individual != NULL )
    {
      ++ind_cnt;
      num_columns_current = 0;

      DOMElement * item = dynamic_cast< DOMElement* >( walker->firstChild() );
      while( item != NULL )
	{
	  ++num_columns_current;
	  item = dynamic_cast< DOMElement* >( walker->nextSibling() );
	}
      if( ind_cnt > 1 )
	{
	  assert( num_columns_current == num_columns_previous );
	}
      num_columns_previous = num_columns_current;
      walker->parentNode();
      individual = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }
  walker->parentNode();
  assert( ind_cnt == spk.nIndividuals );

  individual = dynamic_cast< DOMElement* >(walker->firstChild());
  ind_cnt = 0;// reset the counter!
  int num_columns = num_columns_previous;
  //
  // <data (individual)+>
  // <individual length CDATA #REQUIRED>
  //
  // The number of <individual>s must agree with the number registered in 
  // the value stored in spk.nIndividuals, which has been discovered during
  // parsing of <driver>.
  //
  nonmem.data.resize( spk.nIndividuals );

  while( individual != NULL )
    {
      //
      // <individual (item)+>
      // <item (value)+>
      //
      // <individual order CDATA #IMPLIED>
      // <individual id CDATA #REQUIRED>
      // <individual length CDATA #REQUIRED>
      // <item label (id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3|drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt|call|cnt|CDATA) #REQUIRED>
      // <item synonym (id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3|drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt|call|cnt|CDATA) #IMPLIED>
      //
      assert( XMLString::equals( individual->getNodeName(), X("individual") ) );
      ++ind_cnt;

      int length = atoi( trim( individual->getAttribute( X("length") ) ) );

      int order;
      const XMLCh* xml_order = individual->getAttribute( X("order") );
      if( xml_order == NULL )
	order = ind_cnt - 1;
      else 
	order = atoi( C( xml_order ) ) - 1;

      const char* id = C( individual->getAttribute( X("id") ) );

      //      cout << id << "(" << order << ")" << endl;
      nonmem.data[order].records.resize( num_columns );
      nonmem.data[order].owner =  const_cast<char*>( id );

      DOMElement * item = dynamic_cast< DOMElement* >( walker->firstChild() );
      int item_cnt=0;
      while( item != NULL )
	{
	  ++item_cnt;
	  const XMLCh* xml_label = item->getAttribute( X("label") );
	  assert( xml_label != NULL );
	  const char* label = C( xml_label );

	  const char * synonym = C( item->getAttribute( X("synonym") ) );

	  nonmem.data[order].records[item_cnt-1].values.resize( length );
	  nonmem.data[order].records[item_cnt-1].label = const_cast<char*>(label);
          nonmem.data[order].records[item_cnt-1].synonym = const_cast<char*>(synonym);

	  bool isNonmemKeyword = true;
	  /*
	  if( strcmp( label, "id" ) == 0 |
	      strcmp( label, "l1" ) == 0 |
	      strcmp( label, "l2" ) == 0 |
	      strcmp( label, "dv" ) == 0 |
	      strcmp( label, "mdv" ) == 0 |
	      strcmp( label, "time" ) == 0 |
	      strcmp( label, "data" ) == 0 |
	      strcmp( label, "dat1" ) == 0 |
	      strcmp( label, "dat2" ) == 0 |
	      strcmp( label, "dat3" ) == 0 |
	      strcmp( label, "drop" ) == 0 |
	      strcmp( label, "skip" ) == 0 |
	      strcmp( label, "evid" ) == 0 |
	      strcmp( label, "amt" ) == 0 |
	      strcmp( label, "rate" ) == 0 |
	      strcmp( label, "ss" ) == 0 |
	      strcmp( label, "ii" ) == 0 |
	      strcmp( label, "add1" ) == 0 |
	      strcmp( label, "cmt" ) == 0 |
	      strcmp( label, "pcmt" ) == 0 |
	      strcmp( label, "call" ) == 0 |
	      strcmp( label, "cont" ) == 0 )
	    isNonmemKeyword = true;
	  */
	  Symbol lab( label, Symbol::VECTOR, Symbol::DOUBLE, isNonmemKeyword ); 
	  lab.size( length );
	  table->insert( lab );
	  /*
	  isNonmemKeyword = false;
	  if( strcmp( synonym, "id" ) == 0 |
	      strcmp( synonym, "l1" ) == 0 |
	      strcmp( synonym, "l2" ) == 0 |
	      strcmp( synonym, "dv" ) == 0 |
	      strcmp( synonym, "mdv" ) == 0 |
	      strcmp( synonym, "time" ) == 0 |
	      strcmp( synonym, "data" ) == 0 |
	      strcmp( synonym, "dat1" ) == 0 |
	      strcmp( synonym, "dat2" ) == 0 |
	      strcmp( synonym, "dat3" ) == 0 |
	      strcmp( synonym, "drop" ) == 0 |
	      strcmp( synonym, "skip" ) == 0 |
	      strcmp( synonym, "evid" ) == 0 |
	      strcmp( synonym, "amt" ) == 0 |
	      strcmp( synonym, "rate" ) == 0 |
	      strcmp( synonym, "ss" ) == 0 |
	      strcmp( synonym, "ii" ) == 0 |
	      strcmp( synonym, "add1" ) == 0 |
	      strcmp( synonym, "cmt" ) == 0 |
	      strcmp( synonym, "pcmt" ) == 0 |
	      strcmp( synonym, "call" ) == 0 |
	      strcmp( synonym, "cont" ) == 0 )
	    isNonmemKeyword = true;
	  */
	  Symbol syn( synonym, Symbol::VECTOR, Symbol::DOUBLE, isNonmemKeyword ); 
	  syn.size( length );
	  table->insert( syn );

	  /*
	  cout << data[order].columns[item_cnt-1].label;
	  if( synonym != NULL ) cout << " = " << data[order].columns[item_cnt-1].synonym;
          cout << endl;
	  */

	  DOMElement * valueNode = dynamic_cast< DOMElement* >( walker->firstChild() );
	  int value_cnt=0;
	  while( valueNode != NULL )
	    {
	      ++value_cnt;
	      //
	      // <value> may be empty.
	      //
	      DOMNode * actual = valueNode->getFirstChild();
	      double value = atof( actual != NULL ? C( actual->getNodeValue() ) : "0.0" );

	      nonmem.data[order].records[item_cnt-1].values[value_cnt-1] = value;
	      /*
      	      cout << "value = " << data[order].columns[item_cnt-1].values[value_cnt-1] << endl;
	      */	      
	      valueNode = dynamic_cast< DOMElement* >( walker->nextSibling() );
	    }
	  walker->parentNode();
	  item = dynamic_cast< DOMElement* >( walker->nextSibling() );
	}
      walker->parentNode(); 
      individual = dynamic_cast< DOMElement* >( walker->nextSibling() );
    }
}
void NonmemSpkMLToCpp::interpretModel()
{
  //
  // Get a pointer to the root of "model" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  assert( tree->getElementsByTagName( X("model") ) != NULL );
  DOMNode * modelTree = tree->getElementsByTagName( X("model") )->item(0);
  assert( modelTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  SymbolTable * table = gSpkExpSymbolTable;
  assert( table != NULL );

  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker = doc->createTreeWalker( modelTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  
  // 
  // Determine if a canned model is requested.
  //
  const char* c_baseModel = C( dynamic_cast<DOMElement*>(modelTree)->getAttribute( X("base") ) );
  if( !isCannedModelUsed )
    {
      //
      // If no canned model is used, <pred> has to follow.
      //
      DOMNode * pred = walker->firstChild();
      assert( XMLString::equals( pred->getNodeName(), X("pred") ) );
    }
  else
    {
      //
      // When a canned model is used, either (comp_model, diffeqn) or (comp_model?, (pk, error), diffeqn?)
      // combination must follow.
      //
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
    }
}

void NonmemSpkMLToCpp::emitDriver()
{
}
void NonmemSpkMLToCpp::emitModel()
{
}
const struct FitParameters * NonmemSpkMLToCpp::getSpkParameters() const
{
  return &spk;
}
const void * NonmemSpkMLToCpp::getClientParameters() const
{
  return static_cast<const void*>( &nonmem );
}
////////////////////////////////////////////////////////////////////////////////
//  new class
////////////////////////////////////////////////////////////////////////////////
NonmemTranslator::NonmemTranslator( DOMDocument* treeIn )
  : tree( treeIn ), canned_model( NONE ), isCannedModelUsed( false )
{
  gSpkExpSymbolTable = new SymbolTable;
  gSpkExpTree        = expTreeUtils.createTree( "unit" );

  assert( tree->getElementsByTagName( X("model" ) ) != NULL );
  DOMElement * modelTag 
    = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("model" ) )->item(0));
  assert( modelTag != NULL );
  const char * advan = C( modelTag->getAttribute( X("base") ) );
  if( advan != NULL )
    {
      if( strcmp( advan, "advan2" )==0 )
	{
	  //symbol_checker = NULL;//new NonmemAdvan2Translator;
	}
      else
	{
	  char buf[126];
	  sprintf( buf, "\"%s\" is not supported! (%d, %s)\n", advan, __LINE__, __FILE__ );
	  fprintf( stderr, buf );
	  exit(-1);
	}
      canned_model = ADVAN2;
      isCannedModelUsed = true;
    }
  else
    {
      canned_model = NONE;
      //symbol_checker = NULL; //new NonmemPredTranslator;
    }
}
NonmemTranslator::~NonmemTranslator( )
{
  //delete symbol_checker;
  delete gSpkExpSymbolTable;
}

void NonmemTranslator::assemble( DOMDocument* tree )
{
  interpretContent();

  // <driver> must be parsed before <data> so that
  // the size of population is discovered first.
  interpretDriver();

  // <data> must be parsed before <model> so that
  // symbols are registered into the symbol table.
  interpretData();
  interpretModel();

  //interpretOutput();

}
void NonmemTranslator::emit( DOMDocument* tree )
{
  //emitModel();
  //emitDriver();
}
void NonmemTranslator::initSymbolTable( SymbolTable& )
{
}
void NonmemTranslator::interpretContent()
{
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  assert( tree != NULL );

  //////////////////////////////////////////////////////////////////////////
  // START FROM HERE!!! 7/11/03 (FRI)
  //
  // Run in debugger.  Segmentation error happens at the immediately
  // following line.  tree is surely not NULL.  A same kind of operation
  // done in the constructor goes successfully.
  //////////////////////////////////////////////////////////////////////////
  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
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
void NonmemTranslator::interpretDriver()
{
  //
  // Get the root of "driver" subtree.  Since there's one and only one
  // <driver> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.
  //
  assert( tree->getElementsByTagName( X("driver") ) != NULL );
  DOMNode * driverTree = tree->getElementsByTagName( X("driver") )->item(0);
  assert( driverTree != NULL );

  //
  // Get a pointer to the symbol table.  In this <driver> section parsing, 
  // the components in FitParameters data structure held in the table
  // get filled with actual values.
  // The objectof the data structure in the table is called "spkSymbols".
  //
  SymbolTable * table = gSpkExpSymbolTable;
  

  //
  // Declare an integer placeholder for #of individuals.
  // 
  int nIndividuals = 0;

  //
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker 
    = doc->createTreeWalker( driverTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
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
	  
	  Symbol theta( "theta", Symbol::VECTOR, Symbol::DOUBLE, true );
	  theta.size( len );
	  table->insert( theta );

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
		  nonmem.thetaIn.resize( len );
		  nonmem.thetaFixed.resize( len );

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
		      
		      nonmem.thetaIn[cnt]    = value;
		      nonmem.thetaFixed[cnt] = isFixed;
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
		  nonmem.thetaLow.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmem.thetaLow[cnt] = value;
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
		  nonmem.thetaUp.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmem.thetaUp[cnt] = value;
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

	  Symbol omega( "omega", Symbol::MATRIX, Symbol::DOUBLE, true );
	  omega.dim( span, span );
	  table->insert( omega );
	  //
	  // <omega struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmem.omegaIn.resize( dimensions );
          nonmem.omegaFixed.resize( dimensions );

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
	      
	      nonmem.omegaIn[cnt]    = value;
              nonmem.omegaFixed[cnt] = isFixed;
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

	  Symbol sigma( "sigma", Symbol::MATRIX, Symbol::DOUBLE, true );
	  sigma.dim( span, span );
	  table->insert( sigma );

	  //
	  // <sigma struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ),
			    "diagonal" ) == 0 );
	  if( !isDiag )
	    assert( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X( "struct" ) ) ), 
			    "block" ) == 0 );

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmem.sigmaIn.resize( dimensions );
          nonmem.sigmaFixed.resize( dimensions );

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
	      
	      nonmem.sigmaIn[cnt]    = value;
              nonmem.sigmaFixed[cnt] = isFixed;
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
	  
	  Symbol eta( "eta", Symbol::VECTOR, Symbol::DOUBLE, true );
	  eta.size( len );

	  nonmem.etaIn.resize( len );
          nonmem.etaFixed.resize( len );

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
	      
	      nonmem.etaIn[cnt]    = value;
              nonmem.etaFixed[cnt] = isFixed; 
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
	    spk.objective = FO;
	  else if( strcmp( approx, "foce" ) == 0 )
	    spk.objective = FOCE;
	  else if( strcmp( approx, "laplace" ) == 0 )
	    spk.objective = LAPLACE;
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
	  spk.nIndividuals = nIndividuals;

	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon
	    = atof( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  spk.popEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spk.popMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spk.popTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spk.isPopWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spk.isPopParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isPopObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spk.isPopObjOut = isPopObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isPopObj_popParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spk.isPopObj_popParOut = isPopObj_popParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isPopObj_popPar_popParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spk.isPopObj_popPar_popParOut = isPopObj_popPar_popParOut;
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
	  spk.indEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) > 0 );
	  int mitr 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spk.indMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spk.indTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spk.isIndWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spk.isIndParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ) > 0 );
	  bool isIndObjOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spk.isIndObjOut = isIndObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isIndObj_indParOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spk.isIndObj_indParOut = isIndObj_indParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isIndObj_indPar_indParOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spk.isIndObj_indPar_indParOut = isIndObj_indPar_indParOut;
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
	  spk.isPopStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spk.isPopCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spk.isPopCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spk.isPopCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spk.isPopConfidenceOut = isConfidenceOut;
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
	  spk.isIndStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spk.isIndCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spk.isIndCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spk.isIndCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut 
	    = ( strcmp( C( dynamic_cast<DOMElement*>(categoryNode)->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spk.isIndConfidenceOut = isConfidenceOut;
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

void NonmemTranslator::interpretData()
{
  //
  // Get a pointer to the root of "data" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMNode * dataTree = tree->getElementsByTagName( X("data") )->item(0);
  assert( dataTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  SymbolTable *table = gSpkExpSymbolTable;
  assert( table != NULL );

  //
  // Discover the number of data columns per individual set.
  // The number must be the same for all individuals.
  //
  int ind_cnt=0;
  int num_columns_previous = 0;
  int num_columns_current  = 0;

  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker = doc->createTreeWalker( dataTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  DOMElement * individual = dynamic_cast< DOMElement* >(walker->firstChild());
  while( individual != NULL )
    {
      ++ind_cnt;
      num_columns_current = 0;

      DOMElement * item = dynamic_cast< DOMElement* >( walker->firstChild() );
      while( item != NULL )
	{
	  ++num_columns_current;
	  item = dynamic_cast< DOMElement* >( walker->nextSibling() );
	}
      if( ind_cnt > 1 )
	{
	  assert( num_columns_current == num_columns_previous );
	}
      num_columns_previous = num_columns_current;
      walker->parentNode();
      individual = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }
  walker->parentNode();
  assert( ind_cnt == spk.nIndividuals );

  individual = dynamic_cast< DOMElement* >(walker->firstChild());
  ind_cnt = 0;// reset the counter!
  int num_columns = num_columns_previous;
  //
  // <data (individual)+>
  // <individual length CDATA #REQUIRED>
  //
  // The number of <individual>s must agree with the number registered in 
  // the value stored in spk.nIndividuals, which has been discovered during
  // parsing of <driver>.
  //
  nonmem.data.resize( spk.nIndividuals );

  while( individual != NULL )
    {
      //
      // <individual (item)+>
      // <item (value)+>
      //
      // <individual order CDATA #IMPLIED>
      // <individual id CDATA #REQUIRED>
      // <individual length CDATA #REQUIRED>
      // <item label (id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3|drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt|call|cnt|CDATA) #REQUIRED>
      // <item synonym (id|l1|l2|dv|mdv|time|data|dat1|dat2|dat3|drop|skip|evid|amt|rate|ss|ii|add1|cmt|pcmt|call|cnt|CDATA) #IMPLIED>
      //
      assert( XMLString::equals( individual->getNodeName(), X("individual") ) );
      ++ind_cnt;

      int length = atoi( trim( individual->getAttribute( X("length") ) ) );

      int order;
      const XMLCh* xml_order = individual->getAttribute( X("order") );
      if( xml_order == NULL )
	order = ind_cnt - 1;
      else 
	order = atoi( C( xml_order ) ) - 1;

      const char* id = C( individual->getAttribute( X("id") ) );

      //      cout << id << "(" << order << ")" << endl;
      nonmem.data[order].records.resize( num_columns );
      nonmem.data[order].owner =  const_cast<char*>( id );

      DOMElement * item = dynamic_cast< DOMElement* >( walker->firstChild() );
      int item_cnt=0;
      while( item != NULL )
	{
	  ++item_cnt;
	  const XMLCh* xml_label = item->getAttribute( X("label") );
	  assert( xml_label != NULL );
	  const char* label = C( xml_label );

	  const char * synonym = C( item->getAttribute( X("synonym") ) );

	  nonmem.data[order].records[item_cnt-1].values.resize( length );
	  nonmem.data[order].records[item_cnt-1].label = const_cast<char*>(label);
          nonmem.data[order].records[item_cnt-1].synonym = const_cast<char*>(synonym);

	  bool isNonmemKeyword = true;
	  /*
	  if( strcmp( label, "id" ) == 0 |
	      strcmp( label, "l1" ) == 0 |
	      strcmp( label, "l2" ) == 0 |
	      strcmp( label, "dv" ) == 0 |
	      strcmp( label, "mdv" ) == 0 |
	      strcmp( label, "time" ) == 0 |
	      strcmp( label, "data" ) == 0 |
	      strcmp( label, "dat1" ) == 0 |
	      strcmp( label, "dat2" ) == 0 |
	      strcmp( label, "dat3" ) == 0 |
	      strcmp( label, "drop" ) == 0 |
	      strcmp( label, "skip" ) == 0 |
	      strcmp( label, "evid" ) == 0 |
	      strcmp( label, "amt" ) == 0 |
	      strcmp( label, "rate" ) == 0 |
	      strcmp( label, "ss" ) == 0 |
	      strcmp( label, "ii" ) == 0 |
	      strcmp( label, "add1" ) == 0 |
	      strcmp( label, "cmt" ) == 0 |
	      strcmp( label, "pcmt" ) == 0 |
	      strcmp( label, "call" ) == 0 |
	      strcmp( label, "cont" ) == 0 )
	    isNonmemKeyword = true;
	  */
	  Symbol lab( label, Symbol::VECTOR, Symbol::DOUBLE, isNonmemKeyword ); 
	  lab.size( length );
	  table->insert( lab );
	  /*
	  isNonmemKeyword = false;
	  if( strcmp( synonym, "id" ) == 0 |
	      strcmp( synonym, "l1" ) == 0 |
	      strcmp( synonym, "l2" ) == 0 |
	      strcmp( synonym, "dv" ) == 0 |
	      strcmp( synonym, "mdv" ) == 0 |
	      strcmp( synonym, "time" ) == 0 |
	      strcmp( synonym, "data" ) == 0 |
	      strcmp( synonym, "dat1" ) == 0 |
	      strcmp( synonym, "dat2" ) == 0 |
	      strcmp( synonym, "dat3" ) == 0 |
	      strcmp( synonym, "drop" ) == 0 |
	      strcmp( synonym, "skip" ) == 0 |
	      strcmp( synonym, "evid" ) == 0 |
	      strcmp( synonym, "amt" ) == 0 |
	      strcmp( synonym, "rate" ) == 0 |
	      strcmp( synonym, "ss" ) == 0 |
	      strcmp( synonym, "ii" ) == 0 |
	      strcmp( synonym, "add1" ) == 0 |
	      strcmp( synonym, "cmt" ) == 0 |
	      strcmp( synonym, "pcmt" ) == 0 |
	      strcmp( synonym, "call" ) == 0 |
	      strcmp( synonym, "cont" ) == 0 )
	    isNonmemKeyword = true;
	  */
	  Symbol syn( synonym, Symbol::VECTOR, Symbol::DOUBLE, isNonmemKeyword ); 
	  syn.size( length );
	  table->insert( syn );

	  /*
	  cout << data[order].columns[item_cnt-1].label;
	  if( synonym != NULL ) cout << " = " << data[order].columns[item_cnt-1].synonym;
          cout << endl;
	  */

	  DOMElement * valueNode = dynamic_cast< DOMElement* >( walker->firstChild() );
	  int value_cnt=0;
	  while( valueNode != NULL )
	    {
	      ++value_cnt;
	      //
	      // <value> may be empty.
	      //
	      DOMNode * actual = valueNode->getFirstChild();
	      double value = atof( actual != NULL ? C( actual->getNodeValue() ) : "0.0" );

	      nonmem.data[order].records[item_cnt-1].values[value_cnt-1] = value;
	      /*
      	      cout << "value = " << data[order].columns[item_cnt-1].values[value_cnt-1] << endl;
	      */	      
	      valueNode = dynamic_cast< DOMElement* >( walker->nextSibling() );
	    }
	  walker->parentNode();
	  item = dynamic_cast< DOMElement* >( walker->nextSibling() );
	}
      walker->parentNode(); 
      individual = dynamic_cast< DOMElement* >( walker->nextSibling() );
    }
}
void NonmemTranslator::interpretModel()
{
  //
  // Get a pointer to the root of "model" subtree.  Since there's one and only one
  // <data> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.  If ever there's more
  // than one such a section, the very first occurence of them
  // will be processed and others will be untouched.
  //
  assert( tree->getElementsByTagName( X("model") ) != NULL );
  DOMNode * modelTree = tree->getElementsByTagName( X("model") )->item(0);
  assert( modelTree != NULL );
  
  //
  // Get a pointer to the symbol table where we collect information.
  // Register the number of individuals, information derived from the number of
  // <individual> tags in <data> subtree.
  //
  SymbolTable * table = gSpkExpSymbolTable;
  assert( table != NULL );

  DOMDocument * doc = const_cast<DOMDocument*>(tree);
  DOMTreeWalker * walker = doc->createTreeWalker( modelTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  
  // 
  // Determine if a canned model is requested.
  //
  const char* c_baseModel = C( dynamic_cast<DOMElement*>(modelTree)->getAttribute( X("base") ) );
  if( !isCannedModelUsed )
    {
      //
      // If no canned model is used, <pred> has to follow.
      //
      DOMNode * pred = walker->firstChild();
      assert( XMLString::equals( pred->getNodeName(), X("pred") ) );
    }
  else
    {
      //
      // When a canned model is used, either (comp_model, diffeqn) or (comp_model?, (pk, error), diffeqn?)
      // combination must follow.
      //
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
    }
}

void NonmemTranslator::emitDriver()
{
}
void NonmemTranslator::emitModel()
{
}
const struct FitParameters * NonmemTranslator::getSpkParameters() const
{
  return &spk;
}
const void * NonmemTranslator::getClientParameters() const
{
  return static_cast<const void*>( &nonmem );
}
const char * NonmemTranslator::getDriverFilename() const
{
  return NULL;
}
const std::vector< const char * > NonmemTranslator::getModelFilenameList() const
{
  std::vector<const char*> empty;
  return empty;
}
enum client::type NonmemTranslator::getClient() const
{
  return client::NONMEM;
}
