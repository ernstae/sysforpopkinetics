#include "NonmemTranslator.h"
#include "SpkCompilerUtil.h"

#include <xercesc/dom/DOM.hpp>

#include <iostream>

using namespace xercesc;
using namespace std;

////////////////////////////////////////////////////////////////////////////////////
//
// LOCAL FUNCTIONS
//
////////////////////////////////////////////////////////////////////////////////////

//=======================================================
// trimToChar( const XMLCh* source )
//
// Remove leading and trailing whitespaces from "source"
// and convert the string which is in XMLCh characters
// to ANSII C characters.  
// Return a pointer to the array.
//=======================================================
static const char* trimToChar( const XMLCh* source )
{
  return C( trim(source) );
}
//=======================================================
// tolower( char* buf, const char* mixed )
// 
// Convert the string in "buf" to all lower-cases.
//=======================================================
static const char* tolower( char* buf, const char* mixed )
{
  int len = strlen( mixed );
  for( int i=0; i<len; i++ )
    buf[i] = tolower( mixed[i] );
  buf[len] = 0;
  return buf;
}

//=======================================================
// isNonmemKeyword( const string var )
//
// Return true if "var" is one of the NONMEM reserved
// words.
//=======================================================
static bool isNonmemKeyword( const string var )
{
  if( var =="id" |
      var =="l1" |
      var =="l2" |
      var =="dv" |
      var =="mdv" |
      var =="time" |
      var =="data" |
      var =="dat1" |
      var =="dat2" |
      var =="dat3" |
      var =="drop" |
      var =="skip" |
      var =="evid" |
      var =="amt" |
      var =="rate" |
      var =="ss" |
      var =="ii" |
      var =="add1" |
      var =="cmt" |
      var =="pcmt" |
      var =="call" |
      var =="cont" )
    {
      return true;
    }
  else
    return false;
}


//=======================================================
// error( const char * message )
//
// Redirect "message" to the standard error and
// increment the error counter.
// 
// REVISIT: 06/03/03, Sachiko
// This routine should be replaced by 
// something more sophisticated.
//
//=======================================================
static int errors = 0;
static int error( const char * message, int line, const char* file )
{
  fprintf( stderr, "!!! ERROR !!! %s (%d: %s)\n", message, line, file );
  ++errors;
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
//
//  NonmemTranslator class
//
////////////////////////////////////////////////////////////////////////////////

NonmemTranslator::NonmemTranslator( )
  : nonmemModel( NONE ),
    STR_NONE   ( "none" ),
    STR_ADVAN1 ( "advan1" ),
    STR_ADVAN2 ( "advan2" ),
    STR_ADVAN3 ( "advan3" ),
    STR_ADVAN4 ( "advan4" ),
    STR_ADVAN5 ( "advan5" ),
    STR_ADVAN6 ( "advan6" ),
    STR_ADVAN7 ( "advan7" ),
    STR_ADVAN8 ( "advan8" ),
    STR_ADVAN9 ( "advan9" ),
    STR_ADVAN10( "advan10" ),
    STR_ADVAN11( "advan11" ),
    STR_ADVAN12( "advan12" )
{
  gSpkExpSymbolTable = new SymbolTable;
  gSpkExpTree        = expTreeUtils.createTree( "unit" );
}
NonmemTranslator::~NonmemTranslator( )
{
  //delete symbol_checker;
  delete gSpkExpSymbolTable;
}
enum NonmemTranslator::NonmemModel NonmemTranslator::toEnum( const char* str ) const
{
  if( strcmp( str, STR_NONE ) == 0 )
    return NONE;
  if( strcmp( str, STR_ADVAN1 ) == 0 )
    return ADVAN1;
  if( strcmp( str, STR_ADVAN2 ) == 0 )
    return ADVAN2;
  if( strcmp( str, STR_ADVAN3 ) == 0 )
    return ADVAN3;
  if( strcmp( str, STR_ADVAN4 ) == 0 )
    return ADVAN4;
  if( strcmp( str, STR_ADVAN5 ) == 0 )
    return ADVAN5;
  if( strcmp( str, STR_ADVAN6 ) == 0 )
    return ADVAN6;
  if( strcmp( str, STR_ADVAN7 ) == 0 )
    return ADVAN7;
  if( strcmp( str, STR_ADVAN8 ) == 0 )
    return ADVAN8;
  if( strcmp( str, STR_ADVAN9 ) == 0 )
    return ADVAN9;
  if( strcmp( str, STR_ADVAN10 ) == 0 )
    return ADVAN10;
  if( strcmp( str, STR_ADVAN11 ) == 0 )
    return ADVAN11;
  if( strcmp( str, STR_ADVAN12 ) == 0 )
    return ADVAN12;
}
const char* const NonmemTranslator::toString( enum NonmemTranslator::NonmemModel e ) const
{
  if( e == NONE )
    return STR_NONE;
  if( e == ADVAN1 )
    return STR_ADVAN1;
  if( e == ADVAN2 )
    return STR_ADVAN2;
  if( e == ADVAN3 )
    return STR_ADVAN3;
  if( e == ADVAN4 )
    return STR_ADVAN4;
  if( e == ADVAN5 )
    return STR_ADVAN5;
  if( e == ADVAN6 )
    return STR_ADVAN6;
  if( e == ADVAN7 )
    return STR_ADVAN7;
  if( e == ADVAN8 )
    return STR_ADVAN8;
  if( e == ADVAN9 )
    return STR_ADVAN9;
  if( e == ADVAN10 )
    return STR_ADVAN10;
  if( e == ADVAN11 )
    return STR_ADVAN11;
  if( e == ADVAN12 )
    return STR_ADVAN12;
}
std::vector<std::string> NonmemTranslator::translate( DOMDocument* tree )
{
  if( !readContent( tree, ourSpk ) )
    {
      char buf[] = "Rough content checking failed!";
      exit( error(buf, __LINE__, __FILE__) );
    }

  int nIndividuals = readDriver( tree, ourSpk, ourNonmem );

  //
  // This table maps labels and corresponding aliases.
  // When no alias is defined for a label, the entry (alias) field shall contain
  // an empty string.
  //
  //     key          entry
  //   (label)       (alias)
  // +---------+   +---------+
  // | label_a |---| alias_a |
  // +---------+   +---------+
  // | label_b |---|   ""    |
  // +---------+   +---------+
  //      .             .
  //      .             .
  //      .             .
  // +---------+   +---------+
  // | label_x |   | alias_x |
  // +---------+   +---------+
  //
  map<LABEL, ALIAS> label_alias_mapping;


  //
  // This table is used to record the following map:
  //
  //
  //     index          key                 entry
  // (individual#)    (label)           (measurements)
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | 0 |------>| label_a |--->| 0.0 | 1.0 | 2.0 | 2.3 |...
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | 1 |       | label_b |
  //    +---+       +---------+
  //      .         | label_c |
  //      .         +---------+
  //      .             ...
  //      .  
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | n |------>| label_a |--->| 0.0 | 1.2 | 1.9 | 2.3 |...
  //    +---+       +---------+    +-----+-----+-----+-----+
  //                | label_b |
  //                +---------+
  //                | label_c |
  //                +---------+
  //                    ...
  // 
  map< LABEL, MEASUREMENT > data_for[ nIndividuals +1 ];

  //
  // This table records the processing order vs. the identifier pair of each
  // individual.
  //
  string order_id_pair[ nIndividuals +1 ];

  readData( tree, nIndividuals, gSpkExpSymbolTable, label_alias_mapping, data_for, order_id_pair );

  readModel( tree, nIndividuals, gSpkExpSymbolTable );

  emitData( nIndividuals, gSpkExpSymbolTable, label_alias_mapping, data_for, order_id_pair );

  vector<string> files;
  return files;
}

void NonmemTranslator::initSymbolTable( SymbolTable& )
{
}

bool NonmemTranslator::readContent( DOMDocument* tree, SpkParameters& spkOut )
{
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  assert( tree != NULL );

  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node
    = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
  assert( content_node != NULL );

  //
  // Verify SpkInML version specification
  //
  const char* c_spkml_ver = C( content_node->getAttribute( X("spkinml_ver") ) );
  if( strcmp( c_spkml_ver, "1.0" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "SpkInML version mismatch!  Only \"1.0\" supported!  \
                   The version you gave me says %s.\n", c_spkml_ver );
    exit( error( buf, __LINE__, __FILE__ ) );
  }
  
  //
  // Verify client specification
  //
  const char * c_client = C( content_node->getAttribute( X("client") ) );
  if( strcmp( c_client, "nonmem" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Anything besides \"nonmem\" does not make sense in this context!  \
                  You gave me %s (case sensitive).\n", c_client );
    exit( error( buf, __LINE__, __FILE__ ) );
  }

  //
  // analysis level
  //
  const char * c_analysis = C( content_node->getAttribute( X("analysis") ) );
  if( strcmp( c_analysis, "population" ) != 0 )
  {
    char buf[128];
    sprintf( buf, "Currently only \"population\" is supported!  You gave me %s (case sensitive).\n",
	     c_analysis );
    exit( error( buf, __LINE__, __FILE__ ) );
  }
  else
    spkOut.analysis = SpkParameters::POPULATION;

  return true;
}

//=====================================================================================
//
// Read <driver> section and returns the number of individuals in the population.
//
//=====================================================================================
int NonmemTranslator::readDriver( 
   DOMDocument* tree, 
   SpkParameters & spkOut, 
   NonmemParameters& nonmemOut )
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
  // Declare an integer placeholder for #of individuals.
  // 
  int nIndividuals = 0;

  //
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMTreeWalker * walker 
    = tree->createTreeWalker( driverTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  DOMNode *driverElement = walker->firstChild();

  while( driverElement != NULL )
    {
      const char * driverElementName = C( driverElement->getNodeName() );

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
      if( strcmp( driverElementName, "theta" ) == 0 )
	{
	  //
	  // <theta length CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( dynamic_cast<DOMElement*>(driverElement)
					->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(driverElement)
			     ->getAttribute( X( "length" ) ) ) );
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
		  nonmemOut.thetaIn.resize( len );
		  nonmemOut.thetaFixed.resize( len );

		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      bool isFixed 
			= strcmp( C( dynamic_cast<DOMElement*>(valueNode)
				     ->getAttribute( X( "fixed" ) ) ), "yes" )==0;
		      double value 
			= atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmemOut.thetaIn[cnt]    = value;
		      nonmemOut.thetaFixed[cnt] = isFixed;
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
		  nonmemOut.thetaLow.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmemOut.thetaLow[cnt] = value;
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
		  nonmemOut.thetaUp.resize( len );
		  DOMNode * valueNode = walker->firstChild();
		  assert( valueNode != NULL );
		  int cnt;
		  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
		    {
		      double value 
			= atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
		      
		      nonmemOut.thetaUp[cnt] = value;
		      valueNode = walker->nextSibling();
		    }	
		  assert( cnt == len );
		  walker->parentNode();// back to <up>
		}
	      else
		{
		  char buf[128];
		  sprintf( buf, "Unknown tag <%s>! Note that tag names are case sensitive.\n",
			   driverElementName );
		  exit( error( buf, __LINE__, __FILE__ ) );
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
      else if( strcmp( driverElementName, "omega" ) == 0 )
	{
	  //
	  // <omega span CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( 
		              dynamic_cast<DOMElement*>(driverElement)
		              ->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(driverElement)
			      ->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  //
	  // <omega struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(driverElement)
				     ->getAttribute( X( "struct" ) ) ),
			          "diagonal" ) == 0 );
	  if( !isDiag )
	    {
	      assert( strcmp( C( dynamic_cast<DOMElement*>(driverElement)
				 ->getAttribute( X( "struct" ) ) ), 
			      "block" ) == 0 );
	    }
          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmemOut.omegaIn.resize( dimensions );
          nonmemOut.omegaFixed.resize( dimensions );

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
	      bool isFixed = strcmp( C( dynamic_cast<DOMElement*>(valueNode)
					->getAttribute( X( "fixed" ) ) ), "yes" ) == 0;
	      double value = atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      nonmemOut.omegaIn[cnt]    = value;
              nonmemOut.omegaFixed[cnt] = isFixed;
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
      else if( strcmp( driverElementName, "sigma" ) == 0 )
	{
	  //
	  // <simga span CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen( 
                              dynamic_cast<DOMElement*>(driverElement)
		              ->getAttribute( X( "span" ) ) ) > 0 );
	  int span = atoi( C( dynamic_cast<DOMElement*>(driverElement)
			      ->getAttribute( X( "span" ) ) ) );
	  assert( span > 0 );

	  //
	  // <sigma struct (diagonal|block) #REQUIRED>
	  //
	  bool isDiag = ( strcmp( C( dynamic_cast<DOMElement*>(driverElement)
				  ->getAttribute( X( "struct" ) ) ),
			         "diagonal" ) == 0 );
	  if( !isDiag )
	    {
	      assert( strcmp( C( dynamic_cast<DOMElement*>(driverElement)
				 ->getAttribute( X( "struct" ) ) ), 
			      "block" ) == 0 );
	    }

          int dimensions = (isDiag? span : span*(span+1)/2 );
	  nonmemOut.sigmaIn.resize( dimensions );
          nonmemOut.sigmaFixed.resize( dimensions );

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
		= strcmp( C( dynamic_cast<DOMElement*>(valueNode)
					->getAttribute( X( "fixed" ) ) ), "yes" )==0;
	     double value 
	       = atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      nonmemOut.sigmaIn[cnt]    = value;
              nonmemOut.sigmaFixed[cnt] = isFixed;
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
      else if( strcmp( driverElementName, "eta" ) == 0 )
	{
	  //
	  // <eta length CDATA #REQUIRED>
	  //
	  assert( XMLString::stringLen(
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X( "length" ) ) ) > 0 );
	  int len = atoi( C( dynamic_cast<DOMElement*>(driverElement)
			     ->getAttribute( X( "length" ) ) ) );
	  assert( len > 0 );
	  
	  Symbol eta( "eta", Symbol::VECTOR, Symbol::DOUBLE, true );
	  eta.size( len );

	  nonmemOut.etaIn.resize( len );
          nonmemOut.etaFixed.resize( len );

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
	      bool isFixed = strcmp( C( dynamic_cast<DOMElement*>(valueNode)
					->getAttribute( X( "fixed" ) ) ), "yes" )==0;
	      double value = atof( trimToChar( valueNode->getFirstChild()->getNodeValue() ) );
	      
	      nonmemOut.etaIn[cnt]    = value;
              nonmemOut.etaFixed[cnt] = isFixed; 
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
      else if( strcmp( driverElementName, "pop_opt" ) == 0 )
	{
	  // approximation (fo|foce|laplace)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("approximation") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("approximation") ) ) > 0 );

	  const char *approx = C( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("approximation") ) );
	  if( strcmp( approx, "fo" ) == 0 )
	    spkOut.objective = FIRST_ORDER;
	  else if( strcmp( approx, "foce" ) == 0 )
	    spkOut.objective = EXPECTED_HESSIAN;
	  else if( strcmp( approx, "laplace" ) == 0 )
	    spkOut.objective = MODIFIED_LAPLACE;
	  else
	    {
	      char buf[128];
	      sprintf( buf, "Unknown objective (%s)\n", approx );
	      exit( error( buf, __LINE__, __FILE__ ) );
	    }
	  
	  // pop_size
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("pop_size") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("pop_size") ) ) > 0 );

	  nIndividuals = atoi( C( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("pop_size") ) ) );
	  assert( nIndividuals > 0 );
	  spkOut.nIndividuals = nIndividuals;

	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon = atof( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  spkOut.popEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) ) > 0 );
	  int mitr = atoi( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spkOut.popMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) ) > 0 );
	  int trace 
	    = atoi( C( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spkOut.popTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spkOut.isPopWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut 
	    = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spkOut.isPopParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) ) > 0 );
	  bool isPopObjOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spkOut.isPopObjOut = isPopObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isPopObj_popParOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spkOut.isPopObj_popParOut = isPopObj_popParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isPopObj_popPar_popParOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spkOut.isPopObj_popPar_popParOut = isPopObj_popPar_popParOut;
	}
      //
      // Visit <ind_opt>.
      //
      else if( strcmp( driverElementName, "ind_opt" ) == 0 )
	{
	  // epsilon (>0.0)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("epsilon") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("epsilon") ) ) > 0 );
	  double epsilon = atof( C( 
                 dynamic_cast<DOMElement*>(driverElement)
		 ->getAttribute( X("epsilon") ) ) );
	  assert( epsilon >= 0.0 );
	  spkOut.indEpsilon = epsilon;

	  // mitr (>=0)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) ) > 0 );
	  int mitr = atoi( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("mitr") ) ) );
	  assert( mitr >= 0 );
	  spkOut.indMaxItr = mitr;

	  // trace (0-5)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) != NULL );
	  assert( XMLString::stringLen(
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) ) > 0 );
	  int trace = atoi( C( 
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("trace") ) ) );
	  assert( trace >= 0 && trace <= 5 );
	  spkOut.indTrace = trace;

	  // restart (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) ) > 0 );
	  bool isRestart = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("restart") ) ), "yes" ) == 0 );
	  spkOut.isIndWarmStart = isRestart;

	  // par_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) ) > 0 );
	  bool isParOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("par_out") ) ), "yes" ) == 0 );
	  spkOut.isIndParOut = isParOut;

	  // obj_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) ) > 0 );
	  bool isIndObjOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
	  spkOut.isIndObjOut = isIndObjOut;

	  // deriv1_out (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) ) > 0 );
	  bool isIndObj_indParOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
	  spkOut.isIndObj_indParOut = isIndObj_indParOut;

	  // deriv2_out (yes|no) 
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) ) > 0 );
	  bool isIndObj_indPar_indParOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
	  spkOut.isIndObj_indPar_indParOut = isIndObj_indPar_indParOut;
	}
      //
      // Visit <pop_stat>.
      //
      else if( strcmp( driverElementName, "pop_stat" ) == 0 )
	{
	  // stderror (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("stderror") ) != NULL );
	  assert( XMLString::stringLen( 
                 dynamic_cast<DOMElement*>(driverElement)
		 ->getAttribute( X("stderror") ) ) > 0 );
	  bool isStderrorOut = ( strcmp( C( 
                 dynamic_cast<DOMElement*>(driverElement)
		 ->getAttribute( X("stderror") ) ), "yes" ) == 0 );
	  spkOut.isPopStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spkOut.isPopCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spkOut.isPopCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut = ( strcmp( C(
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spkOut.isPopCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spkOut.isPopConfidenceOut = isConfidenceOut;
	}
      //
      // Visit <ind_stat>.
      //
      else if( strcmp( driverElementName, "ind_stat" ) == 0 )
	{
	  // stderror (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("stderror") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("stderror") ) ) > 0 );
	  bool isStderrorOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("stderror") ) ), "yes" ) == 0 );
	  spkOut.isIndStderrorOut = isStderrorOut;

	  // correlation (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) != NULL );
	  assert( XMLString::stringLen(
		  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) ) > 0 );
	  bool isCorrelationOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("correlation") ) ), "yes" ) == 0 );
	  spkOut.isIndCorrelationOut = isCorrelationOut;

	  // cov (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) ) > 0 );
	  bool isCovOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("cov") ) ), "yes" ) == 0 );
	  spkOut.isIndCovarianceOut = isCovOut;

	  // coefficient (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
                  ->getAttribute( X("coefficient") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("coefficient") ) ) > 0 );
	  bool isCoefficientOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
	  spkOut.isIndCoefficientOut = isCoefficientOut;

	  // confidence (yes|no)
	  assert( dynamic_cast<DOMElement*>(driverElement)
                  ->getAttribute( X("confidence") ) != NULL );
	  assert( XMLString::stringLen( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("confidence") ) ) > 0 );
	  bool isConfidenceOut = ( strcmp( C( 
                  dynamic_cast<DOMElement*>(driverElement)
		  ->getAttribute( X("confidence") ) ), "yes" ) == 0 );
	  spkOut.isIndConfidenceOut = isConfidenceOut;
	}

      //
      // Unknown elements.
      //
      else
	{
	  char buf[128];
	  sprintf( buf, "Unknown tag <%s>! Note that tag names are case sensitive.\n",
		   driverElementName );
	  exit( error( buf, __LINE__, __FILE__ ) );
	}
      driverElement = walker->nextSibling();
    }
    return nIndividuals;
}

/**
 * Process <data> section of SpkInML document to gather information
 * needed to genreate the source code for the following entities:
 *
 * - the definition of IndRecords class
 * - the initialization of IndRecords objects for all individuals
 * 
 * IndRecords class
 *   This class lists all observation records associated with an individual.
 *   It allows accessing a set of observation records (column) by both
 *   "label" and "alias" (if an alias is given).
 */
void NonmemTranslator::readData( 
	xercesc::DOMDocument* tree, 
        int nIndividuals,
	SymbolTable * table,
	map<LABEL, ALIAS> &label_alias_mappingOut,
	map<LABEL, MEASUREMENT> data_forOut[],
	string order_id_pairOut[]
      )
{
  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMNode * dataTree = tree->getElementsByTagName( X("data") )->item(0);
  assert( dataTree != NULL );
  //
  // Get the list of <individual> nodes.  Each <individual> node is the root
  // of that individual's data subtree and determine the number of 
  // sets (= #individuals) of data.
  //
  DOMTreeWalker * walker = tree->createTreeWalker( dataTree,
						   DOMNodeFilter::SHOW_ELEMENT,
						   NULL,
						   false );
       
  //=================================================================================
  //
  // Traversing the DATA tree to gather information.
  //
  //=================================================================================

  //
  // Iterate through the list of <individual> blocks to register
  // label-alias mappings and to populate the data_forOut table.
  //
  //
  // <individual> tag comes with the following attributes:
  //
  // order  --- (optional) the order at which the individual's data shall be processed.
  // id     --- (required) the alpha-numerical value identifying the individual.
  // length --- (required) the number of measurements for every observation.
  //
  // and contains x number of <item> tags.
  // <item> tag comes with the following attributes:
  //
  // label  --- (required) the title used to refer to the observation.
  // synonym--- (optional) an alias for the label.
  // 
  // Either of the label or the synonym in each pair MUST be one of 
  // NONMEM-reserved words 
  // (see "$INPUT" section, p56, NONMEM User's Guide VIII, for a complete list).
  // If either of them is "skip" or "drop", ignore the entire measurement vector.
  //
  DOMElement * individual = dynamic_cast<DOMElement*>( walker->firstChild() );
  for( int i=0; i<nIndividuals; i++ )
    {
      //      DOMElement * individual = dynamic_cast<DOMElement*>( individualsList->item(i) );

      //
      // First, take care the <individual> tag's attributes.
      //
      int nMeasurements = atoi( C( individual->getAttribute( X("length") ) ) );
      const string id = string( C( individual->getAttribute( X("id") ) ) );

      int order = i;
      const XMLCh* xml_order = individual->getAttribute( X("order") );
      if( !XMLString::isAllWhiteSpace( xml_order ) || xml_order != NULL )
	{
	  order = atoi( C( xml_order ) ) - 1;
	}

      //
      // Map the processing order and the individual's identifier.
      //
      order_id_pairOut[order] = id;

      //
      // Next, get the entire subtree of <individual> and traverse
      // the tree to collect a data set for this individual.
      // 
      //           <individual>
      //                 |
      //                 |
      //                \|/
      //              <item> ---> <item> ---> <item> ---> <item> ---> ...   --+
      //                 |           |
      //                 |          \|/                                      |
      //                 |        <value> ---> <value> ---> ... --+         NULL
      //                \|/                                       |
      //              <value> ---> <value> ---> ... --+         NULL
      //                                              |
      //                                             NULL
      //
      /*
      DOMTreeWalker * walker = dataTree->createTreeWalker( individual,
							   DOMNodeFilter::SHOW_ELEMENT,
							   NULL,
							   false );
      */
      int nItems = 0;
      DOMElement * item = dynamic_cast<DOMElement*>( walker->firstChild() );
      for( nItems=0; item != NULL; ++nItems )
	{
	  // 
	  // Retrieve the attributes (label and synonym) for this item/column
	  // and register the pair in the label-alias map.
	  // 
	  // - label is required.
	  // - synonym is optional.
	  //
	  // Make sure, if no alias is defined, the entry associated with
	  // the key (label) must be set to NULL because the later operations
	  // will assume it's NULL.
	  //
	  // If either of them says "skip" or "drop", ignore the column 
	  // completely.
	  //
	  const XMLCh * xml_label = item->getAttribute( X("label" ) );
	  assert( !XMLString::isAllWhiteSpace( xml_label ) );

	  const XMLCh * xml_synonym = item->getAttribute( X("synonym") );

	  const XMLCh* X_SKIP = X("skip");
	  const XMLCh* X_DROP = X("drop");
	  if( XMLString::equals( xml_label, X_SKIP ) || XMLString::equals( xml_label, X_DROP )
	      || XMLString::equals( xml_synonym, X_SKIP ) || XMLString::equals( xml_synonym, X_DROP ) )
	    {
	      // This column is to be ignored.
	    }
	  else
	    {
	      const string label = string( C( xml_label ) );
	      const string synonym = string( C( xml_synonym ) );

	      bool isLabel_NonmemKeyword = isNonmemKeyword( label );	      
	      //Symbol name1( label, Symbol::VECTOR, Symbol::DOUBLE, isLabel_NonmemKeyword );
	      //name1.size( nMeasurements );
	      //table->insert( name1 );
	      if( synonym != "" )
		{
		  bool isAlias_NonmemKeyword = isNonmemKeyword( synonym );
		  
		  // When an alias is given, either the label or alias MUST be one of NONMEM keywords.
		  assert( isLabel_NonmemKeyword || isAlias_NonmemKeyword );
		  
		  //Symbol name2( synonym, Symbol::VECTOR, Symbol::DOUBLE, isAlias_NonmemKeyword );
		  //name2.size( nMeasurements );
		  //table->insert( name2 );
		}
	      label_alias_mappingOut[ label ] = synonym;
	      //
	      // Now, go though the set (column) of measurement data.
	      //
	      // The <value> subtree is an interesting one.  In the xml document
	      // it appears like this:
	      //   <value>1.0</value>
	      // so, it looks like the Node Value of <value> is "1.0".
	      // Wrong!  The value "1.0" is stored as the value of a text node (DOMText) 
	      // in a deeper level.
	      // 
	      // NOTE by Sachiko:
	      // Thought specifying DOMNodeFilter::SHOW_ELEMENT when creating a DOMTreeWalker
	      // would supress this DOMText representation but doesn't.  Why?
	      //
	      int nValues = 0;
	      valarray<double> values(nMeasurements);
	      DOMElement * valueTag = dynamic_cast<DOMElement*>( walker->firstChild() );
	      for( nValues=0; valueTag != NULL; ++nValues )
		{
		  //
		  // <value> tag could be empty, implying the value is supposed to be 0.0.
		  //
		  DOMText * val_node = dynamic_cast<DOMText*>( valueTag->getFirstChild() );
		  if( val_node != NULL )
		    {
		      values[nValues] = atof( C( trim( val_node->getNodeValue() ) ) );
		    }
		  else
		    {
		      values[nValues] = 0.0;
		    }
		  valueTag = dynamic_cast<DOMElement*>( walker->nextSibling() );
		}
	      assert( nMeasurements == nValues );
	      
	      //
	      // Register the label-values pair in the data_forOut map.
	      //
	      data_forOut[order].insert( pair<string, valarray<double> >(label, values) );
	      walker->parentNode();
	    } 
	  item = dynamic_cast<DOMElement*>( walker->nextSibling() );
	}
      
      walker->parentNode();
      individual = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }
}

std::vector<string> NonmemTranslator::emitData( 		
		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<LABEL, ALIAS> & label_alias_mapping,
		 const std::map<LABEL, MEASUREMENT> data_for[],
		 const string order_id_pair[]
		 )
{
  //=================================================================================
  //
  // Convert the gathered information into C++ source code.
  //
  //=================================================================================

  //
  // Write the definition of "class IndRecords".
  //
  const char* str_IndRecords = "IndRecords";

  cout << "class " << str_IndRecords << "{\n";
  cout << "public:\n";
  cout << "\t" << str_IndRecords << "(\n";
  map<LABEL, ALIAS>::const_iterator names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      if( names != label_alias_mapping.begin() )
	{
	  cout << ", \n";
	}
      cout << "\t\t" << "const double * " << names->first << "In";
      ++names;
    }
  cout << "\n";
  cout << "\t)\n";
  cout << "\t : ";
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      if( names != label_alias_mapping.begin() )
	{
	  cout << ",\n\t";
	}
      cout << names->first << "(" << names->first << "In" << ")";
      if( names->second != "" )
	{
	  cout << ", " << names->second << "(" << names->first << ")";
	}
      ++names;
    }
  cout << "\n";
  cout << "\t{ /* have nothing really to do */ }\n";
  cout << "\n";
  
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      cout << "\tconst double * " << names->first << ";\n";
      if( names->second != "" )
	{
	  cout << "\tconst double * " << names->second << ";\n";
	}
      ++names;
    }

  cout << "\n";
  cout << "protected:\n";
  cout << "\t" << str_IndRecords << "(){}\n";
  cout << "\t" << str_IndRecords << "( const " << str_IndRecords << "& ){}\n";
  cout << "\t" << str_IndRecords << "* operator=( const " << str_IndRecords << "& ){}\n";
  cout << "};\n";

  //
  // Declare an array of #nIndividual number of IndRecords objects.
  //
  cout << "const int nIndividuals = " << nIndividuals << ";\n";
  cout << str_IndRecords << " * data[nIndividuals+1]" << ";\n";
  cout << "\n";

  //
  // Write the initialization code for IndRecords records for each individual.
  //
  map< LABEL, MEASUREMENT >::const_iterator map_records;
  for( int i=0; i<nIndividuals; i++ )
    {
      cout << "// " << order_id_pair[i] << "'s data (process order = " << i+1 << ")\n";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  cout << "const double " << map_records->first << "_" << order_id_pair[i] << "[] = ";
       	  cout << "{ ";
	  for( int j=0; j<map_records->second.size(); j++ )
	    {
	      if( j>0 )
		cout << ", ";
	      cout << map_records->second[j];
	    }
	  cout << " };\n";
	  ++map_records;
	}
      
      cout << str_IndRecords << " * data" << "_" << order_id_pair[i] << " = new " << str_IndRecords << "( ";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  if( map_records != data_for[i].begin() )
	    cout << ", ";
	  cout << map_records->first << "_" << order_id_pair[i];
	  ++map_records;
	}
      cout << " );\n";
      cout << "data_for[" << i << "] = " << "data_" << i << ";\n";
      cout << endl;
   }

  //
  // Clean-up code
  //
  cout << "// Release memory allocated for " << str_IndRecords << " objects.\n";
  cout << "for( int i=0; i<nIndividuals; i++ )\n";
  cout << "{\n";
  cout << "   delete data[i];\n";
  cout << "}\n";

  vector<string> filenames;
  return filenames;
}


void NonmemTranslator::readModel( DOMDocument* tree, int nIndividuals, SymbolTable* table )
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
  
  DOMTreeWalker * walker 
    = tree->createTreeWalker( modelTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );
  
  // 
  // Determine if a canned model is requested.
  //
  const XMLCh* xml_cannedModel = dynamic_cast<DOMElement*>(modelTree)
                               ->getAttribute( X("base") );

  bool isCannedModelUsed = !( XMLString::isAllWhiteSpace( xml_cannedModel ) );
  enum NonmemModel cannedModel = NONE;
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
      cannedModel = toEnum( C(xml_cannedModel) );
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
	      const char * mixed = trimToChar( model->getFirstChild()->getNodeValue() );
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
	      exit( error( buf, __LINE__, __FILE__ ) );
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
const struct SpkParameters * NonmemTranslator::getSpkParameters() const
{
  return &ourSpk;
}
const void * NonmemTranslator::getClientParameters() const
{
  return static_cast<const void*>( &ourNonmem );
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

