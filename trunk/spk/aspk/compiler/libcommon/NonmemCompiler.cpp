#include <iostream>
#include <set>
#include <map>
#include <xercesc/dom/DOM.hpp>

#include "NonmemCompiler.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

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
static int error( const char * message )
{
  fprintf( stderr, "!!! ERROR !!! %s (%d: %s)\n", message, __LINE__, __FILE__);
  return 1;
}
NonmemCompiler::NonmemCompiler( const char* filename )
  : SpkCompiler( client::NONMEM, filename ), 
    isDataProcessed(false) 
{
}

NonmemCompiler::~NonmemCompiler( )
{
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
  interpretData();

  assert( getTable()->spkSymbols->nIndividuals > 0 && isDataProcessed );
  interpretDriver();
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
  // If 
  //
  SymbolTable * table = getTable();
  int nIndividuals = ( dynamic_cast<DOMElement*>(dataTree)
		       ->getElementsByTagName( X("individual") ) )->getLength();
  //
  // REVISIT - Sachiko
  // 
  // Better error handling required in the future.
  //
  // If there's no <individual> tag found in <data> subtree, there's nothing
  // we can do.  Get out!
  //
  if( nIndividuals < 1 )
    {
      char buf[128];
      sprintf( buf, "The number of individuals must be greater than zero! (%d)", table->spkSymbols->nIndividuals );
      exit( error(buf) );
    }

  //
  // Otherwise, register the number of individuals in an appropriate place in the symbol table.
  //
  else
    table->spkSymbols->nIndividuals = nIndividuals;

  //
  // With #individuals figured out, we can now set the dimensions of some vectors.
  //
  // nMeasurementsAll is equivalent to N (in Brad's paper).
  //
  table->spkSymbols->nMeasurementsAll.resize( table->spkSymbols->nIndividuals );

  //
  // Preapare nIndividuals number of DataSetMap maps.  Each map will contains
  // pairs like this:
  //
  //  Key  :  DATA
  // ------   -----
  //   WT      {..., ..., ... }
  //  DOSE     {..., ..., ... }
  //  ...
  //
  dataSetsForAllIndividuals.resize( nIndividuals );

  //
  // Create a tree walker that will be used to traverse the tree
  //
  DOMTreeWalker * walker = doc->createTreeWalker( dataTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );

  //
  // First, get the very first child of <data> tree, which should be an <individual>.
  // It assumes there's at least one data set, implying the population has at least
  // one individual.
  //
  DOMNode * data_setNode = walker->firstChild();
  assert( data_setNode != NULL );
  assert( XMLString::equals( data_setNode->getNodeName(), X("individual") ) );

  //
  // Set its current node to the first child <individual> in the <data> subtree.
  //
  walker->setCurrentNode( data_setNode );

  //
  // alias_y will contain the name used within the client space
  // that corresponds to y in the SPK space.
  // The name shall appear in each entry (map) in the dataSetsForAllIndividuals set
  // as a key.  So, this string will be used as the key to access y values later.
  //
  char alias_y[128];
  alias_y[0] = '\0';

  //
  // n will indicate the number of individuals upon the completion of the following loop.
  //
  int n;
  for( n=0; data_setNode != NULL; n++ )
    {
      //
      // This flag is used to determine whether at least one dependent variables,
      // is found in the SpkInML specification.
      // 
      bool yGiven   = false;

      //
      // This is the order in which this individual's data set is supposed to processed
      // during the optimziation effort.  In the client space, the number starts from
      // one (1), so subtract one from it to make it start from 0 in C++ world.
      //
      int order      
	= atoi( C( dynamic_cast<DOMElement*>(data_setNode)->getAttribute( X("order") ) ) ) - 1;
      assert( order >= 0 );

      // 
      // This is the identifier associated with this individual.  It may be
      // some number or string, we don't know... so treat it as a alphanumeric string.
      // This attribute, however, is optional; so, if this attribute is not
      // specified, use alphanumeric version of "order" as an id.
      //
      const char* id 
	= trim( dynamic_cast<DOMElement*>(data_setNode)->getAttribute( X("id") ) );
      if( id == NULL )
       {
	 id = C( dynamic_cast<DOMElement*>(data_setNode)->getAttribute( X("order") ) );
       }

      //
      // This is what the user claims to be the number of data in a set.
      // We'd better check against the actual number of data given in the document though.
      //
      int data_len     
	= atoi( C( dynamic_cast<DOMElement*>(data_setNode)->getAttribute( X("length") ) ) );
      assert( data_len > 0 );

      //
      // Register id-order mapping, the id as key and the order as value.
      //
      IDs.insert( pair<string, int>(id, order) );
      table->spkSymbols->nMeasurementsAll[order] = data_len;
      
      //
      // Now, get the first child under <individual> subtree.
      //
      DOMNode * variableNode = walker->firstChild();
      walker->setCurrentNode( variableNode );

      while( variableNode != NULL )
	{
	  //
	  // One of the following tags is the current node:
	  // 
	  // <independent>
	  // <dependent>
	  // <attribute>
	  //
	  // Validate it!
	  // 
	 const char * variableType
	   = trim( variableNode->getNodeName() );
	 assert( stricmp( variableType, "independent" ) 
		 || stricmp( variableType, "dependent" )
		 || stricmp( variableType, "attribute" ) );

	 //
	 // A variable must have a name!
	 //
	 const char * variableName 
	   = trim( dynamic_cast<DOMElement*>(variableNode)->getAttribute( X("label") ) );
	 assert( variableName != NULL );

	 //
	 // <independent> contains a data set that is an independent parameter like t(ime).
	 // There usually be a single parameter but the user may refer to more than
	 // parameters to compute the model (f).  SPK Doesn't care about this acutally.
	 // So, none is accesptable.
	 //
	 if( stricmp( variableType, "independent" ) == 0 )
	   {
	     //
	     // Under <independent> are at least one <value> subtrees.
	     // Each <value> DOMElement node contains another kind of node, 
	     // a DOMAttribute, whose value is the <value>'s actual value (confusing?).
	     // 
	     DOMNode * valueNode = walker->firstChild();
	     assert( valueNode != NULL );
	     assert( XMLString::equals( valueNode->getNodeName(), X("value") ) );
	     walker->setCurrentNode( valueNode );   // Now, <value> is the current node.

	     //
	     // Resize the vector that will contain this particular data set to
	     // the size claimed earlier, which *may* be wrong!!!
	     //
	     valarray<double> independent_data(data_len);
	     int i;
	     for( i=0; i<data_len, valueNode != NULL; i++ )
	       {
		 double value = atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		 independent_data[i] = value;

		 valueNode = walker->nextSibling();
	       }
	     //
	     // Verify if the number of values given was the same as the number claimed
	     // by the user earlier.
	     //
	     assert( i == data_len );

	     //
	     // Register this data set for this individual in the map array.
	     //
             dataSetsForAllIndividuals[order].insert( 
	           pair<const char*, valarray<double> >( variableName, independent_data ) 
             );
	     	    
	     //
	     // Back to <independent> as current node for the tree walker.
	     //
             walker->setCurrentNode( walker->parentNode() );
	     assert( XMLString::equals(walker->getCurrentNode()->getNodeName(), X("independent") ) );
	   }
	 //
	 // <dependent> contains a data set that is a dependent variable such as
	 // measurements, y(t).
	 // There usually be a single variable of such but the user may refer to more than
	 // dependent variables to compute the model (f).
	 // Regarless, SPK cares about one and only one variable set,
	 // so none is not acceptable!
	 // 
	 else if( stricmp( variableType, "dependent" ) == 0 )
	   {
	     //
	     // Under <dependent> are at least one <value> subtrees.
	     // Each <value> DOMElement node contains another kind of node, 
	     // a DOMAttribute, whose value is the <value>'s actual value (confusing?).
	     // 
	     DOMNode * valueNode = walker->firstChild();
	     assert( valueNode != NULL );
	     assert( XMLString::equals( valueNode->getNodeName(), X("value") ) );
	     walker->setCurrentNode( valueNode ); // Now, <value> is the current node.

	     //
	     // In case there are more than one dependent variables,
	     // this flag indicates this particular data set being processed
	     // is the one SPK cares about.
	     //
             bool isPrimary 
	       = ( stricmp( 
			   trim( 
				dynamic_cast<DOMElement*>(variableNode)
				->getAttribute( X("primary") ) 
				), 
			   "YES" ) 
		   == 0 );

	     //
	     // Resize the vector that will contain this particular data set to
	     // the size claimed earlier, which *may* be wrong!!!
	     //
	     valarray<double> dependent_data( data_len );
	     int i;
	     for( i=0; i<data_len, valueNode != NULL; i++ )
	       {
		 double value = atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
		 dependent_data[i] = value;

		 valueNode = walker->nextSibling();
	       }
	     //
	     // Verify if the number of values given was the same as the number claimed
	     // by the user earlier.
	     //
	     assert( i == data_len );

	     //
	     // Register this data set for this individual in the map array.
	     //
	     dataSetsForAllIndividuals[order].insert( 
	           pair<const char*, valarray<double> >( variableName, dependent_data ) );

	     //
	     // Remember the name of this data set if it is marked as "primary".
	     // It means that the name is an alias to "y".
	     // 
             if( isPrimary )
	       {
		 if( strlen( alias_y ) == 0 || stricmp( alias_y, variableName ) == 0 )
		   strcpy( alias_y, variableName );
		 else
		   {
		     if( stricmp( alias_y, variableName ) != 0 )
		       {
			 char buf[256];
			 sprintf( buf, 
				  "The variable name assigned to y is not consistent! old(%s), new(%s)", 
				  alias_y, variableName );
			 exit( error( buf ) );
		       }
		   }
		 
                 // This was redandant, though with this it was possible to
		 // do dataSetsForAllIndividuals[i]["primary"] properly.
		 /*
		   dataSetsForAllIndividuals[order].insert( pair<const char*, valarray<double> >
					       ( alias_y, dependent_data ) );
		 */
		 assert( stricmp( alias_y, variableName ) == 0 );

		 //
		 // Mark that, for this individual, a dependent variable is given.
		 //
		 yGiven = true;
	       }
	     
	     //
	     // Back to <dependent> as current node for the tree walker.
	     //
	     walker->setCurrentNode( walker->parentNode() ); // Back to <dependent> as current node.
	     assert( XMLString::equals(walker->getCurrentNode()->getNodeName(), X("dependent")));
	   }
	
	 //
	 // <attribute> contains a scalar variable that is seen as an attribute/trait/steady-state
	 // of this particular individual which does not vary over time or some other 
	 // parameters.
	 //
	 else if( stricmp( variableType, "attribute" ) == 0 )
	   {
	     //
	     // Set <attribute> as the current node for the tree walker.
	     //
	     DOMNode * valueNode = walker->firstChild();
	     assert( valueNode != NULL );
	     assert( XMLString::equals( valueNode->getNodeName(), X("value") ) );
	     walker->setCurrentNode( valueNode );

	     //
	     // An attribute is a scalar value.  So, size the vector to one.
	     //
	     valarray<double> attribute(1);
	     double value = atof( trim( valueNode->getFirstChild()->getNodeValue() ) );
	     attribute[0] = value;

	     //
	     // Register the attribute value and name into the map.
	     //
	     dataSetsForAllIndividuals[order].insert( 
	           pair<const char*, valarray<double> >( variableName, attribute ) );

	     //
	     // Back to <attribute> as current node for the tree walker.
	     walker->setCurrentNode( walker->parentNode() );
	     assert( XMLString::equals(walker->getCurrentNode()->getNodeName(), X("attribute")));
	   }
	 else
	   {
	     //
	     // This cannot happen!  In debug configuration, an earlier assertion will blow up
	     // the execution, but this block is to ensure blowing up in release configuration too.
	     //
	     char buf[128];
	     sprintf( buf, "Unknown data type: %s\n", variableType );
	     exit( error( buf ) );
	   }
	
	 //
	 // Move to a next data set for this individual.  
	 // Whether it exists or not will be tested at the top of this while loop.
	 //
         variableNode = walker->nextSibling();
      }
      //
      // Must have a (primary) dependent variable used as y.
      //
      assert( yGiven );
      
      //
      // Back to <individual>.
      //
      walker->setCurrentNode( walker->parentNode() );
      assert( XMLString::equals(walker->getCurrentNode()->getNodeName(), X("individual")) );
      data_setNode = walker->nextSibling();
    }
  assert( n == nIndividuals );

  isDataProcessed = true;
  walker->setCurrentNode( walker->parentNode() );


  table->spkSymbols->measurementsAll.resize( table->spkSymbols->nMeasurementsAll.sum() );
  //
  // This is where all the individuals' y are assembled into a single vector, measurementsAll.
  // 
  // REVISIT - Sachiko
  // It is a mistery, why dataSetsForAllIndividuals[i][alias_y] does not
  // return a vector associated with the key.  It does not find the entry!
  // That's why it's done this clumsy.
  // 
  for( int i=0, offset=0; i<n; i++ )
    {
      int m = table->spkSymbols->nMeasurementsAll[i];
     
      DataSetMap::const_iterator itr = dataSetsForAllIndividuals[i].begin();
      for( DataSetMap::const_iterator itr = dataSetsForAllIndividuals[i].begin(); 
	   itr != dataSetsForAllIndividuals[i].end(); itr++ )
	{
	  const char * name = itr->first;
	  if( stricmp( name, alias_y ) != 0 )
	    continue;
	  valarray<double> data = itr->second;
	  for( int k=0; k<data.size(); k++ )
	    {
	      table->spkSymbols->measurementsAll[ offset + k ] = data[k];
	    }
	}
        offset += m;
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
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMTreeWalker * walker = doc->createTreeWalker( driverTree, DOMNodeFilter::SHOW_ELEMENT, NULL, false );

  int nIndividuals;
  if( !isDataProcessed )
    {
      DOMNode * dataTree = doc->getElementsByTagName( X("data") )->item(0);
      assert( dataTree != NULL );
      nIndividuals = ( dynamic_cast<DOMElement*>(dataTree)
		       ->getElementsByTagName( X("individual") ) )->getLength();
    }
  else
    nIndividuals = table->spkSymbols->nIndividuals;

  //
  // Get the first child under <driver> tree, which is one of the followings:
  // <poppar>, <indpar>, <statistics> or <optimization>
  //
  DOMNode* category = walker->firstChild();
  assert( category != NULL );
  walker->setCurrentNode( category );
  while( category != NULL )
    {      
      const char * tagname = C( category->getNodeName() );

      //
      // <poppar> section contains an initial estimate, boundary values and 
      // step values used for finite-differencing for the population parameter
      // vector.
      //
      if( stricmp( tagname, "poppar" ) == 0 )
	{
	  //
	  // First determine the length of this parameter.
	  //
	  DOMNamedNodeMap *attrs = category->getAttributes();
	  int nPopPar = atoi( C( attrs->getNamedItem( X( "length" ) )->getNodeValue() ) );

	  //
	  // Resize vectors that are in "table->spkSybmols" FitParameters data structure,
	  // which will be used as place holders for values found in the <driver> tree.
	  //
	  table->spkSymbols->popParIn.  resize( nPopPar );
	  table->spkSymbols->popParLow. resize( nPopPar );
	  table->spkSymbols->popParUp.  resize( nPopPar );
	  table->spkSymbols->popParStep.resize( nPopPar );
	  
	  //
	  // Move to the first child of "poppar" category.
	  //
	  DOMNode * parameterNode = walker->firstChild();
     	  walker->setCurrentNode( parameterNode );
    
	  //
	  // The children of this category are: <in>, <low>, <up> and <step>.
	  //
	  while( parameterNode != NULL )
	    {
	      DOMNode * valueTagNode = walker->firstChild();
	      walker->setCurrentNode( valueTagNode );
	      const char* parameterName = trim( parameterNode->getNodeName() );
	      if( stricmp( parameterName, "in" ) == 0 )
		{		  
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char * value 
			= trim(  valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->popParIn[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "low" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char * value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->popParLow[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "up" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->popParUp[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "step" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->popParStep[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else
		{
		  char buf[128];
		  sprintf( buf, "Unknown paremeter set for popPar: %s\n", 
			   C( parameterNode->getNodeName() ) );
		  exit( error( buf ) );
		}
		parameterNode = walker->nextSibling();
	    }
	} 
      else if( stricmp( tagname, "indpar" ) == 0 )
	{
	  // This "indpar" section contains information associated with the size of the population
	  // and the mapping between data sets and individuals' identifiers.
	  // These info, which are specified either in "data" section,
	  // must be already obtained to proceed further in this section.
	  assert( isDataProcessed );
	  
	  DOMNamedNodeMap *attrs = category->getAttributes();
	  int nIndPar = atoi( C( attrs->getNamedItem( X("length" ) )->getNodeValue() ) );

	  bool isUniform 
	    =  XMLString::equals( attrs->getNamedItem( X("uniform") )->getNodeValue(), X("YES") );

	  assert( nIndividuals > 0 );
	  assert( nIndPar > 0 );
	  table->spkSymbols->indParIn.  resize( nIndPar * nIndividuals );
	  table->spkSymbols->indParLow. resize( nIndPar );
	  table->spkSymbols->indParUp.  resize( nIndPar );
	  table->spkSymbols->indParStep.resize( nIndPar );
	  
	  // Move to the first child of "indpar" category.
	  DOMNode * parameterNode = walker->firstChild();
     	  walker->setCurrentNode( parameterNode );
    
	  // The children of this category are the initial values,
	  // lower/upper boundary values and step values.
	  while( parameterNode != NULL )
	    {
	      DOMNode * valueTagNode = walker->firstChild();
	      walker->setCurrentNode( valueTagNode );
	      const char* parameterName = trim( parameterNode->getNodeName() );
	      
	      if( stricmp( parameterName, "in" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      
		      if( isUniform )
			{
			  for( int who=0; who<nIndividuals; who++ )
			    {
			      table->spkSymbols->indParIn[k+who*nIndPar] = atof( value );
			    }
			}
		      else
			{
			  ///////////////////////////////////////////////////////////////
			  // REVISIT: 06/03/03
			  // This section must be tested after implementing
			  // "process data section".
			  ///////////////////////////////////////////////////////////////
			  const char* id 
			    = C( dynamic_cast<DOMElement*>(parameterNode)->getAttribute( X("id") ) );
			  assert( id != NULL );
			  int order = IDs[ string(id) ];

			  table->spkSymbols->indParIn[k + order * nIndPar] = atof( value );
			}
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "low" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->indParLow[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "up" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->indParUp[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else if( stricmp( parameterName, "step" ) == 0 )
		{
		  for( int k=0; valueTagNode != NULL; k++ )
		    {
		      const char* value 
			= trim( valueTagNode->getChildNodes()->item(0)->getNodeValue() );
		      table->spkSymbols->indParStep[k] = atof( value );
		      valueTagNode = walker->nextSibling();
		    }
		  walker->setCurrentNode(walker->parentNode());
		}
	      else
		{
		  char buf[128];
		  sprintf( buf, "Unknown paremeter set for indPar: %s\n",
			   C( parameterNode->getNodeName() ) );
		  exit( error(buf) );
		}
		parameterNode = walker->nextSibling();
	    }
 	}
      else if( stricmp( tagname, "optimization" ) == 0 )
	{
	  DOMNode * opt_levelNode = walker->firstChild();// either "population" or "individual"
	  walker->setCurrentNode( opt_levelNode );
    
	  // The children of this category are the initial values,
	  // lower/upper boundary values and step values.
	  while( opt_levelNode != NULL )
	    {
	      const char* opt_level = trim( opt_levelNode->getNodeName() );
	      
	      if( stricmp( opt_level, "population" ) == 0 )
	      {
		DOMNode * parameterTagNode = walker->firstChild();
		walker->setCurrentNode( parameterTagNode );
		DOMNode * parameter;
		while( parameterTagNode != NULL )
		  {
		    parameter = parameterTagNode->getFirstChild();
		    const char * value 
		      = trim( parameterTagNode->getFirstChild()->getNodeValue() );
		    const char * parameterTagName =  trim( parameterTagNode->getNodeName() );
		    if( stricmp( parameterTagName, "approximation" ) == 0 )
		      {
			if( stricmp( value, "FO" ) == 0 )
			  table->spkSymbols->objective = FO;
			else if( stricmp( value, "FOCE" ) == 0 )
			  table->spkSymbols->objective = FOCE;
			else if( stricmp( value, "LAPLACE" ) == 0 )
			  table->spkSymbols->objective = LAPLACE;
			else
			  {
			    char buf[128];
			    sprintf( buf, "Unknown objective (%s)\n", value );
			    exit( error( buf ) );
			  }
		      }
		    else if( stricmp( parameterTagName, "epsilon" ) == 0 )
		      {
			table->spkSymbols->popEpsilon = atof( value );
		      }
		    else if( stricmp( parameterTagName, "mitr" ) == 0 )
		      {
			table->spkSymbols->popMaxItr = atoi( value );
		      }
		    else if( stricmp( parameterTagName, "trace" ) == 0 )
		      {
			table->spkSymbols->popTrace = atoi( value );
		      }
		    else if( stricmp( parameterTagName, "restart" ) == 0 )
		      {
			table->spkSymbols->isPopWarmStart 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else if( stricmp( parameterTagName, "poppar_out" ) == 0 )
		      {
			table->spkSymbols->isPopParOut 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else if( stricmp( parameterTagName, "objective_out" ) == 0 )
		      {
			table->spkSymbols->isPopObjOut 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else if( stricmp( parameterTagName, "deriv1_out" ) == 0 )
		      {
			table->spkSymbols->isPopObj_popParOut 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else if( stricmp( parameterTagName, "deriv2_out" ) == 0)
		      {
			table->spkSymbols->isPopObj_popPar_popParOut 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else
		      {
			char buf[128];
			sprintf( buf, "Unknown optimization (population) paremeter: %s\n", 
				 C( parameterTagNode->getNodeName() ));
			exit( error( buf ) );
		      }
		    parameterTagNode = walker->nextSibling();
		  }
	      }
	      else if( stricmp( opt_level, "individual" ) == 0 )
	      {
		DOMNode * parameterTagNode = walker->firstChild();
		walker->setCurrentNode( parameterTagNode );
		DOMNode * parameter;
		while( parameterTagNode != NULL )
		  {
		    parameter = parameterTagNode->getFirstChild();
		    const char * value = trim( parameter->getNodeValue() );
		    const char * parameterTagName = trim( parameterTagNode->getNodeName() );
		    if( stricmp( parameterTagName, "epsilon" ) == 0 )
		      {
			table->spkSymbols->indEpsilon = atof( value );
		      }
		    else if( stricmp( parameterTagName, "mitr" ) == 0 )
		      {
			table->spkSymbols->indMaxItr = atoi( value );
		      }
		    else if( stricmp( parameterTagName, "trace" ) == 0 )
		      {
			table->spkSymbols->indTrace = atoi( value );
		      }
		    else if( stricmp( parameterTagName, "restart" ) == 0 )
		      {
			table->spkSymbols->isIndWarmStart 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    else if( stricmp( parameterTagName, "indpar_out" ) == 0 )
		      {
			table->spkSymbols->isIndParAllOut 
			  = (stricmp( value, "YES" ) == 0? true : false );
		      }
		    parameterTagNode = walker->nextSibling();
		  }
	      }
	      else
		{
		  char buf[128];
		  sprintf( buf, "Unknown optimization level: %s\n", opt_level );
		  exit( error( buf ) );
		}
	      walker->setCurrentNode( walker->parentNode() );
	      opt_levelNode = walker->nextSibling();
	    }
 	}
      else if( stricmp( tagname, "statistics" ) == 0 )
        {
	  DOMNode * output_yes_noNode = walker->firstChild();
	  walker->setCurrentNode( output_yes_noNode );
    
	  // The children of this category are the initial values,
	  // lower/upper boundary values and step values.
	  while( output_yes_noNode != NULL )
	    {
	      const char* output_yes_no = trim( output_yes_noNode->getNodeName() );
	      if( stricmp( output_yes_no, "population" ) == 0 )
	      {
		DOMNode * parameterTagNode = walker->firstChild();
		walker->setCurrentNode( parameterTagNode );
		DOMNode * parameter;
		while( parameterTagNode != NULL )
		  {
		    parameter = parameterTagNode->getFirstChild();
		    const char * value = trim( parameter->getNodeValue() );
		    const char * parameter = trim( parameterTagNode->getNodeName() );
		    if( stricmp( parameter, "stderror") == 0 )
		      {
			table->spkSymbols->isPopStderrorOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "correlation") == 0 )
		      {
			table->spkSymbols->isPopCorrelationOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "poppar_cov") == 0 )
		      {
			table->spkSymbols->isPopCovarianceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "coefficient") == 0 )
		      {
			table->spkSymbols->isPopCoefficientOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "confidence") == 0 )
		      {
			table->spkSymbols->isPopConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    parameterTagNode = walker->nextSibling();
		  }
	      }
	      else if( stricmp( output_yes_no, "individual" ) == 0 )
	      {
		DOMNode * parameterTagNode = walker->firstChild();
		walker->setCurrentNode( parameterTagNode );
		DOMNode * parameterNode;
		while( parameterTagNode != NULL )
		  {
		    parameterNode = parameterTagNode->getFirstChild();
		    const char * value = trim( parameterNode->getNodeValue() );
		    const char * parameter = trim( parameterTagNode->getNodeName() );
		    
		    if( stricmp( parameter, "stderror" ) == 0 )
		      {
			table->spkSymbols->isIndConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "correlation") == 0 )
		      {
			table->spkSymbols->isIndConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "indpar_cov") == 0 )
		      {
			table->spkSymbols->isIndConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "coefficient") == 0 )
		      {
			table->spkSymbols->isIndConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    else if( stricmp( parameter, "confidence") == 0 )
		      {
			table->spkSymbols->isIndConfidenceOut 
			  = ( stricmp( value, "YES")==0? true : false );
		      }
		    
		    parameterTagNode = walker->nextSibling();
		  }
	      }
	      walker->setCurrentNode( walker->parentNode() );
	      output_yes_noNode = walker->nextSibling();
	    }
         }
      else
	{
	  char buf[128];
	  sprintf( buf, "Unknown tag name: %s\n", tagname );
	  exit( error( buf ) );
	}
	     
      walker->setCurrentNode( walker->parentNode() );
      category = walker->nextSibling();
    }
    return;
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
