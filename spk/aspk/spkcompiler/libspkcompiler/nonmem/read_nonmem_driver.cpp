#include <iostream>

#include "nonmem/read_nonmem_driver.h"
#include "SpkCompilerUtil.h"
#include <spk/popStatistics.h>

using namespace std;
using namespace xercesc;

static void read_pop_opt( DOMElement* driverElement,
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_ind_opt( DOMElement* driverElement,
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_pop_stat(DOMElement* driverElement,
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_ind_stat(DOMElement* driverElement,
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_theta(   DOMElement* driverElement, 
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_omega(   DOMElement* driverElement, 
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_sigma(   DOMElement* driverElement, 
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );

static void read_eta(     DOMElement* driverElement, 
			  SpkParameters& spkOut, 
			  NonmemParameters& nonmemOut );
int read_nonmem_driver( 
   DOMElement* driverNode, 
   SpkParameters & spkOut, 
   NonmemParameters& nonmemOut )
{
  assert( driverNode != NULL );
  //
  // Declare an integer placeholder for #of individuals.
  // 
  int nIndividuals = 0;

  //
  // Get a pointer to DOMTreeWalker to traverse the tree.
  // Make only DOMElement nodes visible.
  //
  DOMDocument *doc = driverNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( driverNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );
  DOMElement *driverElement = dynamic_cast<DOMElement*>( walker->firstChild() );
  
  while( driverElement != NULL )
    {
      const char * driverElementName = C( driverElement->getNodeName() );

      if( strcmp( driverElementName, "theta" ) == 0 )
	{
	  read_theta( driverElement, spkOut, nonmemOut );
	}
      else if( strcmp( driverElementName, "omega" ) == 0 )
	{
	  read_omega( driverElement, spkOut, nonmemOut );
	}
      else if( strcmp( driverElementName, "sigma" ) == 0 )
	{
	  read_sigma( driverElement, spkOut, nonmemOut );
	}
      else if( strcmp( driverElementName, "eta" ) == 0 )
	{
	  read_eta( driverElement, spkOut, nonmemOut );
	}
      else if( strcmp( driverElementName, "pop_opt" ) == 0 )
	{
	  read_pop_opt( driverElement, spkOut, nonmemOut );
          nIndividuals = spkOut.nIndividuals;
	}
      else if( strcmp( driverElementName, "ind_opt" ) == 0 )
	{
	  read_ind_opt( driverElement, spkOut, nonmemOut );
	}

      else if( strcmp( driverElementName, "pop_stat" ) == 0 )
	{
	  read_pop_stat( driverElement, spkOut, nonmemOut );
	}
      else if( strcmp( driverElementName, "ind_stat" ) == 0 )
	{
	  read_ind_stat( driverElement, spkOut, nonmemOut );
	}

      //
      // Unknown elements.
      //
      else
	{
	  char buf[128];
	  fprintf( stderr, "Unknown tag <%s>! Note that tag names are case sensitive.\n\
                   %d, %s\n",
		   driverElementName, __LINE__, __FILE__ );
	  exit( -1 );
	}
     
      driverElement = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }
    // map alpha = { theta, omega, sigma } in the order
    int nTheta = nonmemOut.thetaIn.size();
    int nOmega = nonmemOut.omegaIn.size();
    int nSigma = nonmemOut.sigmaIn.size();
    int nAlp = nTheta + nOmega + nSigma;
    spkOut.popParIn .resize( nAlp );
    spkOut.popParUp .resize( nAlp );
    spkOut.popParLow.resize( nAlp );

    spkOut.popParIn [ slice( 0, nTheta, 1 ) ] 
      = nonmemOut.thetaIn;
    spkOut.popParIn [ slice( nTheta, nOmega, 1 ) ]
      = nonmemOut.omegaIn;
    spkOut.popParIn [ slice( nTheta+nOmega, nSigma, 1 ) ] 
      = nonmemOut.sigmaIn;

    spkOut.popParUp [ slice( 0, nTheta, 1 ) ] 
      = nonmemOut.thetaUp;

    spkOut.popParLow[ slice( 0, nTheta, 1 ) ] 
      = nonmemOut.thetaLow;
    
    // map b = eta
    int nEta   = nonmemOut.etaIn.size(); 
    int nB     = nEta;
    spkOut.indParIn .resize( nB );
    spkOut.indParUp .resize( nB );
    spkOut.indParLow.resize( nB );
    spkOut.indParIn [ slice( 0, nEta, 1 ) ] 
      = nonmemOut.etaIn;
    
    //
    // REVISIT - Sachiko 09/08/03
    // How do we decide the eta boundary values?
    //
    /*
    spkOut.indParUp [ slice( 0, nEta, 1 ) ] 
      = nonmemOut.etaUp;
    spkOut.indParLow[ slice( 0, nEta, 1 ) ] 
      = nonmemOut.etaLow;
    */
 
    return nIndividuals;
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
void read_pop_opt( DOMElement* pop_optNode, 
		   SpkParameters& spkOut, 
		   NonmemParameters& nonmemOut )
{
  DOMDocument *doc = pop_optNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( pop_optNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );

  int nIndividuals = 0; 
  // approximation (fo|foce|laplace)
  assert( pop_optNode->getAttribute( X("approximation") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("approximation") ) ) > 0 );
  
  const char *approx = C(pop_optNode->getAttribute( X("approximation") ) );
  if( strcmp( approx, "fo" ) == 0 )
    spkOut.objective = FIRST_ORDER;
  else if( strcmp( approx, "foce" ) == 0 )
    spkOut.objective = EXPECTED_HESSIAN;
  else if( strcmp( approx, "laplace" ) == 0 )
    spkOut.objective = MODIFIED_LAPLACE;
  else
    {
      char buf[128];
      fprintf( stderr, "Unknown objective (%s)\n%d, %s\n", approx,
	       __LINE__, __FILE__);
      exit( -1 );
    }
  
  // pop_size
  assert( pop_optNode->getAttribute( X("pop_size") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("pop_size") ) ) > 0 );
  
  nIndividuals = atoi( C(pop_optNode->getAttribute( X("pop_size") ) ) );
  assert( nIndividuals > 0 );
  spkOut.nIndividuals = nIndividuals;
  
  // epsilon (>0.0)
  assert( pop_optNode->getAttribute( X("epsilon") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("epsilon") ) ) > 0 );
  double epsilon = atof( C(pop_optNode->getAttribute( X("epsilon") ) ) );
  assert( epsilon >= 0.0 );
  spkOut.popEpsilon = epsilon;
  
  // mitr (>=0)
  assert( pop_optNode->getAttribute( X("mitr") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("mitr") ) ) > 0 );
  int mitr = atoi( C(pop_optNode->getAttribute( X("mitr") ) ) );
  assert( mitr >= 0 );
  spkOut.popMaxItr = mitr;
  
  // trace (0-5)
  assert( pop_optNode->getAttribute( X("trace") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("trace") ) ) > 0 );
  int trace 
    = atoi( C(pop_optNode->getAttribute( X("trace") ) ) );
  assert( trace >= 0 && trace <= 5 );
  spkOut.popTrace = trace;
  
  // restart (yes|no)
  assert( pop_optNode->getAttribute( X("restart") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("restart") ) ) > 0 );
  bool isRestart = ( strcmp( C(pop_optNode->getAttribute( X("restart") ) ), "yes" ) == 0 );
  spkOut.isPopWarmStart = isRestart;
  
  // par_out (yes|no)
  assert( pop_optNode->getAttribute( X("par_out") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("par_out") ) ) > 0 );
  bool isParOut 
    = ( strcmp( C(pop_optNode->getAttribute( X("par_out") ) ), "yes" ) == 0 );
  spkOut.isPopParOut = isParOut;
  
  // obj_out (yes|no)
  assert( pop_optNode->getAttribute( X("obj_out") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("obj_out") ) ) > 0 );
  bool isPopObjOut = ( strcmp( C(pop_optNode->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
  spkOut.isPopObjOut = isPopObjOut;
  
  // deriv1_out (yes|no)
  assert( pop_optNode->getAttribute( X("deriv1_out") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("deriv1_out") ) ) > 0 );
  bool isPopObj_popParOut = ( strcmp( C(pop_optNode->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
  spkOut.isPopObj_popParOut = isPopObj_popParOut;
  
  // deriv2_out (yes|no) 
  assert( pop_optNode->getAttribute( X("deriv2_out") ) != NULL );
  assert( XMLString::stringLen(pop_optNode->getAttribute( X("deriv2_out") ) ) > 0 );
  bool isPopObj_popPar_popParOut = ( strcmp( C( 
					       pop_optNode->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
  spkOut.isPopObj_popPar_popParOut = isPopObj_popPar_popParOut;
}

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
void read_theta( DOMElement* thetaNode,
		 SpkParameters& spkOut, 
		 NonmemParameters& nonmemOut )
{
  assert( thetaNode != NULL );
  DOMDocument *doc = thetaNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( thetaNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );
  //
  // <theta length CDATA #REQUIRED>
  //
  assert( XMLString::stringLen( thetaNode
				->getAttribute( X( "length" ) ) ) > 0 );
  int len = 0;
  len = atoi( C( thetaNode->getAttribute( X( "length" ) ) ) );
  assert( len > 0 );
  
  DOMElement * inNode = dynamic_cast<DOMElement*>( walker->firstChild() );
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
	  
	  DOMElement * valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
	  assert( valueNode != NULL );
	  int cnt;
	  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
	    {
	      bool isFixed 
		= strcmp( C( valueNode->getAttribute( X( "fixed" ) ) ), "yes" )==0;
	      double value 
		= atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
	      
	      nonmemOut.thetaIn[cnt]    = value;
	      nonmemOut.thetaFixed[cnt] = isFixed;
	      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
	    }	  
	  assert( cnt == len );
	}
      //
      // <low (value)+>
      // <value>
      //
      else if( strcmp( C( inNode->getNodeName() ), "low" ) == 0 )
	{
	  nonmemOut.thetaLow.resize( len );
	  DOMElement* valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
	  assert( valueNode != NULL );
	  int cnt;
	  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
	    {
	      double value 
		= atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
	      
	      nonmemOut.thetaLow[cnt] = value;
	      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
	    }	
	  assert( cnt == len );
	}
      //
      // <up (value)+>
      // <value>
      //
      else if( strcmp( C( inNode->getNodeName() ), "up" ) == 0 )
	{
	  nonmemOut.thetaUp.resize( len );
	  DOMElement* valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
	  assert( valueNode != NULL );
	  int cnt;
	  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
	    {
	      double value 
		= atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
	      
	      nonmemOut.thetaUp[cnt] = value;
	      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
	    }	
	  assert( cnt == len );
	}
      else
	{
	  char buf[128];
	  fprintf( stderr, "Unknown tag <%s>! Note that tag names are case sensitive.\n \
                                    %d, %s\n",
		   thetaNode->getNodeName(), __LINE__, __FILE__ );
	  exit(-1);
	}
      walker->parentNode();
      inNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
    } 
}

//
// <omega dimension="xxx" struct(diagonal|block)>
//        <in fixed=(yes|no)>
//            <value>xxx</value>
//            <value>xxx</value>
//            ...
//        </in>
void read_omega( DOMElement* omegaNode,
		 SpkParameters& spkOut, 
		 NonmemParameters& nonmemOut )
{
  DOMDocument *doc = omegaNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( omegaNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );

  //
  // <omega dimension CDATA #REQUIRED>
  //
  assert( XMLString::stringLen(omegaNode->getAttribute( X( "dimension" ) ) ) > 0 );
  int dimension = atoi( C( omegaNode->getAttribute( X( "dimension" ) ) ) );
  assert( dimension > 0 );
  
  //
  // <omega struct (diagonal|block) #REQUIRED>
  //
  bool isDiag = ( strcmp( C( omegaNode->getAttribute( X( "struct" ) ) ),
			  "diagonal" ) == 0 );
  if( !isDiag )
    {
      assert( strcmp( C( omegaNode->getAttribute( X( "struct" ) ) ), 
		      "block" ) == 0 );
    }
  int dimensions = (isDiag? dimension : dimension*(dimension+1)/2 );
  nonmemOut.omegaIn.resize( dimensions );
  nonmemOut.omegaFixed.resize( dimensions );
  
  //
  // <in (value)+>
  // <value fixed (yes|no) #FIXED "no">
  //
  DOMElement * inNode = dynamic_cast<DOMElement*>( walker->firstChild() );
  assert( inNode != NULL );
  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
  
  DOMElement* valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
  int cnt;
  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
    {
      bool isFixed = strcmp( C( valueNode->getAttribute( X( "fixed" ) ) ), "yes" ) == 0;
      double value = atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
      
      nonmemOut.omegaIn[cnt]    = value;
      nonmemOut.omegaFixed[cnt] = isFixed;
      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }	
  assert( cnt == dimensions );

}

//
// <sigma dimension="xxx" struct=(diagonal|block)>
//    <in fixed=(yes|no)>
//       <value>xxx</value>
//       <value>xxx</value>
//    </in>
// </sigma>
//
void read_sigma( DOMElement* sigmaNode,
		 SpkParameters& spkOut, 
		 NonmemParameters& nonmemOut )
{
  DOMDocument *doc = sigmaNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( sigmaNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );
  
  //
  // <simga dimension CDATA #REQUIRED>
  //
  assert( XMLString::stringLen(sigmaNode->getAttribute( X( "dimension" ) ) ) > 0 );
  int dimension = atoi( C( sigmaNode->getAttribute( X( "dimension" ) ) ) );
  assert( dimension > 0 );
  
  //
  // <sigma struct (diagonal|block) #REQUIRED>
  //
  bool isDiag = ( strcmp( C( sigmaNode->getAttribute( X( "struct" ) ) ),
			  "diagonal" ) == 0 );
  if( !isDiag )
    {
      assert( strcmp( C( sigmaNode->getAttribute( X( "struct" ) ) ), 
		      "block" ) == 0 );
    }
  
  int dimensions = (isDiag? dimension : dimension*(dimension+1)/2 );
  nonmemOut.sigmaIn.resize( dimensions );
  nonmemOut.sigmaFixed.resize( dimensions );
  
  //
  // <in (value)+>
  // <value fixed (yes|no) #FIXED "no">
  //
  DOMNode * inNode = walker->firstChild();
  assert( inNode != NULL );
  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
  
  DOMElement* valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
  int cnt;
  for( cnt=0; cnt < dimensions, valueNode != NULL; cnt++ )
    {
      bool isFixed 
	= strcmp( C( valueNode->getAttribute( X( "fixed" ) ) ), "yes" )==0;
      double value 
	= atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
      
      nonmemOut.sigmaIn[cnt]    = value;
      nonmemOut.sigmaFixed[cnt] = isFixed;
      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }	  
  assert( cnt == dimensions );
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
void read_eta( DOMElement* etaNode,
		 SpkParameters& spkOut, 
		 NonmemParameters& nonmemOut )
{
  DOMDocument *doc = etaNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( etaNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );
  
  //
  // <eta length CDATA #REQUIRED>
  //
  assert( XMLString::stringLen(etaNode->getAttribute( X( "length" ) ) ) > 0 );
  int len = atoi( C( etaNode->getAttribute( X( "length" ) ) ) );
  assert( len > 0 );
  
  Symbol eta( "eta", Symbol::VECTOR, Symbol::DOUBLE, true );
  eta.size( len );
  
  nonmemOut.etaIn.resize( len );
  nonmemOut.etaFixed.resize( len );
  
  //
  // <in (value)+>
  // <value fixed (yes|no) #FIXED "no">...</value>
  //
  DOMElement * inNode = dynamic_cast<DOMElement*>( walker->firstChild() );
  assert( inNode != NULL );
  assert( strcmp( C( inNode->getNodeName() ), "in" ) == 0 );
  
  DOMElement* valueNode = dynamic_cast<DOMElement*>( walker->firstChild() );
  int cnt;
  for( cnt=0; cnt < len, valueNode != NULL; cnt++ )
    {
      bool isFixed = strcmp( C( valueNode->getAttribute( X( "fixed" ) ) ), "yes" )==0;
      double value = atof( C( trim( valueNode->getFirstChild()->getNodeValue() ) ) );
      
      nonmemOut.etaIn[cnt]    = value;
      nonmemOut.etaFixed[cnt] = isFixed; 
      valueNode = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }	  
  assert( cnt == len );
}
void read_ind_opt( DOMElement* ind_optNode, 
		   SpkParameters& spkOut, 
		   NonmemParameters& nonmemOut )
{
  DOMDocument *doc = ind_optNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( ind_optNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );

  // epsilon (>0.0)
  assert( ind_optNode->getAttribute( X("epsilon") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("epsilon") ) ) > 0 );
  double epsilon = atof( C(ind_optNode->getAttribute( X("epsilon") ) ) );
  assert( epsilon >= 0.0 );
  spkOut.indEpsilon = epsilon;
  
  // mitr (>=0)
  assert( ind_optNode->getAttribute( X("mitr") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("mitr") ) ) > 0 );
  int mitr = atoi( C(ind_optNode->getAttribute( X("mitr") ) ) );
  assert( mitr >= 0 );
  spkOut.indMaxItr = mitr;
  
  // trace (0-5)
  assert( ind_optNode->getAttribute( X("trace") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("trace") ) ) > 0 );
  int trace = atoi( C(ind_optNode->getAttribute( X("trace") ) ) );
  assert( trace >= 0 && trace <= 5 );
  spkOut.indTrace = trace;
  
  // restart (yes|no)
  assert( ind_optNode->getAttribute( X("restart") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("restart") ) ) > 0 );
  bool isRestart = ( strcmp( C(ind_optNode->getAttribute( X("restart") ) ), "yes" ) == 0 );
  spkOut.isIndWarmStart = isRestart;
  
  // par_out (yes|no)
  assert( ind_optNode->getAttribute( X("par_out") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("par_out") ) ) > 0 );
  bool isParOut = ( strcmp( C(ind_optNode->getAttribute( X("par_out") ) ), "yes" ) == 0 );
  spkOut.isIndParOut = isParOut;
  
  // obj_out (yes|no)
  assert( ind_optNode->getAttribute( X("obj_out") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("obj_out") ) ) > 0 );
  bool isIndObjOut = ( strcmp( C(ind_optNode->getAttribute( X("obj_out") ) ), "yes" ) == 0 );
  spkOut.isIndObjOut = isIndObjOut;
  
  // deriv1_out (yes|no)
  assert( dynamic_cast<DOMElement*>(ind_optNode)
	  ->getAttribute( X("deriv1_out") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("deriv1_out") ) ) > 0 );
  bool isIndObj_indParOut = ( strcmp( C(ind_optNode->getAttribute( X("deriv1_out") ) ), "yes" ) == 0 );
  spkOut.isIndObj_indParOut = isIndObj_indParOut;
  
  // deriv2_out (yes|no) 
  assert( ind_optNode->getAttribute( X("deriv2_out") ) != NULL );
  assert( XMLString::stringLen(ind_optNode->getAttribute( X("deriv2_out") ) ) > 0 );
  bool isIndObj_indPar_indParOut = ( strcmp( C( 
					       ind_optNode->getAttribute( X("deriv2_out") ) ), "yes" ) == 0 );
  spkOut.isIndObj_indPar_indParOut = isIndObj_indPar_indParOut;
}
void read_pop_stat(DOMElement* pop_statNode,
		   SpkParameters& spkOut, 
		   NonmemParameters& nonmemOut )
{
  DOMDocument *doc = pop_statNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( pop_statNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );

  // covariance formulation {RSR, R, S}
  assert( pop_statNode->getAttribute( X("covariance_form") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("covariance_form") ) ) > 0 );
  enum PopCovForm cov;
  const char * cov_str = C( pop_statNode->getAttribute( X("covariance_form") ) );
  if( strcmp( cov_str, "rsr" ) == 0 )
	  cov = RSR;
  else if( strcmp( cov_str, "r" ) == 0 )
	  cov = R;
  else if( strcmp( cov_str, "s" ) == 0 )
	  cov = S;
  else
  {
	  fprintf( stderr, "Invalid covariance form <%s> specified!!!\n", cov_str );
          abort();	  
  }
  
  // stderror (yes|no)
  assert( pop_statNode->getAttribute( X("stderror") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("stderror") ) ) > 0 );
  bool isStderrorOut = ( strcmp( C( 
				   pop_statNode->getAttribute( X("stderror") ) ), "yes" ) == 0 );
  spkOut.isPopStderrorOut = isStderrorOut;
  
  // correlation (yes|no)
  assert( pop_statNode->getAttribute( X("correlation") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("correlation") ) ) > 0 );
  bool isCorrelationOut = ( strcmp( C( 
				      pop_statNode->getAttribute( X("correlation") ) ), "yes" ) == 0 );
  spkOut.isPopCorrelationOut = isCorrelationOut;
  
  // cov (yes|no)
  assert( pop_statNode->getAttribute( X("cov") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("cov") ) ) > 0 );
  bool isCovOut = ( strcmp( C( 
			      pop_statNode->getAttribute( X("cov") ) ), "yes" ) == 0 );
  spkOut.isPopCovarianceOut = isCovOut;
  
  // coefficient (yes|no)
  assert( pop_statNode->getAttribute( X("coefficient") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("coefficient") ) ) > 0 );
  bool isCoefficientOut = ( strcmp( C(
				      pop_statNode->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
  spkOut.isPopCoefficientOut = isCoefficientOut;
  
  // confidence (yes|no)
  assert( pop_statNode->getAttribute( X("confidence") ) != NULL );
  assert( XMLString::stringLen(pop_statNode->getAttribute( X("confidence") ) ) > 0 );
  bool isConfidenceOut = ( strcmp( C( 
				     pop_statNode->getAttribute( X("confidence") ) ), "yes" ) == 0 );
  spkOut.isPopConfidenceOut = isConfidenceOut; 
}

void read_ind_stat(DOMElement* ind_statNode,
		   SpkParameters& spkOut, 
		   NonmemParameters& nonmemOut )
{
  DOMDocument *doc = ind_statNode->getOwnerDocument( );
  DOMTreeWalker * walker = doc->createTreeWalker( ind_statNode, DOMNodeFilter::SHOW_ELEMENT, NULL, false  );
  
  // stderror (yes|no)
  assert( ind_statNode->getAttribute( X("stderror") ) != NULL );
  assert( XMLString::stringLen(ind_statNode->getAttribute( X("stderror") ) ) > 0 );
  bool isStderrorOut = ( strcmp( C( 
				   ind_statNode->getAttribute( X("stderror") ) ), "yes" ) == 0 );
  spkOut.isIndStderrorOut = isStderrorOut;
  
  // correlation (yes|no)
  assert( ind_statNode->getAttribute( X("correlation") ) != NULL );
  assert( XMLString::stringLen(ind_statNode->getAttribute( X("correlation") ) ) > 0 );
  bool isCorrelationOut = ( strcmp( C( 
				      ind_statNode->getAttribute( X("correlation") ) ), "yes" ) == 0 );
  spkOut.isIndCorrelationOut = isCorrelationOut;
  
  // cov (yes|no)
  assert( ind_statNode->getAttribute( X("cov") ) != NULL );
  assert( XMLString::stringLen(ind_statNode->getAttribute( X("cov") ) ) > 0 );
  bool isCovOut = ( strcmp( C( 
			      ind_statNode->getAttribute( X("cov") ) ), "yes" ) == 0 );
  spkOut.isIndCovarianceOut = isCovOut;
  
  // coefficient (yes|no)
  assert( ind_statNode->getAttribute( X("coefficient") ) != NULL );
  assert( XMLString::stringLen(ind_statNode->getAttribute( X("coefficient") ) ) > 0 );
  bool isCoefficientOut = ( strcmp( C( 
				      ind_statNode->getAttribute( X("coefficient") ) ), "yes" ) == 0 );
  spkOut.isIndCoefficientOut = isCoefficientOut;
  
  // confidence (yes|no)
  assert( ind_statNode->getAttribute( X("confidence") ) != NULL );
  assert( XMLString::stringLen(ind_statNode->getAttribute( X("confidence") ) ) > 0 );
  bool isConfidenceOut = ( strcmp( C( 
				     ind_statNode->getAttribute( X("confidence") ) ), "yes" ) == 0 );
  spkOut.isIndConfidenceOut = isConfidenceOut;
}
