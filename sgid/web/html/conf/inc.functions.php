<?

/***************************************************************************
 * PEAR ERROR HANDLING
 *
 ***************************************************************************/
//PEAR::setErrorHandling ( PEAR_ERROR_DIE );
// PEAR::setErrorHandling ( PEAR_ERROR_TRIGGER );
// PEAR::setErrorHandling ( PEAR_ERROR_PRINT );


/***************************************************************************
 * INCLUDE LIBRARIES
 *
 ***************************************************************************/
require_once("MDB2.php");
require_once("XML/Serializer.php");
require_once("XML/Unserializer.php");
require_once("inc.errors.php");
require_once("inc.config.php");
require_once("Mail.php");

/****************************************************************************
 * GLOBAL VARIABLES
 *
 ***************************************************************************/
$error_list        = array();
$steps             = array("Input Data", "Review Data", "Processing");
$max_steps         = sizeof($steps);
$special_key       = "012af024ab-05";


/***************************************************************************
 * Open global database connection
 *
 ***************************************************************************/
$db =& MDB2::connect($GLOBALS['OPTIONS']['DSN']);

if (PEAR::isError($db)) {
  add_error("DB", $db->getMessage());
  die($db->getMessage());
 }

$db->setFetchMode(MDB2_FETCHMODE_OBJECT);



/***************************************************************************
 * FUNCTIONS
 *
 ***************************************************************************/
function SGID_identify ( $equations, $email_address, $seed=NULL  ) {
$TDATA = array( 'input_eq' => array(),
		'out_eq' => array(),
		'alg_eq' => array(),
		'parameter_list' => array(),
		'inputs' => array(),
		'outputs' => array()
		);

 $operators = "*/+-";
 $delims = "\r\n";
 
 $I_elements = array();
 
 $not_params = array("()","Y");

 // check random seed
 if ( $seed == NULL ) {
   $seed = rand($GLOBALS['OPTIONS']['seed_min'], $GLOBALS['OPTIONS']['seed_max']);
 }

 clean_equations ( $equations );

 
 // tokenize the whole equation input data
  $tok = strtok( $equations, $delims );
  while ( $tok !== false ) {
    $toks[] = $tok; 
    $tok = strtok($delims);
  }
  
  // loop through the equations
  $i = 0;
  while ( each ($toks) ) {
    
    #echo "Found: $toks[$i] <br />\n";

    // first determine whether there are any function calls within the code
    // and add the function names to the not_parameters array.
    $regexp = "/([A-Z0-9]+)\(/";
    
    preg_match_all( $regexp, $toks[$i], $full_matches, PREG_PATTERN_ORDER );
    $matches = $full_matches[1];
      
    foreach ( $matches as $val ) {
      $not_params[] = $val;
    }

    $_SESSION['not_params_ernst'] = $not_params;

    // if the equation ends with an operator, this not not allowed.
    if ( eregi ("[" . $operators . "]$", $toks[$i], $regs ))
      {
	add_error('parse','The following equation must not end in an operator: ' . $toks[$i]);
      }
    elseif ( eregi ("^(A[1-9][0-9]{0,2})\[T\][ ]{0,10}\=(.*)$", $toks[$i], $regs ) ) {
      
      // add equation name to list of not_params
      $not_params[] = $regs[1];
      $elements = $regs[2];
      
      isolate_elements( $elements, $I_elements, $not_params );
      
      $TDATA['input_eq'][] = $toks[$i];
      if ( $GLOBALS['OPTIONS']['debug'] >0 ) echo "  is an input equation<br />\n"; 
    }
    elseif ( eregi ("^(Y.*)\=(.*)$", $toks[$i], $regs )) {
      // if the equation starts with Y it is an output equation.
      $ae = $regs[2];
      $not_params[] = $regs[1];
      $TDATA['outputs'][] = $regs[1];

      isolate_elements( $ae, $I_elements, $not_params);

      $TDATA['out_eq'][] = $toks[$i];
      if ( $GLOBALS['OPTIONS']['debug'] > 0 ) echo "  is an output equation<br />\n";
    }    elseif ( eregi ("^([A-Z][A-Z0-9]{0,100})[ ]{0,10}\=(.*)$", $toks[$i], $regs ) ) {
      $ae = $regs[2];

      // we are looking at assignments of algebraic equations.
      $not_params[] = $regs[1];

      isolate_elements( $ae, $I_elements, $not_params );

      // store the equivalents in the data structure.
      $TDATA['alg_eq'][] = $toks[$i];
      if ( $GLOBALS['OPTIONS']['debug'] >0 ) echo "  is an algebraic equation<br />\n";
    }

    else {
      add_error('parse_unknown', "<em>" . $toks[$i] . "</em>");
    }

    $i++;
  }

  // store the elements in TDATA for other use
  $TDATA['elements'] = $I_elements;

  // perform algebraic equation substitutions.
  SGID_replaceEquivalent( $TDATA );

  // look at all the elements and build an array of inputs
  foreach ( $I_elements as $element ) {
    
    // are we an input, otherwise known as { U1, U2, U3, ... }
    if ( eregi("^U[1-9]{0,1}[0-9]{0,3}$", $element) ) {
      $TDATA['inputs'][]= $element;
      $not_params[] = $element;
    }

    // determine whether this is a parameter or not
    elseif ( in_array( $element, $not_params ) === FALSE 
	     && $element != NULL
	     && eregi("^[A-Z]+[A-Z1-9]{0,1}[A-Z0-9]{0,100}$", $element)
	     ) {
      //	     && !eregi("[^A-Z]+[1-9]{0,1}[0-9]+", $element ) ) {
            $TDATA['parameter_list'][] = $element;
	    $not_params[] = $element;
    }
    
  }

  if ( !$GLOBALS['OPTIONS']['debug'] ) echo "<pre>inside SGID_identify\n" . var_dump($TDATA) . "</pre>\n";

  remove_duplicates( $TDATA );
  natsort_array( $TDATA );
  error_check ( $TDATA );

  // send TDATA back to _SESSION
  $_SESSION['TDATA_web'] = $TDATA;

  if ( $GLOBALS['OPTIONS']['debug'] )
    {
      echo "There are " . sizeof($TDATA['parameter_list']) . " parameters\n";
      var_dump($TDATA);
      echo "</pre>";
    }
}

function remove_duplicates ( &$TDATA ) {
  /* remove_duplicates traverses the array structure and
   * removes the duplicate values found in the equations
   * 
   * takes:  an arbitrary array
   * status: reusable
   */
  
  foreach ( $TDATA as $key => $val ) {
    $TDATA[$key] = array_unique($TDATA[$key]);
  }
  
  if ( $GLOBALS['OPTIONS']['debug'] ) echo "<pre>inside remove_duplicates\n" . var_dump($TDATA) . "</pre>\n";

}


function natsort_array ( &$TDATA ) {
  /* This function was written because sort() doesn't have an option
   * to do natural language sorting, and natsort() preserves the
   * array indexes/keys and for the purpose of this program we
   * need for the data structure to maintain U1, U2, U10 in array
   * indexes 0, 1 and 2 
   *
   * takes:  an arbitrary array of arrays.
   * status: reusable
   *
   * note:   this destroys the original array and re-creates it.
   */

  $new_arr = array();
  $tmp_arr = array();
  
  foreach ( $TDATA as $key => $val ) {
    //    echo "seen $key / $val<br>";
    
    $tmp_arr = $TDATA[$key];

    natsort($tmp_arr);
    foreach ( $tmp_arr as $ta_key => $ta_val ) {
      $new_arr[$key][] = $ta_val;
    }
  }

  $TDATA = $new_arr;
}


function error_check( &$TDATA ) {
  /* error_check goes through the TDATA structure and looks for errors
   * with user input.
   *
   * takes:   relies on global variable TDATA.
   * status:  application-specific, only for TDATA structure
   *
   * note:    add_error is called from within, which updates the global
   *          error_list array.
   */

  // ERROR CHECKING BEGINS
  
  // error check the inputs to determine if the user has consecutively numbered inputs
  //  if not, throw error requiring them to be numbered { U1, U2, U3, .., Un }
  
  if ( !(!array_search ( "**", $TDATA['input_eq'] ) || !array_search ( "**", $TDATA['out_eq']) || !array_search ( "**", $TDATA['alg_eq'] )) )
    {
      add_error('parse_exponent');
    }


  if ( sizeof($TDATA['inputs']) == 1 ) {
    if ( array_search ( "U", $TDATA['inputs'] ) === FALSE ) {
      add_error('parse_in_Umissing', $TDATA['inputs'][0]);
    }
  }
  elseif ( sizeof($TDATA['inputs']) > 1 ) {
    // check to see if there is a lone "U" and a "U1" element
    if ( array_search ( "U", $TDATA['inputs'] ) !== FALSE ) {
      add_error('parse_in_mismatch');
    }
    elseif ( array_search( "U1", $TDATA['inputs'] ) === FALSE ) {
      add_error('parse_in_U1missing');
    }
    
    for ( $count = 1; $count <= sizeof($TDATA['inputs']);  $count++ ) {
      $regex = "^U" . $count;
      if ( !eregi ( $regex, $TDATA['inputs'][$count-1] ) )
	if ( $count == 1 ) 
	  { add_error('parse_in_U1missing', "Input " . $TDATA['inputs'][$count-1] . " is listed as your starting input"); }
	else 
	  { add_error('parse_in_consecutive', "Input " . $TDATA['inputs'][$count-1] . " is not consecutive with " . $TDATA['inputs'][$count-2]); }
    }
    
  }
  else
    { add_error('parse_in', "You have not defined any inputs."); }
  
  // validate the outputs (bug 756)
  if ( sizeof($TDATA['outputs']) == 1 ) {
    if ( array_search ( "Y", $TDATA['outputs'] ) === FALSE ) {
      add_error('parse_out_Ymissing');
    }
  }
  elseif ( sizeof($TDATA['outputs']) > 1 ) {
    if ( array_search ( "Y", $TDATA['outputs'] ) !== FALSE ) {
      add_error('parse_out_Ymulti');
    }
    
    for ( $count = 1; $count <= sizeof($TDATA['outputs']); $count++ ) {
      $regex = "^Y" . $count;
      if ( !eregi ( $regex, $TDATA['outputs'][$count-1] ) ) {
	if ( $count == 1 )
	  { add_error('parse_out_Ymulti', "I was expecting 'Y1' and found '" . $TDATA['outputs'][$count-1] . " listed as your first output."); }
	else 
	  { add_error('parse_out_Ymulti', "Output " . $TDATA['outputs'][$count-1] . " is not consecutive with " . $TDATA['outputs'][$count-2]); }
      }
    }
  }
  else
    { add_error('parse_out', "You have not defined any outputs."); }
    

  // fix for bug #823
  if ( sizeof($TDATA['parameter_list']) < 1 ) {
    add_error('parse', "You have not defined any parameters.  Please check your equations and ensure at least one parameter");
  }

  
  // look for illegal characters
  foreach ( $TDATA['elements'] as $key => $val )
    {
      if ( eregi("([^A-Z0-9\(\)])", $val, $pregs ) )
	{
	  add_error('parse_illegal', $pregs[1]);
	}
      if ( eregi("^[0-9]+[A-Z0-9]{1,100}", $val, $pregs ) )
	{
	  add_error('parse', 'Variables and parameters may not start with numbers.  I found ' . $val);
	}
      // error on the existence of "i" as a variable (bug #819)
      if ( eregi("^[iI]$", $val, $pregs ))
	{
	  add_error('parse', "The variable '" . $val . "' is reserved.  Please change to another letter and try again.");
	}
    }
}


function add_error ( $error_key, $msg = "" ) {
  /* add_error() adds to the global variable error_list and is used
   * for other functions to report errors before we report back to the
   * end user.
   *
   * takes:   $error_key which is a key to the global $errors[] array.
   *          $msg (optional) which allows you to pass additional information to the user
   *
   * status:  reusable (assuming your $errors variable exists and contains the appropriate
   *          keys.
   */

  global $errors, $error_list;

if ( array_key_exists( $error_key, $errors ) ) {
  if ( sizeof($msg) > 0 ) 
    { $error_list[] = $errors[$error_key] . ": <em>" . $msg . "</em>"; }
  else
    { $error_list[] = $errors[$error_key]; }
  }
 else {
   $error_list[] = $msg;
 }
}

function SGID_testcase ( $equations=NULL ) {
  if ( $equations == NULL ) {
    $equations = "
asdflakjsdfolasdoifuasdfl
Y = - A1/V
A3[T] = s1+cos(l1)
A1[T] = k12*V*A2+CL*U1*A1*U10*A1+U2+k12*A2*A1
A2[T] = k21*A1-k12*A2
Y=F + DV*EPS(1)
";
  }

  SGID_identify($equations, "ernst@u.washington.edu");
}


function clean_equations ( &$equations ) {
  /***************************************************************************
   * clean_equations strips out the whitespace and turns
   * the equations into upper case.
   *
   * takes:     global reference to equations variable.
   * reusable:  sure
   ***************************************************************************/
  
  
  // strip out the whitespace
  $equations = eregi_replace("[ \,]+", "", $equations);  
  
  // strip the equations of comments
  $equations = preg_replace("/\#(.*?)\n/", "", $equations);

  // make string upper case
  $equations = strtoupper($equations);
}


function show_errors( ) {
  /***************************************************************************
   * show_errors()
   * 
   * called on any page that needs to show an error message.  Using styles
   * the user will see red bolded text using the "errortext" style definition
   *
   ***************************************************************************/
  
  $tmp_var = "";
  if ( isset($_SESSION['error_list']) && is_array($_SESSION['error_list']) ) {
    if ( sizeof($_SESSION['error_list']) > 0 ) {
      $tmp_var = '<span class="errortext">';
      foreach ( $_SESSION['error_list'] as $index => $value ) {
	$tmp_var = $tmp_var . $value . "<br /><br />";
      }
      $tmp_var = $tmp_var . '</span>';
      
      return $tmp_var;
    }
  }
  return $tmp_var;
}

function check_required ( $fieldlist, $array ) {
  /***************************************************************************
   * check_required looks through the fieldlist and sees if the 
   * required values are not null and have size greater than 0
   *
   * status:   reusable
   ***************************************************************************/

   $required = array ();
   foreach ( $fieldlist as $key => $value ) {
   
   if ( ! not_null($array[$key]) ) {	
	$required[sizeof($required)] = $value;
   }	
  }
   
   $errors = array();
   
   // check to see if $required has size greater than 0.
   if ( sizeof($required) > 0 ) {
     $tmp_string = "";
     foreach ( $required as $value ) {
       $tmp_string = $tmp_string . "<em>" . $value . "</em>, ";
     }
     // remove the trailing comma.
     $tmp_string = substr($tmp_string,0,strlen($tmp_string)-2);
     if ( sizeof($required) > 1 ) {
       add_error('web_required', $tmp_string . " are required.");
     }
     else {
       add_error('web_required', $tmp_string . " is required.");
     }
   }
}

function valid_syntax ( $txt ) {
  if ( strpos( $txt , "**" ) !== FALSE ) {
    add_error("parse_exponents", "The operand '**' was found in your equations.  Exponents must be represented using the '^' character.");
  }
}


function valid_chars( $txt ) {
  if ( eregi("[^A-Z0-9\-\+\=\[\]\(\)\/\*]", $txt) ) {
    return false;
  }
  else {
    return true;
  }
   
}

function isolate_elements ( $rhs, &$I_elements, $not_params ) {
  /***************************************************************************
   * isolate_elements looks at the RHS of an equation and determines which
   * of the variables are parameters
   *
   * takes:  $rhs (an equation)
   *         $I_elements (reference to global array of tokenized elements
   *         $not_params[] from which includes elements which should NOT be
   *                       treated as parameters.
   ***************************************************************************/

  // now, we need to find all the parameters in the equations
  $regexp = "/[^A-Z0-9]+/";
  $tmp_elements = preg_split( $regexp, $rhs);

  
  // now we remove all the found tokens which are not parameters
  $elements = array();

  foreach ( $tmp_elements as $val ) {
    if ( in_array($val, $not_params ) !== TRUE )
      {
	$elements[] = eregi_replace("[^A-Z0-9]", "", $val);
      }
  }
  
  // join the master array with the new one.
  $I_elements = array_merge( $I_elements, $elements );
}


function SGID_generateXML ( $TDATA, $seed ) {
  $s = new XML_Serializer( $GLOBALS['OPTIONS']['serializer_options'] );

  // create array of data
  $xml = array ("parameters" => array ( "name" => $TDATA['parameter_list'],
					 'attributes' => array ('seed' => $seed )
					 ),
		 "system_experiment_model" =>  
		 array ( "differential_equations"
			 => array ( "equation" 
				    => array ( $TDATA['input_eq'] ),
				    'attributes' => array ('number_of_inputs' => sizeof($TDATA['inputs']) )),
			 "output_equations"
			 => array ( "equation" => $TDATA['out_eq'] )
			 )
		 );

  
  $s->setOption(XML_SERIALIZER_OPTION_ATTRIBUTES_KEY, 'attributes');
  $s->setOption(XML_SERIALIZER_OPTION_MODE, XML_SERIALIZER_MODE_SIMPLEXML);
  $result = $s->serialize($xml);
  
  return ( $s->getSerializedData() );


  if ( $result === TRUE )
    {
      return ( $s->getSerializedData() );
    }
  else {
    add_error('xml', $result);
    return ( $result );
  }
}

function SGID_getXML ( $xml_string ) {
  // returns structure of xml data (array)
  
  $options = array(
                    'complexType'       => 'array'
		    );

  $us = new XML_Unserializer($options);
  $result = $us->unserialize( $xml_string );
  
  if ( PEAR::isError($result) ) {
    add_error("parse", $result->getMessage());
  }

  return ($us->getUnserializedData());
}


// go through the equations and replace all equivalents.
function  SGID_replaceEquivalent( &$TDATA ) {

  
  foreach ( $TDATA['alg_eq'] as $alg_key => $alg_val) {
    // get the equation which will change
    $src = explode("=", $alg_val);

    //    echo "\n\n\nLooking at equation: " . $alg_val . "\n";
  
	
    foreach ( $TDATA['alg_eq'] as $t_key => $t_val ) 
      // get the substitutation values.
      {
	$target = explode("=", $t_val);
	//	echo "looking for instances of:  " . $target[0] . "\n";
	
	
	if ( $src[0] !== $t_key )
	  // if we're not looking at ourself...
	  {
	    // handles if it is in the first line.
	    $src[1] = preg_replace("/(^|[^A-Z0-9])" . $target[0] . "([^A-Z0-9]|$)/", "\\1(" . $target[1] . ")\\2", $src[1]);
	    //	    $src[1] = preg_replace("/([^A-Z0-9])" . $target[0] . "([^A-Z0-9]|$)/", "\\1"  . $target[1] . "\\2", $src[1]);
	    //	    $src[1] = preg_replace("/([^A-Z0-9])" . $target[0] . "([^A-Z0-9])/", "\\1"  . $target[1] . "\\2", $src[1]);
	    $alg_val = $src[0] . "=" . $src[1];
	    $TDATA['alg_eq'][$alg_key] = $src[0] . "=" . $src[1];
	  }
      }
  }
  

  $blocks = array ( "input_eq", "out_eq" );
  

  foreach ( $blocks as $block )
    {
        foreach ( $TDATA[$block] as $eq_key => &$equation ) 
	{
	  //	  	  echo "EQ_KEY: " . $eq_key . "\n";
	  unset ($src);
	  $src = explode ("=", $equation );
	  
	  foreach ( $TDATA['alg_eq'] as $t_key => $t_val )
	    {
	      $target = explode( "=", $t_val );
	      $regexp = "/(|[^A-Z0-9])" . $target[0] . "([^A-Z0-9]|)/";
	      $src[1] = preg_replace($regexp, "\\1(" . $target[1] . ")\\2",  $src[1]);
	      //$TDATA[$block] = $src[0] . "=" . preg_replace("/" . $target[0] . "/", $target[1], $src[1] );
	      //$TDATA['ernst'][] = $src[0] . "=" . preg_replace("/" . $target[0] . "/", $target[1], $src[1] );
	    }
	  // preg_replace was to fix bug #754
	  //	  	  	  $equation = $src[0] . "=" . preg_replace("/[\[\]]/", "", $src[1]);
			  $equation = $src[0] . "=" . $src[1];
	}

    }

}


function SGID_finalParamCheck( $param_arr, $eq_arr ) {
  $missing_params = array();

  return (0);
      foreach ( $param_arr as $param )
	{
	  $found = false;
	  foreach ( $eq_arr as $equation ) 
	    {
	      if ( eregi($param, $equation) )
		$found = true;
	    }
	  if ( ! $found )
	    {
	      $missing_params[] = $param;
	    }
	}

      if ( sizeof($missing_params) > 0 ) 
	{
	  foreach ( $missing_params as $mp ) {
	    $err_message .= ", " . $mp;
	  }
	  $err_message = eregi_replace("^,","", $err_message);
	  add_error('parse','The following parameters were found in your algebraic assignments, but were not');
	}
}


?>


