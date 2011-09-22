<?

require_once("inc.functions.php");

$admin_email = "vicini@u.washington.edu,ernst@u.washington.edu";


/****************************************************************************
 * show_all()
 * 
 * used to show all the variables in session and request scope on page
 *
 ****************************************************************************/
function show_all ( ) {
  $retval = "<strong>Session ID</strong>: " . session_id() . "<br><br>\n"
  		. "<strong>Session Variables</strong>:" .var_dump($_SESSION) . "<br><br>"  
  		. "<strong>Request Variables</strong>:  " . var_dump($_REQUEST);

return ($retval);
}



function size_check ( $field ) {
  global $minlengths;
  if ( strlen($_SESSION[$field]) < $minlengths[$field] ) return false;
  return true;
}

function not_null ( $string ) {
  if ( is_string($string) && strlen($string) > 0 ) { 
    return true;
  }

return false;
}

function notify_customer ( $customer_email, $customer_firstname ) {

  require_once ("Mail.php");

  $headers['From'] = "Paolo Vicini <vicini@u.washington.edu>";
  $headers['To'] = $customer_email;
  $headers['Subject'] = "RFPK Workshop Registration";
  $headers['Bcc'] = $headers['From'];

  $message = "";

  $mail_object =& Mail::factory('sendmail');
  $mail_object->send($customer_email, $headers, $message);
  
  return true;
}

function validate_email ( $field ) {
  // check to see if it is in the format xxx@xxxx.xx
  if(!eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$", $field) ) {
    return $false;
  }
  
  return true;
}

/////////////////////////////////////////////////////////////////
// Identifiability Web Front-end
//
// Developed by the Resource Facility for Population Kinetics
// University of Washington
//
// Architect:  Andrew Ernst (ernst@u)
//
/////////////////////////////////////////////////////////////////



// strip_spaces removes the leading spaces on each line (if any) and also all
// spaces within the equation blocks.
function strip_spaces( $input ) {
  $output = "";

  

  return ( $output );
}



// count_parameters takes an input block and counts the number of differential equations, then returns their count.
function count_parameters ( $input ) {
  $count = 0;

  $count = preg_match_all('/(^.*?\[T\]\=)/', $input, $chunks);

  return ( $count );
}


function count_inputs ( $input ) {
  $count = 0;

  $count = preg_match_all('/[U0-9]+/', $input, $chunks);

  return ($count);
}


// reverse equation ( $input ) takes an equation as input (or equation block
function reverse_equation ( $input ) {

  return ($retval);
}


function getRandom() {
  $retval = 0;
  
  return ( $retval );
}

?>
