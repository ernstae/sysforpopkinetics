<?

require_once('DB.php');
//PEAR::setErrorHandling(PEAR_ERROR_TRIGGER);
//PEAR::setErrorHandling (PEAR_ERROR_DIE);

// Database connection definition.
$dsn = array(
 'phptype'  => 'mysql',
 'hostspec' => 'whitechuck.rfpk.washington.edu',
 'database' => 'spkdb',
 'username' => 'daemon',
 'password' => 'daemon'
);

$db = DB::connect($dsn);
//$db->setFetchMode(DB_FETCHMODE_ASSOC);
$db->setFetchMode(DB_FETCHMODE_OBJECT);

$debug = 1;

$minlengths = array ( 'username' => 5,
		      'password' => 4,
		    );
		   
		   

// define the steps through which a user will progress.
$steps = array ("Terms and Conditions", "Contact Information", "Username Selection", "Set your Password", "Account Request Sent");
$max_steps = sizeof($steps);

$admin_email = "ernst@u.washington.edu";

/****************************************************************************
 * show_errors()
 * 
 * called on any page that needs to show an error message.  Using styles
 * the user will see red bolded text using the "errortext" style definition
 *
 ****************************************************************************/

function show_errors( ) {
$tmp_var = "";
  if ( isset($_SESSION['errors']) && is_array($_SESSION['errors']) ) {
     if ( sizeof($_SESSION['errors']) > 0 ) {
  	$tmp_var = '<span class="errortext">';
  	foreach ( $_SESSION['errors'] as $index => $value ) {
    		$tmp_var = $tmp_var . $value . "<br /><br />";
  	}
  	$tmp_var = $tmp_var . '</span>';

	return $tmp_var;
     }
  }
return $tmp_var;
}


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

/****************************************************************************
 * show_all()
 * 
 * used to show all the variables in session and request scope on page
 *
 ****************************************************************************/
function valid_username ( $username ) {
  // connect to both spkutil and spkdb and check whether the accounts already
  // exist.
  global $db;

  $result = $db->query("select (select count(user_id) from spkdb.user where lower(username)=lower('" . $username . "')) as user_count, (select count(id) from spkutil.user_request where lower(username)=lower('" . $username . "')) as pending_count");

  if ( $result->fetchInto($row) ) {
     if ( $row->user_count > 0 || $row->pending_count > 0 ) return false;
  }
  else {
	throw("error with database");
	return false;
  }

  return true;
}


function size_check ( $field ) {
  global $minlengths;
  if ( strlen($_SESSION[$field]) < $minlengths[$field] ) return false;
  return true;
}

function add_error ($errorarray, $message) {
  $errorarray[sizeof($errorarray)] = $message;
  return $errorarray;
}

function not_null ( $string ) {
  if ( is_string($string) && strlen($string) > 0 ) { 
    return true;
  }

return false;
}

function check_required ( $fieldlist, $array, $errors ) {
   $required = array ();
   foreach ( $fieldlist as $key => $value ) {
   
   if ( ! not_null($array[$key]) ) {	
	$required[sizeof($required)] = $value;
   }	
  }

if ( ! is_array($errors) ) {
  $errors = array ();
}

// check to see if $required has size greater than 0.
if ( sizeof($required) > 0 ) {
	$tmp_string = "";
	foreach ( $required as $value ) {
	  $tmp_string = $tmp_string . "<em>" . $value . "</em>, ";
	}
	// remove the trailing comma.
	$tmp_string = substr($tmp_string,0,strlen($tmp_string)-2);
	if ( sizeof($required) > 1 ) {
	  $errors = add_error ($errors, $tmp_string . " are required fields.");
	}
	else {
	  $errors = add_error ($errors, $tmp_string . " is a required field.");
	}
}
return $errors;
}

function country_drop ($selected) {
global $db;
$db->setFetchMode(DB_FETCHMODE_OBJECT);

$retval = "";

$result = $db->query("select id, code, name from spkutil.countries");
 while ( $result->fetchInto($row) ) {
   if ( $row->id == $selected || ( sizeof($selected) <= 0 && $row->id == 184 )) {
    $retval = $retval . '<option selected value="' . $row->id . '">' . $row->name . '</option>' . "\n";
  } else {
    $retval = $retval . '<option value="' . $row->id . '">' . $row->name . '</option>' . "\n";
  }
}

return $retval;
}

function notify_admin ( $admin_email )  {
global $db;

// get number of pending requests
$result = $db->query("select count(id) cid from spkutil.user_request where status in (1)");

$row = $result->fetchRow();

$num_pending = $row->cid;

require_once ("Mail.php");

$headers['From'] = "ernst@u.washington.edu";
$headers['To'] = $admin_email;
$headers['Subject'] = "New SPK account request";

$message = '
-------- THIS IS AN AUTOMATED MESSAGE ---------

A new request for an SPK account has been made.
There are currently ' . $num_pending . ' pending requests waiting
for your approval.

Please point your browser to
http://milton.rfpk.washington.edu/admin
and either approve or decline the requests.

You can log in with your SPK user account.';

$mail_object =& Mail::factory('sendmail');
$mail_object->send($admin_email, $headers, $message);

return true;

}

function notify_customer ( $customer_email ) {
return true;
}

function validate_email ( $field ) {
  // check to see if it is in the format xxx@xxxx.xx
  if(!eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$", $field) ) {
    return $false;
  }
  
  return true;
}
?>
