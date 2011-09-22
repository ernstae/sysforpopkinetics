<?

include ("includes/config.php");

// this is process.php
if ( $_REQUEST['step'] >= 0 ) {
	$step = $_REQUEST['step'];
}

$errors = array ();

// first thing is to loop through all request variables,
// and set them to session variables.
foreach ($_REQUEST as $key => $value) {
    $_SESSION[$key] = $value;
}

switch ($step) {

// ********************* Process terms and conditons
case 0:
  if ( strcasecmp($_REQUEST['terms'], "accept") != 0 ) {
   $errors = add_error($errors, "You must accept the SPK Terms and Conditons before proceeding.");
  }

break;

// ******************** Process personal information
case 1:
$fieldlist = array( 'firstname' => "First Name",
		    'lastname' => "Last Name",
		    'organization' => "Organization",
		    'city' => "City",
		    'state' => "State",
                    'country' => "Country",
		    'email' => "Email address" );

$errors = check_required($fieldlist, $_SESSION, $errors);

 if ( ! validate_email($_SESSION['email']) ) {
     $errors[sizeof($errors)] = 'The e-mail address, <em>' . $_SESSION['email'] . '</em> is not a valid e-mail address.  Please check that you have entered a valid e-mail address before continuing.<br />';
   }

 if ( strlen(eregi_replace("([^A-Z]+)","",$_SESSION['organization'])) < 2 ) {
   $errors[sizeof($errors)] = 'You must enter more information in the Organization field before continuing.<br />';
 }


 break;

// ******************** Process requested username
case 2:
// we need to know if the requested username already exists in the request
// queue, and then we need to check the user table in "spkdb".
$fieldlist = array( 'username' => 'Username' );

$errors = check_required( $fieldlist, $_SESSION, $errors);

// break if they didn't include the username -- otherwise, we'll be 
// validating a 0 length string against the database.
if ( sizeof($errors) <= 0 ) { 
	if ( ! valid_username($_SESSION['username']) ) {
	// the user picked a username that already exists.  
	$errors = add_error($errors, "Unfortunately, the username <em>" . $_SESSION['username'] . "</em> is unavailable.  Please try choosing another username before continuing."); 
	}
	
	if ( ! size_check("username") ) {
	$errors = add_error($errors, "Usernames must be at least " . $minlengths['username'] . " characters long.");
	} 
}

break;

// ******************** Process requested password
case 3: 
$fieldlist = array ( 'password' => 'Password', 
		     'password_confirm' => 'verification password' );

  $errors = check_required ( $fieldlist, $_SESSION, $errors);

  if ( $_SESSION['password'] != $_SESSION['password_confirm'] ) {
    unset($_SESSION['password']);
    unset($_SESSION['password_confirm']);
    $errors = add_error($errors, "Please ensure your password is the same in both fields");
  }

  if ( ! size_check("password") ) {
    $errors = add_error($errors, "Passwords must be at least " . $minlengths['password'] . " characters long.");
  }

// ******************** Submit to the database & redirect to thank you page

  if ( sizeof($errors) <= 0 ) {
$query = $db->prepare("insert into spkutil.user_request (first_name, surname, password, username, company, country, state, email, city ) values (?,?,?,?,?,?,?,?,?)");

 $data = array ( $_SESSION['firstname'], $_SESSION['lastname'], $_SESSION['password'], $_SESSION['username'], $_SESSION['organization'], $_SESSION['country'], $_SESSION['state'], $_SESSION['email'], $_SESSION['city']);

$db->execute($query, $data);

 if ( ! notify_customer($_SESSION['email'], $_SESSION['firstname']) ) {
   die("Email to customer failed");
 }
 notify_admin();
  }
break;

case 4:

break;
}

// if we made it to this point without an error, then we should increment step.
if ( sizeof($errors) <= 0 ) {
	$step++;
	$_SESSION['step'] = $step;
	unset($_SESSION['errors']);
} else {
	$_SESSION['errors'] = $errors;
	//	echo "Errors : " . var_dump($errors);
}

	header("Location:  newaccount.php?step=" . $step . "\n\r"); 
?>
