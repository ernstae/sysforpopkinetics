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
  $fieldlist = array( 'event_id' => "Choosing an event" );

  $errors = check_required($fieldlist, $_SESSION, $errors);
  $_SESSION['event_name'] = '';

  // fetch the event name.
  $results = $db->query("select name from spkutil.events where id=" . $_SESSION['event_id']);

  if ( $results->fetchInto($row) ) {
    $_SESSION['event_name'] = $row->name;
  }

break;

// ******************** Process personal information
case 1:
$fieldlist = array( 'firstname' => "First Name",
		    'lastname' => "Last Name",
		    'organization' => "Organization",
		    'address' => "Address",
		    'city' => "City",
		    'state' => "State",
		    'postal_code' => "ZIP/Postal Code",
                    'country' => "Country",
		    'email' => "Email address" );

$errors = check_required($fieldlist, $_SESSION, $errors);

 if ( $_REQUEST['user_type'] != 'D' ) {
   $_SESSION['user_type'] = 'R';
 }

 if ( ! validate_email($_SESSION['email']) ) {
     $errors[sizeof($errors)] = 'The e-mail address, <em>' . $_SESSION['email'] . '</em> is not a valid e-mail address.  Please check that you have entered a valid e-mail address before continuing.<br />';
   }


 break;

// ******************** Process requested password
case 2: 
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
    $query = $db->prepare("insert into spkutil.participants (firstname, lastname, affiliation, address, address2, city, state, country, postal_code, email, password) values (?,?,?,?,?,?,?,?,?,?,?)");

    $data = array ( $_SESSION['firstname'], $_SESSION['lastname'], $_SESSION['organization'], $_SESSION['address'], $_SESSION['address2'], $_SESSION['city'], $_SESSION['state'], $_SESSION['country'], $_SESSION['postal_code'], $_SESSION['email'], $_SESSION['password']);

    $db->execute($query, $data);

    $result = $db->query("select last_insert_id() as id");
    if ( $result->fetchInto($row) ) {
      $user_id = $row->id;
    }

    if ( $_SESSION['user_type'] != 'D' ) {
      $_SESSION['user_type'] = 'R';
    }

    $query = $db->prepare("insert into spkutil.registrations (event_id, participant_id, user_type) values ( ?, ?, ?)");
    $data = array ( $_SESSION['event_id'], $user_id, $_SESSION['user_type']);
    $db->execute($query, $data);

    // $result = db->query("select max(id) from participants");
 
// insert into registrations (participant_id, event_id) values (?,?)

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

	header("Location:  workshop.php?step=" . $step . "\n\r"); 
?>
