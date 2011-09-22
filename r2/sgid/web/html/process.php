<?

include ("conf/SGID.php");

// this is process.php
if ( $_REQUEST['step'] >= 0 ) {
	$step = $_REQUEST['step'];
}

// first thing is to loop through all request variables,
// and set them to session variables.
foreach ($_REQUEST as $key => $value) {
    $_SESSION[$key] = $value;
}

switch ($step) {

// ********************* Process terms and conditons
case 0:
  $fieldlist = array( 'equations' => 'Equations',
		      'seed'      => 'Seed',
		      'email_address' => 'Email address',
		      'special_code' => 'Security Code');

  check_required($fieldlist, $_SESSION);  

  if ( $_SESSION['special_code'] != '355061' )
    {
      add_error('security', "The security code was invalid.  Please try again.");
    }

  if ( ! valid_chars($_SESSION['equations']) ) {
    add_error('parse', "Invalid characters were found in your equations");
  }
  
  if ( ! validate_email($_SESSION['email_address']) ) {
    add_error('web_email', $_SESSION['email_address']);
  }


  if ( sizeof($error_list) <= 0 ) {
    SGID_identify( $_SESSION['equations'], $_SESSION['email_address'], $_SESSION['seed'] );
  }

  valid_syntax($_SESSION['equations']);

break;

/***************************************************************************
 * Submit to Database
 ***************************************************************************/
case 1:
  $fieldlist = array( 'equations' => 'Equations',
		      'seed'      => 'Seed',
		      'email_address' => 'Email address');

  check_required($fieldlist, $_SESSION);  

  if ( ! valid_chars($_SESSION['equations']) ) {
    add_error('parse', "Invalid characters were found in your equations");
  }
  
  if ( ! validate_email($_SESSION['email_address']) ) {
    add_error('web_email', $_SESSION['email_address']);
  }

  // ******************** Submit to the database & redirect to thank you page
  
  if ( sizeof($error_list) <= 0 ) {
    SGID_identify( $_SESSION['equations'], $_SESSION['email_address'], $_SESSION['seed'] );
  }

  if ( sizeof($error_list) <= 0 ) {
    $query = $db->prepare('INSERT INTO job (equations, email_address, seed, xml_input, ts_submit) VALUES (:equations, :email_address, :seed, :xml_input, NULL)', TRUE, MDB2_PREPARE_MANIP);
    
    // ask the library to generate the XML input for identifiability
    $generated_xml = SGID_generateXML( $_SESSION['TDATA_web'], $_SESSION['seed']);
    
    if ( sizeof($generated_xml) > 0 ) {
      $data = array (
		     'equations' => $_SESSION['orig_equations'],
		     'email_address' => $_SESSION['email_address'],
		     'seed' => $_SESSION['seed'],
		     'xml_input' => $generated_xml
		     );
      
      $affectedrows = $query->execute( $data );

      var_dump($affectedrows);

      $result = $db->query("select last_insert_id() as id");
      if ( $row = $result->fetchRow() ) 
	{
	  $job_id = $row->id;
	  $jobid = hash('md5', $job_id . $_SESSION['seed']);
	  $_SESSION['web_id'] = $jobid;
	  $_SESSION['myjobs'] .= ",'" . $jobid . "'";
	  $_SESSION['myjobs'] = eregi_replace("^,","", $_SESSION['myjobs']);
	}
      else
	{
	  add_error('database','Database Failure ' . $result);
	}
    }
  }
  
  break;
 }
  
// if we made it to this point without an error, then we should increment step.
if ( sizeof($error_list) <= 0 ) {
  $step++;
  $_SESSION['step'] = $step;
  unset($_SESSION['error_list']);
 } else {
  $_SESSION['error_list'] = $error_list;
  //	echo "Errors : " . var_dump($error_list);
 }

header("Location:  ident.php?step=" . $step . "\n\r"); 
?>
