<?

require_once ("conf/SGID.php");

//session_destroy();
//session_start();

if ( isset($_REQUEST['job_id'] ) ) {
  $jobid = htmlentities($_REQUEST['job_id']);
 }

if ( isset($_REQUEST['jobid']) && strlen($_REQUEST['jobid']) > 0 )
  {
    $jobid = htmlentities($_REQUEST['jobid']);
    
    $_SESSION['jobid'] = $jobid;
    $_SESSION['step'] = 0;
    
    
    $results = $db->query("SELECT * FROM job WHERE md5(concat(id,seed))='" . $jobid . "'");
    
    if ( $row = $results->fetchRow() ) 
      {
	$_SESSION['email_address'] = $row->email_address;
	$_SESSION['seed'] = $row->seed;
	$_SESSION['equations'] = $row->equations;
      }
    
    else {
      add_error("nojob", 'Job ' . $jobid . ' does not exist.');
    }
    
  }


Header("Location: ident.php?step=0\n\r");

?>