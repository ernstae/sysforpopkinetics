<?

require_once ("conf/SGID.php");

if ( isset($_REQUEST['jobid']) && strlen($_REQUEST['jobid']) > 0 )
  {
    $jobid = htmlentities($_REQUEST['jobid']);
    
    $_SESSION['jobid'] = $jobid;
    $_SESSION['step'] = 0;
    
    
    $results = $db->query("SELECT * FROM job WHERE md5(concat(id,seed))='" . $jobid . "'");
    
    if ( $results->fetchInto($row) ) 
      {
	$_SESSION['email_address'] = $row->email_address;
	$_SESSION['seed'] = $row->seed;
	$_SESSION['equations'] = $row->equations;
      }
    
    else {
      add_error("nojob", 'Job ' . $jobid . ' does not exist.');
    }
    
  }


Header("Location: ident.php\n\r");

?>