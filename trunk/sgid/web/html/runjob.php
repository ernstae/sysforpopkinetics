<?
$debug = 1;

require_once ("conf/SGID.php");

$jobid = htmlentities($_REQUEST['jobid']);

if ( isset($_REQUEST['jobid']) && strlen($_REQUEST['jobid']) > 0 )
  {
    $results = $db->query("SELECT email_address, equations, seed FROM job WHERE md5(concat(id,seed))=" . $jobid);
    
    if ( $db->fetchInto($row) ) 
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