<?

// start session
if ( !isset($_SESSION) ) {
  //  session_destroy();
  // session_start();
}

require_once("conf/SGID.php");

$jobid = 0;

if ( isset($_REQUEST['jobid']) && strlen($_REQUEST['jobid']) > 0 ) {
  $jobid = htmlentities($_REQUEST['jobid']);
  $_SESSION['myjobs'] .= ",'" . $jobid . "'";
  $_SESSION['myjobs'] = eregi_replace("^,","",$_SESSION['myjobs']);
 }
 else {
   add_error("nojob", "You must provide a valid job id to me.");
 }


$title = "Job status for SGID job: " . $jobid;

$results = $db->query("SELECT equations, result_xml, id, state_code, end_code FROM job WHERE md5(concat(id,seed))='" . $jobid . "'");

//echo "<pre>" . var_dump($results) . "</pre>\n";

if ( PEAR::isError($results) ) {
  add_error("DB", $results->getMessage());
 }

?>



<? include ('includes/header.php'); ?>

<div id="content">
<h3><?= $GLOBALS['OPTIONS']['site_name'] ?></h3>

<? 
  $show_results = false;

  if ( $results->numRows() > 0 ) 
    {
      $row = $results->fetchRow();
      $xml = SGID_getXML($row->result_xml);
      $job_id = $row->id;  
      $show_results = true;
    }
  else 
    {
      $job_id = "Unavailable";
      add_error("QUEUE","The job you have requested does not exist.");
    }

?>

<?= show_errors() ?>


<p>Here are your job results</p>

<form id="review" name="review" method="POST" action="">
<fieldset>


<legend>Job Results : <?= $job_id ?></legend>

<ol>
<?    
    if ( $row->state_code == 'run' || $row->state_code == 'queue' ) 
      { 
	echo "Your job has not completed running.  It is currently in the " . $row->state_code . " state.\n";
      } 
elseif ( $row->state_code == 'end' ) 
{
  if ( isset($xml) && ($row->end_code != 'srun' || strlen($xml['error_messages']) > 0 )) {
    // we need to show an error block
    ?>
    <b>Errors Were Detected: </b>
      <ul>
      <pre><?= $xml['error_messages'] ?></pre>
      </ul>
      
      <? } // end of error block ?>
     <b>Solutions Found</b>:
  <ul>
	<pre><?= ($xml['number_of_solutions'] >= 0) ? $xml['number_of_solutions'] : "" ?> <?= $xml['status_of_the_solutions'] ?></pre>
     </ul>
     <b>Processing Time</b>:
  <ul><pre><?= $xml['elapsed_seconds'] ?> second(s)</pre></ul>
     
     
     <b>Parameter Seed</b>:
  <ul>
     <pre><?= $xml['parameter_seed'] ?></pre>
     </ul>
     
     <b>Computation Information</b>:
  <ul>
	<pre><?= htmlentities($xml['calculation_details']) ?> </pre>
     </ul>
     
     <b>Your input equations were</b>:
  <ul>
     <pre><?= htmlentities($row->equations) ?></pre>
     </ul>

<?

     }  // elseif
 else {
   echo "We have no record of the job you are requesting.  Please try again.";
 }


?>
</fieldset>
</form>


</div>
</body>
</html>