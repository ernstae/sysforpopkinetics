<?

// start session
if ( !isset($_SESSION['step']) ) {
	//session_start();
	$_SESSION['step'] = 0;
	$step=0;
}

require_once("conf/SGID.php");

$jobid = 0;

if ( isset($_REQUEST['jobid']) && strlen($_REQUEST['jobid']) > 0 ) {
  $jobid = htmlentities($_REQUEST['jobid']);
 }
 else {
   add_error("nojob", "You must provide a valid job id to me.");
 }

$title = "Job status for SGID job: " . $jobid;

$results = $db->query("SELECT equations, result_xml, id, state_code, end_code, equations FROM job WHERE md5(concat(id,seed))='" . $jobid . "'");

//echo "<pre>" . var_dump($results) . "</pre>\n";

if ( PEAR::isError($results) ) {
  add_error("DB", $results->getMessage());
 }

?>



<? include ('includes/header.php'); ?>

<div id="content">
<h3><?= $GLOBALS['OPTIONS']['site_name'] ?></h3>

<? 
  if ( $results->numRows() > 0 ) {
    $row = $results->fetchRow();
    $xml = SGID_getXML($row->result_xml);
    $job_id = $row->id;  }
  else {
    $job_id = "Unavailable";
    add_error("QUEUE","The job you have requested does not exist.");
  }

?>

<?= show_errors() ?>


<p>Here are your job results</p>

<form id="review" name="review" method="POST" action="">
<fieldset>


<legend>Job Results : <?= $job_id ?></legend>

  <?    if ( $row->state_code == 'run' || $row->state_code == 'queue' ) 
      { 
	echo "Your job has not completed running.  It is currently in the " . $row->state_code . " state.\n";
      } 
 else 
   {
     if ( $row->end_code != 'srun' || strlen($xml['error_messages']) > 0 ) {
       // we need to show an error block
     ?>

<ol>
<b>Errors Were Detected: </b>
<ul>
<pre>
<?= $xml['error_messages'] ?>
</pre>
</ul>

<? } // end of error block ?>
<ol>
   <b>Solutions Found</b>:
<ul>
<pre><?= $xml['number_of_solutions'] ?> <?= $xml['status_of_the_solutions'] ?></pre>
</ul>
<b>Processing Time</b>:
<ul><pre><?= $xml['elapsed_seconds'] ?> second(s)</pre></ul>


<b>Parameter Seed</b>:
<ul>
<pre><?= $xml['parameter_seed'] ?></pre>
</ul>

<b>Computation Information</b>:
<ul>
<pre>
<?= eregi_replace("\n{3,100}", "\n", htmlentities($xml['calculation_details'])) ?>
</pre>
  </ul>

   <b>Your input equations were</b>:
<ul>
<pre>
<?= htmlentities($row->equations) ?>
</pre>
</ul>

   <? } // else ?>
</fieldset>
</form>


</div>
</body>
</html>