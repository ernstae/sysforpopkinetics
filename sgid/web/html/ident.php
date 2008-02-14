<?
$debug = 1;

require_once ("conf/SGID.php");

// initialize variables.
$step = '';

// start session
if ( !isset($_SESSION['step']) ) {
	//session_start();
	$_SESSION['step'] = 0;
	$step=0;
}
else {
	// do nothing echo "Session is set";
}

// current step of the process?
if ( $_REQUEST['step'] ) {
	$step = $_REQUEST['step'];
} else { $step = 0; }

// set the current page title (called in header)
$title = $steps[$step];

?>

<? include ('includes/header.php'); ?>

<div id="content">
<h3><?= $GLOBALS['OPTIONS']['site_name'] ?></h3>

<?= show_errors() ?>

<? 
switch ($step) {
case 0:
?>

<p>
<? require_once("includes/instructions.html") ?>
</p>

<form method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<label for="equations">Equations:</label>
<textarea name="equations" rows="10" cols="50"><?= $_SESSION['equations'] ?>
</textarea>
<br />
<label for="seed">Seed:</label><input type="text" id="seed" value="<?= $_SESSION['seed'] > 0 ? $_SESSION['seed'] : rand(1000,9999) ?>" name="seed" maxlength="4">
<br />
<label for="email_address">Email Address:</label>
<input type="text" name="email_address" id="email_address" value="<?= $_SESSION['email_address'] ?>" />
<br />
<label for="special_code">Security Code:</label>
<input type="password" name="special_code" id="special_code" value="<?= $_SESSION['special_code'] ?>" />
<br />
</fieldset>
<input type="submit" name="submit" value="Validate Job" />
</form>


<? /**************************** END OF STEP 0 ******************************/
break; 
case 1:

$parameters = "";
$eq_array = array();

$orig_equations = $_SESSION['equations'];

foreach ( $_SESSION['TDATA_web'] as $rkey => $rval ) {
  if ( eregi("(input_eq|out_eq)", $rkey, $match) ) {
    foreach ( $_SESSION['TDATA_web'][$rkey] as $akey => $aval ) {
      $equations .= $aval . "\n";
      $eq_array[$match[1]] .= $aval . "\n";
    }
  }
  elseif ( eregi("parameter_list", $rkey) ) {
    foreach ( $_SESSION['TDATA_web'][$rkey] as $akey => $aval ) {
      $parameters .= $aval . "\n";
    }
  }
}

?>
<p>Please review.  If everything is satisfactory, you may click "Process Job"</p>

<script language="JavaScript">
review.equations.disabled = true;
review.seed.disabled = true;
review.email_address.disabled = true;
</script>

<form id="review" name="review" method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<input type="hidden" name="equations" value="<?= $equations ?>">
<input type="hidden" name="orig_equations" value="<?= $orig_equations ?>">
<input type="hidden" name="email_address" value="<?= $_SESSION['email_address'] ?>">
<input type="hidden" name="seed" value="<?= $_SESSION['seed'] ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<ol>
System Equations: (<?= sizeof($_SESSION['TDATA_web']['input_eq']) ?>):<br />
<ul>
<pre>
<?= htmlentities($eq_array['input_eq']) ?>
</pre>
</ul>
Measurement Equations: (<?= sizeof($_SESSION['TDATA_web']['out_eq']) ?>):<br />
<ul>
<pre>
<?= htmlentities($eq_array['out_eq']) ?>
</pre>
</ul>
Parameters (<?= sizeof($_SESSION['TDATA_web']['parameter_list']) ?>):<br />
<ul>
<pre>
<?=  $parameters ?>
</pre>
</ul>
Seed:   <?= $_SESSION['seed'] ?>
<br /><br />
Email:  <?= $_SESSION['email_address'] ?>
</ol>
</fieldset>
<input type="submit" name="submit" value="Process Job" />
</form>
<form action="?step=0"><input type="submit" name="submit" value="Change Equations" /></form>


<? /********************* END OF STEP 1 ***************************************/break;
case 2:

$myjobs = "'0'";

if ( strlen($_SESSION['myjobs']) > 0 ) {
  $myjobs = $_SESSION['myjobs'];
 }


// get all the old jobs and bring them to the user's desktop
$result = $db->query("SELECT id, state_code, end_code, md5(concat(id,seed)) as jobid FROM job WHERE md5(concat(id,seed)) IN (" . $myjobs . ")");

if ( PEAR::isError($result) ) {
  die ("Error DB: " . $result->getMessage());
 }

?>

<meta http-equiv="refresh" content="20">

<form>
<fieldset>
<legend><?= $steps[$step] ?></legend>
Thank you.  Your job has been submitted for processing.  You will be notified via e-mail when the job has completed.  This page will also refresh every 30 seconds.
<br />
<br />
<? 
unset($_SESSION['seed']);
unset($_SESSION['equations']); 
?>
<a href="ident.php">Click here</a> to begin another job.

<br />
<br />

<?

if ( $result->numRows() > 0 ) {

?>

<h3>Job History</h3>
Jobs submitted during this session are listed below.  You can view the results of completed jobs, or re-run your jobs by using the "actions" column.
<br />
<br />
<table class="sample">
<tr>
<th>Job #</th>
<th>Actions</th>
<th>Current State</th>
<th>Status</th>
</tr>

<? while ( $row = $result->fetchRow() )  { ?>
<tr>
<td><?= $row->id ?></td>
					   <td><a href="runjob.php?jobid=<?= $row->jobid ?>">re-run</a> <? if ( $row->state_code == 'end' ) { ?> | <a href="showjob.php?jobid=<?= $row->jobid ?>">results</a> <? } ?></td>
<td><?= $GLOBALS['OPTIONS']['state_codes'][$row->state_code] ?></td>
<td><?= $GLOBALS['OPTIONS']['end_codes'][$row->end_code] ?></td>
</tr>
					<? } ?>
</table>

<? } // end of if ( numrows > 0) ?>
</fieldset>
</form>

<? /**************************** END OF STEP 3 ******************************/
break;
}
?>



</div>
</body>
</html>