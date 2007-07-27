<?
$debug = 1;
include ("includes/config.php");

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
<h3>Request Workshop Registration : <?= $steps[$step] ?></h3>

<p class="small">This is step <?= $step+1 ?> of <?= sizeof($steps) ?></p>

<?= show_errors() ?>

<? 
switch ($step) {
case 0:
?>

<p>
Please select from the following lists of RFPK events open for registration:
</p>

<form method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<?= list_events(); ?>
</fieldset>
<input type="submit" name="submit" value="Choose and continue" />
</form>


<? /**************************** END OF STEP 0 ******************************/
break; 
case 1:

// make a DB connection to get the country list
require_once("DB.php");
$db = DB::connect($dsn);
?>

<form method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<? if ( $allow_login == true ) { ?>
<fieldset>
<p>If you have registered for a workshop in the past using this system, you may log in with your e-mail address and password.</p>
<legend>Already Registered?</legend>

<label for="email_login">Email</label>
<input type="text" id="email_login" name="email_login" maxlength="100" style="width:200px" value="<?= strip_tags($_SESSION['email_login']) ?>"><br />
<label for="password">Password:</label> <input type="password" id="password" name="password" maxlength="10" value="<?= strip_tags($_SESSION['password']) ?>" /><br /><br />

<input type="submit" value="Log In" /><br>
</fieldset>
				 <? } ?>
<fieldset>
<p>Please provide your current contact information so we may register you for the <b><?= $_SESSION['event_name'] ?></b>.</p>
<legend><?= $steps[$step] ?></legend>
<label for="firstname">First Name</label>
<input type="text" id="firstname" name="firstname" maxlength="15" style="width:200px" value="<?= strip_tags($_SESSION['firstname']) ?>" /><br>

<label for="lastname">Last Name</label>
<input type="text" id="lastname" name="lastname" maxlength="25" style="width:200px" value="<?= strip_tags($_SESSION['lastname']) ?>" /><br>

<label for="organization">Company</label> 
<input type="text" id="organization" name="organization" maxlength="50" value="<?= strip_tags($_SESSION['organization']) ?>" style="width:200px" /><br>

<label for="address">Address</label>
<input type="text" id="address" name="address" maxlength="25" style="width:200px" value="<?= strip_tags($_SESSION['address']) ?>"  /><br>

<label for="address2">Address</label>
<input type="text" id="address2" name="address2" maxlength="25" style="width:200px" value="<?= strip_tags($_SESSION['address2']) ?>"  /><br>

<label for="city">City</label>
<input type="text" id="city" name="city" maxlength="25" style="width:200px" value="<?= strip_tags($_SESSION['city']) ?>"  /><br>

<label for="state">State/Province</label>
<input type="text" id="state" name="state" style="width:200px;" maxlength="25" value="<?= strip_tags($_SESSION['state']) ?>" /> <br />

<label for="postal_code">Postal Code</label>
<input type="text" id="postal_code" name="postal_code" style="width:50px;" maxlength="7" value="<?= strip_tags($_SESSION['postal_code']) ?>" /> <br />

<label for="country">Country</label>
<select name="country" id="country" style="width:200px;"><?= country_drop($_SESSION['country']) ?></select><br>

<label for="email">Email</label>
<input type="text" id="email" name="email" maxlength="80" value="<?= strip_tags($_SESSION['email']) ?>" /><br>

<label for="user_type">Account type</label>
<input type="checkbox" name="user_type" value="D" <? if ( $_SESSION['user_type'] === 'D' ) { echo 'checked'; } ?> > <span class="smalltext">Select if you are a student or post doc.</span><br />

<input type="submit" value="Continue" /><br>
</fieldset>
</form>

<? /********************* END OF STEP 1 ***************************************/
break;
case 2:
?> 

<p>
In order to facilitate access to your profile for future RFPK events, we ask that you create a password so that you may return at a later date to see updated registration information.  Your password must be at least four (4) characters.</p>


<form method="POST" action="process.php" onsubmit="document.getElementById('submit-button').disabled = true;">
<input type="hidden" name="step" value="<?= $step ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<p><label for="password">Password:</label> <input type="password" id="password" name="password" maxlength="10" value="<?= strip_tags($_SESSION['password']) ?>" /></p>
<p><label for="password_confirm">Pasword(again):</label> <input type="password" id="password_confirm" name="password_confirm" maxlength="10" value="<?= strip_tags($_SESSION['password_confirm']) ?>" /></p>
<p><input type="submit" value="Set Password" id="submit-button" /></p>
</fieldset>
</form>


<? /**************************** END OF STEP 2 ******************************/
break;
case 3:
?>

<form>
<fieldset>
<legend><?= $steps[$step] ?></legend>
We would like to sincerely thank you for registering for the <b><?= $_SESSION['event_name'] ?></b>.  Your information has been received and you should expect to receive confirmation of your registration shortly.  Once your registration has been approved, you will receive further information regarding payment for the event.
<br />
<br />
During the workshop, you will be using the SPK Web Service.  If you do not currently have an SPK account, you may now request an account on that system by <a href="http://newaccount.rfpk.washington.edu/">clicking here</a>.
<br />
</fieldset>
</form>

<? /**************************** END OF STEP 3 ******************************/
break;
}
?>



</div>
</body>
</html>