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
<h3>Request New Account : <?= $steps[$step] ?></h3>

<p class="small">This is step <?= $step+1 ?> of <?= sizeof($steps) ?></p>

<?= show_errors() ?>

<? 
switch ($step) {
case 0:
?>

<p>
We ask that you please review our terms of service before requesting a user account on the SPK Service.
</p>

<form>
<fieldset>
<legend><?= $steps[$step] ?></legend>
<textarea name="termsandconditons" cols="80" rows="25">
<? include ("includes/termsandconditons.txt"); ?>
</textarea>
</fieldset>
</form>
<form method="POST" action="process.php"><input type="hidden" name="step" value="<?= $step ?>"><input type="hidden" name="terms" value="accept"><p><input type="submit" name="submit" value="Accept" /></form> 
<form method="POST" action="process.php"><input type="hidden" name="step" value="<?= $step ?>"><input type="hidden" name="terms" value="decline"><p><input type="submit" name="submit" value="Decline" /></form>
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
<fieldset>
<p>Thank you for your interest in SPK. In order for us to issue an account, please provide us with the following information:</p>
<legend><?= $steps[$step] ?></legend>
<p><label for="firstname">First Name</label> <input type="text" id="firstname" name="firstname" maxlength="15" value="<?= strip_tags($_SESSION['firstname']) ?>" /></p>
<p><label for="lastname">Last Name</label> <input type="text" id="lastname" name="lastname" maxlength="25" value="<?= strip_tags($_SESSION['lastname']) ?>" /></p>
<p><label for="organization">Company</label> <input type="text" id="organization" name="organization" maxlength="50" value="<?= strip_tags($_SESSION['organization']) ?>"  /></p>
<p><label for="city">City</label> <input type="text" id="city" name="city" maxlength="25" value="<?= strip_tags($_SESSION['city']) ?>"  /></p>
<p><label for="state">State/Province</label> <input type="text" id="state" name="state" maxlength="25" value="<?= strip_tags($_SESSION['state']) ?>" /></p>
<p><label for="country">Country</label> <select name="country" id="country"><?= country_drop($_SESSION['country']) ?></select></p>
<p></p>
<p><label for="email">Email</label> <input type="text" id="email" name="email" maxlength="80" value="<?= strip_tags($_SESSION['email']) ?>" /></p>
<p><input type="submit" value="Continue" /></p>
</fieldset>
</form>



<? /**************************** END OF STEP 1 ******************************/
break;
case 2:
?>

<p>You may now choose a username for use on our system.</p>

<form method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<p><label for="username">Username:</label> <input type="text" id="username" name="username" maxlength="10" value="<?= strip_tags($_SESSION['username']) ?>"  /></p>
<p><input type="submit" value="Request Username" /></p>
</fieldset>
</form>

<? /**************************** END OF STEP 2 ******************************/
break;
case 3:
?> 

<p>Now that your username has been assigned, please set your password for the account.  Your password must be at least four (4) characters.</p>

<form method="POST" action="process.php">
<input type="hidden" name="step" value="<?= $step ?>">
<fieldset>
<legend><?= $steps[$step] ?></legend>
<p><label for="username">Username:</label> <?= strip_tags($_SESSION['username']) ?></p>
<p><label for="password">Password:</label> <input type="password" id="password" name="password" maxlength="10" value="<?= strip_tags($_SESSION['password']) ?>" /></p>
<p><label for="password_confirm">Pasword(again):</label> <input type="password" id="password_confirm" name="password_confirm" maxlength="10" value="<?= strip_tags($_SESSION['password_confirm']) ?>" /></p>
<p><input type="submit" value="Set Password" /></p>
</fieldset>
</form>


<? /**************************** END OF STEP 3 ******************************/
break;
case 4:
?>

<form>
<fieldset>
<legend><?= $steps[$step] ?></legend>
Thank you for requesting access to the SPK Service.  The information you have provided will be used to issue your personal account on SPK, and is needed in order to provide user demographic information to our sponsors and to improve our web site.
<br />
<br />
You should expect your account to be issued within 24 hours, and you will receive a confirmation email.
<br />
<br />
You may <a href="start.php">request another account</a> now.
</fieldset>
</form>

<? /**************************** END OF STEP 3 ******************************/
break;
}
?>



</div>
</body>
</html>