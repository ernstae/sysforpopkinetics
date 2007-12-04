<?

// include a checker to make sure the user is logged in & session vars are set
// this also include config.php
include ("../includes/authcheck.php");

$id = strip_tags($_REQUEST['id']);

$db = DB::connect($dsn);
$db->setFetchMode(DB_FETCHMODE_OBJECT);

$result = $db->query("select id, message, from_email, subject from spkutil.email_messages m where id = " . $id);

?>

<style>
<!-- 
table th {
  font-style: bold;
}

table tr {
 color: #c0c0c0;
}


-->
</style>

<? include("../includes/header.php"); ?>

<div id="content">

<h3>Email Message Editor</h3>


<form method="POST" action="email-edit.php">
  <input type="hidden" name="id" value="<?= $result->id ?>">
  <input type="hidden" name="action" value="save">

<fieldset>
<legend>Email Message</legend>
<label for="from_email">From:</label><input type="text" name="from_email" id="from_email" maxlength="128" /><br />
<label for="subject">Subject:</label><input type="text" name="subject" id="subject" maxlength="128" /><br />
<label for="message">Message:</label><textarea name="message" id="message"></textarea>
</fieldset>
<input type="submit" name="submit" value="Save" />
</form>


</div>
</body>
</html>
