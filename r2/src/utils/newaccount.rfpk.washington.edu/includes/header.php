<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<?
if (! $title ) {
	$title = '';
}
?>

<html>
<head>
   <title>SPK: The System for Population Kinetics : <? print($title); ?></title>
   <link href="/css/stylesheet.css" type="text/css" rel="stylesheet" />
</head>
<body>

<div class="header">
<img alt="RFPK logo" height="40" width="112" src="/images/rfpklogo.gif" />
<img alg="Resource Facility for Population Kinetics" height="40" width="474" src="/images/rfpkfull.gif" />
</div>

<!--- navigational links --->
<div id="leftnav" class="quick">
<p>Quick Links</p>

    <a class=quick href="http://depts.washington.edu/rfpk/">RFPK Home</a><br/>
    <a class=quick href="https://spk.rfpk.washington.edu/info/index.jsp">SPK Home</a><br />
    <a class=quick href="https://spk.rfpk.washington.edu/user/index.jsp">MySPK</a><br />
    <a class="quick" href="/start.php">Request Account</a> <br />

<br />

   <? if ( isset($_SESSION['rfpkadmin'])) { ?>
    <a class="quick" href="/admin-list.php?status=1">Pending</a> <br />
    <a class="quick" href="/admin-list.php?status=3">Approved</a> <br />
    <a class="quick" href="/admin-list.php?status=2">Deferred</a> <br />
<?   }   ?>

</div>
<!--- end navigational links --->