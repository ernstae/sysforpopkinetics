<?

include ("../includes/authcheck.php");

$pid = strip_tags($_REQUEST['pid']);
$eid = strip_tags($_REQUEST['eid']);

$action = strip_tags($_REQUEST['a']);

if ( eregi("approved|cancelled|waitlist|pending", $action) ) {
  if ( change_registration_status($eid, $pid, $action ) ) {
    Header("Location: admin-list.php?status=1\r\n");
  }
 }
?>

If you are reading this message, there was an error passing in a valid action type, participant ID or Event ID.

Please <a href="admin-list.php">click here</a> and try again!