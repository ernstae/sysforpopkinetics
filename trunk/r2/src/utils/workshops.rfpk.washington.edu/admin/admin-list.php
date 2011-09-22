<?

// include a checker to make sure the user is logged in & session vars are set
// this also include config.php
include ("../includes/authcheck.php");

$status = strip_tags($_REQUEST['status']);
$show_only = strip_tags($_REQUEST['show_only']);

if ( strlen($show_only) > 0 ) {
  $extra_sql = " AND registration_status = '" . $show_only . "' ";
 }

$db = DB::connect($dsn);
$db->setFetchMode(DB_FETCHMODE_OBJECT);

    $valid_status = array ( "approved" => "Approve",
			    "cancelled" => "Cancel",
			    "waitlist" => "Waitlist",
			    "pending" => "Pending" );



$result = $db->query("select p.firstname, p.lastname, p.email, p.address, p.address2, p.city, p.state, p.postal_code, r.request_date, p.affiliation, p.id, r.event_id, r.registration_status, e.name event_name from spkutil.registrations r, spkutil.participants p, spkutil.events e  where r.participant_id=p.id and r.event_id=e.id " . $extra_sql . " order by event_id, r.registration_status asc, r.request_date");

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

<h3>Event Registrations</h3>

<div id="legend">
  <? foreach ( $valid_status as $key => $value ) { ?>
						   <a href="admin-list.php?show_only=<?= $key ?>"><img src="images/status_<?= $key ?>.gif"> <?= $value ?></a>
						 <? } ?>
						 <a href="admin-list.php">Show All</a>
</div>

<table style="font-size: 8pt;">
<?
// looping for data
$counter = 0;
$event_id = 0;
while ( $result->fetchInto($row) ) { 
  $style = 'pending';
  if ( $row->event_id != $event_id ) {
    $event_id = $row->event_id;
    ?>
<tr>
<td style="padding-top:50px;" colspan="4"><b><?= $row->event_name ?></b></td>
</tr>
<tr>
  <td style="width:5px">Status</td>
  <td style="width:150px">Name</td>
  <td style="width:75px">Email</td>
  <td style="width:150px">Company</td>
  <td style="width:100px">Registration Date</td>
  <td style="width:60px">Action</td>
</TR>
   <? } ?>

<? if ( $row->registration_status === 'approved' ) $style = "approved"; ?>
<TR class="<?= $style ?>">
   <td style="width:16px" align="center"><?= '<img src="images/status_' . $row->registration_status . '.gif" />' ?>
</td>
<td><?= $row->firstname . " " .  $row->lastname ?></td>
<td><a href="mailto:<?= $row->email ?>?subject=<?= $row->event_name ?>"><?= $row->email ?></a></td>
    <td><?= $row->affiliation ?><br /><?= $row->address . " " . $row->address2 . ", " . $row->city . ", " . $row->state . " " . $row->postal_code ?></td>
<td><?= $row->request_date ?></td>
<td>

<?

  foreach ( $valid_status as $key => $value ) {
    if ( $row->registration_status != $key ) {
      echo '<a href="admin-action.php?a=' . $key . '&eid=' . $row->event_id . '&pid=' . $row->id . '"><img src="images/status_' . $key . '.gif" alt="' . $value . '" border="0"></a>';
    }
  }
?>
</td>
</tr>

<?
    }
?>
</table>

</div>