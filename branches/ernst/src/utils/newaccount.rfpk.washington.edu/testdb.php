<?

include ("includes/config.php");

$db = DB::connect($dsn);
$db->setFetchMode(DB_FETCHMODE_OBJECT);
$result = $db->query("select first_name, surname, email from spkutil.user_request where status IN (1)");

?>

<? include("includes/header.php"); ?>

<link rel="stylesheet" href="includes/style.css" type="text/css" />
<script type='text/javascript' src='includes/common.js'></script>
<script type='text/javascript' src='includes/css.js'></script>

<script type='text/javascript' src='includes/standardista-table-sorting.js'></script>

<div id="content">

<p>
PENDING | APPROVED | DENIED
</p>

<table class="sorttable">
<thead>
<TR>
  <th>Actions</th>
  <th>Name</th>
  <th>Company</th>
  <th>Email</th>
</TR>
</thead>
<tbody>
<? 
// looping for data
$counter = 0;
while ( $row = $result->fetchRow() ) { 
?>
<tr<? if ( $counter % 2 != 0 ) { echo(" class='odd'"); } ?>>
  <td><a href="#">APPROVE</a> | <a href="#">VIEW</a></td>
  <TD><?= $row->first_name ?> <?= $row->surname ?></TD>
  <td><?= $row->organization ?></td>
  <td><?= $row->email ?></td>
</tr>
<?  $counter++;
} // end of while row 
?>
</tbody>
</table>

</div>