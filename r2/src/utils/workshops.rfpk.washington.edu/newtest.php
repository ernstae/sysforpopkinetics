<?

include ("includes/config.php");


$result = $db->query("select * from spkutil.user_request where status=3");

while ( $result->fetchInto($row) ) {
  notify_customer_approved($row->username, $row->first_name, $row->surname, $row->email);
 }

?>