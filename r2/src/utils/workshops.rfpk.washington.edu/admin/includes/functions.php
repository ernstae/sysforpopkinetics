<?

// status page

function change_registration_status( $eid, $pid, $newstatus ) {
  global $db;

  $query = $db->prepare ("UPDATE spkutil.registrations set registration_status=? WHERE event_id=? AND participant_id=?");
  
  $data = array ( $newstatus, $eid, $pid );
  
  if (DB::isError($res = $db->execute($query, $data)) ) {
    // get the native backend error
    // and the last query
    die ($res->getDebugInfo());
    return false;
  }
  
}

?>