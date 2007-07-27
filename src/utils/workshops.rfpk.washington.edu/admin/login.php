<?

include ("../includes/config.php");

$username = strip_tags($_REQUEST['username']);
$password = strip_tags($_REQUEST['password']);


$sql = "select count(d.username) cid from spkdb.user d, spkutil.admins a where d.user_id=a.user_id and a.level <= 1 and d.username='" . $username . "' and d.password=md5('" . $password . "')";

$result = $db->query($sql);

$row = $result->fetchRow();

if ( $row->cid == 1 ) 
  {
    $_SESSION['rfpkadmin'] = md5($special_key);
    unset($_SESSION['errors']);
    Header("Location: admin-list.php\r\n");
  }
 else 
   {
     $error = array ('login' => 'Your login attempt failed. Please try again');
     $_SESSION['errors'] = $error;
     Header("Location: admin.php?auth=fail\r\n");
   }

?>