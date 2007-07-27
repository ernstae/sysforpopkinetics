<?

include ("config.php");

if ( strcmp($_SESSION['rpfkadmin'],md5($special_key)) ) {
  //  echo "Authentication Exists"; // do nothing
    }
 else {
   //   echo("[" . $_SESSION['rfpkadmin'] . "] != [" . md5($special_key) . "]");
   Header("Location: admin.php?auth=none\r\n");
 }

?>