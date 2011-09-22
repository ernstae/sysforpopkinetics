<?

// this is the showall script, meant to print out all the session and post variables.

if ( $debug > 0 ) {
echo "<strong>Session ID</strong>: " . session_id() . "<br><br>\n";

echo "<strong>Session Variables</strong>:";
var_dump($_SESSION);

echo "<br><br>";

echo "<strong>Request Variables</strong>:  ";
var_dump($_REQUEST);
}

?>
