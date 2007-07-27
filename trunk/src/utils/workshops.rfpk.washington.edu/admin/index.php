<? 

include ('../includes/header.php');
include ('../includes/config.php');


?>

<div id="content">
	      <h3>Workshop Management</h3>


<?= show_errors() ?>

          
            <form action="login.php" method="post">

    <label for="username">User Name:</label>
    <input type="text" name="username" id="username">
  <br />

    <label for="password">Password:</label>
    <input type="password" name="password" id="password">

    <input type="Submit" value="Log In">

            </form>
	    <p>
              
</div>
  </body>

</html>
