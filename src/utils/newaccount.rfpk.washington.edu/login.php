<!---------------------------------------------------------------------
From:   Resource Facility for Population Kinetics                    
        Department of Bioengineering Box 352255                      
        University of Washington                                     
        Seattle, WA 98195-2255                                       

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
---------------------------------------------------------------------->
<!--
author: Andrew Ernst (ernst@u)
-->

<? include ('includes/header.php'); ?>

<div id="content">
	      <h3>Welcome.  Account Members, Please Log In</h3>
            
          
            <p>
            <font color="red">
              Please log in first.
            </font>

            </p>
            <form action="checkuser.jsp" method="post">
              <input type="hidden" name="origURL" value="https://spk.rfpk.washington.edu/user/usermodels.jsp">                  
              <table border="0" cellspacing = "5">
                <tr>
                  <th align="right">User Name:</th>
                  <th align="left"><input type="text" name="userName" ></td>
                </tr>
                <tr>

                  <th align="right">Password:</th>
                  <th align="left"><input type="password" name="password" ></td>
                </tr>
                <tr>
                  <th align="right"><input type="Submit" value="Log In"></th>
                  <th align="left"><input type="Reset"></td>
                </tr>
              </table>

            </form>
	    <p>
              
                
                  First time users may want to view this <a href="WebHelp/spkstart.htm" target="_blank">Getting Started</a> example.<br><br>
                  When you are done, please <a href="logout.jsp">log out</a>.
                
                
              
            <br></p>
            <h3>Obtaining an Account</h3>

            <p>
                MySPK provides the ability to create population kinetic models using the interactive
                Model Design Agent, to compile these models into highly efficient
                machine code, and to run them on a computational cluster (according to our <a href="RFPK_SPK_TERMS_OF_SERVICE.html" target="_blank">Terms of Service</a>). To obtain an account, please <a href="newaccount.php">click here</a>
	    </p> 
</div>
  </body>

</html>
