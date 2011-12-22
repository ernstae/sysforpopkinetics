<%@page contentType="text/html"%>
<html>
<head>
   <title>SPK Login Page</title>
    <link href=stylesheet.css type="text/css" rel=stylesheet>
</head>
<body>
<%-- <jsp:useBean id="beanInstanceName" scope="session" class="package.class" /> --%>
<%-- <jsp:getProperty name="beanInstanceName"  property="propertyName" /> --%>
<table align=left border=0 width=602>
      <tbody> 
	<tr> 
	  <td colSpan=3 vAlign=top>
	    <img align=top alt="RFPK logo" height=40 src="./Images/rfpklogo.gif" width=112>
	      <img align=top alt="Resource Facility for Population Kinetics" height=40 
		src="./Images/rfpkfull.gif" width=474>
	  </td>
	</tr> 
	<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> 
	<tr>
	  <td valign=top width=102 height="0" colspan="1" rowspan="1">
<%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./Images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>Welcome.  Please Log In</h1>
	    <p>
                Please log into MySPK, the "members only" section of
                SPK.
            </p>

            <form action='checkuser.jsp' method=POST>                      
                <P>
                User Name:
                <input name='userName' type='text' size='15'/>
                Password:
                <input name='password' type='text' size='15'/>
                </P>
            </form>

            <h4>About Membership</H2>
            <p>
                MySPK provides the ability to create PK models using the interactive
                Model Design Agent, to compile these models into highly efficient
                machine code, and to run them on a computational cluster.  Because
                the resources of the system (and of the cluster, in particular) are
                limited, membership is by invitation only. 
                RFPK is looking for qualified researchers who would be willing to   
                participate in the ongoing development of SPK by joining our
                our select group of users.  If you think that you would be a good
                candidate for membership, please contact 
                <a href="mailto:vicini@u.washington.edu">Dr. Paolo Vicini</A>, 
                the RFPK Principal Investigator.              
	  </td>
	</tr>
      </tbody>
    </table>
</body>
</html>
