<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@page contentType="text/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<html>
  <head>
   <title>SPK Login Page</title>
    <link href=stylesheet.css type="text/css" rel=stylesheet>
  </head>
  <body>
    <table align=left border=0 width=602>
      <tbody> 
	<tr> 
	  <td colSpan=3 vAlign=top>
	    <img align=top alt="RFPK logo" height=40 src="./images/rfpklogo.gif" width=112>
	      <img align=top alt="Resource Facility for Population Kinetics" height=40 
		src="./images/rfpkfull.gif" width=474>
	  </td>
	</tr> 
	<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> 
	<tr>
	  <td valign=top width=150 height="0" colspan="1" rowspan="1">
<%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>Welcome.  Please Log In</h1>
            <p>
            <font color="red">
              ${fn:escapeXml(param.errorMsg)}
            </font>
            </p>
            <p>
                To use SPK you need to have Java Runtime Environment (JRE) on your computer. 
                Instructions for downloading and installing JRE are provided 
                <a href="instructWin.jsp">here</a> for Windows users 
                and <a href="instructLin.jsp">here</a> for Linux users.
            </p>
	    <p>
                Please log into MySPK, the "members only" section of SPK. When you are done, please <a href="logout.jsp">log out</a>.
            </p>

            <form action="checkuser.jsp" method="post">
              <input type="hidden" name="OrigURL" value="${fn:escapeXml(param.origURL)}">                  
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

            <h4>About Membership</H2>
            <p>
                MySPK provides the ability to create PK models using the interactive
                Model Design Agent, to compile these models into highly efficient
                machine code, and to run them on a computational cluster.  Because
                the resources of the system (and of the cluster, in particular) are
                limited, membership is by invitation only. 
                RFPK is looking for qualified researchers who would be willing to   
                participate in the ongoing development of SPK by joining our
                selected group of users.  If you think that you would be a good
                candidate for membership, please contact 
                <a href="mailto:vicini@u.washington.edu">Dr. Paolo Vicini</A>, 
                the RFPK Principal Investigator.              
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
