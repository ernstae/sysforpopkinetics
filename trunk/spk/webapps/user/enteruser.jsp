<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@page contentType="text/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
  <head>
    <title>User Information</title>
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
	  <td valign=top width=102 height="0" colspan="1" rowspan="1">
            <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>User Information</h3>
            <p>
               Please enter information about a user below:  If the user's account already exists in the 
               database recognized by the User Name, the user's account will be updated, otherwise, a new 
               user account will be added to the database, using the information entered below.
            </p>
            <form action="validate.jsp" method="post">
              <table>
                <tr>
                  <td>User Name:</td>
                  <td><input type="text" name="userName"
                    value="${fn:escapeXml(param.userName)}">
                  </td>
                  <td><font color="red">${fn:escapeXml(userNameError)}</font></td>
                </tr>
                <tr>
                  <td>Password:</td>
                  <td><input type="text" name="password"
                    value="${fn:escapeXml(param.password)}">
                  </td>
                  <td><font color="red">${fn:escapeXml(passwordError)}</font></td>
                </tr>
                <tr>
                  <td>First Name:</td>
                  <td><input type="text" name="firstName"
                    value="${fn:escapeXml(param.firstName)}">
                  </td>
                  <td><font color="red">${fn:escapeXml(firstNameError)}</font></td>
                </tr>
                <tr>
                  <td>Last Name:</td>
                  <td><input type="text" name="lastName"
                    value="${fn:escapeXml(param.lastName)}">
                  </td>
                  <td><font color="red">${fn:escapeXml(lastNameError)}</font></td>
                </tr>
                <tr>
                  <th align="right"><input type="submit" value="Submit"></th>
                  <th align="left"><input type="Reset"></td>
                </tr>
              </table>
            </form>                   
            <p>
               When you are done, please <a href="logout.jsp">log out</a>.
            </p>
       	  </td>
	</tr>     
      </tbody>
    </table>
  </body>
</html>
