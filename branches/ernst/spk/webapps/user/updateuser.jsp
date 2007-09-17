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
author: Jiaji Du
-->
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@page contentType="text/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
  <head>
    <title>Updating User Account</title>
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
	    <h3>Updating User Account</h3>
            <p>
              Please enter information about an existing user below and click the submit button to update the user account.
              <form action="validate.jsp?task=update" method="post">
              <table>
                <sql:query var="user">
                  SELECT * FROM user WHERE user_id = ?
                  <sql:param value="${param.userId}" />
                </sql:query>
                <c:set var="dbValues" value="${user.rows[0]}" />
                <c:set var="bugLogin" value="${dbValues.email}" scope="session" />
                <c:set var="userId" value="${param.userId}" scope="session" />
                <tr>
                  <td>Username:</td>
                  <td><input type="text" name="userName" value="${fn:escapeXml(dbValues.username)}"></td>
                  <td><font color="red">${fn:escapeXml(userNameError)}</font></td>
                </tr>
                <tr>
                  <td>Password:</td>
                  <td><input type="text" name="password" ></td>
                </tr>
                <tr>
                  <td>First Name:</td>
                  <td><input type="text" name="firstName" value="${fn:escapeXml(dbValues.first_name)}"></td>
                  <td><font color="red">${fn:escapeXml(firstNameError)}</font></td>
                </tr>
                <tr>
                  <td>Last Name:</td>
                  <td><input type="text" name="lastName" value="${fn:escapeXml(dbValues.surname)}"></td>
                  <td><font color="red">${fn:escapeXml(lastNameError)}</font></td>
                </tr>
                <tr>
                  <td>Company:</td>
                  <td><input type="text" name="company" value="${fn:escapeXml(dbValues.company)}"></td> 
                  <td><font color="red">${fn:escapeXml(companyNameError)}</font></td>
                </tr>
                <tr>
                  <td>State:</td>
                  <td><input type="text" name="state" value="${fn:escapeXml(dbValues.state)}"></td>
                  <td><font color="red">${fn:escapeXml(stateNameError)}</font></td>
                </tr>
                <tr>
                  <td>Country:</td>
                  <td><input type="text" name="country" value="${fn:escapeXml(dbValues.country)}"></td>
                  <td><font color="red">${fn:escapeXml(countryNameError)}</font></td>
                </tr>
                <tr>
                  <td>Email:</td>
                  <td><input type="text" name="email" value="${fn:escapeXml(dbValues.email)}"></td>
                  <td><font color="red">${fn:escapeXml(emailAddressError)}</font></td>
                </tr>
                <tr>
                  <td>Group ID:</td>
                  <td><input type="text" name="teamId" value="${fn:escapeXml(dbValues.team_id)}"></td>
                  <td><font color="red">${fn:escapeXml(teamIdError)}</font></td>
                </tr>
                <tr>
                  <td>Tester?:</td>
                  <td><input type="checkbox" name="tester"
                    <c:if test="${dbValues.test == 1}">
                      checked
                    </c:if>
                    value="1">
                  </td>
                </tr>
                <tr>
                  <td>Developer?:</td>
                  <td><input type="checkbox" name="developer"
                    <c:if test="${dbValues.dev == 1}">
                      checked
                    </c:if>
                    value="1">
                  </td>
                </tr>
                <tr>
                  <th align="left"><input type="submit" value="Submit"></th>
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
