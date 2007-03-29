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
<%@ page contentType="text/html" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>
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
    <title>User Account Information</title>
  </head>
  <body bgcolor="white">
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
	  <td vAlign=top width=102 height="0" colspan="1" rowspan="1">
          <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>User Account Information</h3>
	    <p> 
          <c:choose>
            <c:when test="${userList.rowCount == 0}">
              No user information was found ...
            </c:when>
            <c:otherwise>
              <c:if test="${validUser.teamId != 0}"> 
                <sql:query var="db" >
                  SELECT team_name FROM team WHERE team_id = ?
                  <sql:param value="${validUser.teamId}" />
                </sql:query>
                <c:set var="dbValue" value="${db.rows[0]}" />
              </c:if>
              The following user information are found as bellow.<br>You may update the Company, State, Country, Email. 
            <p>
                
            <form action="validateuser.jsp" method="POST">
                <c:set var="bugLogin" value="${validUser.email}" scope="session" />
            <table border="0">
              <tr>
                  <td>User ID</td>
                  <td>${fn:escapeXml(validUser.userId)}</td>
              </tr>
              <tr>
                  <td>Group Name:</td>
                  <td>${fn:escapeXml(dbValue.team_name)}</td>
              </tr>
              <tr>
                  <td>Username:</td>
                  <td>${fn:escapeXml(validUser.userName)}</td>
              </tr>
              <tr>
                  <td>First Name:</td>
                  <td>${fn:escapeXml(validUser.firstName)}</td>
              </tr>
              <tr>
                  <td>Last Name:</td>
                  <td>${fn:escapeXml(validUser.lastName)}</td>
              </tr>
              <tr>
                  <td>Company:</td>
                  <td><input type="text" name="company" value="${fn:escapeXml(validUser.company)}" size="40" /></td>
                  <td><font color="red">${fn:escapeXml(companyNameError)}</font></td>
              </tr>
              <tr>
                  <td>State:</td>
                  <td><input type="text" name="state" value="${fn:escapeXml(validUser.state)}" size="20" /></td>
                  <td><font color="red">${fn:escapeXml(stateNameError)}</font></td>
              </tr>
              <tr>
                  <td>Country:</td>
                  <td><input type="text" name="country" value="${fn:escapeXml(validUser.country)}" size="20" /></td>
                  <td><font color="red">${fn:escapeXml(countryNameError)}</font></td>
              </tr>
              <tr>
                  <td>Email:</td>
                  <td><input type="text" name="email" value="${fn:escapeXml(validUser.email)}" size="40" /></td>
                  <td><font color="red">${fn:escapeXml(emailAddressError)}</font></td>
              </tr>
              <tr><td>Tester?</td>
                <c:choose>
                  <c:when test="${validUser.tester == '1'}">
                    <td>Yes</td></tr>
                  </c:when>
                  <c:otherwise>
                    <td>No</td></tr>
                  </c:otherwise>
                </c:choose>
              <tr><td>Developer?</td>
                <c:choose>
                  <c:when test="${validUser.developer == '1'}">
                    <td>Yes</td></tr>
                  </c:when>
                  <c:otherwise>
                    <td>No</td></tr>
                  </c:otherwise>
                </c:choose>
              </tr>
            </table>
            <br><input type="submit" name="enter" value="Update" />
            </form>
            </c:otherwise>
          </c:choose>
            <p>
               When you are done, please <a href="logout.jsp">log out</a>.
	    </p>
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
