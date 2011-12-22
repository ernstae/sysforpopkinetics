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
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

    <%-- Get group users --%>
    <sql:query var="group">
      SELECT user_id,username,first_name,surname,company,country,state,email,test,dev,team_id FROM user WHERE team_id ="${param.groupId}"
    </sql:query>

<html>
  <head>
    <title>Group Information</title>
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
	    <h3>Group Information</h3>
	    <p> 
          <c:choose>
            <c:when test="${group.rowCount == 0}">
              No user seems to be in this group ...
            <p>             
              <form action="updategroup.jsp" method="post">
                Group ID: ${fn:escapeXml(param.groupId)}<br>
                Group Name:<input type="text" name="groupName" value="${fn:escapeXml(param.groupName)}">
                <input type="submit" value="Update"><br>
                <font color="red">${fn:escapeXml(groupNameError)}</font><br>
                <input type="hidden" name="groupId" value="${fn:escapeXml(param.groupId)}">
              </form>
            <p>
            </c:when>
            <c:otherwise>
              The following group information are found:
            <p>             
              <form action="updategroup.jsp" method="post">
                Group ID: ${fn:escapeXml(param.groupId)}<br>
                Group Name:<input type="text" name="groupName" value="${fn:escapeXml(param.groupName)}">
                <input type="submit" value="Update"><br>
                <font color="red">${fn:escapeXml(groupNameError)}</font><br>
                <input type="hidden" name="groupId" value="${fn:escapeXml(param.groupId)}">
              </form>
            <p>
            <table border="1">
              <th>User ID</th>
              <th>Username</th>
              <th>First Name</th>
              <th>Last Name</th>
              <th>Company</th>
              <th>State</th>
              <th>Country</th>
              <th>Email</th>
              <th>Tester?</th>
              <th>Developer?</th>
              <c:forEach items="${group.rows}" var="row">
                <tr>
                  <td><a href=getuser.jsp?userName=${fn:escapeXml(row.username)}&password=${fn:escapeXml(row.password)}>${fn:escapeXml(row.user_id)}</a></td>
                  <td>${fn:escapeXml(row.username)}</td>
                  <td>${fn:escapeXml(row.first_name)}</td>
                  <td>${fn:escapeXml(row.surname)}</td>
                  <td>${fn:escapeXml(row.company)}</td>
                  <td>${fn:escapeXml(row.state)}</td>
                  <td>${fn:escapeXml(row.country)}</td>
                  <td>${fn:escapeXml(row.email)}</td>
                  <c:choose>
                    <c:when test="${row.test == '1'}">
                      <td>Yes</td>
                    </c:when>
                    <c:otherwise>
                      <td>No</td>
                    </c:otherwise>
                  </c:choose>
                  <c:choose>
                    <c:when test="${row.dev == '1'}">
                      <td>Yes</td>
                    </c:when>
                    <c:otherwise>
                      <td>No</td>
                    </c:otherwise>
                  </c:choose>
                </tr>
              </c:forEach>
            </table>
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
