<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@ page contentType="text/html" %>
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
              The following user information are found:
            <p>
            <table border="1">
              <th>User ID</th>
              <th>User Name</th>
              <th>First Name</th>
              <th>Last ID</th>
              <tr>
                <td>${fn:escapeXml(validUser.userId)}</td>
                <td>${fn:escapeXml(validUser.userName)}</td>
                <td>${fn:escapeXml(validUser.firstName)}</td>
                <td>${fn:escapeXml(validUser.lastName)}</td>
              </tr>
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
