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
  <title>Member Main Page</title>
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
	    <h3>Welcome ${fn:escapeXml(validUser.firstName)}  ${fn:escapeXml(validUser.lastName)}</h3>
	    <p>
               Select one of the following:
            </p>
            <p>
            <c:choose>
              <c:when test="${validUser.userName == 'useradmin'}">
                <a href="chpassword.jsp">Change my password</a><br>
                <a href="userlist.jsp">View all user list</a><br>
                <a href="enteruser.jsp?task=addnew">Add new user account</a><br>
                <a href="enteruser.jsp?task=update">Update user account</a><br>
                <a href="searchuser.jsp">Search in user database</a>
              </c:when>
              <c:otherwise>
                <a href="chpassword.jsp">Change my password</a><br>
                <a href="getmda.jsp">Get my Model Design Agent</a><br>
                <a href="userjobs.jsp">View my job status</a><br>
                <a href="usermodels.jsp?start=0&counter=0">View my model archive</a><br>
                <a href="userdatasets.jsp?start=0&counter=0">View my dataset archive</a><br>
                <a href="examplejobs.jsp?start=0&count=0">View example jobs</a><br>
                <a href="modellibrary.jsp?start=0&counter=0">View model library</a><br>
                <a href="datasetlibrary.jsp?start=0&counter=0">View dataset library</a><br>
                <a href="userinfo.jsp">View my account information</a>
              </c:otherwise> 
            </c:choose>       
            </p>
            <p>
               When you are done, please <a href="logout.jsp">log out</a>.
            </p>
       	  </td>
	</tr>     
      </tbody>
    </table>
  </body>
</html>
