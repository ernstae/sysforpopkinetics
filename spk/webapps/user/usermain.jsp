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
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<%-- Verify that the user is useradmin when SPK is not available --%>
<c:if test="${notice == '1' && validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp" />
</c:if>

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
                <a href="userlistenter.jsp">View all user list</a><br>
                <a href="enteruser.jsp?">Add new user account</a><br>
                <a href="userlistupdate.jsp">Update user account</a><br>
                <a href="searchuser.jsp">Search in user database</a><br>
                <a href="usagelist.jsp">View SPK usage list</a><br>
                <a href="sendmail.jsp">Send email to users</a><br>
                <a href="grouplist.jsp">View all user groups</a><br>
                <form action="newgroup.jsp" method="post">
                  New group:<input type="text" name="groupName" value="${fn:escapeXml(param.groupName)}">
                  <input type="submit" value="Submit"><br>
                  <font color="red">${fn:escapeXml(groupNameError)}</font><br>
                </form>
                <form action="notice.jsp" method="post">
                  <c:choose>
                    <c:when test="${notice == '1'}">
                      <input type="checkbox" name="alert" checked value="1">Display maintenance notice&nbsp
                    </c:when>
                    <c:otherwise>
                      <input type="checkbox" name="alert" value="1">Display maintenance notice&nbsp
                    </c:otherwise>
                  </c:choose>
                  <input type="submit" value="Set"><br>
                </form>
                <c:choose>
                  <c:when test="${notice == '1'}">
                    <c:out value="Maintenance Notice is ON." />
                  </c:when>
                  <c:otherwise>
                    <c:out value="Maintenance Notice is OFF." />
                  </c:otherwise>
                </c:choose>
              </c:when>
              <c:otherwise>
                <a href="getmda.jsp">Get my Model Design Agent</a><br>
                <a href="userjobs.jsp">View my job status</a><br>
                <a href="usermodels.jsp?start=0&counter=0">View my model archive</a><br>
                <a href="userdatasets.jsp?start=0&counter=0">View my dataset archive</a><br>
                <a href="examplejobs.jsp?start=0&count=0">View example jobs</a><br>
                <a href="modellibrary.jsp?start=0&counter=0">View model library</a><br>
                <a href="datasetlibrary.jsp?start=0&counter=0">View dataset library</a><br>
                <a href="userinfo.jsp">View my account information</a><br>
                <a href="chpassword.jsp">Change my password</a><br>
                <% String url = getServletContext().getInitParameter("bugzillaURL");
                   String bugzilla = "<a href=" + url + ">Go to SPK bug report page</a>"; %>
                <%=bugzilla%><br>
                <c:if test="${validUser.developer == '1' && pageContext.request.serverPort==8443}">
                  <a href="userlistdev.jsp">View all user list</a>                
                </c:if>
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
