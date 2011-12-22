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

<html>
  <head>
    <title>SPK Usage List</title>
  </head>
  <body bgcolor="white">

    <%-- Set number of rows to process --%>
    <c:set var="noOfRows" value="${initParam.maxNum}" />

    <%-- Get users --%>
    <sql:query var="userList"
      sql="SELECT job.user_id,user.username,user.company,user.country,user.test,user.dev,COUNT(job.job_id) AS jobs FROM job,user WHERE job.user_id=user.user_id GROUP BY job.user_id" 
      startRow="${param.start}" maxRows="${noOfRows}"
    />
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
	    <h3>SPK Usesage List</h3>
	    <p> 
          <c:choose>
            <c:when test="${userList.rowCount == 0}">
              No one seems to use it anymore ...
            </c:when>
            <c:otherwise>
              The following usages are found:
            <p>
            <table border="1">
              <th>User ID</th>
              <th>Username</th>
              <th>Company</th>
              <th>Country</th>
              <th>Tester</th>
              <th>Developer</th>
              <th>Jobs</th>
              <c:forEach items="${userList.rows}" var="row">
              <tr>
                <td>${fn:escapeXml(row.user_id)}</td>
                <td>${fn:escapeXml(row.username)}</td>
                <td>${fn:escapeXml(row.company)}</td>
                <td>${fn:escapeXml(row.country)}</td>
                <c:choose>
                  <c:when test="${row.test == 'true'}">
                    <td>Yes</td>
                  </c:when>
                  <c:otherwise>
                    <td>No</td>
                  </c:otherwise>
                </c:choose>
                <c:choose>
                  <c:when test="${row.dev == 'true'}">
                    <td>Yes</td>
                  </c:when>
                  <c:otherwise>
                    <td>No</td>
                  </c:otherwise>
                </c:choose>
                <td>${fn:escapeXml(row.jobs)}</td>
              </tr>
              </c:forEach>
            </table>
            </c:otherwise>
          </c:choose>
          <p>
          <c:choose>
            <c:when test="${param.start > 0}">
              <a href="usagelist.jsp?start=${param.start - noOfRows}">
                Previous Page</a>
            </c:when>
            <c:otherwise>
                Previous Page
          </c:otherwise>
          </c:choose>
          <c:choose>
            <c:when test="${userList.limitedByMaxRows}">
              <a href="usagelist.jsp?start=${param.start + noOfRows}">
                Next Page</a>
            </c:when>
          <c:otherwise>
            Next Page
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
