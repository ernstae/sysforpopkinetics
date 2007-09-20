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
<c:remove var="conversion" />
<html>
  <head>
    <title>User Job List</title>
  </head>
  <body bgcolor="white">

    <%-- Set number of rows to process --%>
    <c:set var="noOfRows" value="${initParam.maxNum}" />

    <%-- Get user id --%>
    <sql:query var="user">
      SELECT user_id FROM user WHERE username = ?
      <sql:param value="${validUser.userName}" />
    </sql:query>

    <%-- Get user jobs --%>
    <c:set var="dbValues" value="${user.rows[0]}" />
    <sql:query var="userJobs" startRow="${param.start}" maxRows="${noOfRows}">
      SELECT start_time,state_code,end_code,abstract FROM job WHERE user_id = ? ORDER BY job_id desc
      <sql:param value="${dbValues.user_id}" />
    </sql:query>

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
	  <td vAlign=top width=150 height="0" colspan="1" rowspan="1">
          <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>User Job List</h3>
	    <p> 
          <c:choose>
            <c:when test="${userJobs.rowCount == 0}">
              No job seems to be in there anymore ...
            </c:when>
            <c:otherwise>
              The following user jobs are found:
            <p>
            <table border="1">
              <th>Submission Time</th>
              <th>State Code</th>
              <th>End Code</th>
              <th>Description</th>
              <jsp:useBean id="conversion" scope="request" class="uw.rfpk.beans.Conversion" />
              <% conversion.initConversion(getServletContext().getInitParameter("database_name"),
                                           getServletContext().getInitParameter("database_host"),
                                           getServletContext().getInitParameter("database_username"),
                                           getServletContext().getInitParameter("database_password")); %>
              <c:forEach items="${userJobs.rows}" var="row">
              <c:set target="${conversion}" property="time" value="${row.start_time}" />
              <c:set target="${conversion}" property="state" value="${row.state_code}" />
              <c:choose>
                <c:when test="${row.end_code != null}"> 
                  <c:set target="${conversion}" property="end" value="${row.end_code}" />
                </c:when>
                <c:otherwise>
                  <c:set target="${conversion}" property="end" value="" />                  
                </c:otherwise>
              </c:choose>
              <tr>
                <td>${fn:escapeXml(conversion.time)}</td>
                <td>${fn:escapeXml(conversion.state)}</td>
                <td>${fn:escapeXml(conversion.end)}</td>
                <td>${fn:escapeXml(row.abstract)}</td>
              </tr>
              </c:forEach>
            </table>
            </c:otherwise>
          </c:choose>
          <p>
          <c:choose>
            <c:when test="${param.start > 0}">
              <a href="userjobs.jsp?start=${param.start - noOfRows}">
                Previous Page</a>
            </c:when>
            <c:otherwise>
                Previous Page
          </c:otherwise>
          </c:choose>
          <c:choose>
            <c:when test="${userJobs.limitedByMaxRows}">
              <a href="userjobs.jsp?start=${param.start + noOfRows}">
                Next Page</a>
            </c:when>
          <c:otherwise>
            Next Page
          </c:otherwise>
          </c:choose>
	    </p>
	    <p> 
               For further information please use a <a href="getmda.jsp">Model Design Agent</a>.
               When you are done, please <a href="logout.jsp">log out</a>.
	    </p>
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
