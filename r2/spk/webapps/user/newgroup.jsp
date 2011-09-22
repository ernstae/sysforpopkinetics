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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>
<c:choose>
  <c:when test="${empty param.groupName}">
    <c:set var="groupNameError" scope="request"
      value="Group name missing" />
      <jsp:forward page= "usermain.jsp" />
  </c:when>
  <c:otherwise>
    <sql:query var="group">
      SELECT * FROM team 
        WHERE team_name = ?
      <sql:param value="${param.groupName}" />
    </sql:query>
    <c:if test="${group.rowCount != 0}">
      <c:set var="groupNameError" scope="request" value="Group Name already used" />
        <jsp:forward page= "usermain.jsp" />  
    </c:if>
  </c:otherwise>
</c:choose>
<sql:update>
  INSERT INTO team (team_name) VALUES (?)
  <sql:param value="${param.groupName}" />
</sql:update>
<c:redirect url="usermain.jsp" />
