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

<%-- Remove the validUser session bean, if any --%>
<c:remove var="validUser" />
<c:remove var="digest" />

<%-- Check if the user has logged in. --%>
<c:if test="${empty param.userName || empty param.password}">
  <c:redirect url="index.jsp" >
    <c:param name="errorMsg" value="You must enter a User Name and Password." />
  </c:redirect>
</c:if>

<%-- Create a bean for digesting password. --%>
<jsp:useBean id="digest" scope="session" class="uw.rfpk.beans.DigestPassword" >
  <c:set target="${digest}" property="password" value="${param.password}" />
</jsp:useBean>

<%--
  See if the user name and password combination is valid. If not
  redirect back to the login page with a message.
--%>
<sql:query var="userInfo">
  SELECT * FROM user WHERE username = ? AND password = ?
  <sql:param value="${param.userName}" />
  <sql:param value="${param.password}" />
</sql:query>

<c:if test="${userInfo.rowCount == 0}">
  <c:redirect url="index.jsp" >
    <c:param name="errorMsg" value="The User Name or Password you entered is not valid." />
  </c:redirect>
</c:if>

<%--
  Create a UserBean and save it in the session
  scope and redirect to the appropriate page
--%>
<c:set var="dbValues" value="${userInfo.rows[0]}" />
<jsp:useBean id="validUser" scope="session" class="uw.rfpk.beans.UserInfo" >
  <c:set target="${validUser}" property="userId" value="${dbValues.user_id}" />
  <c:set target="${validUser}" property="userName" value="${dbValues.username}" />
  <c:set target="${validUser}" property="firstName" value="${dbValues.first_name}" />
  <c:set target="${validUser}" property="lastName" value="${dbValues.surname}" />
  <c:set target="${validUser}" property="company" value="${dbValues.company}" />
  <c:set target="${validUser}" property="country" value="${dbValues.country}" />
  <c:set target="${validUser}" property="state" value="${dbValues.state}" />
  <c:set target="${validUser}" property="email" value="${dbValues.email}" />
  <c:set target="${validUser}" property="tester" value="${dbValues.test}" />
  <c:set target="${validUser}" property="developer" value="${dbValues.dev}" />
  <c:set target="${validUser}" property="teamId" value="${dbValues.team_id}" />
</jsp:useBean>
 
<c:choose>
  <c:when test="${!empty param.origURL}">
    <c:redirect url="${param.origURL}" />
  </c:when>
  <c:otherwise>
    <c:redirect url="usermain.jsp" />
  </c:otherwise>
</c:choose>

