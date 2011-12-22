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
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<%-- Create a bean for digesting password --%>
<c:set target="${digest}" property="password" value="${param.password}" />

<%-- Create a bean for crypting password. --%>
<jsp:useBean id="crypt" scope="request" class="uw.rfpk.beans.CryptPassword" >
  <c:set target="${crypt}" property="password" value="${param.password}" />
  <c:set target="${crypt}" property="firstName" value="${param.firstName}" />
  <c:set target="${crypt}" property="lastName" value="${param.lastName}" />
</jsp:useBean>

<c:set var="dev" value='0'  />
<c:if test="${param.developer == '1'}">
  <c:set var="dev" value='1'  />
</c:if>
<c:set var="test" value='0'  />
<c:if test="${param.tester == '1'}">
  <c:set var="test" value='1'  />
</c:if>
<c:choose>
  <c:when test="${param.task == 'addnew'}">
    <%-- Add a new user account --%>
    <sql:update>
      INSERT INTO user 
        (username, password, first_name, surname, company, state, country, email, team_id, test, dev)
        VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      <sql:param value="${param.userName}" />
      <sql:param value="${digest.password}" />
      <sql:param value="${param.firstName}" />
      <sql:param value="${param.lastName}" />
      <sql:param value="${param.company}" />
      <sql:param value="${param.state}" />
      <sql:param value="${param.country}" />
      <sql:param value="${param.email}" />
      <sql:param value="${param.teamId}" />
      <sql:param value="${test}" />
      <sql:param value="${dev}" />
    </sql:update>
    <sql:update dataSource="${bugdb}">
        INSERT INTO profiles (login_name, cryptpassword, realname)
        VALUES (?, ?, ?)
      <sql:param value="${param.email}" />
      <sql:param value="${crypt.password}" />
      <sql:param value="${crypt.fullName}" />
    </sql:update>
  </c:when>
  <c:otherwise>
    <%-- Update an existing user account --%>
    <c:choose>
      <c:when test="${empty param.password}">
        <sql:update>
          UPDATE user
            SET username = ?,
                first_name = ?, 
                surname = ?,
                company = ?,
                state = ?,
                country = ?,
                email = ?,
                team_id = ?,
                test = ?,
                dev = ? 
            WHERE user_id = ?
          <sql:param value="${param.userName}" />
          <sql:param value="${param.firstName}" />
          <sql:param value="${param.lastName}" />
          <sql:param value="${param.company}" />
          <sql:param value="${param.state}" />
          <sql:param value="${param.country}" />
          <sql:param value="${param.email}" />
          <sql:param value="${param.teamId}" />
          <sql:param value="${test}" />
          <sql:param value="${dev}" />
          <sql:param value="${userId}" />
        </sql:update>
      </c:when>
      <c:otherwise>
        <sql:update>
          UPDATE user
            SET username = ?,
                password = ?,
                first_name = ?, 
                surname = ?,
                company = ?,
                state = ?,
                country = ?,
                email = ?,
                team_id = ?,
                test = ?,
                dev = ? 
            WHERE user_id = ?
          <sql:param value="${param.userName}" />
          <sql:param value="${digest.password}" />
          <sql:param value="${param.firstName}" />
          <sql:param value="${param.lastName}" />
          <sql:param value="${param.company}" />
          <sql:param value="${param.state}" />
          <sql:param value="${param.country}" />
          <sql:param value="${param.email}" />
          <sql:param value="${param.teamId}" />
          <sql:param value="${test}" />
          <sql:param value="${dev}" />
          <sql:param value="${userId}" />
        </sql:update>
      </c:otherwise>
    </c:choose>
    <c:if test="${not empty initParam.bugdb_driver}">
      <c:if test="${bugLogin != param.email}">
        <sql:update dataSource="${bugdb}">
          UPDATE profiles
            SET login_name = ?
            WHERE login_name = ?
          <sql:param value="${param.email}" />
          <sql:param value="${bugLogin}" />
        </sql:update>
      </c:if>
    </c:if>
  </c:otherwise>
</c:choose>

<%-- Get the new or updated data from the database --%>
<sql:query var="newUserDbInfo" scope="session">
  SELECT user_id,username,first_name,surname,company,country,state,email,test,dev,team_id FROM user 
    WHERE username = ?
  <sql:param value="${param.userName}" />
</sql:query>
<c:if test="${not empty initParam.bugdb_driver}">
  <sql:query var="newBugsDbInfo" scope="session" dataSource="${bugdb}">
    SELECT cryptpassword,login_name FROM profiles 
      WHERE login_name = ?
    <sql:param value="${param.email}" />
  </sql:query>
</c:if>

<%-- Redirect to the confirmation page --%>
<c:redirect url="confirmation.jsp?task=${param.task}&password=${param.password}" />
