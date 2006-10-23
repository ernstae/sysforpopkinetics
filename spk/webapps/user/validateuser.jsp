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
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<c:set var="isValid" value="true" />

<c:if test="${empty param.company}">
  <c:set var="companyNameError" scope="request"
    value="Company name missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:if test="${empty param.state}">
  <c:set var="stateNameError" scope="request"
    value="State name missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:if test="${empty param.country}">
  <c:set var="countryNameError" scope="request"
    value="Country Name missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:if test="${empty param.email}">
  <c:set var="emailAddressError" scope="request"
    value="Email address missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:set target="${validUser}" property="company" value="${param.company}" />
<c:set target="${validUser}" property="country" value="${param.country}" />
<c:set target="${validUser}" property="state" value="${param.state}" />
<c:set target="${validUser}" property="email" value="${param.email}" />
<c:choose>
  <c:when test="${isValid}">
      <sql:update>
          UPDATE user
            SET company = ?,
                state = ?,
                country = ?,
                email = ?
                WHERE user_id = ?
          <sql:param value="${param.company}" />
          <sql:param value="${param.state}" />
          <sql:param value="${param.country}" />
          <sql:param value="${param.email}" />
          <sql:param value="${validUser.userId}" />
      </sql:update>
      <c:redirect url="usermain.jsp" />
  </c:when>
  <c:otherwise>
    <jsp:forward page="userinfo.jsp" />
  </c:otherwise>
</c:choose>
