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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="ora" uri="orataglib" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<c:set var="isValid" value="true" />

<c:set var="userNameError" scope="request" />
<c:choose>
  <c:when test="${empty param.userName}">
    <c:set var="userNameError" scope="request" value="User Name missing" />
    <c:set var="isValid" value="false" />
  </c:when>
  <c:otherwise>
    <sql:query var="userDb">
      SELECT * FROM user 
        WHERE username = ?
      <sql:param value="${param.userName}" />
    </sql:query>
    <c:choose>
      <c:when test="${param.task == 'addnew'}">
        <c:if test="${userDb.rowCount != 0}">
          <c:set var="userNameError" scope="request" value="User Name already used" />
          <c:set var="isValid" value="false" />  
        </c:if>
      </c:when>
      <c:otherwise>
        <c:if test="${userDb.rowCount == 0}">
          <c:set var="userNameError" scope="request" value="User Name not found" />
          <c:set var="isValid" value="false" />  
        </c:if>
      </c:otherwise>
    </c:choose>
  </c:otherwise>
</c:choose>
<c:if test="${empty param.password}">
  <c:set var="passwordError" scope="request"
    value="Password missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:if test="${empty param.firstName}">
  <c:set var="firstNameError" scope="request"
    value="First Name missing" />
  <c:set var="isValid" value="false" />
</c:if>
<c:if test="${empty param.lastName}">
  <c:set var="lastNameError" scope="request"
    value="Last Name missing" />
  <c:set var="isValid" value="false" />
</c:if>

<c:choose>
  <c:when test="${isValid}">
    <jsp:forward page="storeuser.jsp" />
  </c:when>
  <c:otherwise>
    <c:choose>
      <c:when test="${param.task == 'addnew'}">
        <jsp:forward page="enteruser.jsp?task=addnew" />
      </c:when>
      <c:otherwise>
        <jsp:forward page="enteruser.jsp?task=update" />
      </c:otherwise>
    </c:choose>
  </c:otherwise>
</c:choose>
