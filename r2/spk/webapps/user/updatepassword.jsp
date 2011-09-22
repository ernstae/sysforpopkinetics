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
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<%-- Make sure that the new password has entered twice. --%>
<c:if test="${empty param.password1 || empty param.password2}">
  <c:redirect url="chpassword.jsp" >
    <c:param name="errorMsg" value="You must enter a new password and confirm it." />
  </c:redirect>
</c:if>

<%-- Make sure that the new password has been confirmed. --%>
<c:if test="${param.password1 != param.password2}">
  <c:redirect url="chpassword.jsp" >
    <c:param name="errorMsg" value="The new password is not confirmed" />
  </c:redirect>
</c:if>

<%-- Create a bean for digesting password. --%>
<c:set target="${digest}" property="password" value="${param.password1}" />

<%-- Update pasword in the database --%>
<sql:update>
  UPDATE user SET password=? WHERE username=?
  <sql:param value="${digest.password}" />
  <sql:param value="${validUser.userName}" />
</sql:update>

<c:choose>
  <c:when test="${!empty param.origURL}">
    <c:redirect url="${param.origURL}" />
  </c:when>
  <c:otherwise>
    <c:redirect url="usermain.jsp" />
  </c:otherwise>
</c:choose>

