<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="ora" uri="orataglib" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<c:set var="isValid" value="true" />

<c:if test="${empty param.userName}">
  <c:set var="userNameError" scope="request"
    value="User Name missing" />
  <c:set var="isValid" value="false" />
</c:if>
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
    <jsp:forward page="enteruser.jsp" />
  </c:otherwise>
</c:choose>
