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

<c:if test="${empty param.password1 || empty param.password2}">
  <c:redirect url="chpassword.jsp" >
    <c:param name="errorMsg" value="You must enter a new password and confirm it." />
  </c:redirect>
</c:if>

<c:if test="${param.password1 != param.password2}">
  <c:redirect url="chpassword.jsp" >
    <c:param name="errorMsg" value="The new password is not confirmed" />
  </c:redirect>
</c:if>

<c:set target="${digest}" property="password" value="${param.password1}" />

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
    <c:redirect url="main.jsp" />
  </c:otherwise>
</c:choose>

