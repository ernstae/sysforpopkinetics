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

<c:set target="${digest}" property="password" value="${param.password}" />
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
    <sql:update>
      INSERT INTO user 
        (username, password, first_name, surname, company, country, state, email, test, dev)
        VALUES(?, ?, ?, ?)
      <sql:param value="${param.userName}" />
      <sql:param value="${digest.password}" />
      <sql:param value="${param.firstName}" />
      <sql:param value="${param.lastName}" />
      <sql:param value="${param.company}" />
      <sql:param value="${param.country}" />
      <sql:param value="${param.state}" />
      <sql:param value="${param.email}" />
      <sql:param value="${test}" />
      <sql:param value="${dev}" />
    </sql:update>
  </c:when>
  <c:otherwise>
    <sql:update>
      UPDATE user
        SET password = ?, 
            first_name = ?, 
            surname = ?,
            company = ?,
            country = ?,
            state = ?,
            email = ?,
            test = ?,
            dev = ? 
        WHERE username = ?
      <sql:param value="${digest.password}" />
      <sql:param value="${param.firstName}" />
      <sql:param value="${param.lastName}" />
      <sql:param value="${param.company}" />
      <sql:param value="${param.country}" />
      <sql:param value="${param.state}" />
      <sql:param value="${param.email}" />
      <sql:param value="${test}" />
      <sql:param value="${dev}" />
      <sql:param value="${param.userName}" />
    </sql:update>
  </c:otherwise>
</c:choose>

<%-- Get the new or updated data from the database --%>
<sql:query var="newUserDbInfo" scope="session">
  SELECT * FROM user 
    WHERE username = ?
  <sql:param value="${param.userName}" />
</sql:query>

<%-- Redirect to the confirmation page --%>
<c:redirect url="confirmation.jsp" />
