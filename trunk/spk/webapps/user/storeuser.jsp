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

<c:set target="${digest}" property="password" value="${param.password}" />
<%-- 
  See if the user is already defined. If not, insert the
  info, else update it.
--%>
<sql:query var="userDbInfo">
  SELECT * FROM user 
    WHERE username = ?
  <sql:param value="${param.userName}" />
</sql:query>

<c:choose>
  <c:when test="${userDbInfo.rowCount == 0}">
    <sql:update>
      INSERT INTO user 
        (username, password, first_name, surname)
        VALUES(?, ?, ?, ?)
      <sql:param value="${param.userName}" />
      <sql:param value="${digest.password}" />
      <sql:param value="${param.firstName}" />
      <sql:param value="${param.lastName}" />
    </sql:update>
  </c:when>
  <c:otherwise>
    <sql:update>
      UPDATE user
        SET password = ?, 
            first_name = ?, 
            surname = ? 
        WHERE username = ?
      <sql:param value="${digest.password}" />
      <sql:param value="${param.firstName}" />
      <sql:param value="${param.lastName}" />
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
