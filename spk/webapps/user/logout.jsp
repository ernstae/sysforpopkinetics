<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="ora" uri="orataglib"%>

<%-- Terminate the session and redirect to the login page --%>
<ora:invalidateSession/>

<c:redirect url="index.jsp" />
