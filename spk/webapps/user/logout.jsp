<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="ora" uri="orataglib"%>

<%-- Terminate the session and redirect to the login page --%>
<ora:invalidateSession/>

<c:redirect url="index.jsp" />
