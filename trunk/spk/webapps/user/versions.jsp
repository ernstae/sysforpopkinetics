<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@ page contentType="text/html" %>
<%@ page import="java.util.Vector" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
  <head>
    <title>Version List</title>
  </head>
  <body bgcolor="white">

    <table align=left border=0 width=602>
      <tbody> 
	<tr>    
	  <td colSpan=3 vAlign=top>
	    <img align=top alt="RFPK logo" height=40 src="./images/rfpklogo.gif" width=112>
	      <img align=top alt="Resource Facility for Population Kinetics" height=40 
		src="./images/rfpkfull.gif" width=474>
 	  </td> 
	</tr> 
	<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> 
	<tr>
	  <td vAlign=top width=102 height="0" colspan="1" rowspan="1">
          <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>Version List</h3>
	    <p> 
              <jsp:useBean id="validUser"scope="session" class="uw.rfpk.beans.UserInfo" />
              <jsp:useBean id="version" scope="session" class="uw.rfpk.beans.VersionList" />
              <% version.setDbHost(getServletContext().getInitParameter("database_host"));
                 version.setDbName(getServletContext().getInitParameter("database_name"));
                 version.setDbUser(getServletContext().getInitParameter("database_username"));
                 version.setDbPass(getServletContext().getInitParameter("database_password"));
                 String[][] versionList = version.getVersionList(Long.parseLong(request.getParameter("id")), 
                                                                  request.getParameter("type"));
                 int size = versionList.length; 
                 if(size == 0)
                 { %>
                     No model seems to be in there anymore ...
              <% }
                 else
                 { %>
                     The following user versions are found:
                 <p>
                 <table border="1">
                 <th>Revision</th>
                 <th>Author</th>
                 <th>Revised Time</th>
                 <th>Log Message</th>
                  <% for(int i = 0; i < size; i++)
                     { %>     
                  <tr>
                    <td align="center"><%=versionList[i][0]%></td>
                    <td><%=versionList[i][1]%></td>
                    <td><%=versionList[i][2]%></td>
                    <td><%=versionList[i][3]%></td>
                  </tr>
                  <% } %>
                 </table>
              <% } %>
	    </p>
	    <p> 
               When you are done, please <a href="logout.jsp">log out</a>.
	    </p>
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
