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
    <title>User Model List</title>
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
	    <h3>User Model List</h3>
	    <p> 
              <jsp:useBean id="validUser"scope="session" class="uw.rfpk.beans.UserInfo" />
              <jsp:useBean id="models" scope="session" class="uw.rfpk.beans.ModelList" />
              <% int maxNum = Integer.parseInt(getServletContext().getInitParameter("maxNum")); 
                 models.setUsername(validUser.getUserName());
                 models.setDbHost(getServletContext().getInitParameter("database_host"));
                 models.setDbName(getServletContext().getInitParameter("database_name"));
                 models.setDbUser(getServletContext().getInitParameter("database_username"));
                 models.setDbPass(getServletContext().getInitParameter("database_password"));
                 Vector modelList = models.getModelList(maxNum + 1, Long.parseLong(request.getParameter("start")));
                 Vector startList = null;
                 int counter = Integer.parseInt(request.getParameter("counter"));
                 int size = modelList.size();
                 boolean isMore = false; 
                 if(size == 0)
                 { %>
                     No model seems to be in there anymore ...
              <% }
                 else
                 {
                     if(counter == 0)
                     {
                         startList = new Vector();
                         startList.add(((String[])modelList.get(0))[0]);
                         session.setAttribute("START", startList); 
                     }
                     else
                     {
                         startList = (Vector)session.getAttribute("START");
                         if(counter >= startList.size())
                         {
                             startList.add(((String[])modelList.get(0))[0]);
                             session.setAttribute("START", startList); 
                         }
                     } %>
                     The following user models are found:
                 <p>
                 <table border="1">
                 <th>Model Name</th>
                 <th>No. of Versions</th>
                 <th>Last Revised Time</th>
                 <th>Description</th>
                  <% 
                     if(size > maxNum)
                     {
                         size = maxNum;
                         isMore = true;
                     }
                     for(int i = 0; i < size; i++)
                     { 
                         String[] model = (String[])modelList.get(i);
                         String link = "<a href=versions.jsp?id=" + model[0] + "&type=model>" + model[1] + "</a>"; %>           
                  <tr>
                    <td><%=link%></td>
                    <td align="center"><%=model[2]%></td>
                    <td><%=model[3]%></td>
                    <td><%=model[4]%></td>
                  </tr>
                  <% } %>
                 </table>
            <p>
              <% if(counter > 0)
                 {
                     long start = Long.parseLong((String)startList.get(counter - 1)) + 1;
                     int count = counter - 1;
                     String pageLink = "<a href=usermodels.jsp?start=" + String.valueOf(start) + 
                                       "&counter=" + String.valueOf(count) + ">Previous Page</a>"; %>
                     <%=pageLink%>
              <% }
                 else
                 { %>
                     Previous Page
              <% }
                 if(isMore)
                 {
                     int count = counter + 1;
                     String pageLink = "<a href=usermodels.jsp?start=" + ((String[])modelList.get(size - 1))[0] + 
                                       "&counter=" + String.valueOf(count) + ">Next Page</a>"; %>
                     <%=pageLink%>
              <% }
                 else 
                 { %>
                     Next Page
              <% }} %>
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
