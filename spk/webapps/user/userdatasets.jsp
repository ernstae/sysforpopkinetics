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
    <title>User Dataset List</title>
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
	  <td vAlign=top width=150 height="0" colspan="1" rowspan="1">
          <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>User Dataset List</h3>
	    <p> 
              <jsp:useBean id="validUser"scope="session" class="uw.rfpk.beans.UserInfo" />
              <jsp:useBean id="datasets" scope="request" class="uw.rfpk.beans.DatasetList" />
              <% int maxNum = Integer.parseInt(getServletContext().getInitParameter("maxNum")); 
                 datasets.setUsername(validUser.getUserName());
                 datasets.setDbHost(getServletContext().getInitParameter("database_host"));
                 datasets.setDbName(getServletContext().getInitParameter("database_name"));
                 datasets.setDbUser(getServletContext().getInitParameter("database_username"));
                 datasets.setDbPass(getServletContext().getInitParameter("database_password"));
                 Vector datasetList = datasets.getDatasetList(maxNum + 1, Long.parseLong(request.getParameter("start")));
                 Vector startList = null;
                 int counter = Integer.parseInt(request.getParameter("counter"));
                 int size = datasetList.size();
                 boolean isMore = false; 
                 if(size == 0)
                 { %>
                     No dataset seems to be in there anymore ...
              <% }
                 else
                 {
                     if(counter == 0)
                     {
                         startList = new Vector();
                         startList.add(((String[])datasetList.get(0))[0]);
                         session.setAttribute("START", startList); 
                     }
                     else
                     {
                         startList = (Vector)session.getAttribute("START");
                         if(counter >= startList.size())
                         {
                             startList.add(((String[])datasetList.get(0))[0]);
                             session.setAttribute("START", startList); 
                         }
                     } %>
                     The following user datasets are found:
                 <p>
                 <table border="1">
                 <th>Dataset Name</th>
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
                         String[] dataset = (String[])datasetList.get(i);
                         String link = "<a href=versions.jsp?id=" + dataset[0] + "&type=data>" + dataset[1] + "</a>";
                         String description = dataset[4].startsWith("http://") || dataset[4].startsWith("https://") ? 
                                              "<a href=" + dataset[4] + ">" + dataset[4] + "</a>" : dataset[4]; %> 
                  <tr>
                    <td><%=link%></td>
                    <td align="center"><%=dataset[2]%></td>
                    <td><%=dataset[3]%></td>
                    <td><%=description%></td>
                  </tr>
                  <% } %>
                 </table>
            <p>
              <% if(counter > 0)
                 {
                     long start = Long.parseLong((String)startList.get(counter - 1)) + 1;
                     int count = counter - 1;
                     String pageLink = "<a href=userdatasets.jsp?start=" + String.valueOf(start) + 
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
                     String pageLink = "<a href=userdatasets.jsp?start=" + ((String[])datasetList.get(size - 1))[0] + 
                                       "&counter=" + String.valueOf(count) + ">Next Page</a>"; %>
                     <%=pageLink%>
              <% }
                 else 
                 { %>
                     Next Page
              <% }} %>
	    </p>
	    <p> 
               You may click the dataset name to see the version list of the dataset.
               When you are done, please <a href="logout.jsp">log out</a>.
	    </p>
	  </td>
	</tr>
	</tr>
      </tbody>
    </table>
  </body>
</html>
