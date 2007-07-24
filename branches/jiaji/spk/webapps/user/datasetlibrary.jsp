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
    <title>Library Dataset List</title>
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
	    <h3>Library Dataset List</h3>
	    <p> 
              <jsp:useBean id="library" scope="request" class="uw.rfpk.beans.DatasetList" />
              <% int maxNum = Integer.parseInt(getServletContext().getInitParameter("maxNum")); 
                 library.setUsername("librarian");
                 library.setDbHost(getServletContext().getInitParameter("database_host"));
                 library.setDbName(getServletContext().getInitParameter("database_name"));
                 library.setDbUser(getServletContext().getInitParameter("database_username"));
                 library.setDbPass(getServletContext().getInitParameter("database_password"));
                 Vector datasetList = library.getDatasetList(maxNum + 1, Long.parseLong(request.getParameter("start")));
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
                     The following library datasets are found:
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
                     String pageLink = "<a href=datasetlibrary.jsp?start=" + String.valueOf(start) + 
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
                     String pageLink = "<a href=datasetlibrary.jsp?start=" + ((String[])datasetList.get(size - 1))[0] + 
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
