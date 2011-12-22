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
<%@page contentType="text/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
<head>
   <title>Showing Email</title>
    <link href=stylesheet.css type="text/css" rel=stylesheet>
</head>
<body>

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
	    <h3>Open Email Message</h3>
            
            <sql:query var="mail">
                SELECT * FROM email WHERE email_id = ?
                <sql:param value="${param.emailId}" />
            </sql:query>
            <c:set var="dbValues" value="${mail.rows[0]}" />
            
	    <p>
            Sender:  ${fn:escapeXml(dbValues.sender)}<br>
            Receiver: ${fn:escapeXml(dbValues.receiver)}<br>
            Sent time: ${fn:escapeXml(dbValues.send_time)}<br>
            Subject:<input type="text" name="subject" size="71" value="${fn:escapeXml(dbValues.subject)}" readonly></input><br>
            Message:<br><textarea name="message" rows="30" cols="80" readonly>${fn:escapeXml(dbValues.message)}</textarea><br>
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
