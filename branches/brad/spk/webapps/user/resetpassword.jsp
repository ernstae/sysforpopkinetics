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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="sql" uri="http://java.sun.com/jsp/jstl/sql" %>
<%@ page import="java.io.*" %>

<%-- Check if the user has entered username and email address. --%>
<c:if test="${empty param.userName || empty param.emailAddress}">
  <c:redirect url="enteremail.jsp" >
    <c:param name="msg" value="You must enter your username and email address." />
  </c:redirect>
</c:if>
<%-- Generate a random character string as a password --%>
<% char[] pw = new char[8];
   int c = 0;
   for (int i=0; i < 8; i++)
   {
       switch((int)(Math.random() * 3))
       {
           case 0: c = '0' +  (int)(Math.random() * 10); break;
           case 1: c = 'a' +  (int)(Math.random() * 26); break;
           case 2: c = 'A' +  (int)(Math.random() * 26); break;
       }
       pw[i] = (char)c;
   } 
   String password = new String(pw);
%>

<%-- Create a bean for digesting password. --%>
<jsp:useBean id="digest" scope="request" class="uw.rfpk.beans.DigestPassword" >
  <c:set target="${digest}" property="password" value="<%=password%>" />
</jsp:useBean>

<%-- Update password. --%>
        <sql:update var="count">
          UPDATE user
            SET password = ?
          WHERE username = ? AND email = ?
          <sql:param value="${digest.password}" />
          <sql:param value="${param.userName}" />
          <sql:param value="${param.emailAddress}" />
        </sql:update>
<c:if test="${count == 0}">
  <c:redirect url="enteremail.jsp" >
    <c:param name="msg" value="User account with the email address was not found." />
  </c:redirect>
</c:if> 
       
<%
    Process process = null;
    boolean ok = true;
    try
    {
        String spkEmail = getServletContext().getInitParameter("emailAddress");
        String perlDir = getServletContext().getInitParameter("perlDir");
        String address = request.getParameter("emailAddress");
        String subject = "SPK account information";
        String message = "Your account password has been reset to: " + password + 
                         "\n\nThis message was sent by the SPK service provider.";
        String[] command = {"perl", perlDir + "email.pl", spkEmail, address, "", subject, message};
        process = Runtime.getRuntime().exec(command);
        process.waitFor();
    }
    catch(Exception e)
    {
        ok = false;
    }
    finally
    {
        process.destroy();
    }
if(ok) 
{%>
  <c:redirect url="index.jsp" >
    <c:param name="errorMsg" value="Password has been sent to you by email." />
  </c:redirect>
<%}
else
{%>
  <c:redirect url="enteremail.jsp" >
    <c:param name="msg" value="Problem occurred when sending password by email." />
  </c:redirect>
<%}%>
