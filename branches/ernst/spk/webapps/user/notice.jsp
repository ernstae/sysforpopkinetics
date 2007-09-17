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
<%@ page import="java.io.*" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<c:set var="notice" scope="application" value="${param.alert}" />
<c:if test="${notice != '1'}">
<%
    File file = null;
    BufferedReader reader = null;
    Process process = null;
    try
    {
        String pathName = getServletContext().getInitParameter("jnlp_directory") + 
                          "notifyusers.txt";
        String spkEmail = getServletContext().getInitParameter("emailAddress");
        String perlDir = getServletContext().getInitParameter("perlDir");
        String emailList = "";
        file = new File(pathName);
        reader = new BufferedReader(new FileReader(file));
        String line, email;
        while((line = reader.readLine()) != null) {
            String userEmail = line.trim();
            if(!userEmail.equals("")) {
                if(!emailList.equals("")) emailList += ",";
                emailList += userEmail;
            }
        }
        String subject = "SPK service is now available.";
        String message = subject + "\n\nThis message was sent by the SPK service provider.";
        String[] command = {"perl", perlDir + "email.pl", spkEmail, spkEmail, emailList, subject, message};
        process = Runtime.getRuntime().exec(command);
        process.waitFor();
    }
    finally
    {
        reader.close();
        file.delete();
        process.destroy();
    }
%>
</c:if>
<c:redirect url="usermain.jsp" />
