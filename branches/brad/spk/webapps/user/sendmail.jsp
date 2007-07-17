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
<%@ page import="java.io.BufferedReader" %>
<%@ page import="java.io.BufferedWriter" %>
<%@ page import="java.io.FileReader" %>
<%@ page import="java.io.FileWriter" %>
<%@ page import="java.io.IOException" %>
<%@ page import="java.io.File" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null || validUser.userName != 'useradmin'}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
<head>
   <title>Sending Emeil to Users</title>
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
	    <h3>Send Email to Users</h3>
            
            
            <% String sender = getServletContext().getInitParameter("emailAddress");
               String receiver = "Developers";
               String subject = "";
               String message = "\n\nPlease let us know if you would prefer not to receive future mailings about the SPK software, new features and other training offerings by sending a message to rfpksoft@u.washington.edu with \"REMOVE\" in the subject line.";
               try
               {
                   File file = new File("/tmp/letter");
                   if(file.exists())
                   {
                       BufferedReader i = new BufferedReader(new FileReader(file));
                       receiver = i.readLine();
                       int senderLength = Integer.parseInt(i.readLine());
                       int subjectLength = Integer.parseInt(i.readLine());
                       String line;
                       String text = "";
                       while((line = i.readLine()) != null)
                           text += line + "\n";
                       i.close();
                       sender = text.substring(0, senderLength);
                       subject = text.substring(senderLength, senderLength + subjectLength);
                       message = text.substring(senderLength + subjectLength).trim();
                   }
               }
               catch (IOException e) 
               {
               }
            %>

	    <p>
            <form action="sendmail.jsp" method="post">
                <input type="hidden" name="OrigURL" value="${fn:escapeXml(param.origURL)}">
            Sender: <input type="text" name="sender" size="71" value="<%=sender%>"></input><br>
            Receivers:              
            <% if(receiver.equals("Developers")){ %>
                  <input type="radio" name="receiver" value="Developers" checked />Developers
                  <input type="radio" name="receiver" value="Testers" />Testers
                  <input type="radio" name="receiver" value="All Users" />All Users
            <% }
               else if(receiver.equals("Testers")){ %>
                  <input type="radio" name="receiver" value="Developers" />Developers
                  <input type="radio" name="receiver" value="Testers" checked />Testers
                  <input type="radio" name="receiver" value="All Users" />All Users
            <% }
               else if(receiver.equals("All Users")){ %>
                  <input type="radio" name="receiver" value="Developers" />Developers
                  <input type="radio" name="receiver" value="Testers" />Testers
                  <input type="radio" name="receiver" value="All Users" checked />All Users
            <% } %>
            <br>Subject:<input type="text" name="subject" size="71" value="<%=subject%>"></input><br>
            Message:<br><textarea name="message" rows="30" cols="80"><%=message%></textarea><br>
              <input type="Submit" name="enter" value="Postpone">
              <input type="Submit" name="enter" value="Send Now">
              <input type="Submit" name="enter" value="Clean Up">
              <input type="Submit" name="enter" value="Mail List">
            </form>
	    </p>
            <p>
              When you are done, please <a href="logout.jsp">log out</a>.
            </p>

            <% subject = request.getParameter("subject");
               message = request.getParameter("message");
               sender = request.getParameter("sender");
               receiver = request.getParameter("receiver");
            %>
            
            <c:if test="${param.enter == 'Mail List'}">
                
                <c:redirect url="emaillist.jsp" />
            </c:if>
            
            <c:if test="${param.enter == 'Postpone'}">
            <% try 
               {
                   BufferedWriter o = new BufferedWriter(new FileWriter("/tmp/letter"));
                   StringBuffer buffer = new StringBuffer(receiver);
                   buffer.append("\n").append(sender.length()).append("\n").append(subject.length())
                         .append("\n").append(sender).append(subject).append(message);
                   o.write(buffer.toString());
                   o.close();
               }
               catch (IOException e) 
               {
               }
            %>
            </c:if>        
            
            <c:if test="${param.enter == 'Send Now'}">
            <%
               String perlDir = getServletContext().getInitParameter("perlDir");
               String emailList = null;
            %>
            <%-- Get email addresses --%>
            <jsp:useBean id="email" scope="request" class="uw.rfpk.beans.EmailAddress" />
            <% email.setDbHost(getServletContext().getInitParameter("database_host"));
               email.setDbName(getServletContext().getInitParameter("database_name"));
               email.setDbUser(getServletContext().getInitParameter("database_username"));
               email.setDbPass(getServletContext().getInitParameter("database_password"));
            %>
            <c:choose>
                <c:when test="${param.receiver == 'Developers'}">
                    <% emailList = email.getEmailAddress("developer"); %>
                </c:when>
                <c:when test="${param.receiver == 'Testers'}">
                    <% emailList = email.getEmailAddress("tester"); %>
                </c:when>
                <c:otherwise>
                    <% emailList = email.getEmailAddress("all"); %>
                </c:otherwise>
            </c:choose>              
                
            <% if(emailList != null)
               {
                   String[] command = {"perl", perlDir + "email.pl", sender, sender, emailList, subject, message};
                   Process process = null;
                   try
                   {
                       process = Runtime.getRuntime().exec(command);
                       process.waitFor(); %>
                       <sql:update>
                         INSERT INTO email (send_time,sender,receiver,subject,message) VALUES (now(),?,?,?,?)
                         <sql:param value="${param.sender}" />
                         <sql:param value="${param.receiver}" />
                         <sql:param value="${param.subject}" />
                         <sql:param value="${param.message}" />
                       </sql:update>
            <%    }
                   finally
                   {
                       if(process != null) process.destroy();
                       File file = new File("/tmp/letter");
                       file.delete();
                   }
            } %>
            </c:if>
            <c:if test="${param.enter == 'Clean Up'}">
            <% File file = new File("/tmp/letter");
               file.delete();
               sender = getServletContext().getInitParameter("emailAddress");
               receiver = "Developers";
               subject = "";
               message = "";
            %>
            </c:if>
	  </td>
	</tr>
      </tbody>
    </table>
   
  </body>
</html>
