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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<%-- Verify that the user has logged in --%>
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<html>
<head>
  <title>The User Information Stored</title>
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
	  <td valign=top width=150 height="0" colspan="1" rowspan="1">
            <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>

            <h4>This is the information stored in the user table of the spkdb database:</h4>
            <table>
              <c:forEach items="${newUserDbInfo.rows}" var="row">
              <c:forEach items="${row}" var="column">
              <tr>
                <td align=right>
                  <b>${fn:escapeXml(column.key)}:</b>
                </td>
                <td>
                  ${fn:escapeXml(column.value)}
                </td>
              </tr>
              </c:forEach>
              </c:forEach>
            </table>
          <c:if test="${not empty initParam.bugdb_driver}">
            <h4>This is the information stored in the profiles table of the bugs database:</h4>
            <table>
              <c:forEach items="${newBugsDbInfo.rows}" var="row">
              <c:forEach items="${row}" var="column">
              <tr>
                <td align=right>
                  <b>${fn:escapeXml(column.key)}:</b>
                </td>
                <td>
                  ${fn:escapeXml(column.value)}
                </td>
              </tr>
              </c:forEach>
              </c:forEach>
            </table>
          </c:if>
          <c:if test="${param.task == 'addnew'}" >
            <h4>You may send an email notice to the new user.</h4>
            <c:set var="user" value="${newUserDbInfo.rows[0]}" />
            <form action="confirmation.jsp?to=${user.email}&firstName=${user.first_name}&lastName=${user.surname}&userName=${user.username}&password=${param.password}&email=${user.email}" method="POST">
              <input type="Submit" name="enter" value="Send Email" />
            </form>
          </c:if>
          <c:if test="${param.enter == 'Send Email'}" >
            <%  Process process = null;
                String perlDir = getServletContext().getInitParameter("perlDir");
                String pi_name = getServletContext().getInitParameter("project_PI");
                String pi_info = getServletContext().getInitParameter("project_PI_info");
                String pi_email = getServletContext().getInitParameter("project_PI_email");
                String engineer_name = getServletContext().getInitParameter("software_engineer");
                String engineer_email = getServletContext().getInitParameter("software_engineer_email");
                String scientist_name = getServletContext().getInitParameter("research_scientist");
                String scientist_email = getServletContext().getInitParameter("research_scientist_email");
                String spk_website = getServletContext().getInitParameter("SPK_website");
                String terms_of_service_url = getServletContext().getInitParameter("SPK_terms_of_service_url");
                String bugzilla_url = getServletContext().getInitParameter("bugzillaURL");
                String from = getServletContext().getInitParameter("project_PI_email");;
                String to = request.getParameter("email");
                String subject = "Account Request at SPK";
                String name = request.getParameter("firstName") + " " + request.getParameter("lastName");
                String username = request.getParameter("userName");
                String password = request.getParameter("password");
                StringBuffer buffer = new StringBuffer();
                buffer.append("Dear ").append(name).append(":\n\nGreetings. Thank you for your interest in SPK. As per your request, we have established a user account for you.")
                      .append("\n\nTo access SPK, please direct your browser to the URL\n\n" + spk_website + "\n\nto get to the SPK web site.  Then select the\n\nMySPK\n\nlink and you will be asked to log in.  Enter the following to authenticate yourself:\n\nUser Name: ")
                      .append(username).append("\nPassword: ").append(password).append("\n\nAfter logging in successfully for the first time, please change your password.\n\n") 
                      .append("You have also been installed as a user of our Bugzilla system. Please use it to report bugs in the software or the documentation, or to suggest enhancements.  ")
                      .append("The URL for Bugzilla is\n\n" + bugzilla_url + "\n\nYour authentication parameters are:\n\nLogin: ").append(to).append("\nPassword: ").append(password)
                      .append("\n\nPlease also change your password for your Bugzilla account.  ")
                      .append("If any of this does not work, please let me know, either by email or by phone. Please also make sure to familiarize yourself with our Terms of Service, available as a hyperlink on the left column of the MySPK page, at")
                      .append("\n\n" + terms_of_service_url + ".\n\nQuestions about the user interface (the MDA), should be directed to " + engineer_name + "\n\nemail: " + engineer_email + "\n\n")
                      .append("Please direct questions of a scientific or mathematical nature to " + scientist_name + "\n\nemail: " + scientist_email + "\n\nWe have developed a Getting Started document that should help you to develop your own models in SPK. It is available as a hyperlink on the login page.")
                      .append("\n\nAt RFPK, we are all very excited about having \"outside\" users, and stand ready to assist you in any way that we can.\n\nBest regards,\n\n" + pi_info);
                String message = buffer.toString();
                String[] command = {"perl", perlDir + "email.pl", from, from, to, subject, message};
                try
                {
                    process = Runtime.getRuntime().exec(command);
                    process.waitFor();
                }
                finally
                {
                    if(process != null) process.destroy();
                }  %>
          </c:if>
               When you are done, please <a href="logout.jsp">log out</a>.
            </p>
          </td>
        </tr>
      </tbody>
    </table>
  </body>
</html>
