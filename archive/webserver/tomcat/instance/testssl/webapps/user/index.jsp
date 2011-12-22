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

<html>
  <head>
   <title>SPK Login Page</title>
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
	  <td valign=top width=112 height="0" colspan="1" rowspan="1">
<%@ include file="quicklinks_1.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
          <c:choose>
            <c:when test="${notice == '1'}"><font color="red">
              <h3><c:out value="The SPK service is temporarily unavailable for maintenance. 
                 Please try again later.  We are sorry for the inconvenience." /></h3>
                 <form action="addemail.jsp" method="post">
                   If you enter your email address here:<br> 
                   <input type="text" name="email" size="40">&nbsp<input type="Submit" value="Enter"><br>
                   we will notify you when the SPK service is available.
                 </form>
            </font>
            </c:when>
            <c:otherwise>
	      <h3>Welcome.  Account Members, Please Log In</h3>
              <p>If you do not remember your password, click <a href="enteremail.jsp">here.</a> </p>
            </c:otherwise>
          </c:choose>
            <p>
            <font color="red">
              ${fn:escapeXml(param.errorMsg)}
            </font>
            </p>
            <form action="checkuser.jsp" method="post">
              <input type="hidden" name="origURL" value="${fn:escapeXml(param.origURL)}">                  
              <table border="0" cellspacing = "5">
                <tr>
                  <th align="right">User Name:</th>
                  <th align="left"><input type="text" name="userName" 
                                    value="${fn:escapeXml(cookie.userName.value)}"></th>
                </tr>
                <tr>
                  <th align="right">Password:</th>
                  <th align="left"><input type="password" name="password" 
                                    value="${fn:escapeXml(cookie.password.value)}"></th>
                </tr>
                <tr>
                  <th align="right"><input type="Submit" value="Log In"></th>
                  <th align="left"><input type="Reset"></th>
                </tr>
              </table>
              <p>
                Remember my username and password for 8 hours:
                <input type="checkbox" name="remember" ${!empty cookie.userName ? 'checked' : ''}><br>
                (This feature requires cookies enabled in your browser)
              </p>
            </form>
	    <p>
              <c:choose>
                <c:when test="${notice != '1'}">
                  First time users may want to view this <a href="WebHelp1/SPKStart.htm" target="_blank">Getting Started</a> example.<br><br>
                  Assistance is available at: <%=getServletContext().getInitParameter("research_scientist_email")%>.<br><br>
                  When you are done, please <a href="logout.jsp">log out</a>.
                </c:when>
                <c:otherwise>
                  <font color="blue">User account administrator log in only.</font>
                </c:otherwise>
              </c:choose>
            <br></p>
            <% String terms_of_service = getServletContext().getInitParameter("SPK_terms_of_service_url");
               String project_pi = getServletContext().getInitParameter("project_PI");
               String pi_email = getServletContext().getInitParameter("project_PI_email");
               String new_account = getServletContext().getInitParameter("new_account"); %>
            <h3>Obtaining an Account</h3>
            <p>
                MySPK provides the ability to create population kinetic models using the interactive
                Model Design Agent, to compile these models into highly efficient
                machine code, and to run them on a computational cluster (according to our <a href="<%=terms_of_service%>" target="_blank">Terms of Service</a>). 
                To obtain an account, please go to the 
                <a href="<%=new_account%>">new user account page</a>.  For more information, please contact 
                <a href="mailto:<%=pi_email%>">Dr. <%=project_pi%></a>, 
                the RFPK Principal Investigator.  </font>            
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
