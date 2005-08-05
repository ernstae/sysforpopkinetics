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
<%@ include file="quicklinks.shtml" %>  
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
	      <h3>Welcome.  Please Log In</h3>
            </c:otherwise>
          </c:choose>
            <p>
            <font color="red">
              ${fn:escapeXml(param.errorMsg)}
            </font>
            </p>
            <p><font size=3>
                Please follow these instructions before you attempt to use SPK:<br>
                 - You need to have Java Runtime Environment (JRE) on your computer. 
                Instructions for downloading and installing JRE are provided 
                <a href="instructWin.jsp">here</a> for Windows users 
                and <a href="instructLin.jsp">here</a> for Linux users.<br>
                 - If you would like to use R for SPK output display and post-processing analysis, you
                may download and install R from <a href="http://www.r-project.org/">here</a>.
                Please note that for this option to work you have to manually set the environment variable PATH
                to the bin subdirectory of the
                R installation directory.  For example, if you are using Windows and you have
                installed R in C:\Program Files, you should set PATH to 
                "C:\Program Files\R\rw2001\bin",
                where 2001 represents the installed version (in this case 2.0.1) of R. This can be accomplished by
                accessing the Control Panel, opening the System component, selecting the
                Advanced tab and selecting "Environment Variables".
            </p>
	    <p>
              <c:choose>
                <c:when test="${notice != '1'}">
                  Please log into MySPK, the "members only" section of SPK. When you are done, 
                  please <a href="logout.jsp">log out</a>.  If you are a first time user, 
                  view this <a href="WebHelp/gettingstartedspk.htm" target="_blank">Getting Started</a> 
                  example first.
                </c:when>
                <c:otherwise>
                  <font color="blue">User account administrator log in:</font>
                </c:otherwise>
              </c:choose>
            </p>

            <form action="checkuser.jsp" method="post">
              <input type="hidden" name="origURL" value="${fn:escapeXml(param.origURL)}">                  
              <table border="0" cellspacing = "5">
                <tr>
                  <th align="right">User Name:</th>
                  <th align="left"><input type="text" name="userName" ></td>
                </tr>
                <tr>
                  <th align="right">Password:</th>
                  <th align="left"><input type="password" name="password" ></td>
                </tr>
                <tr>
                  <th align="right"><input type="Submit" value="Log In"></th>
                  <th align="left"><input type="Reset"></td>
                </tr>
              </table>
            </form>

            <h4>About Membership</H2>
            <p>
                MySPK provides the ability to create PK models using the interactive
                Model Design Agent, to compile these models into highly efficient
                machine code, and to run them on a computational cluster.  Because
                the resources of the system (and of the cluster, in particular) are
                limited, membership is by invitation only. 
                RFPK is looking for qualified researchers who would be willing to   
                participate in the ongoing development of SPK by joining our
                selected group of users.  If you think that you would be a good
                candidate for membership, please contact 
                <a href="mailto:vicini@u.washington.edu">Dr. Paolo Vicini</A>, 
                the RFPK Principal Investigator.  </font>            
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
