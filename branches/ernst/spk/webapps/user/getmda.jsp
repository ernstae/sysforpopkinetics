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
<%@ page import="java.io.BufferedWriter" %>
<%@ page import="java.io.File" %>
<%@ page import="java.io.FileWriter" %>
<%@ page import="java.io.IOException" %>
<%@ page import="java.security.SecureRandom" %>

<%-- Verify that the user is logged in --%>
<c:if test="${validUser == null}">
  <jsp:forward page="index.jsp">
    <jsp:param name="origURL" value="${pageContext.request.requestURL}" />
    <jsp:param name="errorMsg" value="Please log in first." />
  </jsp:forward>
</c:if>

<jsp:useBean id="sessionObj" class="uw.rfpk.beans.SessionObject" scope="session" />
<jsp:useBean id="validUser" class="uw.rfpk.beans.UserInfo" scope="session"/>
<%
    String sessionId = session.getId(); 
    String secret = null;
    String host = request.getServerName();
    String port = String.valueOf(request.getServerPort());
    String jnlp_dir = getServletContext().getInitParameter("jnlp_directory");

    if(session.getAttribute("SECRET") == null)
    {
        try
        {
            // Generate random bytes for creating the session secret 
            SecureRandom seed = new SecureRandom();
            byte[] b = seed.generateSeed(16);

            // Convert the byte array to a String object
/*          StringBuffer buf = new StringBuffer(b.length * 8); // binary
            for(int i = 0; i < b.length; i++)
	    {
                int m = 0xF0;
                for(int j = 0; j < 8; j++)
                {
                    char c = '0';                   
                    if((b[i] & m) == m) c = '1';
	 	    m = m >>> 1;
		    buf.append(c);
	        }
            }
*/
            StringBuffer buf = new StringBuffer(b.length * 2); // hex
            String[] c = {"0", "1", "2", "3", "4", "5", "6", "7",
                          "8", "9", "A", "B", "C", "D", "E", "F"};
            int m = 0;
            for(int i = 0; i < b.length; i++)
	    {
                m = b[i] & 0xF0;
                m = m >>> 4;
                buf.append(c[m]);
                m = b[i] & 0x0F;
                buf.append(c[m]);
            }
            secret = buf.toString();
            session.setAttribute("SECRET", secret);
            session.setAttribute("SessionObj", sessionObj);

            // Create the jnlp file with sessionId and secret
            File file = new File(jnlp_dir+secret+".jnlp");
            BufferedWriter o = new BufferedWriter(new FileWriter(file));
            o.write(
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"+
              "<jnlp spec=\"1.0+\" codebase=\"https://"+host+":"+port+"/user\" href=\"jnlp/"+secret+".jnlp\">\n"+
              "<information>\n"+
              "<title>Model Design Agent</title>\n"+
              "<vendor>RFPK UW</vendor>\n"+
              "<homepage href=\"http://www.rfpk.washington.edu\" />\n"+
              "<description>An User Interface of the SPK System</description>\n"+
              "<offline-allowed/>\n"+
              "</information>\n"+
              "<security>\n"+
              "<all-permissions/>\n"+
              "</security>\n"+
              "<resources>\n"+
              "<j2se version=\"1.4+\" initial-heap-size=\"64m\" max-heap-size =\"128m\"/>\n"+
              "<jar href=\"MDAn.jar\"/>\n"+
        //      "<jar href=\"xercesImpl.jar\"/>\n"+
        //      "<jar href=\"xmlParserAPIs.jar\"/>\n"+
              "<jar href=\"jhall.jar\"/>\n"+
              "<jar href=\"JavaHelp.jar\"/>\n"+
              "</resources>\n"+
              "<application-desc main-class=\"uw.rfpk.mda.nonmem.MDA\">\n"+
              "<argument>" + host + "</argument>\n"+
              "<argument>" + port + "</argument>\n"+
              "<argument>" + sessionId + "</argument>\n"+
              "<argument>" + secret + "</argument>\n"+
              "<argument>" + validUser.getTester() + "</argument>\n"+
              "<argument>" + validUser.getDeveloper() + "</argument>\n"+
              "<argument>" + validUser.getUserName() + "</argument>\n"+
              "<argument>" + validUser.getTeamId() + "</argument>\n"+
              "</application-desc>\n"+
              "</jnlp>\n"
             );
            o.flush();
            o.close();
            sessionObj.setSessionObject(file);
        }
        catch(IOException e)
        {
        }
    }
%>

<html>
<head>
  <title>Model Design Agent Download</title>
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
            <h3>Model Design Agent Download</h3>
            <p>
               The Model Design Agent (MDA) is the user interface to create and submit jobs and view and plot results in SPK.  The MDA download checks automatically for the latest version.  While downloading, you will see a Java Web Start window and "Model Design Agent, RFPK UW".  Answer "Yes" to related security questions if they appear.
            </p>
                <p style="color:blue">Important Message:<br>
                As of July 20, 2007, the MDA will require Java Runtime Environment version 1.6.0 or higher. Please follow instructions below.</p>
            <p>
               To download and use the MDA, it is necessary to install the Java Runtime Environment (JRE) on your computer.  Instructions for download and installation of the JRE are provided below:<br>
               <a href="http://www.java.com/en/download/manual.jsp" target="_blank">http://www.java.com/en/download/manual.jsp</a>,
            </p><p>
                <applet code="javaversion.class" WIDTH=500 HEIGHT=60 ></applet>
            </p><p>
               Instructions for linking to R for display and analysis of SPK output can be found <a href="instructR.jsp" target="_blank">here</a>.
            </p>
            <form action="servlet/uw.rfpk.servlets.GetJnlp" method="post">
              <input type="hidden" name="host" value=<%=host%>>
              <input type="hidden" name="port" value=<%=port%>>
              <input type="hidden" name="secret" value=<%=session.getAttribute("SECRET")%>>
              <input type="hidden" name="jnlp_dir" value=<%=jnlp_dir%>>
              <input type="hidden" name="type" value="nonmem">
              Model Design Agent  <input type="Submit" value="Download"><br>
            </form>
            <p>
               Note for MAC users:  While our development team has not tested MAC installation of the MDA, our users have reported that it is completely functional.
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
