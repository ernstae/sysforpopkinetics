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
            // Generate the secret bytes for creating the session key
            SecureRandom seed = new SecureRandom();
            byte[] b = seed.generateSeed(16);

            // Convert the byte array to a String object
            StringBuffer buf = new StringBuffer(); 
            for(int i = 0; i < 16; i++)
	    {
                int m = 0x80;
                for(int j = 0; j < 8; j++)
                {
                    char c = '0';                   
                    if((b[i] & m) == m) c = '1';
	 	    m = m >> 1;
		    buf.append(c);
	        }
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
              "<description>Demonstration of MDA</description>\n"+
              "<offline-allowed/>\n"+
              "</information>\n"+
              "<security>\n"+
              "<all-permissions/>\n"+
              "</security>\n"+
              "<resources>\n"+
              "<j2se version=\"1.4+\" />\n"+
              "<jar href=\"MDA.jar\"/>\n"+
              "<jar href=\"xercesImpl.jar\"/>\n"+
              "<jar href=\"xmlParserAPIs.jar\"/>\n"+
              "</resources>\n"+
              "<application-desc main-class=\"uw.rfpk.mda.nonmem.MDA\">\n"+
              "<argument>" + host + "</argument>\n"+    
              "<argument>" + port + "</argument>\n"+
              "<argument>" + sessionId + "</argument>\n"+    
              "<argument>" + secret + "</argument>\n"+ 
              "</application-desc>\n"+
              "</jnlp>\n"
             );
            o.close();
            sessionObj.setSessionObject(file);
        }
        catch(IOException e)
        {
        }
    }
    String url = "https://"+host+":"+port+"/user/jnlp/"+(String)session.getAttribute("SECRET")+".jnlp";
    String download = "<a href=" + url + ">Model Design Agent - NONMEM type</a>";
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
	  <td valign=top width=102 height="0" colspan="1" rowspan="1">
            <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
            <p>
               Model Design Agent has various user interface types. <br>
               Please select one to download.  You should see a
               window that says "Java loading..." and then "Model Design Agent RFPK
               UW". The MDA will automatically check for the latest version. Answer
               "Yes" if you are asked another security question. Windows user can choose to have
               the MDA on your desktop or not (it's up to you).<br><br>
               <%=download%>
               <br><br><br><br>
            </p><p>
               Note:  FOR WINDOWS USERS: If the MDA fails to install:<br>
               Open Java Web Start<br>
               Go to File -> Preferences<br>
               Go to the Advanced tab<br>
               Change the Applications Folder to<br>
               C:\Documents and Settings\username\.javaws\cache<br>
               where "username" is your Windows user name<br>
               Try to download the MDA again.
            </p><p>
               When you are done, please <a href="logout.jsp">log out</a>.
            </p>
       	  </td>
	</tr>     
      </tbody>
    </table>
  </body>
</html>
