<%@ page import="java.io.BufferedWriter" %>
<%@ page import="java.io.FileWriter" %>
<%@ page import="java.io.IOException" %>
<%@ page import="java.security.SecureRandom" %>
<jsp:useBean id="sessionObj" class="uw.rfpk.beans.SessionObject" scope="session" />
<html>
<head>
<title>User's Home Page</title>
</head>
<body bgcolor="white">
<%
    String sessionId = session.getId(); 
    String secret = null;
    String user = (String)session.getAttribute("USER_NAME");
%>
Welcome <b><%=user %></b><br><br>
<%
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
            sessionObj.setSessionObject(secret);
            session.setAttribute("SECRET", secret);
            session.setAttribute("SessionObj", sessionObj);

            // Create the jnlp file with sessionId and secret
            String file = "/home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/jnlp/"+secret+".jnlp";
            BufferedWriter o = new BufferedWriter(new FileWriter(file));
            o.write(
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"+
              "<jnlp spec=\"1.0+\" codebase=\"https://rose.rfpk.washington.edu:8443/spk\" href=\"jnlp/"+secret+".jnlp\">\n"+
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
              "</resources>\n"+
              "<application-desc main-class=\"uw.rfpk.mda.nonmem.MDA\">\n"+
              "<argument>" + sessionId + "</argument>\n"+    
              "<argument>" + secret + "</argument>\n"+
              "<argument>" + user + "</argument>\n"+  
              "</application-desc>\n"+
              "</jnlp>\n"
             );
            o.close();
        }
        catch(IOException e)
        {
        }
    }
    String url = "https://rose.rfpk.washington.edu:8443/spk/jnlp/"+(String)session.getAttribute("SECRET")+".jnlp";
    String download = "Please get <a href=" + url + ">Model Design Agent</a> from SPK server.";
%>
<%=download%>
<br>
<br>
You can log off by clicking
<a href='<%= response.encodeURL("index.jsp?logoff=true") %>'>here</a>.
This should cause you to be returned to the logon page.
</body>
<html>
