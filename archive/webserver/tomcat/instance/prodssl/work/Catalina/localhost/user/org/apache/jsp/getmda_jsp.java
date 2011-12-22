package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.security.SecureRandom;

public final class getmda_jsp extends org.apache.jasper.runtime.HttpJspBase
    implements org.apache.jasper.runtime.JspSourceDependent {

  private static java.util.Vector _jspx_dependants;

  static {
    _jspx_dependants = new java.util.Vector(1);
    _jspx_dependants.add("/quicklinks.shtml");
  }

  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_if_test;

  public java.util.List getDependants() {
    return _jspx_dependants;
  }

  public void _jspInit() {
    _jspx_tagPool_c_if_test = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
  }

  public void _jspDestroy() {
    _jspx_tagPool_c_if_test.release();
  }

  public void _jspService(HttpServletRequest request, HttpServletResponse response)
        throws java.io.IOException, ServletException {

    JspFactory _jspxFactory = null;
    PageContext pageContext = null;
    HttpSession session = null;
    ServletContext application = null;
    ServletConfig config = null;
    JspWriter out = null;
    Object page = this;
    JspWriter _jspx_out = null;
    PageContext _jspx_page_context = null;


    try {
      _jspxFactory = JspFactory.getDefaultFactory();
      response.setContentType("text/html");
      pageContext = _jspxFactory.getPageContext(this, request, response,
      			null, true, 8192, true);
      _jspx_page_context = pageContext;
      application = pageContext.getServletContext();
      config = pageContext.getServletConfig();
      session = pageContext.getSession();
      out = pageContext.getOut();
      _jspx_out = out;

      out.write("<!---------------------------------------------------------------------\n");
      out.write("From:   Resource Facility for Population Kinetics                    \n");
      out.write("        Department of Bioengineering Box 352255                      \n");
      out.write("        University of Washington                                     \n");
      out.write("        Seattle, WA 98195-2255                                       \n");
      out.write("\n");
      out.write("This file is part of the System for Population Kinetics (SPK), which\n");
      out.write("was developed with support from NIH grants RR-12609 and P41-\n");
      out.write("EB001975. Please cite these grants in any publication for which this\n");
      out.write("software is used and send a notification to the address given above.\n");
      out.write("\n");
      out.write("SPK is Copyright (C) 1998-2003, by the University of Washington,\n");
      out.write("Resource Facility for Population Kinetics, and is made available as\n");
      out.write("free open source software under the terms of the University of\n");
      out.write("Washington Free-Fork License as a public service.  A copy of the\n");
      out.write("License can be found in the COPYING file in the root directory of this\n");
      out.write("distribution.\n");
      out.write("---------------------------------------------------------------------->\n");
      out.write("<!--\n");
      out.write("author: Jiaji Du\n");
      out.write("-->\n");
      out.write("<?xml version=\"1.0\"?>\n");
      out.write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write('\n');
      if (_jspx_meth_c_if_0(_jspx_page_context))
        return;
      out.write('\n');
      out.write('\n');
      uw.rfpk.beans.SessionObject sessionObj = null;
      synchronized (session) {
        sessionObj = (uw.rfpk.beans.SessionObject) _jspx_page_context.getAttribute("sessionObj", PageContext.SESSION_SCOPE);
        if (sessionObj == null){
          sessionObj = new uw.rfpk.beans.SessionObject();
          _jspx_page_context.setAttribute("sessionObj", sessionObj, PageContext.SESSION_SCOPE);
        }
      }
      out.write('\n');
      uw.rfpk.beans.UserInfo validUser = null;
      synchronized (session) {
        validUser = (uw.rfpk.beans.UserInfo) _jspx_page_context.getAttribute("validUser", PageContext.SESSION_SCOPE);
        if (validUser == null){
          validUser = new uw.rfpk.beans.UserInfo();
          _jspx_page_context.setAttribute("validUser", validUser, PageContext.SESSION_SCOPE);
        }
      }
      out.write('\n');

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
              "<j2se version=\"1.6+\" initial-heap-size=\"128m\" max-heap-size =\"1024m\"/>\n"+
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
              "<argument>" + validUser.getUserId() + "</argument>\n"+
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

      out.write("\n");
      out.write("\n");
      out.write("<html>\n");
      out.write("<head>\n");
      out.write("  <title>Model Design Agent Download</title>\n");
      out.write("  <link href=stylesheet.css type=\"text/css\" rel=stylesheet>\n");
      out.write("</head>\n");
      out.write("  <body>\n");
      out.write("    <table align=left border=0 width=602>\n");
      out.write("      <tbody> \n");
      out.write("\t<tr> \n");
      out.write("\t  <td colSpan=3 vAlign=top>\n");
      out.write("\t    <img align=top alt=\"RFPK logo\" height=40 src=\"./images/rfpklogo.gif\" width=112>\n");
      out.write("\t    <img align=top alt=\"Resource Facility for Population Kinetics\" height=40 \n");
      out.write("\t\tsrc=\"./images/rfpkfull.gif\" width=474>\n");
      out.write("\t  </td>\n");
      out.write("\t</tr> \n");
      out.write("\t<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> \n");
      out.write("\t<tr>\n");
      out.write("\t  <td valign=top width=150 height=\"0\" colspan=\"1\" rowspan=\"1\">\n");
      out.write("            ");
      out.write("<!---------------------------------------------------------------------\n");
      out.write("From:   Resource Facility for Population Kinetics                    \n");
      out.write("        Department of Bioengineering Box 352255                      \n");
      out.write("        University of Washington                                     \n");
      out.write("        Seattle, WA 98195-2255                                       \n");
      out.write("\n");
      out.write("This file is part of the System for Population Kinetics (SPK), which\n");
      out.write("was developed with support from NIH grants RR-12609 and P41-\n");
      out.write("EB001975. Please cite these grants in any publication for which this\n");
      out.write("software is used and send a notification to the address given above.\n");
      out.write("\n");
      out.write("SPK is Copyright (C) 1998-2003, by the University of Washington,\n");
      out.write("Resource Facility for Population Kinetics, and is made available as\n");
      out.write("free open source software under the terms of the University of\n");
      out.write("Washington Free-Fork License as a public service.  A copy of the\n");
      out.write("License can be found in the COPYING file in the root directory of this\n");
      out.write("distribution.\n");
      out.write("---------------------------------------------------------------------->\n");
      out.write("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n");
      out.write("\n");
      out.write("<font size=2>\n");
      out.write("<p class=quick>Quick Links</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"http://depts.washington.edu/rfpk/\">RFPK Home</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"/info/index.jsp\">SPK Home</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"index.jsp\">MySPK</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"RFPK_SPK_TERMS_OF_SERVICE.html\" target=\"_blank\">Terms of Service</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"usermain.jsp\">Member</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"chpassword.jsp\">Password</a>\n");
      out.write("</p> \n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"getmda.jsp\">Download</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"WebHelp1/SPKStart.htm\" target=\"_blank\">Getting Started</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"SPK_User_Manual.pdf\">User Manual</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"userjobs.jsp\">My Jobs</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"usermodels.jsp?start=0&counter=0\">My Models</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"userdatasets.jsp?start=0&counter=0\">My Datasets</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"examplejobs.jsp?start=0&counter=0\">Job Examples</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"modellibrary.jsp?start=0&counter=0\">Model Library</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"datasetlibrary.jsp?start=0&counter=0\">Dataset Library</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"userinfo.jsp\">My Account</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"http://bugzilla.rfpk.washington.edu\">Bug Report</a>\n");
      out.write("</p>\n");
      out.write("</font>\n");
      out.write("  \n");
      out.write("\t  </td>\n");
      out.write("\t  <td colspan=1 vAlign=top width=10><img alt=\"trans gif\" height=5 src=\"./images/white.gif\" width=10/>\n");
      out.write("\t  <td colspan=1 vAlign=top>\n");
      out.write("            <h3>Model Design Agent Download</h3>\n");
      out.write("            <p>\n");
      out.write("               The Model Design Agent (MDA) is the user interface to create and submit jobs and view and plot results in SPK.  The MDA download checks automatically for the latest version.  While downloading, you will see a Java Web Start window and \"Model Design Agent, RFPK UW\".  Answer \"Yes\" to related security questions if they appear.\n");
      out.write("            </p>\n");
      out.write("                <p style=\"color:blue\">Important Message:<br>\n");
      out.write("                As of July 20, 2007, the MDA will require Java Runtime Environment version 1.6.0 or higher. Please follow instructions below.</p>\n");
      out.write("            <p>\n");
      out.write("               To download and use the MDA, it is necessary to install the Java Runtime Environment (JRE) on your computer.  Instructions for download and installation of the JRE are provided below:<br>\n");
      out.write("               <a href=\"http://www.java.com/en/download/manual.jsp\" target=\"_blank\">http://www.java.com/en/download/manual.jsp</a>,\n");
      out.write("            </p><p>\n");
      out.write("                <applet code=\"javaversion.class\" WIDTH=500 HEIGHT=60 ></applet>\n");
      out.write("            </p><p>\n");
      out.write("               Instructions for linking to R for display and analysis of SPK output can be found <a href=\"instructR.jsp\" target=\"_blank\">here</a>.\n");
      out.write("            </p>\n");
      out.write("            <form action=\"servlet/uw.rfpk.servlets.GetJnlp\" method=\"post\">\n");
      out.write("              <input type=\"hidden\" name=\"host\" value=");
      out.print(host);
      out.write(">\n");
      out.write("              <input type=\"hidden\" name=\"port\" value=");
      out.print(port);
      out.write(">\n");
      out.write("              <input type=\"hidden\" name=\"secret\" value=");
      out.print(session.getAttribute("SECRET"));
      out.write(">\n");
      out.write("              <input type=\"hidden\" name=\"jnlp_dir\" value=");
      out.print(jnlp_dir);
      out.write(">\n");
      out.write("              <input type=\"hidden\" name=\"type\" value=\"nonmem\">\n");
      out.write("              Model Design Agent  <input type=\"Submit\" value=\"Download\"><br>\n");
      out.write("            </form>\n");
      out.write("            <p>\n");
      out.write("               Note for MAC users:  While our development team has not tested MAC installation of the MDA, our users have reported that it is completely functional.\n");
      out.write("            </p>\n");
      out.write("            <p>\n");
      out.write("               When you are done, please <a href=\"logout.jsp\">log out</a>.\n");
      out.write("            </p>\n");
      out.write("       \t  </td>\n");
      out.write("\t</tr>     \n");
      out.write("      </tbody>\n");
      out.write("    </table>\n");
      out.write("  </body>\n");
      out.write("</html>\n");
    } catch (Throwable t) {
      if (!(t instanceof SkipPageException)){
        out = _jspx_out;
        if (out != null && out.getBufferSize() != 0)
          out.clearBuffer();
        if (_jspx_page_context != null) _jspx_page_context.handlePageException(t);
      }
    } finally {
      if (_jspxFactory != null) _jspxFactory.releasePageContext(_jspx_page_context);
    }
  }

  private boolean _jspx_meth_c_if_0(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    HttpServletRequest request = (HttpServletRequest)_jspx_page_context.getRequest();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_0 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_0.setPageContext(_jspx_page_context);
    _jspx_th_c_if_0.setParent(null);
    _jspx_th_c_if_0.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${validUser == null}", java.lang.Boolean.class, (PageContext)_jspx_page_context, null, false)).booleanValue());
    int _jspx_eval_c_if_0 = _jspx_th_c_if_0.doStartTag();
    if (_jspx_eval_c_if_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write('\n');
        out.write(' ');
        out.write(' ');
        if (true) {
          _jspx_page_context.forward("index.jsp" + (("index.jsp").indexOf('?')>0? '&': '?') + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("origURL", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.request.requestURL}", java.lang.String.class, (PageContext)_jspx_page_context, null, false), request.getCharacterEncoding()) + "&" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("errorMsg", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("Please log in first.", request.getCharacterEncoding()));
          return true;
        }
        out.write('\n');
        int evalDoAfterBody = _jspx_th_c_if_0.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_if_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_0);
    return false;
  }
}
