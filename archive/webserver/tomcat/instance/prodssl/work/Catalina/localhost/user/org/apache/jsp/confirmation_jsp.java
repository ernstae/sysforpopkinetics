package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class confirmation_jsp extends org.apache.jasper.runtime.HttpJspBase
    implements org.apache.jasper.runtime.JspSourceDependent {

static private org.apache.jasper.runtime.ProtectedFunctionMapper _jspx_fnmap_0;

static {
  _jspx_fnmap_0= org.apache.jasper.runtime.ProtectedFunctionMapper.getMapForFunction("fn:escapeXml", org.apache.taglibs.standard.functions.Functions.class, "escapeXml", new Class[] {java.lang.String.class});
}

  private static java.util.Vector _jspx_dependants;

  static {
    _jspx_dependants = new java.util.Vector(1);
    _jspx_dependants.add("/quicklinks.shtml");
  }

  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_if_test;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_forEach_var_items;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_set_var_value_nobody;

  public java.util.List getDependants() {
    return _jspx_dependants;
  }

  public void _jspInit() {
    _jspx_tagPool_c_if_test = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_forEach_var_items = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_set_var_value_nobody = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
  }

  public void _jspDestroy() {
    _jspx_tagPool_c_if_test.release();
    _jspx_tagPool_c_forEach_var_items.release();
    _jspx_tagPool_c_set_var_value_nobody.release();
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

      out.write("<!---------------------------------------------------------------------\r\n");
      out.write("From:   Resource Facility for Population Kinetics                    \r\n");
      out.write("        Department of Bioengineering Box 352255                      \r\n");
      out.write("        University of Washington                                     \r\n");
      out.write("        Seattle, WA 98195-2255                                       \r\n");
      out.write("\r\n");
      out.write("This file is part of the System for Population Kinetics (SPK), which\r\n");
      out.write("was developed with support from NIH grants RR-12609 and P41-\r\n");
      out.write("EB001975. Please cite these grants in any publication for which this\r\n");
      out.write("software is used and send a notification to the address given above.\r\n");
      out.write("\r\n");
      out.write("SPK is Copyright (C) 1998-2003, by the University of Washington,\r\n");
      out.write("Resource Facility for Population Kinetics, and is made available as\r\n");
      out.write("free open source software under the terms of the University of\r\n");
      out.write("Washington Free-Fork License as a public service.  A copy of the\r\n");
      out.write("License can be found in the COPYING file in the root directory of this\r\n");
      out.write("distribution.\r\n");
      out.write("---------------------------------------------------------------------->\r\n");
      out.write("<!--\r\n");
      out.write("author: Jiaji Du\r\n");
      out.write("-->\r\n");
      out.write("<?xml version=\"1.0\"?>\r\n");
      out.write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write('\r');
      out.write('\n');
      if (_jspx_meth_c_if_0(_jspx_page_context))
        return;
      out.write("\r\n");
      out.write("\r\n");
      out.write("<html>\r\n");
      out.write("<head>\r\n");
      out.write("  <title>The User Information Stored</title>\r\n");
      out.write("  <link href=stylesheet.css type=\"text/css\" rel=stylesheet>\r\n");
      out.write("</head>\r\n");
      out.write("  <body>\r\n");
      out.write("    <table align=left border=0 width=602>\r\n");
      out.write("      <tbody> \r\n");
      out.write("\t<tr> \r\n");
      out.write("\t  <td colSpan=3 vAlign=top>\r\n");
      out.write("\t    <img align=top alt=\"RFPK logo\" height=40 src=\"./images/rfpklogo.gif\" width=112>\r\n");
      out.write("\t    <img align=top alt=\"Resource Facility for Population Kinetics\" height=40 \r\n");
      out.write("\t\tsrc=\"./images/rfpkfull.gif\" width=474>\r\n");
      out.write("\t  </td>\r\n");
      out.write("\t</tr> \r\n");
      out.write("\t<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> \r\n");
      out.write("\t<tr>\r\n");
      out.write("\t  <td valign=top width=150 height=\"0\" colspan=\"1\" rowspan=\"1\">\r\n");
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
      out.write("    <a class=quick href=\"WebHelp/spkstart.htm\" target=\"_blank\">Getting Started</a>\n");
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
      out.write("  \r\n");
      out.write("\t  </td>\r\n");
      out.write("\t  <td colspan=1 vAlign=top width=10><img alt=\"trans gif\" height=5 src=\"./images/white.gif\" width=10/>\r\n");
      out.write("\t  <td colspan=1 vAlign=top>\r\n");
      out.write("\r\n");
      out.write("            <h4>This is the information stored in the user table of the spkdb database:</h4>\r\n");
      out.write("            <table>\r\n");
      out.write("              ");
      if (_jspx_meth_c_forEach_0(_jspx_page_context))
        return;
      out.write("\r\n");
      out.write("            </table>\r\n");
      out.write("          ");
      if (_jspx_meth_c_if_1(_jspx_page_context))
        return;
      out.write("\r\n");
      out.write("          ");
      if (_jspx_meth_c_if_2(_jspx_page_context))
        return;
      out.write("\r\n");
      out.write("          ");
      //  c:if
      org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_3 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
      _jspx_th_c_if_3.setPageContext(_jspx_page_context);
      _jspx_th_c_if_3.setParent(null);
      _jspx_th_c_if_3.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.enter == 'Send Email'}", java.lang.Boolean.class, (PageContext)_jspx_page_context, null, false)).booleanValue());
      int _jspx_eval_c_if_3 = _jspx_th_c_if_3.doStartTag();
      if (_jspx_eval_c_if_3 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        do {
          out.write("\r\n");
          out.write("            ");
  Process process = null;
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
                }  
          out.write("\r\n");
          out.write("          ");
          int evalDoAfterBody = _jspx_th_c_if_3.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
      }
      if (_jspx_th_c_if_3.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return;
      _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_3);
      out.write("\r\n");
      out.write("               When you are done, please <a href=\"logout.jsp\">log out</a>.\r\n");
      out.write("            </p>\r\n");
      out.write("          </td>\r\n");
      out.write("        </tr>\r\n");
      out.write("      </tbody>\r\n");
      out.write("    </table>\r\n");
      out.write("  </body>\r\n");
      out.write("</html>\r\n");
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
        out.write("\r\n");
        out.write("  ");
        if (true) {
          _jspx_page_context.forward("index.jsp" + (("index.jsp").indexOf('?')>0? '&': '?') + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("origURL", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.request.requestURL}", java.lang.String.class, (PageContext)_jspx_page_context, null, false), request.getCharacterEncoding()) + "&" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("errorMsg", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("Please log in first.", request.getCharacterEncoding()));
          return true;
        }
        out.write('\r');
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

  private boolean _jspx_meth_c_forEach_0(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:forEach
    org.apache.taglibs.standard.tag.rt.core.ForEachTag _jspx_th_c_forEach_0 = (org.apache.taglibs.standard.tag.rt.core.ForEachTag) _jspx_tagPool_c_forEach_var_items.get(org.apache.taglibs.standard.tag.rt.core.ForEachTag.class);
    _jspx_th_c_forEach_0.setPageContext(_jspx_page_context);
    _jspx_th_c_forEach_0.setParent(null);
    _jspx_th_c_forEach_0.setItems((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${newUserDbInfo.rows}", java.lang.Object.class, (PageContext)_jspx_page_context, null, false));
    _jspx_th_c_forEach_0.setVar("row");
    int[] _jspx_push_body_count_c_forEach_0 = new int[] { 0 };
    try {
      int _jspx_eval_c_forEach_0 = _jspx_th_c_forEach_0.doStartTag();
      if (_jspx_eval_c_forEach_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        do {
          out.write("\r\n");
          out.write("              ");
          if (_jspx_meth_c_forEach_1(_jspx_th_c_forEach_0, _jspx_page_context, _jspx_push_body_count_c_forEach_0))
            return true;
          out.write("\r\n");
          out.write("              ");
          int evalDoAfterBody = _jspx_th_c_forEach_0.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
      }
      if (_jspx_th_c_forEach_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return true;
    } catch (Throwable _jspx_exception) {
      while (_jspx_push_body_count_c_forEach_0[0]-- > 0)
        out = _jspx_page_context.popBody();
      _jspx_th_c_forEach_0.doCatch(_jspx_exception);
    } finally {
      _jspx_th_c_forEach_0.doFinally();
      _jspx_tagPool_c_forEach_var_items.reuse(_jspx_th_c_forEach_0);
    }
    return false;
  }

  private boolean _jspx_meth_c_forEach_1(javax.servlet.jsp.tagext.JspTag _jspx_th_c_forEach_0, PageContext _jspx_page_context, int[] _jspx_push_body_count_c_forEach_0)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:forEach
    org.apache.taglibs.standard.tag.rt.core.ForEachTag _jspx_th_c_forEach_1 = (org.apache.taglibs.standard.tag.rt.core.ForEachTag) _jspx_tagPool_c_forEach_var_items.get(org.apache.taglibs.standard.tag.rt.core.ForEachTag.class);
    _jspx_th_c_forEach_1.setPageContext(_jspx_page_context);
    _jspx_th_c_forEach_1.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_forEach_0);
    _jspx_th_c_forEach_1.setItems((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${row}", java.lang.Object.class, (PageContext)_jspx_page_context, null, false));
    _jspx_th_c_forEach_1.setVar("column");
    int[] _jspx_push_body_count_c_forEach_1 = new int[] { 0 };
    try {
      int _jspx_eval_c_forEach_1 = _jspx_th_c_forEach_1.doStartTag();
      if (_jspx_eval_c_forEach_1 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        do {
          out.write("\r\n");
          out.write("              <tr>\r\n");
          out.write("                <td align=right>\r\n");
          out.write("                  <b>");
          out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(column.key)}", java.lang.String.class, (PageContext)_jspx_page_context, _jspx_fnmap_0, false));
          out.write(":</b>\r\n");
          out.write("                </td>\r\n");
          out.write("                <td>\r\n");
          out.write("                  ");
          out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(column.value)}", java.lang.String.class, (PageContext)_jspx_page_context, _jspx_fnmap_0, false));
          out.write("\r\n");
          out.write("                </td>\r\n");
          out.write("              </tr>\r\n");
          out.write("              ");
          int evalDoAfterBody = _jspx_th_c_forEach_1.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
      }
      if (_jspx_th_c_forEach_1.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return true;
    } catch (Throwable _jspx_exception) {
      while (_jspx_push_body_count_c_forEach_1[0]-- > 0)
        out = _jspx_page_context.popBody();
      _jspx_th_c_forEach_1.doCatch(_jspx_exception);
    } finally {
      _jspx_th_c_forEach_1.doFinally();
      _jspx_tagPool_c_forEach_var_items.reuse(_jspx_th_c_forEach_1);
    }
    return false;
  }

  private boolean _jspx_meth_c_if_1(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_1 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_1.setPageContext(_jspx_page_context);
    _jspx_th_c_if_1.setParent(null);
    _jspx_th_c_if_1.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${not empty initParam.bugdb_driver}", java.lang.Boolean.class, (PageContext)_jspx_page_context, null, false)).booleanValue());
    int _jspx_eval_c_if_1 = _jspx_th_c_if_1.doStartTag();
    if (_jspx_eval_c_if_1 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\r\n");
        out.write("            <h4>This is the information stored in the profiles table of the bugs database:</h4>\r\n");
        out.write("            <table>\r\n");
        out.write("              ");
        if (_jspx_meth_c_forEach_2(_jspx_th_c_if_1, _jspx_page_context))
          return true;
        out.write("\r\n");
        out.write("            </table>\r\n");
        out.write("          ");
        int evalDoAfterBody = _jspx_th_c_if_1.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_if_1.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_1);
    return false;
  }

  private boolean _jspx_meth_c_forEach_2(javax.servlet.jsp.tagext.JspTag _jspx_th_c_if_1, PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:forEach
    org.apache.taglibs.standard.tag.rt.core.ForEachTag _jspx_th_c_forEach_2 = (org.apache.taglibs.standard.tag.rt.core.ForEachTag) _jspx_tagPool_c_forEach_var_items.get(org.apache.taglibs.standard.tag.rt.core.ForEachTag.class);
    _jspx_th_c_forEach_2.setPageContext(_jspx_page_context);
    _jspx_th_c_forEach_2.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_if_1);
    _jspx_th_c_forEach_2.setItems((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${newBugsDbInfo.rows}", java.lang.Object.class, (PageContext)_jspx_page_context, null, false));
    _jspx_th_c_forEach_2.setVar("row");
    int[] _jspx_push_body_count_c_forEach_2 = new int[] { 0 };
    try {
      int _jspx_eval_c_forEach_2 = _jspx_th_c_forEach_2.doStartTag();
      if (_jspx_eval_c_forEach_2 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        do {
          out.write("\r\n");
          out.write("              ");
          if (_jspx_meth_c_forEach_3(_jspx_th_c_forEach_2, _jspx_page_context, _jspx_push_body_count_c_forEach_2))
            return true;
          out.write("\r\n");
          out.write("              ");
          int evalDoAfterBody = _jspx_th_c_forEach_2.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
      }
      if (_jspx_th_c_forEach_2.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return true;
    } catch (Throwable _jspx_exception) {
      while (_jspx_push_body_count_c_forEach_2[0]-- > 0)
        out = _jspx_page_context.popBody();
      _jspx_th_c_forEach_2.doCatch(_jspx_exception);
    } finally {
      _jspx_th_c_forEach_2.doFinally();
      _jspx_tagPool_c_forEach_var_items.reuse(_jspx_th_c_forEach_2);
    }
    return false;
  }

  private boolean _jspx_meth_c_forEach_3(javax.servlet.jsp.tagext.JspTag _jspx_th_c_forEach_2, PageContext _jspx_page_context, int[] _jspx_push_body_count_c_forEach_2)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:forEach
    org.apache.taglibs.standard.tag.rt.core.ForEachTag _jspx_th_c_forEach_3 = (org.apache.taglibs.standard.tag.rt.core.ForEachTag) _jspx_tagPool_c_forEach_var_items.get(org.apache.taglibs.standard.tag.rt.core.ForEachTag.class);
    _jspx_th_c_forEach_3.setPageContext(_jspx_page_context);
    _jspx_th_c_forEach_3.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_forEach_2);
    _jspx_th_c_forEach_3.setItems((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${row}", java.lang.Object.class, (PageContext)_jspx_page_context, null, false));
    _jspx_th_c_forEach_3.setVar("column");
    int[] _jspx_push_body_count_c_forEach_3 = new int[] { 0 };
    try {
      int _jspx_eval_c_forEach_3 = _jspx_th_c_forEach_3.doStartTag();
      if (_jspx_eval_c_forEach_3 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        do {
          out.write("\r\n");
          out.write("              <tr>\r\n");
          out.write("                <td align=right>\r\n");
          out.write("                  <b>");
          out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(column.key)}", java.lang.String.class, (PageContext)_jspx_page_context, _jspx_fnmap_0, false));
          out.write(":</b>\r\n");
          out.write("                </td>\r\n");
          out.write("                <td>\r\n");
          out.write("                  ");
          out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(column.value)}", java.lang.String.class, (PageContext)_jspx_page_context, _jspx_fnmap_0, false));
          out.write("\r\n");
          out.write("                </td>\r\n");
          out.write("              </tr>\r\n");
          out.write("              ");
          int evalDoAfterBody = _jspx_th_c_forEach_3.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
      }
      if (_jspx_th_c_forEach_3.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return true;
    } catch (Throwable _jspx_exception) {
      while (_jspx_push_body_count_c_forEach_3[0]-- > 0)
        out = _jspx_page_context.popBody();
      _jspx_th_c_forEach_3.doCatch(_jspx_exception);
    } finally {
      _jspx_th_c_forEach_3.doFinally();
      _jspx_tagPool_c_forEach_var_items.reuse(_jspx_th_c_forEach_3);
    }
    return false;
  }

  private boolean _jspx_meth_c_if_2(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_2 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_2.setPageContext(_jspx_page_context);
    _jspx_th_c_if_2.setParent(null);
    _jspx_th_c_if_2.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.task == 'addnew'}", java.lang.Boolean.class, (PageContext)_jspx_page_context, null, false)).booleanValue());
    int _jspx_eval_c_if_2 = _jspx_th_c_if_2.doStartTag();
    if (_jspx_eval_c_if_2 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\r\n");
        out.write("            <h4>You may send an email notice to the new user.</h4>\r\n");
        out.write("            ");
        if (_jspx_meth_c_set_0(_jspx_th_c_if_2, _jspx_page_context))
          return true;
        out.write("\r\n");
        out.write("            <form action=\"confirmation.jsp?to=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.email}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("&firstName=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.first_name}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("&lastName=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.surname}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("&userName=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.username}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("&password=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.password}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("&email=");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.email}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
        out.write("\" method=\"POST\">\r\n");
        out.write("              <input type=\"Submit\" name=\"enter\" value=\"Send Email\" />\r\n");
        out.write("            </form>\r\n");
        out.write("          ");
        int evalDoAfterBody = _jspx_th_c_if_2.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_if_2.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_2);
    return false;
  }

  private boolean _jspx_meth_c_set_0(javax.servlet.jsp.tagext.JspTag _jspx_th_c_if_2, PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  c:set
    org.apache.taglibs.standard.tag.rt.core.SetTag _jspx_th_c_set_0 = (org.apache.taglibs.standard.tag.rt.core.SetTag) _jspx_tagPool_c_set_var_value_nobody.get(org.apache.taglibs.standard.tag.rt.core.SetTag.class);
    _jspx_th_c_set_0.setPageContext(_jspx_page_context);
    _jspx_th_c_set_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_if_2);
    _jspx_th_c_set_0.setVar("user");
    _jspx_th_c_set_0.setValue((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${newUserDbInfo.rows[0]}", java.lang.Object.class, (PageContext)_jspx_page_context, null, false));
    int _jspx_eval_c_set_0 = _jspx_th_c_set_0.doStartTag();
    if (_jspx_th_c_set_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_set_var_value_nobody.reuse(_jspx_th_c_set_0);
    return false;
  }
}
