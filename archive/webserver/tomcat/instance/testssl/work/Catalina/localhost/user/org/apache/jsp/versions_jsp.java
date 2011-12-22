package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;
import java.util.Vector;

public final class versions_jsp extends org.apache.jasper.runtime.HttpJspBase
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


    try {
      _jspxFactory = JspFactory.getDefaultFactory();
      response.setContentType("text/html");
      pageContext = _jspxFactory.getPageContext(this, request, response,
      			null, true, 8192, true);
      application = pageContext.getServletContext();
      config = pageContext.getServletConfig();
      session = pageContext.getSession();
      out = pageContext.getOut();
      _jspx_out = out;

      out.write("<?xml version=\"1.0\"?>\r\n");
      out.write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write("\r\n");
      out.write("\r\n");
      if (_jspx_meth_c_if_0(pageContext))
        return;
      out.write("\r\n");
      out.write("\r\n");
      out.write("<html>\r\n");
      out.write("  <head>\r\n");
      out.write("    <title>Version List</title>\r\n");
      out.write("  </head>\r\n");
      out.write("  <body bgcolor=\"white\">\r\n");
      out.write("\r\n");
      out.write("    <table align=left border=0 width=602>\r\n");
      out.write("      <tbody> \r\n");
      out.write("\t<tr>    \r\n");
      out.write("\t  <td colSpan=3 vAlign=top>\r\n");
      out.write("\t    <img align=top alt=\"RFPK logo\" height=40 src=\"./images/rfpklogo.gif\" width=112>\r\n");
      out.write("\t      <img align=top alt=\"Resource Facility for Population Kinetics\" height=40 \r\n");
      out.write("\t\tsrc=\"./images/rfpkfull.gif\" width=474>\r\n");
      out.write(" \t  </td> \r\n");
      out.write("\t</tr> \r\n");
      out.write("\t<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> \r\n");
      out.write("\t<tr>\r\n");
      out.write("\t  <td vAlign=top width=150 height=\"0\" colspan=\"1\" rowspan=\"1\">\r\n");
      out.write("          ");
      out.write("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n");
      out.write("\n");
      out.write("\n");
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
      out.write("    <a class=quick href=\"usermain.jsp\">Member</a>\n");
      out.write("</p>\n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"chpassword.jsp\">Password</a>\n");
      out.write("</p> \n");
      out.write("<p>\n");
      out.write("    <a class=quick href=\"getmda.jsp\">Download</a>\n");
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
      out.write("  \r\n");
      out.write("\t  </td>\r\n");
      out.write("\t  <td colspan=1 vAlign=top width=10><img alt=\"trans gif\" height=5 src=\"./images/white.gif\" width=10/>\r\n");
      out.write("\t  <td colspan=1 vAlign=top>\r\n");
      out.write("\t    <h3>Version List</h3>\r\n");
      out.write("\t    <p> \r\n");
      out.write("              ");
      uw.rfpk.beans.UserInfo validUser = null;
      synchronized (session) {
        validUser = (uw.rfpk.beans.UserInfo) pageContext.getAttribute("validUser", PageContext.SESSION_SCOPE);
        if (validUser == null){
          validUser = new uw.rfpk.beans.UserInfo();
          pageContext.setAttribute("validUser", validUser, PageContext.SESSION_SCOPE);
        }
      }
      out.write("\r\n");
      out.write("              ");
      uw.rfpk.beans.VersionList version = null;
      synchronized (session) {
        version = (uw.rfpk.beans.VersionList) pageContext.getAttribute("version", PageContext.SESSION_SCOPE);
        if (version == null){
          version = new uw.rfpk.beans.VersionList();
          pageContext.setAttribute("version", version, PageContext.SESSION_SCOPE);
        }
      }
      out.write("\r\n");
      out.write("              ");
 String[][] versionList = version.getVersionList(Long.parseLong(request.getParameter("id")), 
                                                                 request.getParameter("type"),
                                                                 getServletContext().getInitParameter("database_name"),
                                                                 getServletContext().getInitParameter("database_host"),
                                                                 getServletContext().getInitParameter("database_username"),
                                                                 getServletContext().getInitParameter("database_password"));
                 int size = versionList.length; 
                 if(size == 0)
                 { 
      out.write("\r\n");
      out.write("                     No model seems to be in there anymore ...\r\n");
      out.write("              ");
 }
                 else
                 { 
      out.write("\r\n");
      out.write("                     The following user versions are found:\r\n");
      out.write("                 <p>\r\n");
      out.write("                 <table border=\"1\">\r\n");
      out.write("                 <th>Revision</th>\r\n");
      out.write("                 <th>Author</th>\r\n");
      out.write("                 <th>Revised Time</th>\r\n");
      out.write("                 <th>Log Message</th>\r\n");
      out.write("                  ");
 for(int i = 0; i < size; i++)
                     { 
      out.write("     \r\n");
      out.write("                  <tr>\r\n");
      out.write("                    <td align=\"center\">");
      out.print(versionList[i][0]);
      out.write("</td>\r\n");
      out.write("                    <td>");
      out.print(versionList[i][1]);
      out.write("</td>\r\n");
      out.write("                    <td>");
      out.print(versionList[i][2]);
      out.write("</td>\r\n");
      out.write("                    <td>");
      out.print(versionList[i][3]);
      out.write("</td>\r\n");
      out.write("                  </tr>\r\n");
      out.write("                  ");
 } 
      out.write("\r\n");
      out.write("                 </table>\r\n");
      out.write("              ");
 } 
      out.write("\r\n");
      out.write("\t    </p>\r\n");
      out.write("\t    <p> \r\n");
      out.write("               For further information please use a <a href=\"getmda.jsp\">Model Design Agent</a>.\r\n");
      out.write("               When you are done, please <a href=\"logout.jsp\">log out</a>.\r\n");
      out.write("\t    </p>\r\n");
      out.write("\t  </td>\r\n");
      out.write("\t</tr>\r\n");
      out.write("      </tbody>\r\n");
      out.write("    </table>\r\n");
      out.write("  </body>\r\n");
      out.write("</html>\r\n");
    } catch (Throwable t) {
      if (!(t instanceof SkipPageException)){
        out = _jspx_out;
        if (out != null && out.getBufferSize() != 0)
          out.clearBuffer();
        if (pageContext != null) pageContext.handlePageException(t);
      }
    } finally {
      if (_jspxFactory != null) _jspxFactory.releasePageContext(pageContext);
    }
  }

  private boolean _jspx_meth_c_if_0(PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    HttpServletRequest request = (HttpServletRequest)pageContext.getRequest();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_0 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_0.setPageContext(pageContext);
    _jspx_th_c_if_0.setParent(null);
    _jspx_th_c_if_0.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${validUser == null}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_0 = _jspx_th_c_if_0.doStartTag();
    if (_jspx_eval_c_if_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\r\n");
        out.write("  ");
        if (true) {
          pageContext.forward("index.jsp" + (("index.jsp").indexOf('?')>0? '&': '?') + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("origURL", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.request.requestURL}", java.lang.String.class, (PageContext)pageContext, null, false), request.getCharacterEncoding()) + "&" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("errorMsg", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("Please log in first.", request.getCharacterEncoding()));
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
}
