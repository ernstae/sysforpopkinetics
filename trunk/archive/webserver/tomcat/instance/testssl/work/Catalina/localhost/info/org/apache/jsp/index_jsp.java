package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class index_jsp extends org.apache.jasper.runtime.HttpJspBase
    implements org.apache.jasper.runtime.JspSourceDependent {

  private static java.util.Vector _jspx_dependants;

  static {
    _jspx_dependants = new java.util.Vector(1);
    _jspx_dependants.add("/quicklinks.shtml");
  }

  public java.util.List getDependants() {
    return _jspx_dependants;
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

      out.write("<?xml version=\"1.0\"?>\n");
      out.write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">\n");
      out.write("\n");
      out.write("<html>\n");
      out.write("<head>\n");
      out.write("   <title>SPK: The System for Population Kinetics</title>\n");
      out.write("    <link href=stylesheet.css type=\"text/css\" rel=stylesheet>\n");
      out.write("</head>\n");
      out.write("<body>\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("<table align=left border=0 width=602>\n");
      out.write("      <tbody> \n");
      out.write("\t<tr>    \n");
      out.write("\t  <td colSpan=3 vAlign=top>\n");
      out.write("\t    <img align=top alt=\"RFPK logo\" height=40 src=\"./images/rfpklogo.gif\" width=112>\n");
      out.write("\t      <img align=top alt=\"Resource Facility for Population Kinetics\" height=40 \n");
      out.write("\t\tsrc=\"./images/rfpkfull.gif\" width=474>\n");
      out.write(" \t  </td> \n");
      out.write("\t</tr> \n");
      out.write("\t<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> \n");
      out.write("\t<tr>\n");
      out.write("\t  <td vAlign=top width=102 height=\"0\" colspan=\"1\" rowspan=\"1\">\n");
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
      out.write("    <a class=quick href=\"/user/index.jsp\">MySPK</a>\n");
      out.write("</p>\n");
      out.write("\n");
      out.write(" \n");
      out.write("  \n");
      out.write("\t  </td>\n");
      out.write("\t  <td colspan=1 vAlign=top width=10><img alt=\"trans gif\" height=5 src=\"./images/white.gif\" width=10/>\n");
      out.write("\t  <td colspan=1 vAlign=top>\n");
      out.write("\t    <h3>SPK -- The System for Population Kinetics</h3>\n");
      out.write("\t    <p> \n");
      out.write("\t      Welcome to the home of SPK, the System for Population Kinetics.  Spk \n");
      out.write("\t      is being developed by the Resource for Population Kinetics (RFPK), in the\n");
      out.write("\t      Bioengineering Department of University of Washington, with partial\n");
      out.write("\t      support from the National Institutes of Health (NIH), under grant\n");
      out.write("\t      P41 EB-001975. The software will be released under an Open Source License.\n");
      out.write("\t    </p>\n");
      out.write("\t    <h3>About Our University of Washington Certificate</h3>\n");
      out.write("\t    <p>\n");
      out.write("\t      This web site is hosted on a secure web server. When you first attempted \n");
      out.write("\t      to access this page, you may have been greeted by a pop-up window warning\n");
      out.write("              you that the security of the web site was suspect. The reason for this\n");
      out.write("\t      lack of trust \n");
      out.write("\t      is that our server certificate was issued by the University of Washington\n");
      out.write("\t      Certificate Authority rather than one of the commercial certificate \n");
      out.write("              authorities known to your browser.\n");
      out.write("\t    </p>\n");
      out.write("            <p>\n");
      out.write("\t      If you intend to be a repeat user of spk.rfpk.washington.edu, we recommend\n");
      out.write("\t      that you import the University of Washington <em>root</em> certificate\n");
      out.write("              into your browser. The process takes only a few seconds.\n");
      out.write("\t      After that has been accomplished, your browser will\n");
      out.write("\t      greet all certificates issued by the University of Washington \n");
      out.write("\t      Certificate Authority, and ours in particular, with the same degree of\n");
      out.write("\t      cordiality that it extends to commercial ones.\n");
      out.write("            </p>\n");
      out.write("            <p>\n");
      out.write("\t      The University of Washington root certificate can be installed\n");
      out.write("              <a href=\"https://www.washington.edu/computing/ca/index.html\">here</a>.\n");
      out.write("            </p>\n");
      out.write("\t  </td>\n");
      out.write("\t</tr>\n");
      out.write("      </tbody>\n");
      out.write("    </table>\n");
      out.write("</body>\n");
      out.write("</html>\n");
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
}
