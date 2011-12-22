package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class index_jsp extends org.apache.jasper.runtime.HttpJspBase
    implements org.apache.jasper.runtime.JspSourceDependent {

  private static java.util.Vector _jspx_dependants;

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


  if (request.getParameter("logoff") != null) {
    session.invalidate();
    response.sendRedirect("index.jsp");
    return;
  }

      out.write("\r\n");
      out.write("<html>\r\n");
      out.write("<head>\r\n");
      out.write("<title>Protected Page for Examples</title>\r\n");
      out.write("</head>\r\n");
      out.write("<body bgcolor=\"white\">\r\n");
      out.write("\r\n");
      out.write("You are logged in as remote user <b>");
      out.print( request.getRemoteUser() );
      out.write("</b>\r\n");
      out.write("in session <b>");
      out.print( session.getId() );
      out.write("</b><br><br>\r\n");
      out.write("\r\n");

  if (request.getUserPrincipal() != null) {

      out.write("\r\n");
      out.write("    Your user principal name is\r\n");
      out.write("    <b>");
      out.print( request.getUserPrincipal().getName() );
      out.write("</b><br><br>\r\n");

  } else {

      out.write("\r\n");
      out.write("    No user principal could be identified.<br><br>\r\n");

  }

      out.write("\r\n");
      out.write("\r\n");

  String role = request.getParameter("role");
  if (role == null)
    role = "";
  if (role.length() > 0) {
    if (request.isUserInRole(role)) {

      out.write("\r\n");
      out.write("      You have been granted role <b>");
      out.print( role );
      out.write("</b><br><br>\r\n");

    } else {

      out.write("\r\n");
      out.write("      You have <i>not</i> been granted role <b>");
      out.print( role );
      out.write("</b><br><br>\r\n");

    }
  }

      out.write("\r\n");
      out.write("\r\n");
      out.write("To check whether your username has been granted a particular role,\r\n");
      out.write("enter it here:\r\n");
      out.write("<form method=\"GET\" action='");
      out.print( response.encodeURL("index.jsp") );
      out.write("'>\r\n");
      out.write("<input type=\"text\" name=\"role\" value=\"");
      out.print( role );
      out.write("\">\r\n");
      out.write("</form>\r\n");
      out.write("<br><br>\r\n");
      out.write("\r\n");
      out.write("If you have configured this app for form-based authentication, you can log\r\n");
      out.write("off by clicking\r\n");
      out.write("<a href='");
      out.print( response.encodeURL("index.jsp?logoff=true") );
      out.write("'>here</a>.\r\n");
      out.write("This should cause you to be returned to the logon page after the redirect\r\n");
      out.write("that is performed.\r\n");
      out.write("\r\n");
      out.write("</body>\r\n");
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
}
