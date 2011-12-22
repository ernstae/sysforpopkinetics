package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class errorpage_jsp extends org.apache.jasper.runtime.HttpJspBase
    implements org.apache.jasper.runtime.JspSourceDependent {

  private static java.util.Vector _jspx_dependants;

  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_ora_debug_type_nobody;

  public java.util.List getDependants() {
    return _jspx_dependants;
  }

  public void _jspInit() {
    _jspx_tagPool_ora_debug_type_nobody = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
  }

  public void _jspDestroy() {
    _jspx_tagPool_ora_debug_type_nobody.release();
  }

  public void _jspService(HttpServletRequest request, HttpServletResponse response)
        throws java.io.IOException, ServletException {

    JspFactory _jspxFactory = null;
    PageContext pageContext = null;
    HttpSession session = null;
    Throwable exception = org.apache.jasper.runtime.JspRuntimeLibrary.getThrowable(request);
    if (exception != null) {
      response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
    }
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
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");
      out.write("\n");

    String emailAddress = getServletContext().getInitParameter("emailAddress");
    String link = "<a href=mailto:" + emailAddress + ">Let us know</a>";

      out.write("\n");
      out.write("\n");
      out.write("<html>\n");
      out.write("  <head>\n");
      out.write("    <title>Error Page</title>\n");
      out.write("  </head>\n");
      out.write("  <body bgcolor=\"white\">\n");
      out.write("    We're sorry but the request could not be processed. \n");
      out.write("    Detailed information about the error has been logged so we will\n");
      out.write("    analyze it and correct whatever is causing it as soon as possible.\n");
      out.write("    <p>\n");
      out.write("    Please try again, and ");
      out.print(link);
      out.write(" if the problem persists.\n");
      out.write("\n");
      out.write("    ");
      if (_jspx_meth_ora_fileWrite_0(_jspx_page_context))
        return;
      out.write("\n");
      out.write("\n");
      out.write("    ");
      if (_jspx_meth_ora_debug_0(_jspx_page_context))
        return;
      out.write("\n");
      out.write("\n");
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

  private boolean _jspx_meth_ora_fileWrite_0(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  ora:fileWrite
    com.ora.jsp.tags.FileWriteTag _jspx_th_ora_fileWrite_0 = new com.ora.jsp.tags.FileWriteTag();
    _jspx_th_ora_fileWrite_0.setJspContext(_jspx_page_context);
    _jspx_th_ora_fileWrite_0.setFileName("log");
    _jspx_th_ora_fileWrite_0.setJspBody(new errorpage_jspHelper( 0, _jspx_page_context, _jspx_th_ora_fileWrite_0, null));
    _jspx_th_ora_fileWrite_0.doTag();
    return false;
  }

  private boolean _jspx_meth_ora_debug_0(PageContext _jspx_page_context)
          throws Throwable {
    PageContext pageContext = _jspx_page_context;
    JspWriter out = _jspx_page_context.getOut();
    //  ora:debug
    com.ora.jsp.tags.DebugTag _jspx_th_ora_debug_0 = (com.ora.jsp.tags.DebugTag) _jspx_tagPool_ora_debug_type_nobody.get(com.ora.jsp.tags.DebugTag.class);
    _jspx_th_ora_debug_0.setPageContext(_jspx_page_context);
    _jspx_th_ora_debug_0.setParent(null);
    _jspx_th_ora_debug_0.setType("params");
    int _jspx_eval_ora_debug_0 = _jspx_th_ora_debug_0.doStartTag();
    if (_jspx_th_ora_debug_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_ora_debug_type_nobody.reuse(_jspx_th_ora_debug_0);
    return false;
  }

  private class errorpage_jspHelper
      extends org.apache.jasper.runtime.JspFragmentHelper
  {
    private javax.servlet.jsp.tagext.JspTag _jspx_parent;
    private int[] _jspx_push_body_count;

    public errorpage_jspHelper( int discriminator, JspContext jspContext, javax.servlet.jsp.tagext.JspTag _jspx_parent, int[] _jspx_push_body_count ) {
      super( discriminator, jspContext, _jspx_parent );
      this._jspx_parent = _jspx_parent;
      this._jspx_push_body_count = _jspx_push_body_count;
    }
    public boolean invoke0( JspWriter out ) 
      throws Throwable
    {
      out.write("\n");
      out.write("      Error in: ");
      out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.errorData.requestURI}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
      out.write("\n");
      out.write("      Error message: ");
      out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.errorData.throwable.message}", java.lang.String.class, (PageContext)_jspx_page_context, null, false));
      out.write("\n");
      out.write("    ");
      return false;
    }
    public void invoke( java.io.Writer writer )
      throws JspException
    {
      JspWriter out = null;
      if( writer != null ) {
        out = this.jspContext.pushBody(writer);
      } else {
        out = this.jspContext.getOut();
      }
      try {
        switch( this.discriminator ) {
          case 0:
            invoke0( out );
            break;
        }
      }
      catch( Throwable e ) {
        if (e instanceof SkipPageException)
            throw (SkipPageException) e;
        throw new JspException( e );
      }
      finally {
        if( writer != null ) {
          this.jspContext.popBody();
        }
      }
    }
  }
}
