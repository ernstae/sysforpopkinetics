package org.apache.jsp;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class updateuser_jsp extends org.apache.jasper.runtime.HttpJspBase
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
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_choose;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_when_test;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_sql_query_var;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_sql_param_value;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_set_var_value;
  private org.apache.jasper.runtime.TagHandlerPool _jspx_tagPool_c_otherwise;

  public java.util.List getDependants() {
    return _jspx_dependants;
  }

  public void _jspInit() {
    _jspx_tagPool_c_if_test = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_choose = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_when_test = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_sql_query_var = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_sql_param_value = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_set_var_value = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
    _jspx_tagPool_c_otherwise = org.apache.jasper.runtime.TagHandlerPool.getTagHandlerPool(getServletConfig());
  }

  public void _jspDestroy() {
    _jspx_tagPool_c_if_test.release();
    _jspx_tagPool_c_choose.release();
    _jspx_tagPool_c_when_test.release();
    _jspx_tagPool_sql_query_var.release();
    _jspx_tagPool_sql_param_value.release();
    _jspx_tagPool_c_set_var_value.release();
    _jspx_tagPool_c_otherwise.release();
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
      if (_jspx_meth_c_if_0(pageContext))
        return;
      out.write("\n");
      out.write("\n");
      out.write("<html>\n");
      out.write("  <head>\n");
      out.write("    <title>User Account</title>\n");
      out.write("    <link href=stylesheet.css type=\"text/css\" rel=stylesheet>\n");
      out.write("  </head>\n");
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
      out.write("<font size=3>\n");
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
      out.write("</font>\n");
      out.write("  \n");
      out.write("\t  </td>\n");
      out.write("\t  <td colspan=1 vAlign=top width=10><img alt=\"trans gif\" height=5 src=\"./images/white.gif\" width=10/>\n");
      out.write("\t  <td colspan=1 vAlign=top>\n");
      out.write("\t    <h3>Updating User Account</h3>\n");
      out.write("            <p>\n");
      out.write("              Please enter information about an existing user below and click the submit button to update the user account.\n");
      out.write("              <form action=\"validate.jsp?task=update\" method=\"post\">\n");
      out.write("              <table>\n");
      out.write("            ");
      if (_jspx_meth_c_choose_0(pageContext))
        return;
      out.write("\n");
      out.write("                <tr>\n");
      out.write("                  <th align=\"left\"><input type=\"submit\" value=\"Submit\"></th>\n");
      out.write("                </tr>\n");
      out.write("              </table>\n");
      out.write("            </form>                   \n");
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
    _jspx_th_c_if_0.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${validUser == null || validUser.userName != 'useradmin'}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_0 = _jspx_th_c_if_0.doStartTag();
    if (_jspx_eval_c_if_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write('\n');
        out.write(' ');
        out.write(' ');
        if (true) {
          pageContext.forward("index.jsp" + (("index.jsp").indexOf('?')>0? '&': '?') + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("origURL", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${pageContext.request.requestURL}", java.lang.String.class, (PageContext)pageContext, null, false), request.getCharacterEncoding()) + "&" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("errorMsg", request.getCharacterEncoding())+ "=" + org.apache.jasper.runtime.JspRuntimeLibrary.URLEncode("Please log in first.", request.getCharacterEncoding()));
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

  private boolean _jspx_meth_c_choose_0(PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:choose
    org.apache.taglibs.standard.tag.common.core.ChooseTag _jspx_th_c_choose_0 = (org.apache.taglibs.standard.tag.common.core.ChooseTag) _jspx_tagPool_c_choose.get(org.apache.taglibs.standard.tag.common.core.ChooseTag.class);
    _jspx_th_c_choose_0.setPageContext(pageContext);
    _jspx_th_c_choose_0.setParent(null);
    int _jspx_eval_c_choose_0 = _jspx_th_c_choose_0.doStartTag();
    if (_jspx_eval_c_choose_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("              ");
        if (_jspx_meth_c_when_0(_jspx_th_c_choose_0, pageContext))
          return true;
        out.write("\n");
        out.write("              ");
        if (_jspx_meth_c_otherwise_0(_jspx_th_c_choose_0, pageContext))
          return true;
        out.write("\n");
        out.write("            ");
        int evalDoAfterBody = _jspx_th_c_choose_0.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_choose_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_choose.reuse(_jspx_th_c_choose_0);
    return false;
  }

  private boolean _jspx_meth_c_when_0(javax.servlet.jsp.tagext.JspTag _jspx_th_c_choose_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:when
    org.apache.taglibs.standard.tag.rt.core.WhenTag _jspx_th_c_when_0 = (org.apache.taglibs.standard.tag.rt.core.WhenTag) _jspx_tagPool_c_when_test.get(org.apache.taglibs.standard.tag.rt.core.WhenTag.class);
    _jspx_th_c_when_0.setPageContext(pageContext);
    _jspx_th_c_when_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_choose_0);
    _jspx_th_c_when_0.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.userId != '0'}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_when_0 = _jspx_th_c_when_0.doStartTag();
    if (_jspx_eval_c_when_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                ");
        if (_jspx_meth_sql_query_0(_jspx_th_c_when_0, pageContext))
          return true;
        out.write("\n");
        out.write("                ");
        if (_jspx_meth_c_set_0(_jspx_th_c_when_0, pageContext))
          return true;
        out.write("\n");
        out.write("                <tr>\n");
        out.write("                  <td>Username:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"userName\" readonly value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.username)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Password:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"password\" ></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>First Name:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"firstName\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.first_name)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Last Name:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"lastName\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.surname)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Company:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"company\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.company)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>               \n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>State:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"state\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.state)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Country:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"country\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.country)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Email:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"email\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.email)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Tester?:</td>\n");
        out.write("                  <td><input type=\"checkbox\" name=\"tester\"\n");
        out.write("                    ");
        if (_jspx_meth_c_if_1(_jspx_th_c_when_0, pageContext))
          return true;
        out.write("\n");
        out.write("                    value=\"1\">\n");
        out.write("                  </td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Developer?:</td>\n");
        out.write("                  <td><input type=\"checkbox\" name=\"developer\"\n");
        out.write("                    ");
        if (_jspx_meth_c_if_2(_jspx_th_c_when_0, pageContext))
          return true;
        out.write("\n");
        out.write("                    value=\"1\">\n");
        out.write("                  </td>\n");
        out.write("                </tr>\n");
        out.write("              ");
        int evalDoAfterBody = _jspx_th_c_when_0.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_when_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_when_test.reuse(_jspx_th_c_when_0);
    return false;
  }

  private boolean _jspx_meth_sql_query_0(javax.servlet.jsp.tagext.JspTag _jspx_th_c_when_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  sql:query
    org.apache.taglibs.standard.tag.rt.sql.QueryTag _jspx_th_sql_query_0 = (org.apache.taglibs.standard.tag.rt.sql.QueryTag) _jspx_tagPool_sql_query_var.get(org.apache.taglibs.standard.tag.rt.sql.QueryTag.class);
    _jspx_th_sql_query_0.setPageContext(pageContext);
    _jspx_th_sql_query_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_when_0);
    _jspx_th_sql_query_0.setVar("user");
    int[] _jspx_push_body_count_sql_query_0 = new int[] { 0 };
    try {
      int _jspx_eval_sql_query_0 = _jspx_th_sql_query_0.doStartTag();
      if (_jspx_eval_sql_query_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
        if (_jspx_eval_sql_query_0 != javax.servlet.jsp.tagext.Tag.EVAL_BODY_INCLUDE) {
          out = pageContext.pushBody();
          _jspx_push_body_count_sql_query_0[0]++;
          _jspx_th_sql_query_0.setBodyContent((javax.servlet.jsp.tagext.BodyContent) out);
          _jspx_th_sql_query_0.doInitBody();
        }
        do {
          out.write("\n");
          out.write("                  SELECT * FROM user WHERE user_id = ?\n");
          out.write("                  ");
          if (_jspx_meth_sql_param_0(_jspx_th_sql_query_0, pageContext, _jspx_push_body_count_sql_query_0))
            return true;
          out.write("\n");
          out.write("                ");
          int evalDoAfterBody = _jspx_th_sql_query_0.doAfterBody();
          if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
            break;
        } while (true);
        if (_jspx_eval_sql_query_0 != javax.servlet.jsp.tagext.Tag.EVAL_BODY_INCLUDE)
          out = pageContext.popBody();
          _jspx_push_body_count_sql_query_0[0]--;
      }
      if (_jspx_th_sql_query_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
        return true;
    } catch (Throwable _jspx_exception) {
      while (_jspx_push_body_count_sql_query_0[0]-- > 0)
        out = pageContext.popBody();
      _jspx_th_sql_query_0.doCatch(_jspx_exception);
    } finally {
      _jspx_th_sql_query_0.doFinally();
      _jspx_tagPool_sql_query_var.reuse(_jspx_th_sql_query_0);
    }
    return false;
  }

  private boolean _jspx_meth_sql_param_0(javax.servlet.jsp.tagext.JspTag _jspx_th_sql_query_0, PageContext pageContext, int[] _jspx_push_body_count_sql_query_0)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  sql:param
    org.apache.taglibs.standard.tag.rt.sql.ParamTag _jspx_th_sql_param_0 = (org.apache.taglibs.standard.tag.rt.sql.ParamTag) _jspx_tagPool_sql_param_value.get(org.apache.taglibs.standard.tag.rt.sql.ParamTag.class);
    _jspx_th_sql_param_0.setPageContext(pageContext);
    _jspx_th_sql_param_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_sql_query_0);
    _jspx_th_sql_param_0.setValue((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.userId}", java.lang.Object.class, (PageContext)pageContext, null, false));
    int _jspx_eval_sql_param_0 = _jspx_th_sql_param_0.doStartTag();
    if (_jspx_th_sql_param_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_sql_param_value.reuse(_jspx_th_sql_param_0);
    return false;
  }

  private boolean _jspx_meth_c_set_0(javax.servlet.jsp.tagext.JspTag _jspx_th_c_when_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:set
    org.apache.taglibs.standard.tag.rt.core.SetTag _jspx_th_c_set_0 = (org.apache.taglibs.standard.tag.rt.core.SetTag) _jspx_tagPool_c_set_var_value.get(org.apache.taglibs.standard.tag.rt.core.SetTag.class);
    _jspx_th_c_set_0.setPageContext(pageContext);
    _jspx_th_c_set_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_when_0);
    _jspx_th_c_set_0.setVar("dbValues");
    _jspx_th_c_set_0.setValue((java.lang.Object) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${user.rows[0]}", java.lang.Object.class, (PageContext)pageContext, null, false));
    int _jspx_eval_c_set_0 = _jspx_th_c_set_0.doStartTag();
    if (_jspx_th_c_set_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_set_var_value.reuse(_jspx_th_c_set_0);
    return false;
  }

  private boolean _jspx_meth_c_if_1(javax.servlet.jsp.tagext.JspTag _jspx_th_c_when_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_1 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_1.setPageContext(pageContext);
    _jspx_th_c_if_1.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_when_0);
    _jspx_th_c_if_1.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${dbValues.test == 1}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_1 = _jspx_th_c_if_1.doStartTag();
    if (_jspx_eval_c_if_1 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                      checked\n");
        out.write("                    ");
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

  private boolean _jspx_meth_c_if_2(javax.servlet.jsp.tagext.JspTag _jspx_th_c_when_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_2 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_2.setPageContext(pageContext);
    _jspx_th_c_if_2.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_when_0);
    _jspx_th_c_if_2.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${dbValues.dev == 1}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_2 = _jspx_th_c_if_2.doStartTag();
    if (_jspx_eval_c_if_2 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                      checked\n");
        out.write("                    ");
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

  private boolean _jspx_meth_c_otherwise_0(javax.servlet.jsp.tagext.JspTag _jspx_th_c_choose_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:otherwise
    org.apache.taglibs.standard.tag.common.core.OtherwiseTag _jspx_th_c_otherwise_0 = (org.apache.taglibs.standard.tag.common.core.OtherwiseTag) _jspx_tagPool_c_otherwise.get(org.apache.taglibs.standard.tag.common.core.OtherwiseTag.class);
    _jspx_th_c_otherwise_0.setPageContext(pageContext);
    _jspx_th_c_otherwise_0.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_choose_0);
    int _jspx_eval_c_otherwise_0 = _jspx_th_c_otherwise_0.doStartTag();
    if (_jspx_eval_c_otherwise_0 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                <tr>\n");
        out.write("                  <td>Username:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"userName\" readonly value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(dbValues.username)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Password:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"password\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.password)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(passwordError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>First Name:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"firstName\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.firstName)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(firstNameError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Last Name:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"lastName\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.lastName)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(lastNameError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Company:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"company\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.company)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(companyNameError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>State:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"state\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.state)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(stateNameError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Country:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"country\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.country)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(countryNameError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Email:</td>\n");
        out.write("                  <td><input type=\"text\" name=\"email\" value=\"");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(param.email)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("\"></td>\n");
        out.write("                  <td><font color=\"red\">");
        out.write((java.lang.String) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${fn:escapeXml(emailAddressError)}", java.lang.String.class, (PageContext)pageContext, _jspx_fnmap_0, false));
        out.write("</font></td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Tester?:</td>\n");
        out.write("                  <td><input type=\"checkbox\" name=\"tester\"\n");
        out.write("                    ");
        if (_jspx_meth_c_if_3(_jspx_th_c_otherwise_0, pageContext))
          return true;
        out.write("\n");
        out.write("                    value=\"1\">\n");
        out.write("                  </td>\n");
        out.write("                </tr>\n");
        out.write("                <tr>\n");
        out.write("                  <td>Developer?:</td>\n");
        out.write("                  <td><input type=\"checkbox\" name=\"developer\" \n");
        out.write("                    ");
        if (_jspx_meth_c_if_4(_jspx_th_c_otherwise_0, pageContext))
          return true;
        out.write("\n");
        out.write("                    value=\"1\">\n");
        out.write("                  </td>\n");
        out.write("                </tr>                                        \n");
        out.write("              ");
        int evalDoAfterBody = _jspx_th_c_otherwise_0.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_otherwise_0.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_otherwise.reuse(_jspx_th_c_otherwise_0);
    return false;
  }

  private boolean _jspx_meth_c_if_3(javax.servlet.jsp.tagext.JspTag _jspx_th_c_otherwise_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_3 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_3.setPageContext(pageContext);
    _jspx_th_c_if_3.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_otherwise_0);
    _jspx_th_c_if_3.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.tester == '1'}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_3 = _jspx_th_c_if_3.doStartTag();
    if (_jspx_eval_c_if_3 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                      checked\n");
        out.write("                    ");
        int evalDoAfterBody = _jspx_th_c_if_3.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_if_3.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_3);
    return false;
  }

  private boolean _jspx_meth_c_if_4(javax.servlet.jsp.tagext.JspTag _jspx_th_c_otherwise_0, PageContext pageContext)
          throws Throwable {
    JspWriter out = pageContext.getOut();
    //  c:if
    org.apache.taglibs.standard.tag.rt.core.IfTag _jspx_th_c_if_4 = (org.apache.taglibs.standard.tag.rt.core.IfTag) _jspx_tagPool_c_if_test.get(org.apache.taglibs.standard.tag.rt.core.IfTag.class);
    _jspx_th_c_if_4.setPageContext(pageContext);
    _jspx_th_c_if_4.setParent((javax.servlet.jsp.tagext.Tag) _jspx_th_c_otherwise_0);
    _jspx_th_c_if_4.setTest(((java.lang.Boolean) org.apache.jasper.runtime.PageContextImpl.proprietaryEvaluate("${param.developer == '1'}", java.lang.Boolean.class, (PageContext)pageContext, null, false)).booleanValue());
    int _jspx_eval_c_if_4 = _jspx_th_c_if_4.doStartTag();
    if (_jspx_eval_c_if_4 != javax.servlet.jsp.tagext.Tag.SKIP_BODY) {
      do {
        out.write("\n");
        out.write("                      checked\n");
        out.write("                    ");
        int evalDoAfterBody = _jspx_th_c_if_4.doAfterBody();
        if (evalDoAfterBody != javax.servlet.jsp.tagext.BodyTag.EVAL_BODY_AGAIN)
          break;
      } while (true);
    }
    if (_jspx_th_c_if_4.doEndTag() == javax.servlet.jsp.tagext.Tag.SKIP_PAGE)
      return true;
    _jspx_tagPool_c_if_test.reuse(_jspx_th_c_if_4);
    return false;
  }
}
