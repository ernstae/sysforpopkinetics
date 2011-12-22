package org.apache.jsp.help;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.jsp.*;

public final class MDAHelp_jsp extends org.apache.jasper.runtime.HttpJspBase
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

      out.write("<html>\n");
      out.write("  <head>\n");
      out.write("    <title>Instructions for Using Model Design Agent</title>\n");
      out.write("  </head>\n");
      out.write("  <body>\n");
      out.write("    <h2>Instructions for Using Model Design Agent</h2>\n");
      out.write("    <p>\n");
      out.write("       ");
 String url = getServletContext().getInitParameter("bugzillaURL");
          String bugzilla = "If you find any errors in the Model Design Agent please <a href=" + url + ">submit a bug report</a>."; 
      out.write("\n");
      out.write("       ");
      out.print(bugzilla);
      out.write("\n");
      out.write("    </p>\n");
      out.write("    <p>\n");
      out.write("       Function of the buttons:\n");
      out.write("    </p>\n");
      out.write("    <table border=1 cellspacing=2>\n");
      out.write("      <th width=125>Button Name</th>\n");
      out.write("      <th width=500>Function</th>\n");
      out.write("      <tr>\n");
      out.write("        <td>Prepare Input</td>  \n");
      out.write("        <td>Starts a tool to help the user to create a NONMEM control file \n");
      out.write("            and a SPK input file.  (SPK: System for Population Kinetics)</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Submit Job</td>      \n");
      out.write("        <td>Submits a job to SPK server.  Before clicking this button a SPK\n");
      out.write("            input file should be opened in the editor window.</td>\n");
      out.write("      </tr>  \n");
      out.write("      <tr>\n");
      out.write("        <td>My Jobs</td>      \n");
      out.write("        <td>Displays user's job list in the SPK server.  If a job is selected \n");
      out.write("            a dialog box is displayed showing the model and dataset used by\n");
      out.write("            the job and by clicking a button the model, the dataset or the SPK \n");
      out.write("            job output is returned and displayed in the MDA editor window.</td> \n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Process Output</td>\n");
      out.write("        <td>Processes the SPK output file shown in the editor window, allows \n");
      out.write("            user to save data and tables that was returned from the server.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>My Models</td>\n");
      out.write("        <td>Displays user's model list in the SPK server, gets and displays\n");
      out.write("            user selected model version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>My Datasets</td>\n");
      out.write("        <td>Displays user's dataset list in the SPK server, gets and displays\n");
      out.write("            user selected dataset version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Job Examples</td>\n");
      out.write("        <td>Displays an example job list in the SPK server, gets and displays \n");
      out.write("            user selected job's SPK output file in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Model Library</td>\n");
      out.write("        <td>Displays model list of model library in the SPK server, gets and \n");
      out.write("            displays user selected model version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Dataset Library</td>\n");
      out.write("        <td>Displays dataset list of dataset library in the SPK server, gets \n");
      out.write("            and displays user selected dataset version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Compare Files</td>\n");
      out.write("        <td>Compares two text files.  Any of the files may be either on the \n");
      out.write("            SPK server as model or data archives or on the user's computer.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Help</td>\n");
      out.write("        <td>Displays this document.</td>\n");
      out.write("      </tr>\n");
      out.write("    </table>\n");
      out.write("    <p>\n");
      out.write("      When the model design agent is working off-line, only Prepare Input button, \n");
      out.write("      Prosess Output button and Help button are enabled.\n");
      out.write("    </p>\n");
      out.write("    <p>\n");
      out.write("      Function of the menus:\n");
      out.write("    </p>\n");
      out.write("    <table border=1 cellspacing=2>\n");
      out.write("      <th width=125>Menu Name</th>\n");
      out.write("      <th width=500>Function</th>\n");
      out.write("      <tr>\n");
      out.write("        <td>File</td>\n");
      out.write("        <td>Performs file opening, closing, saving, saving as and printing.\n");
      out.write("            Stops the model design agent.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Edit</td>\n");
      out.write("        <td>Performs cutting, copying and pasting text and finding strings in\n");
      out.write("            the document.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Presentation</td>\n");
      out.write("        <td>Presents computation results including error massage, the mimimum \n");
      out.write("            value of the objectve function, the parameter estimates, the\n");
      out.write("            statistics of parameter estimates, the tables as specified by the \n");
      out.write("            user in the NONMEM control file, the scatterplots as specified by \n");
      out.write("            the user in the NONMEM control file and a summary report.  The \n");
      out.write("            user may use the File menu to print out the summary report.  The\n");
      out.write("            scatterplots are also printable.  To print out a scatterplot, \n");
      out.write("            click the right mouse button then select portrait or landscape.</td>\n");
      out.write("      </tr>\n");
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
}
