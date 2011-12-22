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
          String bugzilla = "If you find any errors in the Model Design Agent please " +
                            "<a href=" + url + ">submit a bug report</a>."; 
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
      out.write("        <td>Prepare Input</td>\n");
      out.write("        <td>Starts a tool that helps the user to create a model file, which is\n");
      out.write("            compatible to NONMEM as a control file, and a SPK input file in XML\n");
      out.write("            (SPK: System for Population Kinetics).  The tool can also take an\n");
      out.write("            existing model file or SPK input file and helps the user to modify it.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Submit Job</td>\n");
      out.write("        <td>Submits a job to SPK server.  Before clicking this button a SPK\n");
      out.write("            input file should be opened displaying in the editor window.  It\n");
      out.write("            brings up a dialog, Job Submission Dialog.  In this dialog there are\n");
      out.write("            three tabbed panes for \"Model\", Dataset\" and \"Job\", respectively. The\n");
      out.write("            \"Model' and the \"Dataset\" panes allows thw user to specify the model\n");
      out.write("            and the dataset used in for the job, respectively.  In case for using\n");
      out.write("            one of the numerical integration methods, these to panes are ignored\n");
      out.write("            because the parent job's model and dataset must be used and the system\n");
      out.write("            will do it automatically.  The \"Job\" pane allows the user to select\n");
      out.write("            analysis method for the job.  If a likelihood evaluation method (the\n");
      out.write("            radio button text is ended with \"integration on likelihood\") is selected,\n");
      out.write("            the job must have a parent and load the parent job's input file from\n");
      out.write("            the server before cliking the \"Submit Job\" button.\n");
      out.write("            </td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>My Jobs</td>\n");
      out.write("        <td>Displays user's job list in the SPK server.  If a job is selected\n");
      out.write("            a dialog box is displayed showing the model and dataset used by\n");
      out.write("            the job and by clicking a button the model, the dataset or the SPK\n");
      out.write("            job input/output is returned and displayed in the MDA editor window,\n");
      out.write("            or the user may choose to automatically start the SPK input file\n");
      out.write("            preparation tool or the SPK output procesing tool. The user can also\n");
      out.write("            find the job's history, the job's paraent job and the job's\n");
      out.write("            identification number in the dialog.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Process Output</td>\n");
      out.write("        <td>Processes the SPK output file shown in the editor window, parses the\n");
      out.write("            SPK output file in XML and allows the\n");
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
      out.write("        <td>Displays an example job list in the SPK server.  If a job is selected\n");
      out.write("            a dialog box is displayed showing the model and dataset used by\n");
      out.write("            the job and by clicking a button the model, the dataset or the SPK\n");
      out.write("            job input/output is returned and displayed in the MDA editor window,\n");
      out.write("            or the user may choose to automatically start the SPK input file\n");
      out.write("            preparation tool or the SPK output procesing tool. The user can also\n");
      out.write("            find the job's history and the job's paraent job in the dialog.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Model Library</td>\n");
      out.write("        <td>Displays model list of the model library in the SPK server, gets and\n");
      out.write("            displays user selected model version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Dataset Library</td>\n");
      out.write("        <td>Displays dataset list of the dataset library in the SPK server, gets\n");
      out.write("            and displays user selected dataset version in the editor window.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Compare Files</td>\n");
      out.write("        <td>Compares two text files.  Any of the files may be either on the\n");
      out.write("            SPK server as model or data archives or on the user's computer.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Help</td>\n");
      out.write("        <td>Displays this document.</td>\n");
      out.write("      </tr>\n");
      out.write("    </table>\n");
      out.write("    <p>\n");
      out.write("      When the model design agent is working off-line, only Prepare Input button,\n");
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
      out.write("        <td>Presents computation results including error messages, warning messages,\n");
      out.write("            the mimimum value of the objectve function, the parameter estimates, the\n");
      out.write("            statistics of parameter estimates, the tables as specified by the\n");
      out.write("            user in the model (NONMEM control file), the scatterplots as specified by\n");
      out.write("            the user in the model (NONMEM control file), a summary report and\n");
      out.write("            the optimization trace record.  The\n");
      out.write("            user may use the File menu to print out the summary report.  The\n");
      out.write("            scatterplots are also printable.  To print out a scatterplot,\n");
      out.write("            click the right mouse button then select portrait or landscape.</td>\n");
      out.write("      </tr>\n");
      out.write("      <tr>\n");
      out.write("        <td>Plot</td>\n");
      out.write("        <td>The fist 3 menu items plots columns of data returned from the server\n");
      out.write("            as curves on the screen.  The plot is also printable.  To print out a\n");
      out.write("            plot, click the right mouse button then select portrait or landscape.\n");
      out.write("            Before using this menu a SPK data file should be opened in the editor\n");
      out.write("            window. The last menu item starts R for output display and analysis.</td>\n");
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
