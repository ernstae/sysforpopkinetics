<html>
  <head>
    <title>Instructions for Using Model Design Agent</title>
  </head>
  <body>
    <h2>Instructions for Using Model Design Agent</h2>
    <p>
       <% String url = getServletContext().getInitParameter("bugzillaURL");
          String bugzilla = "If you find any errors in the Model Design Agent please <a href=" + url + ">submit a bug report</a>."; %>
       <%=bugzilla%>
    </p>
    <p>
       Function of the buttons:
    </p>
    <table border=1 cellspacing=2>
      <th width=125>Button Name</th>
      <th width=500>Function</th>
      <tr>
        <td>Prepare Input</td>  
        <td>Starts a tool to help the user to create a NONMEM control file 
            and a SPK input file.  (SPK: System for Population Kinetics)</td>
      </tr>
      <tr>
        <td>Submit Job</td>      
        <td>Submits a job to SPK server.  Before clicking this button a SPK
            input file should be opened in the editor window.</td>
      </tr>  
      <tr>
        <td>My Jobs</td>      
        <td>Displays user's job list in the SPK server.  If a job is selected 
            a dialog box is displayed showing the model and dataset used by
            the job and by clicking a button the model, the dataset or the SPK 
            job output is returned and displayed in the MDA editor window.</td> 
      </tr>
      <tr>
        <td>Process Output</td>
        <td>Processes the SPK output file shown in the editor window, allows 
            user to save data and tables that was returned from the server.</td>
      </tr>
      <tr>
        <td>My Models</td>
        <td>Displays user's model list in the SPK server, gets and displays
            user selected model version in the editor window.</td>
      </tr>
      <tr>
        <td>My Datasets</td>
        <td>Displays user's dataset list in the SPK server, gets and displays
            user selected dataset version in the editor window.</td>
      </tr>
      <tr>
        <td>Job Examples</td>
        <td>Displays an example job list in the SPK server, gets and displays 
            user selected job's SPK output file in the editor window.</td>
      </tr>
      <tr>
        <td>Model Library</td>
        <td>Displays model list of model library in the SPK server, gets and 
            displays user selected model version in the editor window.</td>
      </tr>
      <tr>
        <td>Dataset Library</td>
        <td>Displays dataset list of dataset library in the SPK server, gets 
            and displays user selected dataset version in the editor window.</td>
      </tr>
      <tr>
        <td>Compare Files</td>
        <td>Compares two text files.  Any of the files may be either on the 
            SPK server as model or data archives or on the user's computer.</td>
      </tr>
      <tr>
        <td>Help</td>
        <td>Displays this document.</td>
      </tr>
    </table>
    <p>
      When the model design agent is working off-line, only Prepare Input button, 
      Prosess Output button and Help button are enabled.
    </p>
    <p>
      Function of the menus:
    </p>
    <table border=1 cellspacing=2>
      <th width=125>Menu Name</th>
      <th width=500>Function</th>
      <tr>
        <td>File</td>
        <td>Performs file opening, closing, saving, saving as and printing.
            Stops the model design agent.</td>
      </tr>
      <tr>
        <td>Edit</td>
        <td>Performs cutting, copying and pasting text and finding strings in
            the document.</td>
      </tr>
      <tr>
        <td>Presentation</td>
        <td>Presents computation results including error massage, the mimimum 
            value of the objectve function, the parameter estimates, the
            statistics of parameter estimates, the tables as specified by the 
            user in the NONMEM control file, the scatterplots as specified by 
            the user in the NONMEM control file and a summary report.  The 
            user may use the File menu to print out the summary report.  The
            scatterplots are also printable.  To print out a scatterplot, 
            click the right mouse button then select portrait or landscape.</td>
      </tr>
    </table>
  </body>
</html>
