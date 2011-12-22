<!---------------------------------------------------------------------
From:   Resource Facility for Population Kinetics                    
        Department of Bioengineering Box 352255                      
        University of Washington                                     
        Seattle, WA 98195-2255                                       

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
---------------------------------------------------------------------->
<!--
author: Jiaji Du
-->
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@page contentType="text/html"%>
<html>
<head>
   <title>Instructions for Installing JRE on Windows</title>
    <link href=stylesheet.css type="text/css" rel=stylesheet>
</head>
<body>
  <table align=left border=0 width=602>
      <tbody> 
	<tr>    
	  <td colSpan=3 vAlign=top>
	    <img align=top alt="RFPK logo" height=40 src="./images/rfpklogo.gif" width=112>
	      <img align=top alt="Resource Facility for Population Kinetics" height=40 
		src="./images/rfpkfull.gif" width=474>
 	  </td> 
	</tr> 
	<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> 
	<tr>
	  <td vAlign=top width=150 height="0" colspan="1" rowspan="1">
          <%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h3>Instructions for Installing JRE on Windows</h3>
	    <p> 
              1.  To use the MDA, you need the Java Runtime Environment (JRE) from Sun Microsystems. 
                  To download and install the JRE, access the following website:<br>
                  <a href="http://java.sun.com/j2se/1.4.2/download.html" target="_blank">http://java.sun.com/j2se/1.4.2/download.html</a>.
            </p><p>
              2.  Scroll down to "Download J2SE JRE" and click the link.
            </p><p>
              3.  Accept the License Agreement (by scrolling down and clicking on
                  "Accept").
            </p><p>
              4.  You should see "Java(TM) 2 Runtime Environment, Standard
                  Edition", and then the version number.
            </p><p>
              5.  Under the "Windows Platform" choose "Windows Installation...",  
                  choose "Run" when prompted by the File Download window, and choose "Run" when prompted by the Security Warning window.
            </p><p>
              6.  The InstallShield Wizard should start. Accept license aggreement, and choose "typical setup type". Click "Finish" when the installation is complete.  On setup, we recommend that you do not accept the MDA desktop icon.  Currently the MDA will not load from the icon.
            </p><p>
              7.  FOR MOZILLA OR NETSCAPE USERS: If you use Mozilla or Netscape as your
                  web browser, you must set the helper application to be Java Web Start.<br>
                  Select Edit->Preferences->Navigator->Helper Applications->New Type.
                  Enter FileExtension as "jnlp"<br>
                  Enter MIME type as "Application/x-java-jnlp-file"<br>
                  Enter Handled by Application as<br>
                  "C:\Program Files\Java\j2re1.4.2_05\javaws\javaws.exe", then click the "OK" buttons.
            </p><p>
                  Note that the version number may be different in the future.
	    </p>
	  </td>
	</tr>
      </tbody>
    </table>
</body>
</html>
