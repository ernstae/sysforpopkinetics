<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "xhtml1-transitional.dtd">
<%@page contentType="text/html"%>
<html>
<head>
   <title>Instructions for Installing JRE</title>
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
	    <h3>Instructions for Installing JRE</h3>
	    <p> 
              1.  To use the MDA, you need first to download and install the Java
                  Runtime Environment (JRE) from Sun Microsystems. Access the following
                  website:<br>
                  <a href="http://java.sun.com/j2se/1.4.2/download.html">http://java.sun.com/j2se/1.4.2/download.html</a>.
            </p><p>
              2.  Scroll down to "Download J2SE" and choose your operating system from
                  the "JRE" column.
            </p><p>
              3.  Accept the License Agreement (by scrolling down and clicking on
                  "Accept").
            </p><p>
              4.  You should see "Java(TM) 2 Runtime Environment, Standard
                  Edition", and then the version number.
            </p><p>
              5.  Under the "Linux Platform" choose the RPM version.  When the downloading 
                  is complete, bring up a terminal window and enter:<br>
                  chmod a+x j2re-1_4_2_04-linux-i586-rpm.bin<br>
                  ./j2re-1_4_2_01-linux-i586-rpm.bin<br>
                  Then, becoming root, enter:<br>
                  rpm -iv j2re-1_4_2_04-linux-i586.rpm<br>
                  Delete the bin and rpm file if you want to save disk space.
            </p><p>
              6.  FOR MOZILLA OR NETSCAPE USERS: If you use Mozilla or Netscape as your
                  web browser, you must set the helper application to be Java Web Start.<br>
                  Select Edit->Preferences->Navigator->Helper Applications->New Type.
                  Enter FileExtension as "jnlp"<br>
                  Enter MIME type as "Application/x-java-jnlp-file"<br>
                  Enter Handled by Application as<br>
                  "/usr/java/j2re1.4.2_04/javaws/javaws" for Linux or
            </p><p>
                  Note that the version number may be different in the future.
	    </p>
	  </td>
	</tr>
      </tbody>
    </table>
</body>
</html>
