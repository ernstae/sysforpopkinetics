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
   <title>Instructions for Installing R</title>
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
	    <h3>Instructions for Linking SPK to R for display and analysis</h3>
	    <p><font color="red"> 
              Note:  The functionality for display and analysis of SPK output using R is currently under construction.
            </font></p><p>
              If R is not already installed on your computer, you can download it <a href="http://www.r-project.org/">here</a>.
            </p><p>
              For this option to work, you must manually set the environment variable "Path" to the bin subdirectory of the R installation directory.  If you are using Windows and have installed R in "C:\Program Files", you will need to set the Path to "C:\Program Files\R\rw2010\bin" (where 2010 represents the installed version 2.1.0 of R).  To do this:<br>
- Open the Control Panel.<br>
- Open the System component.<br>
- Select the Advanced tab.<br>
- Click the "Environment Variables" button.<br>
- In the System Variables window, scroll down and select "Path" and click "Edit."<br>
- Add "C:\Program Files\R\rw2010\bin" to the Variable Value (use semicolon to separate from other Path definitions).
- Click OK to close each window.
            </p><p>
The display and analysis capabilities in R can now be accessed via the MDA Plot menu.              
            </p>
	  </td>
	</tr>
      </tbody>
    </table>
</body>
</html>
