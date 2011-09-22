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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<html>
  <head>
   <title>Password Reset Request</title>
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
	  <td valign=top width=112 height="0" colspan="1" rowspan="1">
<%@ include file="quicklinks_1.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	      <h3>Please enter username and email address</h3>
            <p>
            <font color="red">
              ${fn:escapeXml(param.msg)}
            </font>
            </p>
            <form action="resetpassword.jsp" method="post">
              <input type="hidden" name="origURL" value="${fn:escapeXml(param.origURL)}">                  
              <table border="0" cellspacing = "5">
                <tr>
                  <th align="right">User Name:</th>
                  <th align="left"><input type="text" name="userName" ></th>
                </tr>
                <tr>
                  <th align="right">Email Address:</th>
                  <th align="left"><input type="text" name="emailAddress" ></th>
                </tr>
                <tr>
                  <th align="right"><input type="Submit" value="Enter"></th>
                  <th align="left"><input type="Reset"></th>
                </tr>
              </table>
            </form>
            <p>
              After the "Enter" button is clicked, your SPK account password will be reset and sent to you by email.  
              If you need further assistance, please contact <a href="mailto:rfpksoft@u.washington.edu">us</a>.
            </p>
	  </td>
	</tr>
      </tbody>
    </table>
  </body>
</html>
