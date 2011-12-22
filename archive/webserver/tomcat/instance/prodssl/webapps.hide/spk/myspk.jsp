<%@page contentType="text/html"%>
<html>
<head>
   <title>MySpk</title>
    <link href=stylesheet.css type="text/css" rel=stylesheet>
</head>
<body>

<%-- <jsp:useBean id="beanInstanceName" scope="session" class="package.class" /> --%>
<%-- <jsp:getProperty name="beanInstanceName"  property="propertyName" /> --%>
<table align=left border=0 width=602>
      <tbody>
	<tr> 
	  <td colSpan=3 vAlign=top>
	    <img align=top alt="RFPK logo" height=40 src="./Images/rfpklogo.gif" width=112>
	      <img align=top alt="Resource Facility for Population Kinetics" height=40 
		src="./Images/rfpkfull.gif" width=474>
	  </td>
	</tr> 
	<tr vAlign=top> <td colSpan=3><p>&nbsp;</p></td></tr> 
	<tr>
	  <td vAlign=top width=102 height="0" colspan="1" rowspan="1">
<%@ include file="quicklinks.shtml" %>  
	  </td>
	  <td colspan=1 vAlign=top width=10><img alt="trans gif" height=5 src="./Images/white.gif" width=10/>
	  <td colspan=1 vAlign=top>
	    <h1>MySPK -- </h1>
	    <p>
              Welcome to the portion of the Spk web site which is reserved for
              currently authorized users of the system.  
	    </p>
            <p> 
              Please log in.
            </p>
	  </td>
	</tr>
      </tbody>
    </table>
</body>
</html>
