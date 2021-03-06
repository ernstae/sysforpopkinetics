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
<%@ page contentType="text/html" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="ora" uri="orataglib" %>
<%@ page isErrorPage="true" %>

<%
    String emailAddress = getServletContext().getInitParameter("emailAddress");
    String link = "<a href=mailto:" + emailAddress + ">Let us know</a>";
%>

<html>
  <head>
    <title>Error Page</title>
  </head>
  <body bgcolor="white">
    We're sorry but the request could not be processed. 
    Detailed information about the error has been logged so we will
    analyze it and correct whatever is causing it as soon as possible.
    <p>
    Please try again, and <%=link%> if the problem persists.

    <ora:fileWrite fileName="log">
      Error in: ${pageContext.errorData.requestURI}
      Error message: ${pageContext.errorData.throwable.message}
    </ora:fileWrite>

    <ora:debug type="params" />

  </body>
</html>
