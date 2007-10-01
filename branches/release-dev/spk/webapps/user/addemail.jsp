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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="ora" uri="orataglib" %>
<%@ page import="java.io.BufferedWriter" %>
<%@ page import="java.io.FileWriter" %>
<%@ page import="java.io.IOException" %>

<ora:ifValidEmailAddr value="${param.email}" var="isValidEmailAddr" />
<c:choose>
  <c:when test="${empty param.email || !isValidEmailAddr}">
    The email address is missing or invalid.  Please <a href="index.jsp">try again</a>.
  </c:when>
  <c:otherwise>
<%
    String pathName = getServletContext().getInitParameter("jnlp_directory") + "/notifyusers.txt";
    try 
    {
        BufferedWriter o = new BufferedWriter(new FileWriter(pathName, true));
        o.write(request.getParameter("email") + "\n");
        o.close();
    } 
    catch (IOException e) 
    {
    }
%>
    We will inform you by email when the SPK service is available.
    Thank you for visiting SPK web site.
  </c:otherwise>
</c:choose>
