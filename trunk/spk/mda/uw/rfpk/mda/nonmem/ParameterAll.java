/**********************************************************************
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
**********************************************************************/
package uw.rfpk.mda.nonmem;

import javax.xml.parsers.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;
import java.io.*;
import javax.swing.JOptionPane;
import java.text.DecimalFormat;

/**
 * This class handels the parameter estimates in all the sub-problems of a job.
 * @author  Jiaji Du
 */
public class ParameterAll {
    
    /** This method takes parameter sections in all the sub-problem's reports of a job to 
     * generate a integrated report.
     * @param reports all the sub-problem's reports of a job.
     * @param first the index of the first String object in the reports to be used.
     * @return a String containing the integrated report. 
     */
    protected static String integrateReports(String[] reports, int first)
    {
        String report = "<?xml version=\"1.0\"?>\n<report_parameter_all>";
        String theta, omega, sigma;
        int beginIndex, endIndex;
        for(int i = first; i < reports.length; i++)
        {
            // Find Theta.
            beginIndex = reports[i].indexOf("<theta_out ") - 1;
            endIndex = reports[i].indexOf("</theta_out>") + 12;
            theta = reports[i].substring(beginIndex, endIndex);
            // Find Omega.
            omega = "";
            endIndex = 0;
            while(true)
            {
                beginIndex = reports[i].indexOf("<omega_out ", endIndex) - 1;
                if(beginIndex == -2)
                    break;
                endIndex = reports[i].indexOf("</omega_out>", beginIndex) + 12;
                omega += reports[i].substring(beginIndex, endIndex);
            }
            // Find sigma.
            sigma = "";
            endIndex = 0;
            while(true)
            {
                beginIndex = reports[i].indexOf("<sigma_out ", endIndex) - 1;
                if(beginIndex == -2)
                    break;
                endIndex = reports[i].indexOf("</sigma_out>", beginIndex) + 12;
                sigma += reports[i].substring(beginIndex, endIndex);
            }

            // Add to report.
            report += "\n<sub_problem>" + theta + omega + sigma + 
                      "\n</sub_problem>"; 
        }
        return report += "\n</report_parameter_all>\n";
    }
    
    /** This method parses a integrated report to generate a String containg a data block
     * of the parameter estimates.
     * @param report a integrated report of a job.
     * @return a String containing a data block of the parameter estimates.
     */
    protected static String getParameterAll(String report)
    {
        Document docDataAll;
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docDataAll = builder.parse(new InputSource(new ByteArrayInputStream(report.getBytes())));
        }
        catch(ParserConfigurationException e)
        {
            JOptionPane.showMessageDialog(null, e, "ParserConfigurationException", JOptionPane.ERROR_MESSAGE);
            return null;
        }
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e, "SAXException", JOptionPane.ERROR_MESSAGE);
            return null;
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            return null;
        }        
        Element parameter_all = docDataAll.getDocumentElement();
        String label,id;
        String header = "Sub-ID";
        String value = "     1";
        String struct = "diagonal";
        int row, col;
        Element problem, parameter;
        NodeList paramList, valueList;
        NodeList problemList = parameter_all.getElementsByTagName("sub_problem");
        DecimalFormat f = new DecimalFormat("0.0000E00");
        for(int i = 0; i < problemList.getLength(); i++)
        {
            problem = (Element)problemList.item(i);
            paramList = problem.getElementsByTagName("theta_out");
            parameter = (Element)paramList.item(0);
            valueList = parameter.getElementsByTagName("value");
            for(int k = 0; k < valueList.getLength(); k++)
            {
                if(i == 0)
                {
                    label = "TH-" + (k + 1);
                    header += getSpace(12 - label.length()) + label;
                }
                value += " " + Utility.formatData(8, f.format(Double.parseDouble(valueList.item(k)
                         .getFirstChild().getNodeValue())));
            }
            paramList = problem.getElementsByTagName("omega_out");
            for(int j = 0; j < paramList.getLength(); j++)
            {
                parameter = (Element)paramList.item(j);
                struct = parameter.getAttribute("struct");
                valueList = parameter.getElementsByTagName("value");
                row = 1;
                col = 1;
                for(int k = 0; k < valueList.getLength(); k++)
                {
                    if(i == 0) 
                    {
                        if(struct.equals("block"))
                        {
                            label = "OM[" + j + "]-" + row + "," + col;
                            if(col == row)
                            {
                                row++;
                                col = 1;
                            }
                            else
                                col++;
                        }
                        else
                            label = "OM[" + j + "]-" + (k + 1) + "," + (k + 1);
                        header += getSpace(12 - label.length()) + label;
                    }
                    value += " " + Utility.formatData(8, f.format(Double.parseDouble(valueList.item(k)
                             .getFirstChild().getNodeValue())));
                }
            }
            paramList = problem.getElementsByTagName("sigma_out");
            for(int j = 0; j < paramList.getLength(); j++)
            {
                parameter = (Element)paramList.item(j);
                struct = parameter.getAttribute("struct");
                valueList = parameter.getElementsByTagName("value");
                row = 1;
                col = 1;                
                for(int k = 0; k < valueList.getLength(); k++)
                {
                    if(i == 0) 
                    {
                        if(struct.equals("block"))
                        {
                            label = "SG[" + j + "]-" + row + "," + col;
                            if(col == row)
                            {
                                row++;
                                col = 1;
                            }
                            else
                                col++;
                        }
                        else
                            label = "SG[" + j + "]-" + (k + 1) + "," + (k + 1);
                        header += getSpace(12 - label.length()) + label;
                    }
                    value += " " + Utility.formatData(8, f.format(Double.parseDouble(valueList.item(k)
                             .getFirstChild().getNodeValue())));
                }
            }            
            if(i < problemList.getLength() - 1)
            {
                id = String.valueOf(i + 2);
                value += "\n" + getSpace(6 - id.length()) + id;
            }
        }
        return header + "\n" + value;
    }

    // This method returns spaces.
    private static String getSpace(int n)
    {
        StringBuffer sb = new StringBuffer();
        for(int i = 0; i < n; i++)
            sb.append(" ");
        return sb.toString();  
    }
}
