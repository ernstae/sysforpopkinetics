/*
 * XMLReader.java
 *
 * Created on November 20, 2003, 3:17 PM
 */

package uw.rfpk.mda.nonmem;

import uw.rfpk.mda.nonmem.wizard.Utility;
import uw.rfpk.mda.nonmem.display.Output;
import org.apache.xerces.parsers.DOMParser;
import org.apache.xerces.dom.DocumentImpl;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;
import java.util.ArrayList;
import java.util.Properties;
import java.io.*;
import javax.swing.JOptionPane;

/**
 * This class defines an object that reads two XML documents: Spk source file and Spk report file.
 * @author  jiaji Du
 */
public class XMLReader {
    
    /** Creates a new instance of XMLReader */
    public XMLReader(String text, Output output) 
    {
        this.output = output;
        
        // Get the XML documents as String objects
        int index = text.indexOf("<spksource>");
        String report = text.substring(0, index - 22);
        String source = text.substring(index - 22);  
        try
        {
            // Parse the XML documents
            DOMParser parser = new DOMParser(); 
            parser.parse(new InputSource(new ByteArrayInputStream(report.getBytes()))); 
            docReport = parser.getDocument(); 
            parser.parse(new InputSource(new ByteArrayInputStream(source.getBytes()))); 
            docSource = parser.getDocument();     
        }
        catch(SAXException se)
        {
            System.out.println(se);
        }
        catch(IOException ioe)
        {
            System.out.println(ioe);
        }
        
        // Get source
        getSource();
        
        // Get report
        getReport();
    }
    
    // Get source
    private void getSource()
    {
        //Get root element of spksource
        Element spksource = docSource.getDocumentElement();
        
        // Get nonmem
        NodeList nonmemList = spksource.getElementsByTagName("nonmem"); 
        Element nonmem = null;
        if(nonmemList.getLength() > 0)
        {
            nonmem = (Element)nonmemList.item(0);  
        
            // Get analysis description
            getConstraint(nonmem);
     
            // Get presentation
            getPresentation(nonmem);
        }
    }
    
    // Get constraint
    private void getConstraint(Element nonmem)
    {
        NodeList constraintList = nonmem.getElementsByTagName("constraint");
        if(constraintList.getLength() > 0)
        {
            Element constraint = (Element)constraintList.item(0);
            NodeList pop_analysisList = constraint.getElementsByTagName("pop_analysis");
            NodeList ind_analysisList = constraint.getElementsByTagName("ind_analysis"); 
            Element analysis = null;
            if(pop_analysisList.getLength() > 0)
                analysis = (Element)pop_analysisList.item(0);
            else if(ind_analysisList.getLength() > 0)
                analysis = (Element)ind_analysisList.item(0);
            else
                return;
            NodeList descriptionList = analysis.getElementsByTagName("description");
            if(descriptionList.getLength() > 0)
            {
                Element description = (Element)descriptionList.item(0);
                output.title = description.getFirstChild().getNodeValue();
            }
            
            NodeList dataLabelList = analysis.getElementsByTagName("data_labels");
            if(dataLabelList.getLength() > 0)
            {
                Element dataLabels = (Element)dataLabelList.item(0); 
                NodeList labelList = dataLabels.getElementsByTagName("label");
                if(labelList.getLength() > 0)
                    output.dataLabelMap = new Properties();  
                for(int i = 0; i < labelList.getLength(); i++)
                {
                    Element label = (Element)labelList.item(i);
                    String name = label.getAttribute("name");
                    String alias = label.getAttribute("synonym");
                    if(alias.equals(""))
                        alias = name;
                    output.dataLabelMap.setProperty(name, alias); 
                }
            }
        }
    }
    
    // Get presentation
    private void getPresentation(Element nonmem)
    {
        NodeList presentationList = nonmem.getElementsByTagName("presentation");
        if(presentationList.getLength() > 0)
        {
            // Get tables
            Element presentation = (Element)presentationList.item(0); 
            NodeList tableList = presentation.getElementsByTagName("table");
            if(tableList.getLength() > 0)
            {
                output.table = new String[tableList.getLength()][2][];
                getTable(tableList);
            }
            
            // Get scatterplots
            NodeList scatterplotList = presentation.getElementsByTagName("scatterplot");
            if(scatterplotList.getLength() > 0)
            {
                output.scatterplot = new String[scatterplotList.getLength()][4][];
                getScatterplot(scatterplotList);
            }
        }
    }
        
    //Get table
    private void getTable(NodeList tableList)
    {
        for(int i = 0; i < tableList.getLength(); i++)
        {
            String[][] tableI = output.table[i];
            Element table = (Element)tableList.item(i);
            tableI[0] = new String[4];
            tableI[0][0] = table.getAttribute("save_as"); 
            tableI[0][1] = table.getAttribute("header"); 
            tableI[0][2] = table.getAttribute("process");                
            NodeList columnList = table.getElementsByTagName("column");
            tableI[1] = new String[columnList.getLength()];
            int nSortingCols = 0;
            for(int j = 0; j < columnList.getLength(); j++)
            {
                Element column = (Element)columnList.item(j);
                int k = Integer.parseInt(column.getAttribute("appearance_order"));
                tableI[1][k - 1] = column.getAttribute("label");
                if(!column.getAttribute("sort_order").equals("0") && 
                   !column.getAttribute("sort_order").equals(""))
                    nSortingCols++;
            }
            tableI[0][3] = String.valueOf(nSortingCols); 
        }
    }
        
    //Get scatterplot
    private void getScatterplot(NodeList scatterplotList)
    {
        for(int i = 0; i < scatterplotList.getLength(); i++)
        {
            String[][] scatterplotI = output.scatterplot[i];
            Element scatterplot = (Element)scatterplotList.item(i);
            scatterplotI[0] = new String[4];
            scatterplotI[0][0] = scatterplot.getAttribute("from");
            scatterplotI[0][1] = scatterplot.getAttribute("to");
            scatterplotI[0][2] = scatterplot.getAttribute("unit_slope");
            scatterplotI[0][3] = scatterplot.getAttribute("x0_line");
            scatterplotI[0][0] = scatterplot.getAttribute("y0_line");
            scatterplotI[0][0] = scatterplot.getAttribute("process");
            NodeList yList = scatterplot.getElementsByTagName("y");
            scatterplotI[1] = new String[yList.getLength()];
            for(int j = 0; j < yList.getLength(); j++)
                scatterplotI[1][j] = ((Element)yList.item(j)).getAttribute("label");
            NodeList xList = scatterplot.getElementsByTagName("x");
            scatterplotI[2] = new String[xList.getLength()];
            for(int j = 0; j < xList.getLength(); j++)
                scatterplotI[2][j] = ((Element)xList.item(j)).getAttribute("label");
            NodeList byList = scatterplot.getElementsByTagName("by");
            scatterplotI[3] = new String[byList.getLength()];
            for(int j = 0; j < byList.getLength(); j++)
                scatterplotI[3][j] = ((Element)byList.item(j)).getAttribute("label");            
        }
    }
    
    // Get report
    private void getReport()
    {
        // Get root element
        Element spkreport = docReport.getDocumentElement();
        
        // Get error message 
        NodeList error_messageList = spkreport.getElementsByTagName("error_message");
        if(error_messageList.getLength() > 0)
            getErrorMessage((Element)error_messageList.item(0));
        
        // Get population analysis result
        NodeList pop_analysis_resultList = spkreport.getElementsByTagName("pop_analysis_result");
        if(pop_analysis_resultList.getLength() > 0)
        {
            Element pop_analysis_result = (Element)pop_analysis_resultList.item(0);
            getPopEstimationResult(pop_analysis_result);
            NodeList stat_resultList = pop_analysis_result.getElementsByTagName("pop_stat_result");
            if(stat_resultList.getLength() > 0)
                getStatisticsResult((Element)stat_resultList.item(0)); 
        }

        // Get individual analysis result
        NodeList ind_analysis_resultList = spkreport.getElementsByTagName("ind_analysis_result");
        if(ind_analysis_resultList.getLength() > 0)
        {
            Element ind_analysis_result = (Element)ind_analysis_resultList.item(0);
            getIndEstimationResult(ind_analysis_result);
            NodeList stat_resultList = ind_analysis_result.getElementsByTagName("ind_stat_result");     
            if(stat_resultList.getLength() > 0)
                getStatisticsResult((Element)stat_resultList.item(0));
        }        
        
        // Get persentation data result
        NodeList presentation_dataList = spkreport.getElementsByTagName("presentation_data");
        if(presentation_dataList.getLength() > 0)       
            getPresentationData((Element)presentation_dataList.item(0));
    }
    
    // Get error message    
    private void getErrorMessage(Element error_message)
    {
        Node error = error_message.getFirstChild();
        if(error != null)
            output.error = error.getNodeValue();  
        else
            output.error = "No error found";
    }
    
    // Get population estimation result
    private void getPopEstimationResult(Element pop_analysis_result)  
    {
        // Get pop_out_result
        NodeList pop_opt_resultList = pop_analysis_result.getElementsByTagName("pop_opt_result");
        if(pop_opt_resultList.getLength() > 0)
        {
            Element pop_opt_result = (Element)pop_opt_resultList.item(0);
 
            // Get objective
            NodeList objectiveList = pop_opt_result.getElementsByTagName("pop_obj_out");
            if(objectiveList.getLength() > 0)
            {
                Element objective = (Element)objectiveList.item(0);
                NodeList valueList = objective.getElementsByTagName("value");
                if(valueList.getLength() > 0)
                    output.objective = ((Element)valueList.item(0)).getFirstChild().getNodeValue();
            }
                
            // Get theta, omega, sigma
            getTheta(pop_opt_result);
            getOmega(pop_opt_result);
            getSigma(pop_opt_result);
        }
    }
      
    // Get population estimation result
    private void getIndEstimationResult(Element ind_analysis_result)  
    {
        // Get pop_out_result
        NodeList ind_opt_resultList = ind_analysis_result.getElementsByTagName("ind_opt_result");
        if(ind_opt_resultList.getLength() > 0)
        {
            Element ind_opt_result = (Element)ind_opt_resultList.item(0);
 
            // Get objective
            NodeList objectiveList = ind_opt_result.getElementsByTagName("ind_obj_out");
            if(objectiveList.getLength() > 0)
            {
                Element objective = (Element)objectiveList.item(0);
                NodeList valueList = objective.getElementsByTagName("value");
                if(valueList.getLength() > 0)
                    output.objective = ((Element)valueList.item(0)).getFirstChild().getNodeValue();
            }
            
            // Get theta, omega
            getTheta(ind_opt_result);
            getOmega(ind_opt_result);
        }
    }    
    
    // Get Theta
    private void getTheta(Element parent)
    {
        NodeList theta_outList = parent.getElementsByTagName("theta_out"); 
        if(theta_outList.getLength() > 0)
        {
            Element theta_out = (Element)theta_outList.item(0);
            NodeList valueList = theta_out.getElementsByTagName("value");
            output.theta = new String[valueList.getLength()];
            for(int i = 0; i < valueList.getLength(); i++)
            {
                Element value = (Element)valueList.item(i);
                output.theta[i] = value.getFirstChild().getNodeValue();
            }
        }
    }
        
    // Get omega
    private void getOmega(Element parent)
    {
        NodeList omega_outList = parent.getElementsByTagName("omega_out"); 
        if(omega_outList.getLength() > 0)
        {
            Element omega_out = (Element)omega_outList.item(0);
            String struct = omega_out.getAttribute("struct");
            int dimension = Integer.parseInt(omega_out.getAttribute("dimension"));
            NodeList valueList = omega_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.omega = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.omega[i] = new String[i + 2];
                    output.omega[i][0] = "ETA" + (i + 1);
                    for(int j = 1; j <= i + 1; j++) 
                    {
                        if(struct.equals("diagonal"))
                        {
                            if(i == j)
                                output.omega[i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                            else
                                output.omega[i][j] = "0";
                        }
                        else if(struct.equals("block"))
                            output.omega[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                        else
                            JOptionPane.showMessageDialog(null, "The OMEGA structure wrong", 
                                                          "Report Error",               
                                                          JOptionPane.ERROR_MESSAGE); 
                    }
                }
            }
        }
    }
    
    // Get sigma
    private void getSigma(Element parent)
    {
        NodeList sigma_outList = parent.getElementsByTagName("sigma_out"); 
        if(sigma_outList.getLength() > 0)
        {
            Element sigma_out = (Element)sigma_outList.item(0);
            String struct = sigma_out.getAttribute("struct");
            int dimension = Integer.parseInt(sigma_out.getAttribute("dimension"));            
            NodeList valueList = sigma_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.sigma = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.sigma[i] = new String[i + 2];
                    output.sigma[i][0] = "EPS" + (i + 1);
                    for(int j = 1; j <= i + 1; j++) 
                    {
                        if(struct.equals("diagonal"))
                        {
                            if(i == j)
                                output.sigma[i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                            else
                                output.sigma[i][j] = "0";
                        }
                        else if(struct.equals("block"))
                            output.sigma[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                        else
                            JOptionPane.showMessageDialog(null, "The SIGMA structure wrong", 
                                                          "Report Error",               
                                                          JOptionPane.ERROR_MESSAGE); 
                    }
                }
            }
        }
    }
    
    // Get statistics result
    private void getStatisticsResult(Element stat_result)
    {
        // Get statistics labels
        getStatisticsLabels(stat_result);
        
        // Get standard error
        getStdError(stat_result);
        
        // Get covariance
        getCovariance(stat_result);
        
        // Get correlation
        getCorrelation(stat_result);
        
        // Get inverse covariance
        getInvCovariance(stat_result);
     }
    
    // Get statistics labels
    private void getStatisticsLabels(Element stat_result)
    {
        int thetaLength = output.theta.length;
        int omegaLength = output.omega.length; 
        int sigmaLength = output.sigma.length;
        output.statLabels = new String[thetaLength + 
                                       (omegaLength + 1) * omegaLength / 2 +
                                       (sigmaLength + 1) * sigmaLength / 2 ];
        int k = 0;
        for(int i = 0; i < thetaLength; i++)
            output.statLabels[k++] = "TH " + (i + 1);
        for(int i = 0; i < omegaLength; i++)
            for(int j = i; j < omegaLength; j++)
                output.statLabels[k++] = "OM" + (i + 1) + (j + 1);  
        for(int i = 0; i < sigmaLength; i++) 
            for(int j = i; j < sigmaLength; j++)
                output.statLabels[k++] = "SG" + (i + 1) + (j + 1);   
    }
        
    // Get standard error
    private void getStdError(Element stat_result)
    {
        String stderr = "pop_stderr_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            stderr = "ind_stderr_out";
        NodeList stderr_outList = stat_result.getElementsByTagName(stderr);
        if(stderr_outList.getLength() > 0)
        {
            Element stderr_out = (Element)stderr_outList.item(0);
            NodeList valueList = stderr_out.getElementsByTagName("value"); 
            int length = valueList.getLength();
            if(length > 0 && length == output.statLabels.length)
            {
                int dimension = output.theta.length;
                output.stdErrTheta = new String[dimension];
                for(int i = 0; i < dimension; i++)
                {
                    Element value = (Element)valueList.item(i);
                    output.stdErrTheta[i] = value.getFirstChild().getNodeValue();
                }
                int k = dimension;
                int l = dimension;
                dimension = output.omega.length;
                output.stdErrOmega = new String[dimension][]; 
                for(int i = 0; i < dimension; i++)
                {
                    output.stdErrOmega[i] = new String[i + 2];
                    output.stdErrOmega[i][0] = "ETA" + (i + l);
                    for(int j = 1; j <= i + 1; j++) 
                        output.stdErrOmega[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                }
                l += dimension;
                dimension = output.sigma.length;
                output.stdErrSigma = new String[dimension][]; 
                for(int i = 0; i < dimension; i++)
                {
                    output.stdErrSigma[i] = new String[i + 2];
                    output.stdErrSigma[i][0] = "EPS" + (i + 1);
                    for(int j = 1; j <= i + 1; j++) 
                        output.stdErrSigma[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                }                
            }
        }
    }
    
    // Get covariance
    private void getCovariance(Element stat_result)
    {
        String covariance = "pop_covariance_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            covariance = "ind_covariance_out";        
        NodeList covariance_outList = stat_result.getElementsByTagName(covariance);
        if(covariance_outList.getLength() > 0)
        {
            Element covariance_out = (Element)covariance_outList.item(0);
            int dimension = Integer.parseInt(covariance_out.getAttribute("dimension"));            
            NodeList valueList = covariance_out.getElementsByTagName("value");  
            int length = valueList.getLength();
            if(length > 0)
            {
                output.covariance = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.covariance[i] = new String[i + 2];
                    output.covariance[i][0] = output.statLabels[i];
                    for(int j = 1; j <= i + 1; j++) 
                        output.covariance[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                }
            }            
        }
    }
    
    // Get inverse covariance
    private void getInvCovariance(Element stat_result)
    {
        String invCovariance = "pop_inverse_covariance_out";
        if(stat_result.getNodeName().equals("ind_stat_result")) 
            invCovariance = "ind_inverse_covariance_out";        
        NodeList inverse_covariance_outList = stat_result.getElementsByTagName(invCovariance);
        if(inverse_covariance_outList.getLength() > 0)
        {
            Element inverse_covariance_out = (Element)inverse_covariance_outList.item(0);
            int dimension = Integer.parseInt(inverse_covariance_out.getAttribute("dimension"));            
            NodeList valueList = inverse_covariance_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.invCovariance = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.invCovariance[i] = new String[i + 2];
                    output.invCovariance[i][0] = output.statLabels[i];
                    for(int j = 1; j <= i + 1; j++) 
                        output.invCovariance[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                }
            }            
        }
    } 
    
    // Get correlation
    private void getCorrelation(Element stat_result)
    {
        String correlation = "pop_correlation_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            correlation = "ind_correlation_out";         
        NodeList correlation_outList = stat_result.getElementsByTagName(correlation);
        if(correlation_outList.getLength() > 0)
        {
            Element correlation_out = (Element)correlation_outList.item(0);
            int dimension = Integer.parseInt(correlation_out.getAttribute("dimension"));
            NodeList valueList = correlation_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.correlation = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.correlation[i] = new String[i + 2];
                    output.correlation[i][0] = output.statLabels[i];
                    for(int j = 1; j <= i + 1; j++) 
                        output.correlation[i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                }
            }            
        }
    }
    
    // Get presentation data
    private void getPresentationData(Element presentation_data)
    {
        int nColumns = Integer.parseInt(presentation_data.getAttribute("columns"));
        int nRows = Integer.parseInt(presentation_data.getAttribute("rows"));
        NodeList rowList = presentation_data.getElementsByTagName("row");
        if(rowList.getLength() != nRows)
            return;
        if(rowList.getLength() > 0)
        {
            // Get data label list
            Element labelRow = (Element)rowList.item(0);
            NodeList valueList = labelRow.getElementsByTagName("value");
            if(valueList.getLength() != nColumns)
                return;
            output.dataItems = new ArrayList(nColumns); 
            for(int i = 0; i < nColumns; i++)
                output.dataItems.add(i, valueList.item(i).getFirstChild().getNodeValue());
        
            // Get all output data
            output.dataAll = new String[nRows - 1][nColumns]; 
            for(int i = 1; i < nRows; i++)
            {
                Element dataRow = (Element)rowList.item(i);
                valueList = dataRow.getElementsByTagName("value");
                if(valueList.getLength() != nColumns)
                    return;
                for(int j = 0; j < nColumns; j++)
                    output.dataAll[i - 1][j] = valueList.item(j).getFirstChild().getNodeValue();
            }
        }
    }
    
    // Report document and source document
    private Document docReport, docSource;
    
    // Output object
    private Output output;
}
