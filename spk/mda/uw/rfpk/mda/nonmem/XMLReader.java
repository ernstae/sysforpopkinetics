/*
 * XMLReader.java
 *
 * Created on November 20, 2003, 3:17 PM
 */

package uw.rfpk.mda.nonmem;

import uw.rfpk.mda.nonmem.Utility;
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
import java.text.DecimalFormat;
import javax.swing.JOptionPane;

/** This class defines an object that reads two XML documents: Spk source file and Spk report file.
 * 
 * @author  jiaji Du
 */
public class XMLReader {
    
    /** Creates a new instance of XMLReader.
     * @param text A Sting object containing two xml documents: spkreport and spksource.
     * @param output An Output object to hold output information.
     */
    public XMLReader(String text, Output output) 
    {
        this.output = output;
        
        // Get the XML documents as String objects
        int index1 = text.indexOf("<spkreport");
        int index2 = text.indexOf("<spksource");        
        String job = text.substring(0, index1 - 22);
        String report = text.substring(index1 - 22, index2 - 22);
        String source = text.substring(index2 - 22);  
        try
        {
            // Parse the XML documents
            DOMParser parser = new DOMParser(); 
            parser.parse(new InputSource(new ByteArrayInputStream(job.getBytes()))); 
            docJob = parser.getDocument();            
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
        
        // Get job
        getJob();
        
        // Get source
        getSource();
        
        // Get report
        getReport();
    }
    
    // Get job
    private void getJob()
    {
        //Get root element of spksource        
        Element spkjob = docJob.getDocumentElement();
        output.submissionTime = spkjob.getAttribute("submission_time");
        output.completionTime = spkjob.getAttribute("completion_time"); 
        output.jobAbstract = spkjob.getAttribute("abstract"); 
        NodeList modelList = spkjob.getElementsByTagName("model");
        if(modelList.getLength() > 0)
        {
            Element model = (Element)modelList.item(0); 
            output.modelName = model.getAttribute("name");
            output.modelVersion = model.getAttribute("version");
            output.modelAbstract = model.getAttribute("abstract");
        }
        NodeList dataList = spkjob.getElementsByTagName("data");
        if(dataList.getLength() > 0)
        {
            Element data = (Element)dataList.item(0); 
            output.dataName = data.getAttribute("name");
            output.dataVersion = data.getAttribute("version");
            output.dataAbstract = data.getAttribute("abstract");
        }
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
            {
                analysis = (Element)pop_analysisList.item(0);
                output.analysis = "population";
            }
            else if(ind_analysisList.getLength() > 0)
            {
                analysis = (Element)ind_analysisList.item(0);
                output.analysis = "individual";
            }
            else
                return;

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
            scatterplotI[0] = new String[6];
            scatterplotI[0][0] = scatterplot.getAttribute("from");
            if(scatterplotI[0][0].equals(""))
                scatterplotI[0][0] = "1";
            scatterplotI[0][1] = scatterplot.getAttribute("to");
            if(scatterplotI[0][1].equals(""))
                scatterplotI[0][1] = "900";
            scatterplotI[0][2] = scatterplot.getAttribute("x0_line");
            scatterplotI[0][3] = scatterplot.getAttribute("y0_line");
            scatterplotI[0][4] = scatterplot.getAttribute("unit_slope");
            scatterplotI[0][5] = scatterplot.getAttribute("process");
            NodeList yList = scatterplot.getElementsByTagName("y");
            scatterplotI[1] = new String[yList.getLength()];
            for(int j = 0; j < yList.getLength(); j++)
                scatterplotI[1][j] = ((Element)yList.item(j)).getAttribute("label");
            NodeList xList = scatterplot.getElementsByTagName("x");
            scatterplotI[2] = new String[xList.getLength()];
            for(int j = 0; j < xList.getLength(); j++)
                scatterplotI[2][j] = ((Element)xList.item(j)).getAttribute("label");
            NodeList byList = scatterplot.getElementsByTagName("by");
            if(byList.getLength() > 0)
            {
                scatterplotI[3] = new String[byList.getLength()];
                for(int j = 0; j < byList.getLength(); j++)
                scatterplotI[3][j] = ((Element)byList.item(j)).getAttribute("label"); 
            }
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
        
        // Get optimization trace output
        NodeList opt_trace_outList = spkreport.getElementsByTagName("opt_trace_out");
        if(opt_trace_outList.getLength() > 0)
            getOptTraceOut((Element)opt_trace_outList.item(0));        
    }

    // Get error message    
    private void getErrorMessage(Element error_message)
    {
        Node node = error_message.getFirstChild();
        if(node != null)
            output.error = node.getNodeValue();  
        else
            output.error = "No error found";
    }
    
    // Get error message    
    private void getOptTraceOut(Element opt_trace_out)
    {
        Node node = opt_trace_out.getFirstChild();
        if(node != null)
            output.trace = node.getNodeValue();  
        else
            output.trace = "Optimization trace output is not available.";
    }
    
    // Get population estimation result
    private void getPopEstimationResult(Element pop_analysis_result)  
    {
        // Get pop_out_result
        output.computingTimes = new String[2];
        NodeList pop_opt_resultList = pop_analysis_result.getElementsByTagName("pop_opt_result");
        if(pop_opt_resultList.getLength() > 0)
        {
            Element pop_opt_result = (Element)pop_opt_resultList.item(0);
            
            // Get elapsed time
            output.computingTimes[0] = pop_opt_result.getAttribute("elapsedtime");
 
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
        output.computingTimes = new String[2];
        NodeList ind_opt_resultList = ind_analysis_result.getElementsByTagName("ind_opt_result");
        if(ind_opt_resultList.getLength() > 0)
        {
            Element ind_opt_result = (Element)ind_opt_resultList.item(0);
            
            // Get computing time
            output.computingTimes[0] = ind_opt_result.getAttribute("elapsedtime");            
 
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
                            if(i == j - 1)
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
                            if(i == j - 1)
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
        // Get elapsed time
        output.computingTimes[1] = stat_result.getAttribute("elapsedtime");        
        
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
        
        // Get coefficient of variation
        getCoefVariation(stat_result);
        
        // Get confidence interval
        getConfInterval(stat_result);
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
        String stdErr = "pop_stderr_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            stdErr = "ind_stderr_out";
        NodeList stderr_outList = stat_result.getElementsByTagName(stdErr);
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
    
    // Get coefficent of variation
    private void getCoefVariation(Element stat_result)
    {    
        String coefVar = "pop_coefficient_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            coefVar = "ind_coefficient_out";   
        NodeList coefVar_outList = stat_result.getElementsByTagName(coefVar);
        if(coefVar_outList.getLength() > 0)
        {
            Element coefVar_out = (Element)coefVar_outList.item(0);
            NodeList valueList = coefVar_out.getElementsByTagName("value"); 
            int length = valueList.getLength();
            if(length > 0 && length == output.statLabels.length)
            {
                output.coefVariation = new String[length];             
                for(int i = 0; i < length; i++)
                {
                    Element value = (Element)valueList.item(i);
                    output.coefVariation[i] = value.getFirstChild().getNodeValue();
                }
            }
        }        
    }
    
    // Get confident interval
    private void getConfInterval(Element stat_result)
    {    
        String ConfInterval = "pop_confidence_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            ConfInterval = "ind_confidence_out"; 
        NodeList ConfInterval_outList = stat_result.getElementsByTagName(ConfInterval);
        if(ConfInterval_outList.getLength() > 0)
        {
            Element ConfInterval_out = (Element)ConfInterval_outList.item(0);
            NodeList valueList = ConfInterval_out.getElementsByTagName("value");
            int length = valueList.getLength()/2;
            if(length > 0 && length == output.statLabels.length)
            {
                output.confInterval = new String[2][length];                
                for(int i = 0; i < length; i++)
                {
                    Element value1 = (Element)valueList.item(i);
                    Element value2 = (Element)valueList.item(i + length);
                    output.confInterval[0][i] = value1.getFirstChild().getNodeValue();
                    output.confInterval[1][i] = value2.getFirstChild().getNodeValue();
                }
            }
        }        
    }
    
    // Get presentation data
    private void getPresentationData(Element presentation_data)
    {
        int nColumns = Integer.parseInt(presentation_data.getAttribute("columns"));
        int nRows = Integer.parseInt(presentation_data.getAttribute("rows"));
        
        // Get data label list
        NodeList labelList = presentation_data.getElementsByTagName("label");
        int nLabels = labelList.getLength();
        if(nColumns == 0 || nRows == 0 || nLabels != nColumns)
            return;
        if(nColumns > 0)
        {
            output.dataItems = new ArrayList(nColumns);             
            for(int i = 0; i < nColumns; i++)
            {
                Element label = (Element)labelList.item(i);
                output.dataItems.add(i, label.getAttribute("name"));               
            }
        }
        NodeList rowList = presentation_data.getElementsByTagName("row");
        if(rowList.getLength() != nRows)
            return;
        if(nRows > 0)
        {
            // Get all output data
            output.dataAll = new double[nRows][nColumns]; 
            for(int i = 0; i < nRows; i++)
            {
                Element dataRow = (Element)rowList.item(i);
                NodeList valueList = dataRow.getElementsByTagName("value");
                if(valueList.getLength() != nColumns)
                    return;
                for(int j = 0; j < nColumns; j++)
                    output.dataAll[i][j] = Double.parseDouble(valueList.item(j).getFirstChild().getNodeValue());
            }
        }
    }
   
    /** Convert the data XML back to the original.
     * @param dataXML Data XML as a String object.
     * @return A String object containing the original data, null if failed.
     */
    public static String parseDataXML(String dataXML)
    {
        String data = "";
        Document docData = null;
        Element row, value;
        try
        {
            // Parse the XML documents
            DOMParser parser = new DOMParser(); 
            parser.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes()))); 
            docData = parser.getDocument();            
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
        
        //Get root element of spkdata
        Element spkdata = docData.getDocumentElement();  
        
        // Get rows
        NodeList rowList = spkdata.getElementsByTagName("row"); 
        if(rowList.getLength() > 1)
        {
            String ls = System.getProperty("line.separator");
            for(int i = 1; i < rowList.getLength(); i++)
            {
                row = (Element)rowList.item(i);
                NodeList valueList = row.getElementsByTagName("value");                
                if(valueList.getLength() > 0)
                {
                    for(int j = 0; j < valueList.getLength(); j++)
                    {
                        value = (Element)valueList.item(j);
                        if(value.getAttribute("type").equals("string"))
                            data += formatString(value.getFirstChild().getNodeValue());
                        else
                            data += formatNumeric(value.getFirstChild().getNodeValue());
                    }
                } 
                data += ls;
            }
        }        
        return data;
    }

    // Format the data of type string
    private static String formatString(String number)
    {
        String string = "";
        for(int i = 0; i < 10 - number.length(); i++)
            string +=" ";
        return string + number;
    }
    
    // Format the data of type numeric 
    private static String formatNumeric(String number)
    {
        if(number.equals("."))
            number += "0";
        DecimalFormat f = new DecimalFormat("0.0000E00");
        return Utility.formatData(8, f.format(Double.parseDouble(number)));  
    }
    
    /** Get model archive text from model XML document.  The first line is removed.
     * @param model A String object containing the model XML document.
     * @return A String object containing the model archive text.
     */ 
    public static String getModelArchive(String model)
    {
        String modelArchive = null;
        try
        {
            // Parse spkmodel XML document
            DOMParser parser = new DOMParser();                
            parser.parse(new InputSource(new ByteArrayInputStream(model.getBytes())));                  
            Document docModel = parser.getDocument();              
            Element spkmodel = docModel.getDocumentElement();
            modelArchive = spkmodel.getFirstChild().getNodeValue();
            if(!modelArchive.equals(""))
                modelArchive = modelArchive.substring(modelArchive.indexOf('\n') + 1);
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "IO exception",
                                          JOptionPane.ERROR_MESSAGE);             
        }        
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SAX exception",
                                          JOptionPane.ERROR_MESSAGE);             
        }         
     
        return modelArchive;
    }
    
    // Report document and source document
    private Document docJob, docReport, docSource;
    
    // Output object
    private Output output;
}
