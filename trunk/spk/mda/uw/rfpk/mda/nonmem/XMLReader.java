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

import uw.rfpk.mda.nonmem.Utility;
import uw.rfpk.mda.nonmem.Output;
import javax.xml.parsers.*;
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
import java.text.NumberFormat;
import javax.swing.JOptionPane;

/** This class defines an object that reads two XML documents: Spk source file and Spk report file.
 * 
 * @author  Jiaji Du
 */
public class XMLReader
{
    /** Creates a new instance of XMLReader */
    public XMLReader(){}
    
    /** Creates a new instance of XMLReader to parse SPKOutput.
     * @param text a Sting object containing three xml documents: spkjob, spkreport and spksource.
     * @param output an Output object to hold output information.
     */
    public XMLReader(String text, Output output) 
    {
        this.output = output;
        
        // Get the XML documents as String objects
        int indexReport = text.lastIndexOf("<?xml ", text.indexOf("<spkreport"));
        int indexSource = text.lastIndexOf("<?xml ", text.indexOf("<spksource"));
        String job = text.substring(0, indexReport);
        String report = text.substring(indexReport, indexSource);
        String source = text.substring(indexSource);
//        int index1 = text.indexOf("<spkreport");
//        int index2 = text.indexOf("<spksource");
//        String job = text.substring(0, index1 - 22);
//        String report = text.substring(index1 - 22, index2 - 22);
//        String source = text.substring(index2 - 22);  
        try
        {
            // Parse the XML documents
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docJob = builder.parse(new InputSource(new ByteArrayInputStream(job.getBytes())));
            docReport = builder.parse(new InputSource(new ByteArrayInputStream(report.getBytes())));
            docSource = builder.parse(new InputSource(new ByteArrayInputStream(source.getBytes())));
//            DOMParser parser = new DOMParser(); 
//            parser.parse(new InputSource(new ByteArrayInputStream(job.getBytes()))); 
//            docJob = parser.getDocument();            
//            parser.parse(new InputSource(new ByteArrayInputStream(report.getBytes()))); 
//            docReport = parser.getDocument(); 
//            parser.parse(new InputSource(new ByteArrayInputStream(source.getBytes()))); 
//            docSource = parser.getDocument();     
        }
        catch(ParserConfigurationException e)
        {
            JOptionPane.showMessageDialog(null, e, "ParserConfigurationException", JOptionPane.ERROR_MESSAGE);
            output.ok = false;
            return;
        }
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e, "SAXException", JOptionPane.ERROR_MESSAGE);
            output.ok = false;
            return;
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            output.ok = false;
            return;            
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
        output.jobId = spkjob.getAttribute("id");
        output.jobAbstract = spkjob.getAttribute("abstract");        
        output.methodCode = spkjob.getAttribute("method_code");
        output.computationMode = spkjob.getAttribute("computation_mode");
        NodeList modelList = spkjob.getElementsByTagName("model");
        if(modelList.getLength() > 0)
        {
            Element model = (Element)modelList.item(0); 
            output.modelName = model.getAttribute("name");
            output.modelVersion = model.getAttribute("version");
            output.modelAbstract = model.getAttribute("abstract");
            output.modelVersionLog = model.getAttribute("log");
        }
        NodeList dataList = spkjob.getElementsByTagName("data");
        if(dataList.getLength() > 0)
        {
            Element data = (Element)dataList.item(0); 
            output.dataName = data.getAttribute("name");
            output.dataVersion = data.getAttribute("version");
            output.dataAbstract = data.getAttribute("abstract");
            output.dataVersionLog = data.getAttribute("log");
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
                if(analysis.getAttribute("approximation").equals("nonparametric"))
                {
                    NodeList pointList = analysis.getElementsByTagName("measure_points_in");
                    if(pointList.getLength() > 0)
                    {
                        Element point = (Element)pointList.item(0);
                        output.nonparamMethod = point.getAttribute("auto_generate_method");
                        if(output.nonparamMethod.equals("random_uniform"))
                        {
                            output.methodCode = "un";
                            output.nonparamNumOfPoints = point.getAttribute("number_of_points");
                            output.nonparamSeed = point.getAttribute("seed");
                        }
                        if(output.nonparamMethod.equals("grid"))
                        {
                            output.methodCode = "gn";
                            output.nonparamPointsPerDim = point.getAttribute("points_per_dimension");
                        }
                    }
                }
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
            if(tableI[0][0].equals("")) tableI[0][0] = "untitled";
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
            NodeList byList = scatterplot.getElementsByTagName("split");
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
        NodeList error_messageList = spkreport.getElementsByTagName("error_list");
        if(error_messageList.getLength() > 0)
            getErrorMessage((Element)error_messageList.item(0));
     
        // Get warning message 
        NodeList warning_messageList = spkreport.getElementsByTagName("warning_list");
        if(warning_messageList.getLength() > 0)
            getWarningMessage((Element)warning_messageList.item(0));         
        
        // Get simulation data
        NodeList simulationList = spkreport.getElementsByTagName("simulation");
        if(simulationList.getLength() > 0)
        {
            output.subProblem = ((Element)simulationList.item(0)).getAttribute("subproblem");
            output.simulationSeed = ((Element)simulationList.item(0)).getAttribute("seed");
        }
        
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

        // Get population Monte carlo result
        NodeList pop_monte_resultList = spkreport.getElementsByTagName("pop_monte_result");
        if(pop_monte_resultList.getLength() > 0)
        {
            Element pop_monte_result = (Element)pop_monte_resultList.item(0);
            output.computingTimes = new String[1];
            output.computingTimes[0] = pop_monte_result.getAttribute("elapsedtime");
            output.nEvaluation = pop_monte_result.getAttribute("number_eval");
            NodeList column_majorList = pop_monte_result.getElementsByTagName("column_major");
            if(column_majorList.getLength() > 0)
            {
                output.alpha = new String[2][];
                for(int j = 0; j < 2; j++)
                {
                    Element alpha = (Element)column_majorList.item(j);
                    NodeList valueList = alpha.getElementsByTagName("value");
                    if(valueList.getLength() > 0)
                    {
                        output.alpha[j] = new String[valueList.getLength()];
                        for(int i = 0; i < valueList.getLength(); i++)
                        {
                            if(alpha.getAttribute("name").equals("alpha_center"))
                                output.alpha[0][i] = ((Element)valueList.item(i)).getFirstChild().getNodeValue().trim();
                            if(alpha.getAttribute("name").equals("alpha_step"))
                                output.alpha[1][i] = ((Element)valueList.item(i)).getFirstChild().getNodeValue().trim();
                        }
                    }
                }
            }
            NodeList row_majorList = pop_monte_result.getElementsByTagName("row_major");
            if(row_majorList.getLength() > 0)
            {
                for(int j = 0; j < 2; j++)
                {
                    Element obj = (Element)row_majorList.item(j);
                    NodeList rowList = obj.getElementsByTagName("row");
                    int r = rowList.getLength();
                    if(rowList.getLength() > 0)
                    {
                        if(obj.getAttribute("name").equals("obj_value"))
                        {
                            output.likelihood = new String[rowList.getLength()][3];
                            for(int k = 0; k < rowList.getLength(); k++)
                            {
                                Element row = (Element)rowList.item(k);
                                NodeList valueList = row.getElementsByTagName("value");
                                if(valueList.getLength() > 0)
                                    for(int i = 0; i < valueList.getLength(); i++)
                                        output.likelihood[k][i] = ((Element)valueList.item(i)).getFirstChild().getNodeValue().trim();
                            }
                        }
                        if(obj.getAttribute("name").equals("obj_std"))
                        {
                            output.likelihood_std = new String[rowList.getLength()][3];
                            for(int k = 0; k < rowList.getLength(); k++)
                            {
                                Element row = (Element)rowList.item(k);                                
                                NodeList valueList = row.getElementsByTagName("value");
                                if(valueList.getLength() > 0)
                                    for(int i = 0; i < valueList.getLength(); i++)
                                        output.likelihood_std[k][i] = ((Element)valueList.item(i)).getFirstChild().getNodeValue().trim();
                            }
                        }                        
                    }
                }
            }
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
            NodeList ident_resultList = ind_analysis_result.getElementsByTagName("ind_ident_result");
            if(ident_resultList.getLength() > 0)
                getIdentifiabilityResult((Element)ident_resultList.item(0));
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

    // Get identifiability result
    private void getIdentifiabilityResult(Element ident_result)
    {
        output.computingTimes = new String[1];
        output.computingTimes[0] = ident_result.getAttribute("elapsedtime");
        NodeList seedList = ident_result.getElementsByTagName("seed");
        NodeList valueList = ((Element)seedList.item(0)).getElementsByTagName("value");
        if(valueList.getLength() > 0)
            output.simulationSeed = valueList.item(0).getFirstChild().getNodeValue();
        NodeList statusList = ident_result.getElementsByTagName("ind_ident_status");
        output.identifyStatus = statusList.item(0).getFirstChild().getNodeValue();
        NodeList solutionList = ident_result.getElementsByTagName("ind_ident_number_of_solutions");
        valueList = ((Element)solutionList.item(0)).getElementsByTagName("value");
        if(valueList.getLength() > 0)
            output.identifySolutions = valueList.item(0).getFirstChild().getNodeValue();
    }
   
    // Get error message    
    private void getErrorMessage(Element error_message)
    {
        NodeList errorList = error_message.getElementsByTagName("error");
        if(errorList.getLength() != 0)
            output.error = new String[errorList.getLength()][3];
        for(int i = 0; i < errorList.getLength(); i++)
        {
            Element error = (Element)errorList.item(i);
            NodeList list = error.getElementsByTagName("message");
            if(list.getLength() != 0)
                output.error[i][0] = list.item(0).getFirstChild().getNodeValue().trim();
            list = error.getElementsByTagName("file_name");
            if(list.getLength() != 0)
                output.error[i][1] = list.item(0).getFirstChild().getNodeValue().trim();           
            list = error.getElementsByTagName("line_number");
            if(list.getLength() != 0)
                output.error[i][2] = list.item(0).getFirstChild().getNodeValue().trim(); 
        }
    }

    // Get warning message    
    private void getWarningMessage(Element warning_message)
    {
        NodeList warningList = warning_message.getElementsByTagName("warning");
        if(warningList.getLength() != 0)
            output.warning = new String[warningList.getLength()][3];
        for(int i = 0; i < warningList.getLength(); i++)
        {
            Element warning = (Element)warningList.item(i);
            NodeList list = warning.getElementsByTagName("message");
            if(list.getLength() != 0)
                output.warning[i][0] = list.item(0).getFirstChild().getNodeValue().trim();
            list = warning.getElementsByTagName("file_name");
            if(list.getLength() != 0)
                output.warning[i][1] = list.item(0).getFirstChild().getNodeValue().trim();           
            list = warning.getElementsByTagName("line_number");
            if(list.getLength() != 0)
                output.warning[i][2] = list.item(0).getFirstChild().getNodeValue().trim();           
        }
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
            
            // Get nonparametric result
            NodeList nonparamList = pop_opt_result.getElementsByTagName("nonparametric_result");
            if(nonparamList.getLength() > 0)
            {
                Element nonparametricResult = (Element)nonparamList.item(0);
                getNonparametricResult(nonparametricResult);
            }
        }
    }
      
    // Get nonparametric result
    private void getNonparametricResult(Element nonparametricResult)
    {
        // Get measure point in result
        NodeList measurePointList = nonparametricResult.getElementsByTagName("measure_point_in");
        Element element = (Element)measurePointList.item(0);
        output.nonparamInPoints = element.getAttribute("number");
        NodeList list = element.getElementsByTagName("theta_all_in");
        String[] rows = ((Element)list.item(0)).getFirstChild().getNodeValue().trim().split("\n");
        output.nonparamInTheta = new String[rows.length][];
        for(int i = 0; i < rows.length; i++)
            output.nonparamInTheta[i] = rows[i].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        list = element.getElementsByTagName("omega_all_in");
        int nBlock = list.getLength();
        output.nonparamInOmega = new String[nBlock][][];
        for(int i = 0; i < nBlock; i++)
        {
            rows = ((Element)list.item(i)).getFirstChild().getNodeValue().trim().split("\n");
            output.nonparamInOmega[i] = new String[rows.length][];
            for(int j = 0; j < rows.length; j++)
                output.nonparamInOmega[i][j] = rows[j].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        }
        
        // Get measure point out result
        measurePointList = nonparametricResult.getElementsByTagName("measure_point_out");
        element = (Element)measurePointList.item(0);
        output.nonparamOutPoints = element.getAttribute("number");
        list = element.getElementsByTagName("theta_all_out");
        rows = ((Element)list.item(0)).getFirstChild().getNodeValue().trim().split("\n");
        output.nonparamOutTheta = new String[rows.length][];
        for(int i = 0; i < rows.length; i++)
            output.nonparamOutTheta[i] = rows[i].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        list = element.getElementsByTagName("omega_all_out");
        nBlock = list.getLength();
        output.nonparamOutOmega = new String[nBlock][][];
        for(int i = 0; i < nBlock; i++)
        {
            rows = ((Element)list.item(i)).getFirstChild().getNodeValue().trim().split("\n");
            output.nonparamOutOmega[i] = new String[rows.length][];
            for(int j = 0; j < rows.length; j++)
                output.nonparamOutOmega[i][j] = rows[j].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        }
        
        // Get weight result
        list = nonparametricResult.getElementsByTagName("weight_all_out");
        output.nonparamWeight = ((Element)list.item(0)).getFirstChild().getNodeValue().trim().replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split("\n");
        
        // Get probability density result
        list = nonparametricResult.getElementsByTagName("probability_density_all_out");
        rows = ((Element)list.item(0)).getFirstChild().getNodeValue().trim().split("\n");
        output.nonparamDensity = new String[rows.length][];
        for(int i = 0; i < rows.length; i++)
            output.nonparamDensity[i] = rows[i].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        
        // Get posterior mean result
        list = nonparametricResult.getElementsByTagName("posterior_mean_out");
        element = (Element)list.item(0);
        list = element.getElementsByTagName("theta_all_out");
        rows = ((Element)list.item(0)).getFirstChild().getNodeValue().trim().split("\n");
        output.nonparamMeanTheta = new String[rows.length][];
        for(int i = 0; i < rows.length; i++)
            output.nonparamMeanTheta[i] = rows[i].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
        list = element.getElementsByTagName("omega_all_out");
        nBlock = list.getLength();
        output.nonparamMeanOmega = new String[nBlock][][];
        for(int i =0; i < nBlock; i++)
        {
            rows = ((Element)list.item(i)).getFirstChild().getNodeValue().trim().split("\n");
            output.nonparamMeanOmega[i] = new String[rows.length][];
            for(int j = 0; j < rows.length; j++)
                output.nonparamMeanOmega[i][j] = rows[j].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
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
    
    // Get Theta out
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
        int nBlock = omega_outList.getLength();
        output.omega = new String[nBlock][][];
        output.omegaStruct = new String[nBlock];
        for(int l = 0; l < nBlock; l++)
        {
            Element omega_out = (Element)omega_outList.item(l);
            output.omegaStruct[l] = omega_out.getAttribute("struct");
            int dimension = Integer.parseInt(omega_out.getAttribute("dimension"));
            NodeList valueList = omega_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.omega[l] = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.omega[l][i] = new String[i + 2];
                    output.omega[l][i][0] = "ETA" + (i + 1);
                    for(int j = 1; j <= i + 1; j++) 
                    {
                        if(output.omegaStruct[l].equals("diagonal"))
                        {
                            if(i == j - 1)
                                output.omega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                            else
                                output.omega[l][i][j] = "0";
                        }
                        else if(output.omegaStruct[l].equals("block"))
                            output.omega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
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
        int nBlock = sigma_outList.getLength();
        output.sigma = new String[nBlock][][];
        output.sigmaStruct = new String[nBlock];       
        for(int l = 0; l < nBlock; l++)
        {
            Element sigma_out = (Element)sigma_outList.item(l);
            output.sigmaStruct[l] = sigma_out.getAttribute("struct");
            int dimension = Integer.parseInt(sigma_out.getAttribute("dimension"));            
            NodeList valueList = sigma_out.getElementsByTagName("value");
            int length = valueList.getLength();
            if(length > 0)
            {
                output.sigma[l] = new String[dimension][];
                int k = 0;
                for(int i = 0; i < dimension; i++)
                {
                    output.sigma[l][i] = new String[i + 2];
                    output.sigma[l][i][0] = "EPS" + (i + 1);
                    for(int j = 1; j <= i + 1; j++) 
                    {
                        if(output.sigmaStruct[l].equals("diagonal"))
                        {
                            if(i == j - 1)
                                output.sigma[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                            else
                                output.sigma[l][i][j] = "0";
                        }
                        else if(output.sigmaStruct[l].equals("block"))
                            output.sigma[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
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
        int omegaSum = 0;
        int[] omegaLength = new int[output.omega.length];
        for(int l = 0; l < output.omega.length; l++)
        {
            if(output.omegaStruct[l].equals("block"))
                omegaLength[l] = (output.omega[l].length + 1) * output.omega[l].length / 2;
            else
                omegaLength[l] = output.omega[l].length;
            omegaSum += omegaLength[l];
        }
        int sigmaSum = 0;
        int[] sigmaLength = null;
        if(output.analysis.equals("population"))
        {
            sigmaLength = new int[output.sigma.length];
            for(int l = 0; l < output.sigma.length; l++)
            {            
                if(output.sigmaStruct[l].equals("block"))
                    sigmaLength[l] = (output.sigma[l].length + 1) * output.sigma[l].length / 2;
                else
                    sigmaLength[l] = output.sigma[l].length;
                sigmaSum += sigmaLength[l];
            }
        }
        output.statLabels = new String[thetaLength + omegaSum + sigmaSum];
                                      
        int k = 0;
        for(int i = 0; i < thetaLength; i++)
            output.statLabels[k++] = "TH " + (i + 1);
        for(int l = 0; l < output.omega.length; l++)
        {
            int length = output.omega[l].length;
            if(output.omegaStruct[l].equals("block"))
                for(int i = 0; i < length; i++)
                    for(int j = i; j < length; j++)
                        output.statLabels[k++] = "OM" + (i + 1) + (j + 1);
            else
                for(int i = 0; i < length; i++)
                    output.statLabels[k++] = "OM" + (i + 1) + (i + 1);
        }
        if(output.sigmaStruct == null)
            return;
        for(int l = 0; l < output.sigma.length; l++)
        {        
            int length = output.sigma[l].length;
            if(output.sigmaStruct[l].equals("block"))        
                for(int i = 0; i < length; i++) 
                    for(int j = i; j < length; j++)
                        output.statLabels[k++] = "SG" + (i + 1) + (j + 1);
            else
                for(int i = 0; i < length; i++)
                    output.statLabels[k++] = "SG" + (i + 1) + (i + 1);
        }
    }
        
    // Get standard error
    private void getStdError(Element stat_result)
    {
        String stdErr = "pop_stderror_out";
        if(stat_result.getNodeName().equals("ind_stat_result"))
            stdErr = "ind_stderror_out";
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
                int nBlock = output.omega.length;
                output.stdErrOmega = new String[nBlock][][];
                for(int l = 0; l < nBlock; l++)
                {
                    dimension = output.omega[l].length;
                    output.stdErrOmega[l] = new String[dimension][];
                    
                    for(int i = 0; i < dimension; i++)
                    {
                        output.stdErrOmega[l][i] = new String[i + 2];
                        output.stdErrOmega[l][i][0] = "ETA" + (i + 1);
                    }
                    for(int j = 1; j <= dimension; j++)
                    {                    
                        for(int i = j - 1; i < dimension; i++)
                        {
                            if(output.omegaStruct[l].equals("diagonal"))
                            {
                                if(i == j - 1)
                                    output.stdErrOmega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                                else
                                    output.stdErrOmega[l][i][j] = "";
                            }
                            else if(output.omegaStruct[l].equals("block"))                                         
                                output.stdErrOmega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                        }
                    }
/*
                    for(int i = 0; i < dimension; i++)
                    {
                        output.stdErrOmega[l][i] = new String[i + 2];
                        output.stdErrOmega[l][i][0] = "ETA" + (i + 1);
                        for(int j = 1; j <= i + 1; j++)
                        {
                            if(output.omegaStruct[l].equals("diagonal"))
                            {
                                if(i == j - 1)
                                    output.stdErrOmega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                                else
                                    output.stdErrOmega[l][i][j] = "";
                            }
                            else if(output.omegaStruct[l].equals("block"))                                         
                                output.stdErrOmega[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                        }
                    }
*/
                }
                if(output.sigma == null)
                    return;
                nBlock = output.sigma.length;
                output.stdErrSigma = new String[nBlock][][];
                for(int l = 0; l < nBlock; l++)
                {                
                    dimension = output.sigma[l].length;
                    output.stdErrSigma[l] = new String[dimension][]; 
                    for(int i = 0; i < dimension; i++)
                    {
                        output.stdErrSigma[l][i] = new String[i + 2];
                        output.stdErrSigma[l][i][0] = "EPS" + (i + 1);
                    }
                    for(int j = 1; j <= dimension; j++)
                    {                    
                        for(int i = j - 1; i < dimension; i++)                                            
                        {
                            if(output.sigmaStruct[l].equals("diagonal"))
                            {
                                if(i == j - 1)
                                    output.stdErrSigma[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();
                                else
                                    output.stdErrSigma[l][i][j] = "";
                            }
                            else if(output.sigmaStruct[l].equals("block"))                    
                                output.stdErrSigma[l][i][j] = valueList.item(k++).getFirstChild().getNodeValue();                    
                        }
                    }
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
        if(nLabels == 0)
        {
            parsePresentationData(presentation_data.getFirstChild().getNodeValue(), nColumns, nRows);
            return;
        }
        if(nColumns == 0 || nRows == 0 || nLabels != nColumns)
            return;
        if(nColumns > 0)
        {
            output.dataItems = new ArrayList<String>(nColumns);             
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
            output.indIDs = new String[nRows]; 
            output.dataAll = new double[nRows][nColumns]; 
            try
            {
                for(int i = 0; i < nRows; i++)
                {
                    Element dataRow = (Element)rowList.item(i);
                    NodeList valueList = dataRow.getElementsByTagName("value");
                    if(valueList.getLength() != nColumns)
                        return;
                    output.indIDs[i] = valueList.item(0).getFirstChild().getNodeValue();
                    for(int j = 0; j < nColumns; j++)
                        output.dataAll[i][j] = Double.parseDouble(valueList.item(j).getFirstChild().getNodeValue());
                }
            }
            catch(NumberFormatException e)
            {
                JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                              "Number Format Error", JOptionPane.ERROR_MESSAGE);
                output.dataAll = null;
            }
        }
    }
   
    private void parsePresentationData(String data, int nColumns, int nRows)
    {
        if(data == null || nColumns <= 0 || nRows <= 0)
        {
            JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                          "Presentation Data Error", JOptionPane.ERROR_MESSAGE);
            output.dataAll = null;
            return;
        }
        String[] rows = data.trim().split("\n");
        if(rows.length != nRows + 1)
        {
            JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                          "Presentation Data Error", JOptionPane.ERROR_MESSAGE);
            output.dataAll = null;
            return;
        }
        String[] elements;
        try
        {
            elements = rows[0].split(",");
            if(elements.length != nColumns)
            {
                JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                              "Presentation Data Error", JOptionPane.ERROR_MESSAGE);
                output.dataAll = null;
                return;
            }
            output.dataItems = new ArrayList<String>(nColumns);             
            for(int j = 0; j < nColumns; j++)
                output.dataItems.add(j, elements[j]);
            output.indIDs = new String[nRows];
            output.dataAll = new double[nRows][nColumns];
            for(int i = 0; i < nRows; i++)
            {
                elements = rows[i + 1].replaceAll("nan", "NaN").replaceAll("inf", "Infinity").split(",");
                if(elements.length != nColumns)
                {
                    JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                                  "Presentation Data Error", JOptionPane.ERROR_MESSAGE);
                    output.dataAll = null;
                    return;
                }
                output.indIDs[i] = elements[0];
                for(int j = 0; j < nColumns; j++)
                    output.dataAll[i][j] = Double.parseDouble(elements[j]);
            }
        }
        catch(NumberFormatException e)
        {
            JOptionPane.showMessageDialog(null, "An error was found in presentation data",
                                          "Number Format Error", JOptionPane.ERROR_MESSAGE);
            output.dataAll = null;
        }
    }
    
    /** Convert the data XML back to the original.
     * @param dataXML data XML as a String object.
     * @param addLabel true for adding labels, false for otherwise.
     * @return a String object containing the original data, null if failed.
     */
    public static String parseDataXML(String dataXML, boolean addLabel)
    {
        StringBuffer data = new StringBuffer();
        Document docData = null;
        Element row;
        String value;
        int columns, rows;
        try
        {
            // Parse the XML documents
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docData = builder.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes())));
//            DOMParser parser = new DOMParser(); 
//            parser.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes()))); 
//            docData = parser.getDocument();            
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
        
        //Get root element of spkdata
        Element spkdata = docData.getDocumentElement();  
        
        // Get dimensions
        NodeList tableList = spkdata.getElementsByTagName("table");
        Element table = (Element)tableList.item(0);
        columns = Integer.parseInt(table.getAttribute("columns"));
        rows = Integer.parseInt(table.getAttribute("rows"));
        
        // Prepare a String[][] for data and a int[] for column widths
        String[][] block = new String[rows][columns];
        int[] length = new int[columns];
        for(int i = 0; i < columns; i++)
            length[i] = 1;
        
        // Get rows
        NodeList rowList = spkdata.getElementsByTagName("row");
        int start = addLabel? 0 : 1;
        if(rowList.getLength() != rows)
        {
            JOptionPane.showMessageDialog(null, "Number of row error was found in data XML."); 
            return "";
        }
        if(rowList.getLength() > 1)
        {
            for(int i = start; i < rowList.getLength(); i++)
            {
                row = (Element)rowList.item(i);
                NodeList valueList = row.getElementsByTagName("value");
                if(valueList.getLength() != columns)
                {
                    JOptionPane.showMessageDialog(null, "Number of column error was found in data XML."); 
                    return "";
                }
                if(valueList.getLength() > 0)
                {
                    for(int j = 0; j < valueList.getLength(); j++)
                    {
                        value = ((Element)valueList.item(j)).getFirstChild().getNodeValue();
                        block[i][j] = value;
                        if(value.length() > length[j]) length[j] = value.length();
                    }
                }                
            }
        }
        for(int i = start; i < rows; i++)
        {
            for(int j = 0; j < columns; j++)
            {
                data.append(formatData(length[j], block[i][j]));
            }
            data.append("\n");
        }
        
        if(addLabel)
            data.setCharAt(0, 'C');
        return data.toString();
    }
    
    private static String formatData(int length, String number)
    {
        length += 2;
        StringBuffer buffer = new StringBuffer(length);
        for(int i = 0; i < length - number.length(); i++) 
            buffer.append(" ");
        buffer.append(number);
        return buffer.toString();
    }
    
/*
    public static String parseDataXML(String dataXML, boolean addLabel)
    {
        String data = "";
        Document docData = null;
        Element row, value;
        try
        {
            // Parse the XML documents
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docData = builder.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes())));
//            DOMParser parser = new DOMParser(); 
//            parser.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes()))); 
//            docData = parser.getDocument();            
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
        
        //Get root element of spkdata
        Element spkdata = docData.getDocumentElement();  
        
        // Get rows
        NodeList rowList = spkdata.getElementsByTagName("row"); 
        if(rowList.getLength() > 1)
        {
            int start = addLabel? 0 : 1;
            for(int i = start; i < rowList.getLength(); i++)
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
                            data += " " + formatNumeric(value.getFirstChild().getNodeValue());
                    }
                } 
                data += "\n";
            }
        }
        if(addLabel)
            data = "C" + data.substring(1);
        return data;
    }
*/
    // Format the data of type string
    private static String formatString(String number)
    {
        StringBuffer stringBuffer = new StringBuffer();
        for(int i = 0; i < 12 - number.length(); i++)
            stringBuffer.append(" ");
        return stringBuffer.append(number).toString();
    }
    
    // Format the data of type numeric 
    private static String formatNumeric(String number)
    {
        if(number.equals("."))
            number += "0";
        DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        f.applyPattern("0.0000E00");
        return Utility.formatData(8, f.format(Double.parseDouble(number)));  
    }
    
    /** Get model archive text from model XML document.  The first line is removed.
     * @param model a String object containing the model XML document.
     * @return a String object containing the model archive text.
     */ 
    public static String getModelArchive(String model)
    {
        String modelArchive = null;
        try
        {
            // Parse spkmodel XML document
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document docModel = builder.parse(new InputSource(new ByteArrayInputStream(model.getBytes())));
//            DOMParser parser = new DOMParser();                
//            parser.parse(new InputSource(new ByteArrayInputStream(model.getBytes())));                  
//            Document docModel = parser.getDocument();              
            Element spkmodel = docModel.getDocumentElement();
            modelArchive = spkmodel.getFirstChild().getNodeValue();
            if(!modelArchive.equals(""))
                modelArchive = modelArchive.substring(modelArchive.indexOf('\n') + 1);
        }
        catch(ParserConfigurationException e)
        {
            JOptionPane.showMessageDialog(null, e, 
                                          "ParserConfigurationException", 
                                          JOptionPane.ERROR_MESSAGE);
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
    
    // Get data labels from source
    public static String[] getDataLabels(String source)
    {
        Document docSource;
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docSource = builder.parse(new InputSource(new ByteArrayInputStream(source.getBytes())));
//            DOMParser parser = new DOMParser();
//            parser.parse(new InputSource(new ByteArrayInputStream(source.getBytes()))); 
//            docSource = parser.getDocument();
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
        Element spksource = docSource.getDocumentElement();
        NodeList dataLabelList = spksource.getElementsByTagName("data_labels");
        Element data_labels = (Element)dataLabelList.item(0);
        NodeList labelList = data_labels.getElementsByTagName("label");
        String[] dataLabels = new String[labelList.getLength()];
        for(int i = 0; i < labelList.getLength(); i++)
        {
            Element label = (Element)labelList.item(i);
            dataLabels[i] = label.getAttribute("name");
        }
        return dataLabels;
    }
    
    /** Get required data from presentation_data.
     * @param data an XML document containing presentation_data element of a report.
     * @param dataLabels a String array containing the labels of the data to get.
     * @return a two-dimensional String array containing the requested data.
     */
    public static String[][] getData(String data, String[] dataLabels)
    {
        Document docDataAll;
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docDataAll = builder.parse(new InputSource(new ByteArrayInputStream(data.getBytes())));
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
        Element presentation_data = docDataAll.getDocumentElement();
        
        // Get data label list
        NodeList labelList = presentation_data.getElementsByTagName("label");
        int nLabels = labelList.getLength();
        
        if(nLabels == 0) return Utility.getSelectedDataItems(data, dataLabels);
        
        int nDataItem = dataLabels.length;
        int nColumns = Integer.parseInt(presentation_data.getAttribute("columns"));
        int nRows = Integer.parseInt(presentation_data.getAttribute("rows"));
        ArrayList<String> dataItems = null;
        String[][] dataAll = null;
        
        if(nColumns == 0 || nRows == 0 || nLabels != nColumns)
            return null;
        if(nColumns > 0)
        {
            dataItems = new ArrayList<String>(nColumns);
            dataAll = new String[nRows][nDataItem];
            for(int i = 0; i < nColumns; i++)
            {
                Element label = (Element)labelList.item(i);
                dataItems.add(i, label.getAttribute("name"));               
            }
        }
        NodeList rowList = presentation_data.getElementsByTagName("row");
        if(rowList.getLength() != nRows)
            return null;
        if(nRows > 0)
        {
            // Get all output data 
            for(int i = 0; i < nRows; i++)
            {
                Element dataRow = (Element)rowList.item(i);
                NodeList valueList = dataRow.getElementsByTagName("value");
                if(valueList.getLength() != nColumns)
                    return null;
                for(int j = 0; j < nDataItem; j++)
                {
                    int k = dataItems.indexOf(dataLabels[j]);
                    dataAll[i][j] = valueList.item(k).getFirstChild().getNodeValue();
                }
            }
        }
        return dataAll;
    }
    
    /** Get parameters from an XML document containing the parameters.
     * @param parameter_out a String object representing an XML document containing the parameters.
     * @param output an Output object contaaining the parameters.
     * @return true if successful, false otherwise.
     */  
    public boolean getParameters(String parameter_out, Output output)
    {
        this.output = output;
        Document docParameterAll;
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docParameterAll = builder.parse(new InputSource(new ByteArrayInputStream(parameter_out.getBytes())));
//            DOMParser parser = new DOMParser();
//            parser.parse(new InputSource(new ByteArrayInputStream(parameter_out.getBytes()))); 
//            docParameterAll = parser.getDocument();
        }
        catch(ParserConfigurationException e)
        {
            JOptionPane.showMessageDialog(null, e, "ParserConfigurationException", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e, "SAXException", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        Element parameters = docParameterAll.getDocumentElement();
        getTheta(parameters);
        getOmega(parameters);
        getSigma(parameters);
        return true;
    }
    
    // Job document, report document and source document
    private Document docJob, docReport, docSource;
    
    // Output object
    private Output output;
}
