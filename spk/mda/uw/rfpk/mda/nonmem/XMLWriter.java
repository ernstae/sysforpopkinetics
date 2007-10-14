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

import uw.rfpk.mda.nonmem.wizard.Source;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.Utility;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;
import java.util.*;
import java.io.PrintWriter;
import java.io.FileWriter;
import javax.swing.JOptionPane; 

/** This class defines an object that generates an XML SPK input file.
 *  
 * @author  Jiaji Du
 */
public class XMLWriter
{
    /** Constructor to initialize data members and then call private functions to
     * generate spksource, spkdata and spkmodel XML documents.
     * @param control a String object containing the text of the NONMEM control file.
     * @param object a MDAObject object containing control and data information.
     */    
    public XMLWriter(String control, MDAObject object)
    {
        this.source = object.getSource();
        this.data = object.getData();
        this.control = control;
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try
        {
            builder = factory.newDocumentBuilder();
        }
        catch(ParserConfigurationException e)
        {
            e.printStackTrace();
        }
        setSource(); 
        setData();
        setModel();
    }

    /** This function creates Content section of the SPK input file. */
    private void setSource()
    {
        docSource = builder.newDocument();
        Element spksource = docSource.createElement("spksource");
        docSource.appendChild(spksource);
        Element nonmem = docSource.createElement("nonmem");
        nonmem.setAttribute("version", "0.1"); 
        spksource.appendChild(nonmem);
        Element constraint = docSource.createElement("constraint");
        nonmem.appendChild(constraint);
        Element analysis = null;
        if(source.analysis.equals("population") || source.analysis.equals("two-stage") || 
           source.analysis.equals("nonparametric"))
            analysis = docSource.createElement("pop_analysis"); 
        else
            analysis = docSource.createElement("ind_analysis"); 
        if(source.estimation != null && !source.analysis.equals("identifiability"))
        {
            analysis.setAttribute("is_estimation", "yes");
            analysis.setAttribute("sig_digits", source.estimation[1]);
            analysis.setAttribute("mitr", source.estimation[2]);
            if(source.analysis.equals("population") || source.analysis.equals("two-stage") || 
               source.analysis.startsWith("nonparametric"))
            {
                if(!source.analysis.startsWith("nonparametric"))
                    analysis.setAttribute("approximation", source.estimation[0]);
                else
                {
                    analysis.setAttribute("approximation", "nonparametric");
                    Element nonparamInfo = docSource.createElement("nonparametric_info");
                    analysis.appendChild(nonparamInfo);
                    Element measurePointsIn = docSource.createElement("measure_points_in");
                    nonparamInfo.appendChild(measurePointsIn);
                    if(source.estimation[0].endsWith("uniform"))
                    {
                        measurePointsIn.setAttribute("auto_generate_method", "random_uniform");
                        measurePointsIn.setAttribute("number_of_points", source.nonparamNumOfPoints);
                        measurePointsIn.setAttribute("seed", source.nonparamSeed);
                    }
                    else if(source.estimation[0].endsWith("grid"))
                    {
                        measurePointsIn.setAttribute("auto_generate_method", "grid");
                        measurePointsIn.setAttribute("points_per_dimension", source.nonparamPointsPerDim);
                    }
                    else
                    {
                        JOptionPane.showMessageDialog(null, "Method not found", "Input Error", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                }
                if(source.analysis.equals("population"))
                {
                    if(source.estimation[0].equals("fo"))
                         analysis.setAttribute("is_eta_out", source.estimation[5]);
                    analysis.setAttribute("interaction", source.estimation[7]);
                    analysis.setAttribute("abort", source.estimation[4]);
                }
            }
        }
        else
        {
            analysis.setAttribute("is_estimation", "no");
        }
        if(source.analysis.equals("identifiability"))
            analysis.setAttribute("is_identifiability", "yes");
        if(source.analysis.equals("population") || source.analysis.equals("two-stage") || 
           source.analysis.equals("nonparametric"))
            analysis.setAttribute("pop_size", String.valueOf(data.size()));
        constraint.appendChild(analysis); 
        setDescription(analysis);
        if(source.analysis.equals("identifiability"))
        {
            Element simulation = docSource.createElement("simulation");
            simulation.setAttribute("seed", source.seed);
            analysis.appendChild(simulation);
        }
        setInput(analysis);
        if(source.analysis.equals("identifiability"))
        {
            Element theta = docSource.createElement("theta");
            theta.setAttribute("length", source.nTheta);
            analysis.appendChild(theta);
            Element omega = docSource.createElement("omega");
            omega.setAttribute("dimension", source.nEta);
            omega.setAttribute("same_as_previous", "no");
            omega.setAttribute("struct", "diagonal");
            analysis.appendChild(omega);
        }
        else
        {
            setTheta(analysis);
            setOmega(analysis);
        }
        if(source.analysis.equals("population") || source.analysis.equals("two-stage") || 
           source.analysis.equals("nonparametric"))
        {
            setSigma(analysis);
            setPop_stat(analysis);
        }
        else
        {
            setInd_stat(analysis);
        }
        setSimulation(analysis); 
        setModel(nonmem);   
        setPresentation(nonmem);
    } 

    // Generate description
    private void setDescription(Element parent)
    {
        if(source.problem != null)
        {
            Element description = docSource.createElement("description");
            parent.appendChild(description);
            description.appendChild(docSource.createTextNode(replaceCharacter(source.problem)));
        }
    }

    // Generate data_labels
    private void setInput(Element parent)
    {
        if(source.input != null && source.data != null)
        {
            Element data_labels = docSource.createElement("data_labels");
            parent.appendChild(data_labels);
            for(int i = 0; i < source.input.length; i++)
            {
                Element label = docSource.createElement("label");
                data_labels.appendChild(label);
                String[] names = source.input[i].split("=");
                label.setAttribute("name", replaceCharacter(names[0]));
                if(names.length == 2 && Utility.isStdItem(names[0])) 
                    label.setAttribute("synonym", replaceCharacter(names[1]));
            }
        }
    }
    
    // Generate theta
    private void setTheta(Element parent)
    {
        if(source.theta != null)
        {
            Element theta = docSource.createElement("theta");
            int size = source.theta.length; 
            String length = String.valueOf(size);
            theta.setAttribute("length", length );
            
            Element low = docSource.createElement("low");
            for(int i = 0; i < size; i++)
	    {
                Element value = docSource.createElement("value");
                value.setAttribute("fixed", source.theta[i][3]);
                value.appendChild(docSource.createTextNode(source.theta[i][0]));
                low.appendChild(value);
	    }
            theta.appendChild(low);            
            
            Element in = docSource.createElement("in");
            for(int i = 0; i < size; i++)
	    {            
                Element value = docSource.createElement("value");
                value.setAttribute("fixed", source.theta[i][3]);
                value.appendChild(docSource.createTextNode(source.theta[i][1]));
                in.appendChild(value);
	    }
            theta.appendChild(in);

            Element up = docSource.createElement("up");
            for(int i = 0; i < size; i++)
	    {
                Element value = docSource.createElement("value");
                value.setAttribute("fixed", source.theta[i][3]);
                value.appendChild(docSource.createTextNode(source.theta[i][2]));
                up.appendChild(value);
	    }
            theta.appendChild(up);
            parent.appendChild(theta);
        }
    }
    
    // Generate sigma
    private void setSigma(Element parent)
    {
        if(source.analysis.equals("two-stage") || source.analysis.equals("nonparametric"))
            source.sigma = source.omega;
        if(source.sigma != null)
        {
            for(int i = 0; i < source.sigma.length; i++)
	    {
                Element sigma = docSource.createElement("sigma");    
                sigma.setAttribute("struct", source.sigma[i][0]);
                sigma.setAttribute("dimension", source.sigma[i][1]);
                sigma.setAttribute("same_as_previous", source.sigma[i][2]);

                Element in = docSource.createElement("in");
                sigma.appendChild(in);
                int nData = source.sigma[i].length;
                for(int j = 3; j < nData; j++)
	        {   
                    Element value = docSource.createElement("value");
                    if(source.sigma[i][j].endsWith("F"))
	            {
                        value.setAttribute("fixed", "yes");
                        value.appendChild(docSource.createTextNode(source.sigma[i][j].substring(
                                          0, source.sigma[i][j].length() - 1)));
		    }
                    else
	            {
                        value.setAttribute("fixed", "no");
                        value.appendChild(docSource.createTextNode(source.sigma[i][j]));
		    }
                    in.appendChild(value);
                }
                parent.appendChild(sigma);
            } 
        }
    }
    
    // Generate omega
    private void setOmega(Element parent)
    {
        if(source.omega != null)
        {
            if(!source.analysis.equals("two-stage") && !source.analysis.equals("nonparametric"))
            { 
                for(int i = 0; i < source.omega.length; i++)
	        {
                    Element omega = docSource.createElement("omega");     
                    omega.setAttribute("struct", source.omega[i][0]);
                    omega.setAttribute("dimension", source.omega[i][1]);
                    omega.setAttribute("same_as_previous", source.omega[i][2]);

                    Element in = docSource.createElement("in");
                    omega.appendChild(in);
                    int nData = source.omega[i].length;
                    for(int j = 3; j < nData; j++)
	            {   
                        Element value = docSource.createElement("value");                    
                        if(source.omega[i][j].endsWith("F"))
	                {
                            value.setAttribute("fixed", "yes"); 
                            value.appendChild(docSource.createTextNode(source.omega[i][j].substring(
                                              0, source.omega[i][j].length() - 1)));
		        }
                        else
	                {
                            value.setAttribute("fixed", "no");
                            value.appendChild(docSource.createTextNode(source.omega[i][j]));
		        }                      
                        in.appendChild(value);
                    }            
                    parent.appendChild(omega);
                }
            }
            else
            {
                int size = source.theta.length; 
                String length = String.valueOf(size);
                Element omega = docSource.createElement("omega");
                omega.setAttribute("struct", "block");
                omega.setAttribute("dimension", length);
                omega.setAttribute("same_as_previous", "no");

                Element in = docSource.createElement("in");
                omega.appendChild(in);
                int l = 0;
                for(int k = 0; k < size; k++)
                {
                    for(int j = 0; j <= k; j++)
	            {   
                        Element value = docSource.createElement("value");
                        value.setAttribute("fixed", "no");
                        if(source.covTheta != null)
                        {
                            value.appendChild(docSource.createTextNode(source.covTheta[l++]));
                        }
                        else
                        {
                            if(j == k)
                                value.appendChild(docSource.createTextNode("1")); 
                            else
                                value.appendChild(docSource.createTextNode("0"));
                        }
                        in.appendChild(value);
                    }
                }
                
                parent.appendChild(omega);
            }
        }
    }

    // Generate pop_stat
    private void setPop_stat(Element parent)
    {
        if(source.analysis.equals("population") && source.covariance != null)
	{
            Element pop_stat = docSource.createElement("pop_stat");
            pop_stat.setAttribute("covariance_form", source.covariance);
            parent.appendChild(pop_stat);        
	}
    }

    // Generate ind_stat
    private void setInd_stat(Element parent)
    {
        if(source.analysis.equals("individual") && source.covariance != null)
	{        
            Element ind_stat = docSource.createElement("ind_stat");
            ind_stat.setAttribute("covariance_form", source.covariance);
            parent.appendChild(ind_stat); 
        }
    }    
    
    // Generate simulation
    private void setSimulation(Element parent)
    {
        if(source.simulation != null)
        {
            Element simulation = docSource.createElement("simulation");
            simulation.setAttribute("seed", source.simulation[0]);
            simulation.setAttribute("subproblems", source.simulation[1]);
            parent.appendChild(simulation);
        }
    }
    
    // This function creates Model section of the SPK input file
    private void setModel(Element parent)
    {
        Element model = docSource.createElement("model");
        if(source.subroutines != null)
        {
            if(!source.analysis.equals("identifiability"))
            {
                model.setAttribute("advan", source.subroutines[0].substring(5));
                if(source.subroutines[1] != null)
                    model.setAttribute("tolerance", source.subroutines[1]);
                if(source.subroutines[2] != null)
                    model.setAttribute("trans", source.subroutines[2]);
            }
            else
                model.setAttribute("advan", "6");
        }
        if(source.model != null)
        {
            Element comp_model = docSource.createElement("comp_model");
            comp_model.setAttribute("ncompartments", source.model[0][0]);
            comp_model.setAttribute("nequilibriums", source.model[0][1]);
            if(source.model[0][2] != null)
                comp_model.setAttribute("nparameters", source.model[0][2]);
            for(int i = 1; i < source.model.length; i++)
            {
                Element compartment = docSource.createElement("compartment");
                compartment.setAttribute("name", replaceCharacter(source.model[i][0]));
                compartment.setAttribute("initial_off", source.model[i][1]);
                compartment.setAttribute("no_off", source.model[i][2]);                
                compartment.setAttribute("no_dose", source.model[i][3]);
                if(source.subroutines[0].equals("advan9"))
                {
                    compartment.setAttribute("equilibrium", source.model[i][4]);
                    compartment.setAttribute("exclude", source.model[i][5]);
                }
                compartment.setAttribute("default_observation", source.model[i][6]);
                compartment.setAttribute("default_dose", source.model[i][7]);
                comp_model.appendChild(compartment);
            }
            model.appendChild(comp_model);
        }
        // Generate diffeqn
        if(source.des != null)
        {
            Element diffeqn = docSource.createElement("diffeqn"); 
            diffeqn.appendChild(docSource.createTextNode(replaceCharacter(source.des)));
            model.appendChild(diffeqn);
        }
        // Generate pk
        if(source.pk != null)
        {            
            Element pk = docSource.createElement("pk");
            pk.appendChild(docSource.createTextNode(replaceCharacter(source.pk)));
            model.appendChild(pk); 
        }
        // Generate error
        if(source.error != null)
        { 
            Element error = docSource.createElement("error");
            error.appendChild(docSource.createTextNode(replaceCharacter(source.error)));
            model.appendChild(error);
        }
        // Generate pred
        if(source.pred != null)
        {            
            Element pred = docSource.createElement("pred");
            pred.appendChild(docSource.createTextNode(replaceCharacter(source.pred)));
            model.appendChild(pred); 
        }
        parent.appendChild(model); 
    }

    // This function generates output section of the SPK input file  
    private void setPresentation(Element parent)
    {
        if(source.table != null || source.splot != null)
        {
            Element presentation = docSource.createElement("presentation");
            parent.appendChild(presentation); 
            if(source.table != null)
                setTable(presentation);
            if(source.splot != null)
                setScatterplot(presentation);
	}
    }
            
    // Generate table
    private void setTable(Element parent)
    {
        String[][][] controlTable = null;
        controlTable = source.table;
        if(controlTable != null)
        {
            for(int i = 0; i < controlTable.length; i++)
            {
                String[][] tableI = controlTable[i];
                Element table = docSource.createElement("table");
                if(tableI[0][0] != null)
                    table.setAttribute("save_as", replaceCharacter(tableI[0][0]));
                table.setAttribute("header", tableI[0][1]); 
                parent.appendChild(table);
                
                for(int j = 0; j < tableI[1].length; j++)
	        {
                    Element column = docSource.createElement("column");
                    column.setAttribute("label", replaceCharacter(tableI[1][j]));
                    column.setAttribute("appearance_order", tableI[2][j]); 
                    column.setAttribute("sort_order", tableI[3][j]);   
                    table.appendChild(column);
                }
	    }
        }
    }
    
    // Generate scatterplot
    private void setScatterplot(Element parent)
    {
        String[][][] controlSplot = null;
        controlSplot = source.splot;
        if(controlSplot != null)
	{
            for(int i = 0; i < controlSplot.length; i++)
	    {
                String[][] scatter = controlSplot[i];
                Element scatterplot = docSource.createElement("scatterplot");
                if(scatter[0][2] != null)
                    scatterplot.setAttribute("unit_slope", scatter[0][2]);
                if(scatter[0][3] != null)
                    scatterplot.setAttribute("x0_line", scatter[0][3]);
                if(scatter[0][4] != null)
                    scatterplot.setAttribute("y0_line", scatter[0][4]);
                if(scatter[0][0] != null)                    
                    scatterplot.setAttribute("begin", scatter[0][0]);
                if(scatter[0][1] != null)                    
                    scatterplot.setAttribute("end", scatter[0][1]);
                parent.appendChild(scatterplot);

                int xlength = scatter[2].length;
                for(int j = 0; j < xlength; j++)
	        {
                    Element x = docSource.createElement("x");
                    x.setAttribute("label", replaceCharacter(scatter[2][j]));
                    scatterplot.appendChild(x);
		}

                int ylength = scatter[1].length;
                for(int j = 0; j < ylength; j++)
	        {
                    Element y = docSource.createElement("y");
                    y.setAttribute("label", replaceCharacter(scatter[1][j]));
                    scatterplot.appendChild(y);
	        }
     
                if(scatter.length == 4 && scatter[3] != null)
		{
                    int splitlength = scatter[3].length;
                    for(int j = 0; j < splitlength; j++)
	            {
                        Element split = docSource.createElement("split");
                        split.setAttribute("label", replaceCharacter(scatter[3][j]));
                        scatterplot.appendChild(split);
		    }
	        }
	    }
	}
    }

    // This method creates Data section of the SPK input file.   
    private void setData()
    { 
        if(data == null || data.size() == 0 || source.input == null ||
           source.input.length == 0)
            return;
        int nInd = data.size();
        int nRows = 1;
        for(int i =0; i < nInd; i++)
            nRows += ((Vector)data.get(i)).size();
        int nColumns = source.input.length;
        docData = builder.newDocument();
        Element spkdata = docData.createElement("spkdata");
        spkdata.setAttribute("version", "0.1");
        docData.appendChild(spkdata);
        Element table = docData.createElement("table");
        table.setAttribute("columns", String.valueOf(nColumns));
        table.setAttribute("rows", String.valueOf(nRows));
        spkdata.appendChild(table);
        
        Element description = docData.createElement("description");
        description.appendChild(docData.createTextNode(replaceCharacter(source.data)));
        table.appendChild(description);

        Element row = docData.createElement("row");
        row.setAttribute("position", "1");
        table.appendChild(row);
        for(int k = 0; k < nColumns; k++)
        {
            Element value = docData.createElement("value");
            value.setAttribute("type", "string");
            value.appendChild(docData.createTextNode(source.input[k].split("=")[0]));
            row.appendChild(value);
        }
        
        int position = 2;
        for(int i = 0; i < data.size(); i++)
	{
            int size = ((Vector)data.get(i)).size();
            Vector indValue = (Vector)data.get(i);
            for(int j = 0; j < size; j++)
	    {
                row = docData.createElement("row");
                row.setAttribute("position", String.valueOf(j + position));
		table.appendChild(row);
                for(int k = 0; k < nColumns; k++)
		{
                    Element value = docData.createElement("value");
                    if(k == 0 && source.input[k].split("=")[0].equals("ID"))
                        value.setAttribute("type", "string");
                    String dataValue = ((String[])indValue.get(j))[k];
                    value.appendChild(docData.createTextNode(dataValue));
                    row.appendChild(value);
		}
	    }
            position += size;
        }
    }

    // This method generates model section of the SPK input file.    
    private void setModel()
    {    
        docModel = builder.newDocument();
        Element spkmodel = docModel.createElement("spkmodel");
        docModel.appendChild(spkmodel);
        spkmodel.appendChild(docModel.createTextNode("\n" + replaceCharacter(control)));
    }
    
    /** This method generates model section of the SPK input file.
     * @param text the model text.
     * @return a String object containing the text of the model XML document.
     */
    public static String setModel(String text)
    {   
        text = text.trim();
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try
        {
            builder = factory.newDocumentBuilder();
        }
        catch(ParserConfigurationException e)
        {
            e.printStackTrace();
        }
        Document document = builder.newDocument();
        Element spkmodel = document.createElement("spkmodel");
        document.appendChild(spkmodel);
        spkmodel.appendChild(document.createTextNode("\n" + replaceCharacter(text) + "\n"));
        return getString(document);
//        return Utility.formatXML(((DocumentImpl)document).saveXML(document));
    }
    
    /** Generate SPK output file content.
     * @param spkOutput a Properties object containing job id and SPK output data returned from database.
     * @return a String object containing the SPK output file content.
     */
    public static String setOutput(Properties spkOutput)
    {
        // Generate Job XML
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try
        {
            builder = factory.newDocumentBuilder();
        }
        catch(ParserConfigurationException e)
        {
            e.printStackTrace();
        }
        Document docJob = builder.newDocument();
        Element spkjob = docJob.createElement("spkjob");
        spkjob.setAttribute("id", spkOutput.getProperty("jobId"));
        spkjob.setAttribute("abstract", replaceCharacter(spkOutput.getProperty("jobAbstract")));
        spkjob.setAttribute("submission_time", spkOutput.getProperty("startTime"));
        spkjob.setAttribute("completion_time", spkOutput.getProperty("eventTime"));
        spkjob.setAttribute("method_code", spkOutput.getProperty("methodCode"));
        String computationMode = spkOutput.getProperty("parallel").equals("0") ? "single" : "parallel";
        spkjob.setAttribute("computation_mode", computationMode);
        Element model = docJob.createElement("model");
        Element data = docJob.createElement("data");
        model.setAttribute("name", replaceCharacter(spkOutput.getProperty("modelName")));
        model.setAttribute("version", spkOutput.getProperty("modelVersion"));
        model.setAttribute("abstract", replaceCharacter(spkOutput.getProperty("modelAbstract")));
        model.setAttribute("log", replaceCharacter(spkOutput.getProperty("modelVersionLog")));
        data.setAttribute("name", replaceCharacter(spkOutput.getProperty("datasetName")));
        data.setAttribute("version", spkOutput.getProperty("datasetVersion"));
        data.setAttribute("abstract", replaceCharacter(spkOutput.getProperty("datasetAbstract")));
        data.setAttribute("log", replaceCharacter(spkOutput.getProperty("datasetVersionLog")));
        spkjob.appendChild(model);
        spkjob.appendChild(data);
        docJob.appendChild(spkjob);
        String job = getString(docJob);
//        String job = Utility.formatXML(((DocumentImpl)docJob).saveXML(docJob));

        // Return Spk output
//        String ls = System.getProperty("line.separator");
        return job + "\n" + spkOutput.getProperty("report") + spkOutput.getProperty("source");
    }
    
    // Generate a dataset XML
    public static String setData(String[][] dataAll, String[] dataLabels, String datasetDescription)
    {
        int nRows = dataAll.length;
        int nColumns = dataAll[0].length;
        Document docData = builder.newDocument(); 
        Element spkdata = docData.createElement("spkdata");
        spkdata.setAttribute("version", "0.1");
        docData.appendChild(spkdata);
        Element table = docData.createElement("table");
        table.setAttribute("columns", String.valueOf(nColumns));
        table.setAttribute("rows", String.valueOf(nRows + 1));
        spkdata.appendChild(table);
        Element description = docData.createElement("description");
        description.appendChild(docData.createTextNode(replaceCharacter(datasetDescription)));
        table.appendChild(description);
        Element row = docData.createElement("row");
        row.setAttribute("position", "1");
        table.appendChild(row);
        for(int k = 0; k < nColumns; k++)
        {
            Element value = docData.createElement("value");
            value.setAttribute("type", "string");
            value.appendChild(docData.createTextNode(dataLabels[k]));
            row.appendChild(value);
        }
        for(int j = 0; j < nRows; j++)
	{                
            row = docData.createElement("row");
            row.setAttribute("position", String.valueOf(j + 2));
	    table.appendChild(row); 
            for(int k = 0; k < nColumns; k++)
            {
                Element value = docData.createElement("value");
                if(k == 0 && dataLabels[k].equals("ID"))
                    value.setAttribute("type", "string");
                value.appendChild(docData.createTextNode(dataAll[j][k]));
                row.appendChild(value);
            }
        }
        return getString(docData);
//        return Utility.formatXML(((DocumentImpl)docData).saveXML(docData));
    }    
    /** This method saves the XML document as a text file in XML format.
     * @param file a String object as the filename of the file to be saved.
     */    
    public void save(String file)
    {
        try
	{
            PrintWriter writer = new PrintWriter(new FileWriter(file));
//            String ls = System.getProperty("line.separator");
            writer.println(getString(docSource) + "\n" + getString(docData) + "\n" + getString(docModel));
//            writer.println(Utility.formatXML(((DocumentImpl)docSource).saveXML(docSource)) + ls + 
//                           Utility.formatXML(((DocumentImpl)docData).saveXML(docData)) + ls + 
//                           Utility.formatXML(((DocumentImpl)docModel).saveXML(docModel))); 
            writer.close();
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Error saving file.",  // Display saving file
                                          "File Error",                // error message
                                          JOptionPane.ERROR_MESSAGE);
	}    
    }
    
    /** This method returns the XML documents, spksource, spkdata and spkarchibe 
     * as a String object.
     * @return a String object as the formated XML file content.
      */    
    public String getDocument()
    {
        return getString(docSource) + "\n" + getString(docData) + "\n" + getString(docModel);
//        String ls = System.getProperty("line.separator");
//        return Utility.formatXML(((DocumentImpl)docSource).saveXML(docSource)) + ls +
//               Utility.formatXML(((DocumentImpl)docData).saveXML(docData)) + ls +
//               Utility.formatXML(((DocumentImpl)docModel).saveXML(docModel));
    }
    
    private boolean exist(String[] strings, String string)
    {
        for(int i = 0; i < strings.length; i++)
            if(string.equals(strings[i]))
                return true;
        return false;
    }
    
    private static String replaceCharacter(String text)
    {
        text = text.replaceAll("<", "&lt;");
        text = text.replaceAll("&", "&amp;");
        text = text.replaceAll(">", "&gt;");
        text = text.replaceAll("\"", "&quot;");
        text = text.replaceAll("'", "&apos;");
        return text;
    }
    
    private static String getString(Document document)
    {
        java.io.StringWriter writer = new java.io.StringWriter();
        try
        {
            Transformer t = TransformerFactory.newInstance().newTransformer();
            t.transform(new DOMSource(document), new StreamResult(writer));
        }
        catch(TransformerConfigurationException e){}
        catch(TransformerException e){};
        return Utility.formatXML(writer.toString());
    }
    
    // XML document builder
    private static DocumentBuilder builder;
    
    // XML documents
    private Document docSource, docData, docModel;

    // Source object
    private Source source;
    
    // Data vector
    private Vector data;
    
    // Control file text
    private String control;
}
