package uw.rfpk.mda.nonmem;

import uw.rfpk.mda.nonmem.wizard.Source;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.Utility;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.apache.xerces.dom.DocumentImpl;   
import java.util.*;
import java.io.PrintWriter;
import java.io.FileWriter;
import javax.swing.JOptionPane; 

/** This class defines an object that generates an XML SPK input file.
 *  
 * @author  Jiaji Du
 * @version 1.0
 */
public class XMLWriter
{
    /** Constructor to initialize data members and then call private functions to
     * generate spksource, spkdata and spkmodel XML documents.
     * @param modelArchive An Archive object containing information for the model.
     * @param dataArchive An Archive object containing information for the data.
     * @param control A String object containing the text of the NONMEM control file.
     * @param object A MDAObject object containing control and data information.
     */    
    public XMLWriter(ArchiveInfo modelArchive, ArchiveInfo dataArchive, String control, MDAObject object)
    {
        this.source = object.getSource();
        this.data = object.getData();
        this.modelArchive = modelArchive;
        this.dataArchive = dataArchive;
        this.control = control;
        setSource(); 
        setData();
        setModel();
    }

    /** This function creates Content section of the SPK input file. */    
    private void setSource()
    {
        docSource = new DocumentImpl(); 
        Element spksource = docSource.createElement("spksource");  
        docSource.appendChild(spksource);  
        Element nonmem = docSource.createElement("nonmem"); 
        nonmem.setAttribute("version", "0.1"); 
        spksource.appendChild(nonmem);
        Element constraint = docSource.createElement("constraint");
        nonmem.appendChild(constraint);
        Element analysis = null;
        if(source.analysis.equals("population"))
            analysis = docSource.createElement("pop_analysis"); 
        else
            analysis = docSource.createElement("ind_analysis"); 
        if(source.estimation != null)
        {
            analysis.setAttribute("is_estimation", "yes");
            analysis.setAttribute("sig_digits", source.estimation[1]);
            String restart = source.isRestart ? "yes" : "no";
            analysis.setAttribute("is_restart", restart);
            analysis.setAttribute("mitr", source.estimation[2]);
            if(source.analysis.equals("population"))
            {
                analysis.setAttribute("approximation", source.estimation[0]);
                if(source.estimation[0].equals("fo"))
                    analysis.setAttribute("is_eta_out", source.estimation[5]);
                analysis.setAttribute("pop_size", String.valueOf(data.size()));
                analysis.setAttribute("interaction", source.estimation[7]);
            }
            analysis.setAttribute("abort", source.estimation[4]);
        }
        else
        {
            analysis.setAttribute("is_estimation", "no");
        }
        constraint.appendChild(analysis); 
        setDescription(analysis);
        setInput(analysis);
        setTheta(analysis);
        setOmega(analysis);
        if(source.analysis.equals("population"))
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
            description.appendChild(docSource.createTextNode(source.problem));
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
                String name = names[0];
                label.setAttribute("name", name);
                if(names.length == 2 && Utility.isStdItem(names[0])) 
                    label.setAttribute("synonym", names[1]);
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
        if(source.sigma != null)
        {
            for(int i = 0; i < source.sigma.length; i++)
	    {
                Element sigma = docSource.createElement("sigma");    
                sigma.setAttribute("struct", source.sigma[i][0]);
                sigma.setAttribute("dimension", source.sigma[i][1]);
                if(source.sigma[i][2].equals("SAME"))
                    sigma.setAttribute("same_as_previous", "yes");
                else
                    sigma.setAttribute("same_as_previous", "no");
                Element in = docSource.createElement("in");
                sigma.appendChild(in);
                int nData = source.sigma[i].length;
                for(int j = 2; j < nData; j++)
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
            for(int i = 0; i < source.omega.length; i++)
	    {
                Element omega = docSource.createElement("omega");     
                omega.setAttribute("struct", source.omega[i][0]);
                omega.setAttribute("dimension", source.omega[i][1]);
                if(source.omega[i][2].equals("SAME"))
                    omega.setAttribute("same_as_previous", "yes");
                else
                    omega.setAttribute("same_as_previous", "no");
                Element in = docSource.createElement("in");
                omega.appendChild(in);
                int nData = source.omega[i].length;
                for(int j = 2; j < nData; j++)
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
            simulation.setAttribute("onlysimulation", source.simulation[1]);
            simulation.setAttribute("subproblems", source.simulation[2]);
            parent.appendChild(simulation);
        }
    }
    
    // This function creates Model section of the SPK input file
    private void setModel(Element parent)
    {
        Element model = docSource.createElement("model");
        if(source.subroutines != null)
        {
            model.setAttribute("advan", source.subroutines[0]);
            if(source.subroutines[1] != null)
                model.setAttribute("tolerance", source.subroutines[1]);
            if(source.subroutines[2] != null)
                model.setAttribute("trans", source.subroutines[2]);
        }
        if(source.model != null)
        {
            Element comp_model = docSource.createElement("comp_model");
            comp_model.setAttribute("ncompartments", source.model[0][0]);
            comp_model.setAttribute("nequilibrium", source.model[0][1]);
            if(source.model[0][2] != null)
                comp_model.setAttribute("nparameters", source.model[0][2]);
            for(int i = 1; i < source.model.length; i++)
            {
                Element compartment = docSource.createElement("compartment");
                compartment.setAttribute("name", source.model[i][0]);
                compartment.setAttribute("initial_off", source.model[i][1]);
                compartment.setAttribute("no_off", source.model[i][2]);                
                compartment.setAttribute("no_dose", source.model[i][3]);
                compartment.setAttribute("equilibrium", source.model[i][4]);
                compartment.setAttribute("exclude", source.model[i][5]);
                compartment.setAttribute("def_observation", source.model[i][6]);
                compartment.setAttribute("def_dose", source.model[i][7]);
                comp_model.appendChild(compartment);
            }
            model.appendChild(comp_model);
        }
        // Generate diffeqn
        if(source.des != null)
        {
            Element diffeqn = docSource.createElement("diffeqn"); 
            diffeqn.appendChild(docSource.createTextNode(source.des));
            model.appendChild(diffeqn);
        }
        // Generate pk
        if(source.pk != null)
        {            
            Element pk = docSource.createElement("pk");
            pk.appendChild(docSource.createTextNode(source.pk));
            model.appendChild(pk); 
        }
        // Generate error
        if(source.error != null)
        { 
            Element error = docSource.createElement("error");
            error.appendChild(docSource.createTextNode(source.error));
            model.appendChild(error);
        }
        // Generate pred
        if(source.pred != null)
        {            
            Element pred = docSource.createElement("pred");
            pred.appendChild(docSource.createTextNode(source.pred));
            model.appendChild(pred); 
        }
        parent.appendChild(model); 
    }

    // This function generates output section of the SPK input file  
    private void setPresentation(Element parent)
    {
        if(source.tableEst != null || source.splotEst != null ||
           source.tableSim != null || source.splotSim != null)
        {
            Element presentation = docSource.createElement("presentation");
            parent.appendChild(presentation); 
            if(source.tableEst != null)
                setTable(presentation, true);
            if(source.tableSim != null)
                setTable(presentation, false);
            if(source.splotEst != null)
                setScatterplot(presentation, true);
            if(source.splotSim != null)
                setScatterplot(presentation, false);
	}
    }
            
    // Generate table
    private void setTable(Element parent, boolean isEstimation)
    {
        String[][][] controlTable = null;
        if(isEstimation)
            controlTable = source.tableEst;
        else
            controlTable = source.tableSim;
        if(controlTable != null)
        {
            for(int i = 0; i < controlTable.length; i++)
            {
                String[][] tableI = controlTable[i];
                Element table = docSource.createElement("table");
                if(isEstimation)
                    table.setAttribute("process", "estimation");
                else
                    table.setAttribute("process", "simulation");
                if(tableI[0][0] != null)
                    table.setAttribute("save_as", tableI[0][0]);
                table.setAttribute("header", tableI[0][1]); 
                parent.appendChild(table);
                
                for(int j = 0; j < tableI[1].length; j++)
	        {
                    Element column = docSource.createElement("column");
                    column.setAttribute("label", tableI[1][j]);
                    column.setAttribute("appearance_order", tableI[2][j]); 
                    column.setAttribute("sort_order", tableI[3][j]);   
                    table.appendChild(column);
                }
	    }
        }
    }
    
    // Generate scatterplot
    private void setScatterplot(Element parent, boolean isEstimation)
    {
        String[][][] controlSplot = null;
        if(isEstimation)
            controlSplot = source.splotEst;
        else
            controlSplot = source.splotSim;        
        if(controlSplot != null)
	{
            for(int i = 0; i < controlSplot.length; i++)
	    {
                String[][] scatter = controlSplot[i];
                Element scatterplot = docSource.createElement("scatterplot");
                if(isEstimation)
                    scatterplot.setAttribute("process", "estimation");
                else
                    scatterplot.setAttribute("process", "simulation");
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

                int xlength = scatter[1].length;
                for(int j = 0; j < xlength; j++)
	        {
                    Element x = docSource.createElement("x");
                    x.setAttribute("label", scatter[1][j]);
                    scatterplot.appendChild(x);
		}

                int ylength = scatter[2].length;
                for(int j = 0; j < ylength; j++)
	        {
                    Element y = docSource.createElement("y");
                    y.setAttribute("label", scatter[2][j]);
                    scatterplot.appendChild(y);
	        }
     
                if(scatter.length == 4 && scatter[3] != null)
		{
                    int splitlength = scatter[3].length;
                    for(int j = 0; j < splitlength; j++)
	            {
                        Element split = docSource.createElement("split");
                        split.setAttribute("label", scatter[3][j]);
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
        docData = new DocumentImpl(); 
        Element spkdata = docData.createElement("spkdata");
        spkdata.setAttribute("version", "0.1");
        docData.appendChild(spkdata);         
        Element table = docData.createElement("table");
        table.setAttribute("columns", String.valueOf(nColumns));
        table.setAttribute("rows", String.valueOf(nRows));        
        spkdata.appendChild(table);
        
        Element description = docData.createElement("description");
        description.appendChild(docData.createTextNode(source.data));
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
        docModel = new DocumentImpl();  
        Element spkmodel = docModel.createElement("spkmodel"); 
        docModel.appendChild(spkmodel);
        spkmodel.appendChild(docModel.createTextNode(System.getProperty("line.separator") + control));        
    }    
    
    /** This method generates model section of the SPK input file.
     * @param text The model text.
     * @ return A String object containing the text of the model XML document.
     */
    public static String setModel(String text)
    {    
        Document document = new DocumentImpl();  
        Element spkmodel = document.createElement("spkmodel"); 
        document.appendChild(spkmodel);
        spkmodel.appendChild(document.createTextNode(System.getProperty("line.separator") + text));
        return Utility.formatXML(((DocumentImpl)document).saveXML(document));      
    }
    
    /** Generate SPK output file content.
     * @param spkOutput A Properties object containing SPK output data returned from database.
     * @return A String object containing the SPK output file content.
     */
    public static String setOutput(Properties spkOutput)
    {
        // Generate Job XML
        Document docJob = new DocumentImpl();
        Element spkjob = docJob.createElement("spkjob");
        spkjob.setAttribute("abstract", spkOutput.getProperty("jobAbstract")); 
        spkjob.setAttribute("submission_time", spkOutput.getProperty("startTime"));
        spkjob.setAttribute("completion_time", spkOutput.getProperty("eventTime"));          
        Element model = docJob.createElement("model");
        Element data = docJob.createElement("data");
        model.setAttribute("name", spkOutput.getProperty("modelName"));
        model.setAttribute("version", spkOutput.getProperty("modelVersion"));
        model.setAttribute("abstract", spkOutput.getProperty("modelAbstract")); 
        data.setAttribute("name", spkOutput.getProperty("datasetName"));
        data.setAttribute("version", spkOutput.getProperty("datasetVersion"));
        data.setAttribute("abstract", spkOutput.getProperty("datasetAbstract"));
        spkjob.appendChild(model);
        spkjob.appendChild(data);
        docJob.appendChild(spkjob);
        String job = Utility.formatXML(((DocumentImpl)docJob).saveXML(docJob));                 

        // Return Spk output
        String ls = System.getProperty("line.separator");
        return job + ls + spkOutput.getProperty("report") + spkOutput.getProperty("source");        
    }
    
    /** This method saves the XML document as a text file in XML format.
     * @param file A String object as the filename of the file to be saved.
     */    
    public void save(String file)
    {
        try
	{
            PrintWriter writer = new PrintWriter(new FileWriter(file));
            String ls = System.getProperty("line.separator");
            writer.println(Utility.formatXML(((DocumentImpl)docSource).saveXML(docSource)) + ls + 
                           Utility.formatXML(((DocumentImpl)docData).saveXML(docData)) + ls + 
                           Utility.formatXML(((DocumentImpl)docModel).saveXML(docModel))); 
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
     * @return A String object as the formated XML file content.
      */    
    public String getDocument()
    {
        String ls = System.getProperty("line.separator");
        return Utility.formatXML(((DocumentImpl)docSource).saveXML(docSource)) + ls +
               Utility.formatXML(((DocumentImpl)docData).saveXML(docData)) + ls +
               Utility.formatXML(((DocumentImpl)docModel).saveXML(docModel));      
    }
    
    private boolean exist(String[] strings, String string)
    {
        for(int i = 0; i < strings.length; i++)
            if(string.equals(strings[i]))
                return true;
        return false;
    }
    
    // XML documents
    private Document docSource, docData, docModel;

    // Source object
    private Source source;
    
    // Data vector
    private Vector data;
    
    // Model archive information
    private ArchiveInfo modelArchive;
    
    // Data archive information
    private ArchiveInfo dataArchive;  
    
    // Control file text
    private String control;
}
