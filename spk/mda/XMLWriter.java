package uw.rfpk.mda.nonmem;

import uw.rfpk.mda.nonmem.wizard.Control;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.apache.xerces.dom.DocumentImpl;   
import java.util.*;
import java.io.PrintWriter;
import java.io.FileWriter;
import javax.swing.JOptionPane; 

/**
 * This class defines an object that generates an XML SPk input file. 
 * @author  Jiaji Du
 * @version 1.0
 */
public class XMLWriter
{
    /** Constructor to initialize control and data, and create a root document
     * @param control Control class instance
     * @param data A Vector that contains the parsed NONMEM input data
     */    
    public XMLWriter(Control control, Vector data)
    {
        this.control = control;                      
        this.data = data; 
        doc = new DocumentImpl(); 
        Element root = doc.createElement("spkinml");  
        doc.appendChild(root);                      
    }

    /** This function creates Content section of the SPK input file */    
    public void setGeneral()
    {
        Element root = doc.getDocumentElement();
        Element content = doc.createElement("content");
        content.setAttribute("spkinml_ver", "1.0");
        content.setAttribute("client", "nonmem");
        if(control.estimation != null)
            content.setAttribute("estimation", "yes"); 
        else
            content.setAttribute("estimation", "no");
        if(control.simulation != null)
            content.setAttribute("simulation", "yes"); 
        else
            content.setAttribute("simulation", "no");
        content.setAttribute("analysis", control.analysis);         
        content.appendChild(doc.createTextNode(control.problem)); 
        root.appendChild(content);
    }
   
    /** This function creates Driver section of the SPK input file */ 
    public void setDriver()
    {
        Element root = doc.getDocumentElement();
        Element driver = doc.createElement("driver");
        root.appendChild(driver);

        // Generate theta
        if(control.theta != null)
        {
            Element theta = doc.createElement("theta");
            int size = control.theta.length; 
            String length = String.valueOf(size);
            theta.setAttribute("length", length );
            Element in = doc.createElement("in");
            for(int i = 0; i < size; i++)
	    {            
                Element value = doc.createElement("value");
                value.setAttribute("fixed", control.theta[i][3]);
                value.appendChild(doc.createTextNode(control.theta[i][1]));
                in.appendChild(value);
	    }
            theta.appendChild(in);
            Element low = doc.createElement("low");
            for(int i = 0; i < size; i++)
	    {
                Element value = doc.createElement("value");
                value.setAttribute("fixed", control.theta[i][3]);
                value.appendChild(doc.createTextNode(control.theta[i][0]));
                low.appendChild(value);
	    }
            theta.appendChild(low);
            Element up = doc.createElement("up");
            for(int i = 0; i < size; i++)
	    {
                Element value = doc.createElement("value");
                value.setAttribute("fixed", control.theta[i][3]);
                value.appendChild(doc.createTextNode(control.theta[i][2]));
                up.appendChild(value);
	    }
            theta.appendChild(up);
            driver.appendChild(theta);
        }
	
        // Generate sigma
        if(control.sigma != null)
        {
            for(int i = 0; i < control.sigma.length; i++)
	    {
                Element sigma = doc.createElement("sigma");    
                sigma.setAttribute("struct", control.sigma[i][0]);
                sigma.setAttribute("span", control.sigma[i][1]);
                if(control.sigma[i][2].equals("SAME"))
                    sigma.setAttribute("same_as_previous", "yes");
                else
                    sigma.setAttribute("same_as_previous", "no");
                Element in = doc.createElement("in");
                sigma.appendChild(in);
                int nData = control.sigma[i].length;
                for(int j = 2; j < nData; j++)
	        {   
                    Element value = doc.createElement("value");
                    if(control.sigma[i][j].endsWith("F"))
		    {
                        value.setAttribute("fixed", "yes");
                        value.appendChild(doc.createTextNode(control.sigma[i][j].substring(
                                          0, control.sigma[i][j].length() - 1)));
		    }
                    else
	            {
                        value.setAttribute("fixed", "no");
                        value.appendChild(doc.createTextNode(control.sigma[i][j]));
		    }
                    in.appendChild(value);
                }
                driver.appendChild(sigma);
            } 
        }

        // Generate omega
        if(control.omega != null)
        {
            for(int i = 0; i < control.omega.length; i++)
	    {
                Element omega = doc.createElement("omega");     
                omega.setAttribute("struct", control.omega[i][0]);
                omega.setAttribute("span", control.omega[i][1]);
                if(control.omega[i][2].equals("SAME"))
                    omega.setAttribute("same_as_previous", "yes");
                else
                    omega.setAttribute("same_as_previous", "no");
                Element in = doc.createElement("in");
                omega.appendChild(in);
                int nData = control.omega[i].length;
                for(int j = 2; j < nData; j++)
	        {   
                    Element value = doc.createElement("value");
                    if(control.omega[i][j].endsWith("F"))
		    {
                        value.setAttribute("fixed", "yes"); 
                        value.appendChild(doc.createTextNode(control.omega[i][j].substring(
                                          0, control.omega[i][j].length() - 1)));
		    }
                    else
	            {
                        value.setAttribute("fixed", "no");
                        value.appendChild(doc.createTextNode(control.omega[i][j]));
		    }
                    in.appendChild(value);
                }
                driver.appendChild(omega);
            }
        }

        // Generate eta
        if(control.omega != null)
        {
            Element eta = doc.createElement("eta");
            int nOmega = control.omega.length;
            int size = 0;
            for(int i = 0; i < nOmega; i++)
            {
                if(!control.omega[i][2].equals("SAME"))
                    size += Integer.parseInt(control.omega[i][1]);
            }
            eta.setAttribute("length", String.valueOf(size * nOmega));
            Element in = doc.createElement("in");
            for(int i = 0; i < size; i++)
	    {
                Element value = doc.createElement("value");
                value.appendChild(doc.createTextNode("0.0"));
                in.appendChild(value);            
	    }
            eta.appendChild(in);  
            driver.appendChild(eta);  
        }

        // Generate pop_opt
        if(control.analysis.equals("population") && control.estimation != null)
        {
            Element pop_opt = doc.createElement("pop_opt");
            pop_opt.setAttribute("approximation", control.estimation[0]);
            pop_opt.setAttribute("pop_size", String.valueOf(data.size()));
            pop_opt.setAttribute("epsilon", control.estimation[1]);
            pop_opt.setAttribute("mitr", control.estimation[2]);
            pop_opt.setAttribute("trace", control.estimation[3]);
            pop_opt.setAttribute("restart", control.estimation[4]);
            pop_opt.setAttribute("par_out", "yes");
            pop_opt.setAttribute("obj_out", "yes");
            pop_opt.setAttribute("derive1_out", "yes");
            pop_opt.setAttribute("derive2_out", "yes");
            driver.appendChild(pop_opt); 
        }

        // Generate pop_sim
        if(control.analysis.equals("population") && control.simulation != null)
        {
            Element pop_sim = doc.createElement("pop_sim");
            pop_sim.setAttribute("seed", control.simulation);
            driver.appendChild(pop_sim);
        }
        
        // Generate ind_opt
        if(control.analysis.equals("individual") && control.estimation != null)
        {        
            Element ind_opt = doc.createElement("ind_opt");
            ind_opt.setAttribute("epsilon", "");
            ind_opt.setAttribute("mitr", "");
            ind_opt.setAttribute("trace", "");
            ind_opt.setAttribute("restart", "");
            ind_opt.setAttribute("par_out", control.estimation[5]);
            ind_opt.setAttribute("obj_out", control.estimation[5]);
            ind_opt.setAttribute("derive1_out", control.estimation[5]);
            ind_opt.setAttribute("derive2_out", control.estimation[5]);
            driver.appendChild(ind_opt); 
        }

        // Generate ind_sim
        if(control.analysis.equals("individual") && control.simulation != null)
        {
            Element ind_sim = doc.createElement("ind_sim");
            ind_sim.setAttribute("seed", control.simulation);
            driver.appendChild(ind_sim);
        }
        
        // Generate pop_stat
        if(control.analysis.equals("population") && control.covariance != null)
	{
            Element pop_stat = doc.createElement("pop_stat");
            pop_stat.setAttribute("formulation", control.covariance);
            pop_stat.setAttribute("covariance", "yes");
            pop_stat.setAttribute("stderror", "yes");
            pop_stat.setAttribute("correlation", "yes");
            pop_stat.setAttribute("coefficient", "yes");
            pop_stat.setAttribute("confidence", "yes");
            driver.appendChild(pop_stat);        
	}

        // Generate ind_stat
        if(control.analysis.equals("individual") && control.covariance != null)
	{        
            Element ind_stat = doc.createElement("ind_stat");
            ind_stat.setAttribute("covariance", "covariance");
            ind_stat.setAttribute("stderror", "stderror");
            ind_stat.setAttribute("correlation", "correlation");
            ind_stat.setAttribute("coefficient", "coefficient");
            ind_stat.setAttribute("confidence", "confidence");
            driver.appendChild(ind_stat); 
        }
    }

    /** This function creates Model section of the SPK input file */ 
    public void setModel()
    {
        Element root = doc.getDocumentElement();
        Element model = doc.createElement("model");
        if(control.subroutines != null)
        {
            model.setAttribute("base", control.subroutines[0]);
            if(control.subroutines[1] != null)
                model.setAttribute("tolerance", control.subroutines[1]);
            if(control.subroutines[2] != null)
                model.setAttribute("trans", control.subroutines[2]);
        }
        if(control.model != null)
        {
            Element comp_model = doc.createElement("comp_model");
            comp_model.setAttribute("nequilibrium", control.model[0][1]);
            comp_model.setAttribute("nparameters", control.model[0][2]);
            for(int i = 1; i < control.model.length; i++)
            {
                Element compartment = doc.createElement("compartment");
                compartment.setAttribute("name", control.model[i][0]);
                compartment.setAttribute("initial_off", control.model[i][1]);
                compartment.setAttribute("no_off", control.model[i][2]);                
                compartment.setAttribute("no_dose", control.model[i][3]);
                compartment.setAttribute("equilibrium", control.model[i][4]);
                compartment.setAttribute("exclude", control.model[i][5]);
                compartment.setAttribute("def_observation", control.model[i][6]);
                compartment.setAttribute("def_dose", control.model[i][7]);
                comp_model.appendChild(compartment);
            }
            model.appendChild(comp_model);
        }
        // Generate diffeqn
        if(control.des != null)
        {
            Element diffeqn = doc.createElement("diffeqn"); 
            diffeqn.appendChild(doc.createTextNode(control.des));
            model.appendChild(diffeqn);
        }
        // Generate pk
        if(control.pk != null)
        {            
            Element pk = doc.createElement("pk");
            pk.appendChild(doc.createTextNode(control.pk));
            model.appendChild(pk); 
        }
        // Generate error
        if(control.error != null)
        { 
            Element error = doc.createElement("error");
            error.appendChild(doc.createTextNode(control.error));
            model.appendChild(error);
        }
        // Generate pred
        if(control.pred != null)
        {            
            Element pred = doc.createElement("pred");
            pred.appendChild(doc.createTextNode(control.pred));
            model.appendChild(pred); 
        }
        root.appendChild(model); 
    }

    /** This function creates Data section of the SPK input file */     
    public void setData()
    { 
        if(data == null || data.size() == 0 || control.input == null || 
           control.input.length == 0) 
            return;  
        Element root = doc.getDocumentElement();
        Element dataElement = doc.createElement("data");
        root.appendChild(dataElement);
        String[] stdItems = new String[] { "DV", "MDV", "EVID", "TIME", "DATE", 
                                           "DATE1", "DATE2", "DATE3", "AMT", 
                                           "RATE", "SS", "ADDL", "II", "ABS", 
                                           "LAG", "UPPER", "LOWER", "L1", "L2", 
                                           "CMT", "PCMT", "CALL", "CONT" };
                                           
        // Generate individuals
        int nColumns = control.input.length;                                    
        double[] mean = new double[nColumns];
        for(int i = 0; i < nColumns; i++)
            mean[i] = 0.0;  
        for(int i = 0; i < data.size(); i++)
	{
            int size = ((Vector)data.get(i)).size();
            Element individual = doc.createElement("individual");
            individual.setAttribute("order", String.valueOf(i));
            individual.setAttribute("id", ((String[])((Vector)data.get(i)).get(0))[0]);
            individual.setAttribute("length", String.valueOf(size));
            dataElement.appendChild(individual);
            Vector indValue = (Vector)data.get(i);
             
            for(int j = 1; j < nColumns; j++)
	    {                
                Element item = doc.createElement("item");
                String dataItem = control.input[j]; 
                String[] items = dataItem.split("=");
                for(int k = 0; k < items.length; k++)
                    items[k] = items[k].trim();
                if(items[0].equals("DROP") || items[0].equals("SKIP"))
                    continue;
                if(items.length == 2 && (items[1].equals("DROP") || items[1].equals("SKIP")))
                    continue;
                item.setAttribute("label", items[0]);
                if(items.length == 2) 
                {
                    if(exist(stdItems, items[0]))
                    {
		        item.setAttribute("synonym", items[1]);
                    }
                    else
                    {
                        if(items[1].equals("CENTERED"))
                        {
                            if(mean[j] == 0.0)
                            {
                                String[] numbers = new String[data.size()];
                                for(int k = 0; k < data.size(); k++)
		                {
                                    numbers[k] = ((String[])((Vector)data.get(k)).get(0))[j];  
                                    mean[j] += Double.parseDouble(numbers[k]);
                                }
                                mean[j] /= data.size();
                            }
                            double number = Double.parseDouble(((String[])indValue.get(0))[j]); 
                            ((String[])indValue.get(0))[j] = String.valueOf(number - mean[j]);
                        }
                        if(items[1].equals("CAT"))
                        {
                        
                        }
                    }
                }
		individual.appendChild(item); 
                for(int k = 0; k < size; k++)
		{
                    Element value = doc.createElement("value");
                    item.appendChild(value);
                    String dataValue = ((String[])indValue.get(k))[j];
                    value.appendChild(doc.createTextNode(dataValue));
		}
	    }
        }
    }

    /** This function creates Output section of the SPK input file */     
    public void setPresentation()
    {
        if(control.tableEst != null || control.splotEst != null ||
           control.tableSim != null || control.splotSim != null)
        {
            Element root = doc.getDocumentElement();
            Element presentation = doc.createElement("presentation");
            root.appendChild(presentation); 
	
            // Generate table for estimation
            if(control.tableEst != null)
	    {
                for(int i = 0; i < control.tableEst.length; i++)
	        {
                    String[][] tableI = control.tableEst[i];
                    Element table = doc.createElement("table");
                    if(tableI[0][0] != null)
                        table.setAttribute("save_as", tableI[0][0]);
                    table.setAttribute("header", tableI[0][1]); 
                    presentation.appendChild(table);

                    for(int j = 0; j < tableI[1].length; j++)
	            {
                        Element column = doc.createElement("column");
                        column.setAttribute("label", tableI[1][j]);
                        column.setAttribute("appearance_order", tableI[2][j]);                        
                        column.setAttribute("sort_order", tableI[3][j]);   
                        table.appendChild(column);
	            }
	        }
	    }
	
            // Generate scatterplot for estimation
            if(control.splotEst != null)
	    {
                for(int i = 0; i < control.splotEst.length; i++)
	        {
                    String[][] scatter = control.splotEst[i];
                    Element scatterplot = doc.createElement("scatterplot");
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
                    presentation.appendChild(scatterplot);

                    int xlength = scatter[1].length;
                    for(int j = 0; j < xlength; j++)
	            {
                        Element x = doc.createElement("x");
                        x.setAttribute("label", scatter[1][j]);
                        scatterplot.appendChild(x);
		    }

                    int ylength = scatter[2].length;
                    for(int j = 0; j < ylength; j++)
	            {
                        Element y = doc.createElement("y");
                        y.setAttribute("label", scatter[2][j]);
                        scatterplot.appendChild(y);
	            }
     
                    if(scatter.length == 4)
		    {
                        int splitlength = scatter[3].length;
                        for(int j = 0; j < splitlength; j++)
	                {
                            Element split = doc.createElement("split");
                            split.setAttribute("label", scatter[3][j]);
                            scatterplot.appendChild(split);
			}
		    }
	        }
	    }
            
            // Generate table for simulation
            if(control.tableSim != null)
	    {
                for(int i = 0; i < control.tableSim.length; i++)
	        {
                    String[][] tableI = control.tableEst[i];
                    Element table = doc.createElement("table");
                    if(tableI[0][0] != null)
                        table.setAttribute("save_as", tableI[0][0]);
                    table.setAttribute("header", tableI[0][1]); 
                    presentation.appendChild(table);

                    for(int j = 0; j < tableI[1].length; j++)
	            {
                        Element column = doc.createElement("column");
                        column.setAttribute("label", tableI[1][j]);
                        column.setAttribute("appearance_order", tableI[2][j]);                        
                        column.setAttribute("sort_order", tableI[3][j]);   
                        table.appendChild(column);
	            }
	        }
	    }
	
            // Generate scatterplot for simulation
            if(control.splotSim != null)
	    {
                for(int i = 0; i < control.splotSim.length; i++)
	        {
                    String[][] scatter = control.splotSim[i];
                    Element scatterplot = doc.createElement("scatterplot");
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
                    presentation.appendChild(scatterplot);

                    int xlength = scatter[1].length;
                    for(int j = 0; j < xlength; j++)
	            {
                        Element x = doc.createElement("x");
                        x.setAttribute("label", scatter[1][j]);
                        scatterplot.appendChild(x);
		    }

                    int ylength = scatter[2].length;
                    for(int j = 0; j < ylength; j++)
	            {
                        Element y = doc.createElement("y");
                        y.setAttribute("label", scatter[2][j]);
                        scatterplot.appendChild(y);
	            }
     
                    if(scatter.length == 4)
		    {
                        int splitlength = scatter[3].length;
                        for(int j = 0; j < splitlength; j++)
	                {
                            Element split = doc.createElement("split");
                            split.setAttribute("label", scatter[3][j]);
                            scatterplot.appendChild(split);
			}
		    }
	        }
	    }                        
	}
    }

    /** This function saves the XML document as a text file in XML format
     * @param file A String object as the filename of the file to be saved
     */    
    public void save(String file)
    {
        try
	{
            PrintWriter writer = new PrintWriter(new FileWriter(file));
            writer.println(formatXML(((DocumentImpl)doc).saveXML(doc)));
            writer.close();
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Error saving file",  // Display saving file
                                          "File Error",               // error message
                                          JOptionPane.ERROR_MESSAGE);
	}    
    }
    
    /** This function returns the XML file content as a String object
     * @return A String object as the formated XML file content
      */    
    public String getDocument()
    {
        return formatXML(((DocumentImpl)doc).saveXML(doc));
    }
    
    private boolean exist(String[] strings, String string)
    {
        for(int i = 0; i < strings.length; i++)
            if(string.equals(strings[i]))
                return true;
        return false;
    }
    
    /** This function format the XML file for better deadability
     * @return A String object as the formated file content
     * @param text
     */    
    private String formatXML(String text)
    {
        StringBuffer buffer = new StringBuffer();
        int length = text.length();
        boolean isValue = true;
        for(int i = 0; i < length; i++)
	{
            char c = text.charAt(i);
	    if(c == '<' && !isValue)
                buffer.append("\n");  
            if(c != '>' && c != '<')
                isValue = true;
            else isValue = false;
            buffer.append(c);        
	}
        return buffer.toString();
    }

    // Parsed control 
    private Control control = null;

    // Parsed data
    private Vector data = null;

    // XML document
    private Document doc = null;
}
