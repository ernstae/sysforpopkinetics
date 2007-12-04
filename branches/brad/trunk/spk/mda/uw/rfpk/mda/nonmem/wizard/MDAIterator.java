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
package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.MDAFrame;
import uw.rfpk.mda.nonmem.XMLReader;
import uw.rfpk.mda.Server;
import uw.rfpk.mda.nonmem.Utility;
import uw.rfpk.mda.nonmem.Output;
import org.netbeans.ui.wizard.*;
import java.util.Vector;
import java.util.HashSet;
import java.util.Properties;
import javax.swing.JOptionPane;
import javax.swing.JFileChooser;
import java.util.StringTokenizer;

/**
 * This class defines a iterator for the wizard.
 * @author  Jiaji Du
 */
public class MDAIterator implements StepIterator{ 

    private Vector<Object> steps = new Vector<Object>(); 
    private int actual = 0;
    private boolean isFirst = true;
//    private boolean isInd = false;
//    private boolean isTwoStage = false;
    private boolean isPred = false;
    private boolean isCov = false;
    private boolean isEstimation = true;
    private boolean isSimulation = false;
    /** Is table output requested */
    protected boolean isTable = false;
    /** Is plot output requested */
    protected boolean isPlot = false;
    private boolean isMethod1OrPosthoc = false;
    private boolean isNewData = true;
    private boolean isTester = false;
    private boolean isDeveloper = false;
    private int advan = 0;
    private int nDataCol = 0;
    private int nTheta = 0;
    private int nEta = 0;
    private int nEps = 0;
    private int nComp = 0;
    private String trans = null;
    private Vector<Object> beginningSteps = new Vector<Object>();
    private GettingStarted gettingStarted = new GettingStarted(this);
    private Continue cont = new Continue(this); 
    private Pred pred = new Pred(this); 
    private Subroutines subroutines = new Subroutines(this);  
    private Model model = new Model(this); 
    private PK pk = new PK(this); 
    private Des des = new Des(this);
    private Aesinitial aesinitial = new Aesinitial(this);
    private Aes aes = new Aes(this);
    private Error error = new Error(this);
    private Theta theta = new Theta(this); 
    private Omega omega = new Omega(this); 
    private Sigma sigma = new Sigma(this); 
    private Covariance covariance = new Covariance(this);
    private Estimation estimation = new Estimation(this);
    private Simulation simulation = new Simulation(this);
    private Table table = new Table(this); 
    private ScatterPlot scatterPlot = new ScatterPlot(this); 
    private Confirmation confirmation = new Confirmation(this);
    private int current = -1;
    private boolean isBack = false;
    private boolean isOnline = false;
    private Server server = null;
    private Properties reload = null;
    private boolean isReload = false;
    private boolean isDataXML = false;
    private String[] dataXML = new String[2];
    private String[] datasetName = new String[2];
    private JFileChooser files = null;
    private long jobId = 0;
    private boolean isLast = false;
    
    /** Analysis type */
    public String analysis = "population";
    
    /** Identifiability seed */
    public String identifiabilitySeed = null;
    
    /** Nonparametric seed */
    public String nonparamSeed = null;
    
    /** Nonparametric number of points */
    public String nonparamNumberOfPoints = null;
    
    /** Nonparametric points per dimension */
    public String nonparamPointsPerDim = null;
    
    /** ADVAN number */
    public int adn = 0;
    
    /** TRANS number*/
    public int trn = 0;
    
    /** MDA Frame */
    protected MDAFrame frame = null;
    
    /** Initialization for ADVAN set */
    public HashSet<String> initAdvan = new HashSet<String>();
  
    /** Is graphic */
    protected boolean isGraphic = false;
    
    /** Constructor to create a MDAIterator object.
     * @param server the web server associated with the MDA.
     * @param isOnline true if the MDA is online, false otherwise.
     * @param frame a reference to the MDA main window class.
     * @param isTester true if the user is a SPK tester, false otherwise.
     * @param isDeveloper true if the user is a SPK developer, false otherwise.
     * @param files the JFileChooer reference.
     * @param jobId the job identification number.
     */  
    public MDAIterator(Server server, boolean isOnline, MDAFrame frame,
                       boolean isTester, boolean isDeveloper, JFileChooser files, long jobId)
    {
        beginningSteps.add(gettingStarted);
        beginningSteps.add(new Problem(this));
        beginningSteps.add(new Data(this));
        beginningSteps.add(new Input(this));
        steps.addAll(beginningSteps);
        steps.add(cont);
        this.server = server;
        this.isOnline = isOnline;
        this.frame = frame;
        this.isTester = isTester;
        this.isDeveloper = isDeveloper;
        this.files = files;
        this.jobId = jobId;
    }

    /** Set if the back button was clicked.
     * @param b a boolean, true if the back button was clicked, false for otherwise.
     */    
    public void setIsBack(boolean b) { isBack = b; }
    
    /** Set if a control file is reloaded.
     * @param b a boolean, true for a control file is reloaded, false for otherwise.
     */    
    public void setIsReload(boolean b) { isReload = b; }    
              
    /** Set if using XML data file.
     * @param b a boolean, true for XML data file loaded, false for otherwise.
     */    
    public void setIsDataXML(boolean b) { isDataXML = b; }    
           
    /** Set if a new data file is loaded.
     * @param b a boolean, true for new data file loaded, false for otherwise.
     */    
    public void setIsNewData(boolean b) { isNewData = b; }    
        
    /** Set if using user predefined PK model.
     * @param b a boolean, true for using user defined model, false for using ADVANs.
     */    
    public void setIsPred(boolean b) { isPred = b; }
    
    /** Set if statistics of estimate step is included. 
     * @param b a boolean, true for including statistics, false for otherwise.
     */    
    public void setIsCov(boolean b) { isCov = b; }
    
    /** Set if the estimation step is included.
     * @param b a boolean, true for including estimation, false for otherwise.
     */    
    public void setIsEstimation(boolean b) { isEstimation = b; }    
    
    /** Set if the simulation step is included.
     * @param b a boolean, true for including simulation, false for otherwise.
     */    
    public void setIsSimulation(boolean b) { isSimulation = b; } 
    
     /** Set if the estimation method = 1 or posthoc is specified.
     * @param b a boolean, true for estimation method = 1 or posthoc is specified, false for otherwise.
     */    
    public void setIsMethod1OrPosthoc(boolean b) { isMethod1OrPosthoc = b; }    

    /** Set data XML.
     * @param s a String containing the data XML.
     * @param i the index of the data XML.
     */    
    public void setDataXML(String s, int i) { dataXML[i] = s; }
    
    /** Set dataset name.
     * @param s a String containing the dataset name.
     * @param i the index of the data XML.
     */    
    public void setDatasetName(String s, int i) { datasetName[i] = s; }
    
    /** Set which ADVAN subroutine is used.
     * @param i an int, the ADVAN number.
     */    
    public void setAdvan(int i) { advan = i; }
    
    /** Set which TRANS subroutine is used.
     * @param s an int, the TRANS number.
     */    
    public void setTrans(String s) { trans = s; }
    
    /** Set the number of data column in the data file.
     * @param i an int, number of data columns in the data file.
     */    
    public void setNDataCol(int i) { nDataCol = i; }
    
    /** Set the number of THETAs in $PK or $PRED record.
     * @param i an int, number of THETAs.
     */    
    public void setNTheta(int i) { nTheta = i; }
    
    /** Set the number of ETAs in $PK or $PRED record.
     * @param i an int, number of ETAs.
     */    
    public void setNEta(int i) { nEta = i; }
    
    /** Set the number of EPSs in $ERROR or $PRED record.
     * @param i an int, number of EPSs.
     */    
    public void setNEps(int i) { nEps = i; }

    /** Set the number of compartments.
     * @param i an int, number of compartments.
     */    
    public void setNComp(int i) { nComp = i; }
    
    /** Get if a control file is reloaded.
     * @return a boolean, true if a control file is reloaded, false if otherwise.
     */    
    public boolean getIsReload() { return isReload; };

    /** Get if using XML data file.
     * @return a boolean, true if a XML data file is loaded, false if otherwise.
     */    
    public boolean getIsDataXML() { return isDataXML; };
        
    /** Get if a new data file is loaded.
     * @return a boolean, true if a new data file is loaded, false if otherwise.
     */    
    public boolean getIsNewData() { return isNewData; };
    
     /** Get if $ESTIMATION is included in the analysis.
     * @return a boolean, true if $ESTIMATION is in analysis, false if otherwise.
     */    
    public boolean getIsEstimation() { return isEstimation; };
    
     /** Get if $SIMULATION is included in the analysis.
     * @return a boolean, true if $SIMULATION is in analysis, false if otherwise.
     */    
    public boolean getIsSimulation() { return isSimulation; };
    
    /** Get if using user predefined PK model.
     * @return a boolean, true if using user defined model, false if otherwise.
     */    
    public boolean getIsPred() { return isPred; };    
 
    /** Get if estimation method = 1 or posthoc is specified.
     * @return a boolean, true if estimation method = 1 or posthoc is specified, false if otherwise.
     */    
    public boolean getIsMethod1OrPosthoc() { return isMethod1OrPosthoc; };        

    /** Get the ADVAN number.
     * @return an int, the ADVAN number.
     */    
    public int getAdvan() { return advan; };
    
    /** Get the TRANS number.
     * @return an int, the TRANS number.
     */    
    public String getTrans() { return trans; };
    
    /** Get the number of data columns in the data file.
     * @return an int, the number of data columns.
     */    
    public int getNDataCol() { return nDataCol; }
    
    /** Get the number of THETAs in $PK or $PRED record.
     * @return an int, the number of THETAs.
     */    
    public int getNTheta() { return nTheta; }
    
    /** Get the number of ETAs in $PK or $PRED record.
     * @return an int, the number of ETAs.
     */    
    public int getNEta() { return nEta; }
  
    /** Get the number of compartments.
     * @return an int, the number of compartments.
     */    
    public int getNComp() { return nComp; }
  
    /** Get the number of EPSs in $ERROR record.
     * @return an int, the number of EPSs.
     */    
    public int getNEps() { return nEps; }
    
    /** Get the name of the server.
     * @return the name of the server.
     */    
    public String getServerName() { return server.getHost(); }
    
    /** Get the port number of the server.
     * @return the port number of the server.
     */    
    public String getServerPort() { return server.getPort(); }

    /** Get if the back button was clicked.
     * @return true if the back button was clicked, false otherwise.
     */    
    public boolean getIsBack() { return isBack; }    
    
    /** Get if the MDA is on-line.
     * @return true if MDA is on-line, false otherwise.
     */    
    public boolean getIsOnline() { return isOnline; }    

    /** Get if the user is a SPK tester.
     * @return true if the user is a SPK tester, false otherwise.
     */    
    public boolean getIsTester() { return isTester; }    
    
    /** Get if the user is a SPK developer.
     * @return true if the user is a SPK developer, false otherwise.
     */    
    public boolean getIsDeveloper() { return isDeveloper; }
    
    /** Get the JFileChooser.
     * @return the JFileChooser.
     */    
    public JFileChooser getFileChooser() { return files; } 
    
    /** Get SPK input data XML document.
     * @param i the index of the data XML.
     * @return a String object containing SPK data XML document.
     */    
    public String getDataXML(int i) { return dataXML[i]; }
     
    /** Get SPK input dataset name.
     * @param i the index of the data XML.
     * @return a String object containing SPK dataset name.
     */    
    public String getDatasetName(int i) { return datasetName[i]; }
    
    /** Get reload control records.
     * @return a Properties object containing the reload control records.
     */    
    public Properties getReload() { return reload; } 
    
    /** Get GettingStarted object reference.
     * @return a GettingStarted object reference.
     */    
    public GettingStarted getGettingStarted() { return gettingStarted; }

    /** Set the step sequence according to user input in Getting started step. */
    public void setSteps(){
        if(actual == 0)
        {
            isFirst = true;
            steps.retainAll(beginningSteps); 
            steps.add(cont);
        }
        if(actual != 3 || !isFirst)
            return;
        isFirst = false;
        steps.remove(4);
        if(isPred)        
            steps.add(pred);
        else
        {
            steps.add(subroutines); 
            if(advan >= 5 && advan <= 9)
                if(!isGraphic) steps.add(model);
            if(!isGraphic) steps.add(pk);
            if(advan == 6 || advan == 8 || advan == 9)
                if(!isGraphic) steps.add(des);
            if(advan == 9)
            {
                steps.add(aesinitial); 
                steps.add(aes); 
            }
            if(!isGraphic) steps.add(error); 
        }
        if(!analysis.equals("identifiability"))
        {
            steps.add(theta);
            steps.add(omega);
        }
        if(analysis.equals("population")) steps.add(sigma);
        if(isSimulation) steps.add(simulation);
        if(isEstimation) steps.add(estimation);
        if(isCov) steps.add(covariance);
        if(isTable) steps.add(table);            
        if(isPlot) steps.add(scatterPlot);     
        steps.add(confirmation);
    }

    /** Reset the iterator. */
    public void reset(){
        actual = 0;
    }
    
    /** Get the current step.
     * @return an WizardStep object, the current step object.
     */    
    public WizardStep getCurrent(){
        if(current != -1 && isBack)
        {
            actual = current;
            current = -1;
        }
//        isBack = false;
        return (WizardStep)steps.get(actual); 
    }
    
    /** Record the current step and set the actual step to the last step. */
    public void lastStep(){
        current = actual;
        actual = steps.size() - 1;
        isLast = current == actual - 1 && !isFirst? false:true;
    }
    
    /** Determine is finish is allowed.
     * @return a boolean, true for finish is allowed, false for otherwise.
     */    
    public boolean canFinish(){
        boolean finish = true;
        if(isLast) 
            finish = false;
        isLast = false;
        return finish;
    }
    
    /** Determine if the next step exists.
     * @return a boolean, true for the next step exists, false for otherwise.
     */    
    public boolean hasNext(){
        return (actual < steps.size() - 1);
    }
    
    /** Determine if the previous step exists.
     * @return a boolean, true for the previous step exists, false for otherwise.
     */    
    public boolean hasPrevious(){
        return (actual > 0);
    }
    
    /** Set the actual step to the next step. */
    public void nextStep(){
        ++actual;
    }
    
    /** Set the actual step to the previous step. */
    public void previousStep(){
        --actual;
    }
    
    /** Reload either a SPK input file or a model opened in MDA editor window, or 
     * from a file in the file system. 
     * @return 0 if either a model or a Spk input file is loaded, -1 otherwise.
     */
    public int reloadInput()
    {
        String text = null;
        Object[] possibleValues = {"Read the text in the editor.", "Open a file from the system."};
        String way = (String)JOptionPane.showInputDialog(null, "Choose a way to load it:", "Input",
                                                         JOptionPane.INFORMATION_MESSAGE, null,
                                                         possibleValues, 
                                                         possibleValues[0]);
        if(way == null)
            return -1;
        if(way.equals((String)possibleValues[0]))
            text = frame.getEditorText();
        if(way.equals((String)possibleValues[1]))
        {
            String[] file = frame.openOperation();
            if(file != null)
                text = file[1];
            else
                return -1;
        }
        
        if(text.indexOf("<spksource>") != -1 && text.indexOf("<spkdata") != -1 && 
           text.indexOf("<spkmodel>") != -1)
        {
            int indexData = text.lastIndexOf("<?xml ", text.indexOf("<spkdata"));
            int indexModel = text.lastIndexOf("<?xml ", text.indexOf("<spkmodel"));
            dataXML[0] = text.substring(indexData, indexModel);
            isDataXML = true;
            String method = null;
            String covTheta = null;
            if(text.indexOf("<pop_analysis ") != -1)
            {
                int i = text.indexOf("<pop_analysis ");
                String analysis = text.substring(i, text.indexOf(">", i));
                if(analysis.indexOf("is_estimation=\"yes\"") != -1)
                {
                    i = analysis.indexOf(" approximation=") + 16;                    
                    method = analysis.substring(i, analysis.indexOf("\"", i));
                    if(method.endsWith("two_stage"))
                    {
                        analysis = "two-stage";
                        if(method.startsWith("map")) 
                        {
                            covTheta = Utility.getOmegaValues(text);
                            if(covTheta == null)
                            {
                                JOptionPane.showMessageDialog(null, "Omega is not found in source.",
                                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                                return -1;   
                            }
                        }
                    }
                    if(method.equals("nonparametric"))
                    {
                        analysis = "nonparametric";
                        int index = text.indexOf("measure_points_in");
                        String nonparam = text.substring(index, text.indexOf("/>", index));
                        index = nonparam.indexOf("auto_generate_method=") + 22;
                        String methodString = nonparam.substring(index, nonparam.indexOf("\"", index));
                        if(methodString.equals("random_uniform"))
                        {
                            method = "nonparametric_uniform";
                            index = nonparam.indexOf("number_of_points=") + 18;
                            nonparamNumberOfPoints = nonparam.substring(index, nonparam.indexOf("\"", index));
                            index = nonparam.indexOf("seed=") + 6;
                            nonparamSeed = nonparam.substring(index, nonparam.indexOf("\"", index));
                        }
                        if(methodString.equals("grid"))
                        {
                            method = "nonparametric_grid";  
                            index = nonparam.indexOf("points_per_dimension=") + 22;
                            nonparamPointsPerDim = nonparam.substring(index, nonparam.indexOf("\"", index));
                        }
                    }
                    else
                    {
                        nonparamSeed = null;
                        nonparamNumberOfPoints = null;
                        nonparamPointsPerDim = null;
                    }
                }
            }
            if(text.indexOf("<ind_analysis ") != -1)
            {
                int i = text.indexOf("<ind_analysis ");
                String analysis = text.substring(i, text.indexOf(">", i));
                if(analysis.indexOf("is_identifiability=\"yes\"") != -1)
                {
                    method = "identifiability";
                    i = text.indexOf("<simulation ");
                    String simulation = text.substring(i, text.indexOf("/>", i));
                    identifiabilitySeed = simulation.substring(18, simulation.lastIndexOf("\""));
                }
            }
            parseControl(XMLReader.getModelArchive(text.substring(indexModel)), method, covTheta);           
        }
        else if(text.indexOf("$PROBLEM") != -1 && text.indexOf("$DATA") != -1 &&
                text.indexOf("$INPUT") != -1)
        {
            identifiabilitySeed = null;
            nonparamSeed = null;
            nonparamNumberOfPoints = null;
            nonparamPointsPerDim = null;
            isDataXML = false;
            parseControl(text, null, null);
        }
        else
        {
            JOptionPane.showMessageDialog(null, "It is neither a SPK input file nor a model.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return -1;   
        }
        return 0;
    }
  
    /** Parse a model (NONMEM control file).
     *  @param text the model text.
     */
    public void parseControl(String text, String method, String covTheta)
    {
        StringTokenizer records = new StringTokenizer(text.trim(), "$");
        int nTokens = records.countTokens();
        reload = new Properties();
        if(method != null)
        {
            reload.setProperty("METHOD", method);
            if(covTheta != null)
                reload.setProperty("COVTHETA", covTheta);
        }
        for(int i = 0; i < nTokens; i++)
        {
            String value = records.nextToken();
            String key = value.split("\n")[0].trim().split(" ")[0];
            if(key.equals("TABLE"))
                value = getAllValues("TABLE", value);
            if(key.equals("SCATTERPLOT"))
                value = getAllValues("SCATTERPLOT", value);        
            if(key.equals("OMEGA"))
                value = getAllValues("OMEGA", value);
            if(key.equals("SIGMA"))
                value = getAllValues("SIGMA", value);           
            reload.setProperty(key, value);
        }
        isReload = true;
    }
    
    private String getAllValues(String key, String value)
    {
        String previousValue = reload.getProperty(key);
        if(previousValue != null)
            value = previousValue + "," + value;
        return value;
    }
}