/*
 * MDAIterator.java
 *
 * Created on August 14, 2003, 9:35 AM
 */

package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.MDAFrame;
import uw.rfpk.mda.nonmem.XMLReader;
import org.netbeans.ui.wizard.*;
import java.util.Vector;
import java.util.Properties;
import javax.swing.JOptionPane;
import java.util.StringTokenizer;

/**
 * This class defines a iterator for the wizard.
 * @author  Jiaji Du
 */
public class MDAIterator implements StepIterator{ 

    private Vector steps = new Vector(); 
    private int actual = 0;
    private boolean isFirst = true;
    private boolean isInd = false;
    private boolean isPred = false;
    private boolean isCov = false;
    private boolean isEstimation = true;
    private boolean isSimulation = false; 
    private boolean isEstTable = false;
    private boolean isEstPlot = false;
    private boolean isSimTable = false;
    private boolean isSimPlot = false;
    private boolean isMethod1OrPosthoc = false;
    private boolean isNewData = true;
    private boolean isTester = false;
    private boolean isDeveloper = false;
    private int advan = 0;
    private int nDataCol = 0;
    private int nTheta = 0;
    private int nEta = 0;
    private int nEps = 0;
    private String trans = null;
    private Vector beginningSteps = new Vector();
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
    private Table tableEst = new Table(this); 
    private ScatterPlot scatterPlotEst = new ScatterPlot(this); 
    private Table tableSim = new Table(this); 
    private ScatterPlot scatterPlotSim = new ScatterPlot(this); 
    private Confirmation confirmation = new Confirmation(this);
    private int current = -1;
    private boolean isBack = false;
    private boolean isOnline = false;
    private String serverName = null;
    private String serverPort = null;
    private Properties reload = null;
    private MDAFrame frame = null;
    private boolean isReload = false;
    private boolean isDataXML = false;
    private String dataXML = null;

    /** Constructor to create a MDAIterator object.
     * @param serverName name of the web server associated with the MDA.
     * @param serverPort port on the web server associated with the MDA.
     * @param isOnline true if the MDA is online, false otherwise.
     * @param frame a reference to the MDA main window class.
     * @param isTester true if the user is a SPK tester, false otherwise.
     * @param isDeveloper true if the user is a SPK developer, false otherwise.
     */  
    public MDAIterator(String serverName, String serverPort, boolean isOnline, MDAFrame frame,
                       boolean isTester, boolean isDeveloper)
    {
        beginningSteps.add(gettingStarted);
        beginningSteps.add(new Problem(this)); 
        beginningSteps.add(new Data(this));
        beginningSteps.add(new Input(this)); 
        steps.addAll(beginningSteps);
        steps.add(cont);
        this.serverName = serverName;
        this.serverPort = serverPort;
        this.isOnline = isOnline;
        this.frame = frame;
        this.isTester = isTester;
        this.isDeveloper = isDeveloper;
    }

    /** Set if the back button was clicked.
     * @param b A boolean, true if the back button was clicked, false for otherwise.
     */    
    public void setIsBack(boolean b) { isBack = b; }
    
    /** Set if a control file is reloaded.
     * @param b A boolean, true for a control file is reloaded, false for otherwise.
     */    
    public void setIsReload(boolean b) { isReload = b; }    
              
    /** Set if using XML data file.
     * @param b A boolean, true for XML data file loaded, false for otherwise.
     */    
    public void setIsDataXML(boolean b) { isDataXML = b; }    
           
    /** Set if a new data file is loaded.
     * @param b A boolean, true for new data file loaded, false for otherwise.
     */    
    public void setIsNewData(boolean b) { isNewData = b; }    
        
    /** Set if it is an individual analysis.
     * @param b A boolean, true for individual, false for population analysis.
     */    
    public void setIsInd(boolean b) { isInd = b; }
    
    /** Set if using user predefined PK model.
     * @param b A boolean, true for using user defined model, false for using ADVANs.
     */    
    public void setIsPred(boolean b) { isPred = b; }
    
    /** Set if statistics of estimate step is included. 
     * @param b A boolean, true for including statistics, false for otherwise.
     */    
    public void setIsCov(boolean b) { isCov = b; }
    
    /** Set if the estimation step is included.
     * @param b A boolean, true for including estimation, false for otherwise.
     */    
    public void setIsEstimation(boolean b) { isEstimation = b; }    
    
    /** Set if the simulation step is included.
     * @param b A boolean, true for including simulation, false for otherwise.
     */    
    public void setIsSimulation(boolean b) { isSimulation = b; } 
    
    /** Set if the estimation step requires table output.
     * @param b A boolean, true for requiring table output, false for otherwise.
     */    
    public void setIsEstTable(boolean b) { isEstTable = b; }
    
    /** Set if the estimation step requires scatterplot output.
     * @param b A boolean, true for requiring scatterplot output, false for otherwise.
     */    
    public void setIsEstPlot(boolean b) { isEstPlot = b; } 
    
    /** Set if the simulation step requires table output.
     * @param b A boolean, true for requiring table output, false for otherwise.
     */     
    public void setIsSimTable(boolean b) { isSimTable = b; }
    
    /** Set if the simulation step requires scatterplot output.
     * @param b A boolean, true for requiring scatterplot output, false for otherwise.
     */    
    public void setIsSimPlot(boolean b) { isSimPlot = b; } 

     /** Set if the estimation method = 1 or posthoc is specified.
     * @param b A boolean, true for estimation method = 1 or posthoc is specified, false for otherwise.
     */    
    public void setIsMethod1OrPosthoc(boolean b) { isMethod1OrPosthoc = b; }    

    /** Set data XML.
     * @param s A String containing the data XML.
     */    
    public void setDataXML(String s) { dataXML = s; }
        
    /** Set which ADVAN subroutine is used.
     * @param i An int, the ADVAN number.
     */    
    public void setAdvan(int i) { advan = i; }
    
    /** Set which TRANS subroutine is used.
     * @param s An int, the TRANS number.
     */    
    public void setTrans(String s) { trans = s; }
    
    /** Set the number of data column in the data file.
     * @param i An int, number of data columns in the data file.
     */    
    public void setNDataCol(int i) { nDataCol = i; }
    
    /** Set the number of THETAs in $PK or $PRED record.
     * @param i An int, number of THETAs.
     */    
    public void setNTheta(int i) { nTheta = i; }
    
    /** Set the number of ETAs in $PK or $PRED record.
     * @param i An int, number of ETAs.
     */    
    public void setNEta(int i) { nEta = i; }
    
    /** Set the number of EPSs in $ERROR or $PRED record.
     * @param i An int, number of EPSs.
     */    
    public void setNEps(int i) { nEps = i; }

    /** Get if a control file is reloaded.
     * @return A boolean, true if a control file is reloaded, false if otherwise.
     */    
    public boolean getIsReload() { return isReload; };

    /** Get if using XML data file.
     * @return A boolean, true if a XML data file is loaded, false if otherwise.
     */    
    public boolean getIsDataXML() { return isDataXML; };
        
    /** Get if a new data file is loaded.
     * @return A boolean, true if a new data file is loaded, false if otherwise.
     */    
    public boolean getIsNewData() { return isNewData; };
    
     /** Get if $ESTIMATION is included in the analysis.
     * @return A boolean, true if $ESTIMATION is in analysis, false if otherwise.
     */    
    public boolean getIsEstimation() { return isEstimation; };    
       
    /** Get if it is an individual analysis.
     * @return A boolean, true if individual analysis, false if otherwise.
     */    
    public boolean getIsInd() { return isInd; };    
    
    /** Get if using user predefined PK model.
     * @return A boolean, true if using user defined model, false if otherwise.
     */    
    public boolean getIsPred() { return isPred; };    
 
    /** Get if estimation method = 1 or posthoc is specified.
     * @return A boolean, true if estimation method = 1 or posthoc is specified, false if otherwise.
     */    
    public boolean getIsMethod1OrPosthoc() { return isMethod1OrPosthoc; };        

    /** Get the ADVAN number.
     * @return An int, the ADVAN number.
     */    
    public int getAdvan() { return advan; };
    
    /** Get the TRANS number.
     * @return An int, the TRANS number.
     */    
    public String getTrans() { return trans; };
    
    /** Get the number of data columns in the data file.
     * @return An int, the number of data columns.
     */    
    public int getNDataCol() { return nDataCol; }
    
    /** Get the number of THETAs in $PK or $PRED record.
     * @return An int, the number of THETAs.
     */    
    public int getNTheta() { return nTheta; }
    
    /** Get the number of ETAs in $PK or $PRED record.
     * @return An int, the number of ETAs.
     */    
    public int getNEta() { return nEta; }
    
    /** Get the number of EPSs in $ERROR record.
     * @return An int, the number of EPSs.
     */    
    public int getNEps() { return nEps; }
    
    /** Get the name of the server.
     * @return The name of the server.
     */    
    public String getServerName() { return serverName; }
    
    /** Get the port number of the server.
     * @return The port number of the server.
     */    
    public String getServerPort() { return serverPort; }

    /** Get if the back button was clicked.
     * @return True if the back button was clicked.  False otherwise.
     */    
    public boolean getIsBack() { return isBack; }    
    
    /** Get if the MDA is on-line.
     * @return True if MDA is on-line.  False otherwise.
     */    
    public boolean getIsOnline() { return isOnline; }    

    /** Get if the user is a SPK tester.
     * @return True if the user is a SPK tester.  False otherwise.
     */    
    public boolean getIsTester() { return isTester; }    
    
    /** Get if the user is a SPK developer.
     * @return True if the user is a SPK developer.  False otherwise.
     */    
    public boolean getIsDeveloper() { return isDeveloper; }
    
    /** Get SPK input data XML document.
     * @return A String object containing SPK data XML document.
     */    
    public String getDataXML() { return dataXML; }
    
    /** Get reload control records.
     * @return A Properties object containing the reload control records.
     */    
    public Properties getReload() { return reload; }      
    
    /** Get gettingStarted object reference.
     * @return A gettingStarted object reference.
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
                steps.add(model);
            steps.add(pk);
            if(advan == 6 || advan == 8 || advan == 9)
                steps.add(des);
            if(advan == 9)
            {
                steps.add(aesinitial); 
                steps.add(aes); 
            }
            steps.add(error); 
        }
        steps.add(theta);
        steps.add(omega);
        if(!isInd)
            steps.add(sigma);

        if(isSimulation)
        {
            steps.add(simulation);
            if(isSimTable)
            {
                tableSim.setWhich("SIMULATION");
                steps.add(tableSim);
            }
            if(isSimPlot)
            {
                scatterPlotSim.setWhich("SIMULATION");
                steps.add(scatterPlotSim);
            }
        }
        if(isEstimation)
        {
            steps.add(estimation);
            if(isCov)
                steps.add(covariance);
            if(isEstTable)
            {
                tableEst.setWhich("ESTIMATION");
                steps.add(tableEst);            
            }
            if(isEstPlot)
            {
                scatterPlotEst.setWhich("ESTIMATION");
                steps.add(scatterPlotEst); 
            }            
        }        
        steps.add(confirmation);
    }

    /** Reset the iterator. */
    public void reset(){
        actual = 0;
    }
    
    /** Get the current step.
     * @return An WizardStep object, the current step object.
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
    }
    
    /** Determine is finish ids allowed.
     * @return A boolean, true for finish is allowed, false for otherwise.
     */    
    public boolean canFinish(){
        return true;
    }
    
    /** Determine if the next step exists.
     * @return A boolean, true for the next step exists, false for otherwise.
     */    
    public boolean hasNext(){
        return (actual < steps.size() - 1);
    }
    
    /** Determine if the previous step exists.
     * @return A boolean, true for the previous step exists, false for otherwise.
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
    
    /** Reload either a SPK input file or a model opened in MDA editor window. */
    public void reloadInput()
    {
        String text = frame.getEditorText();
        if(text.indexOf("<spksource>") != -1 && text.indexOf("<spkdata") != -1 && 
           text.indexOf("<spkmodel>") != -1)
        {
            int index1 = text.indexOf("<spkdata");
            int index2 = text.indexOf("<spkmodel");        
            dataXML = text.substring(index1 - 22, index2 - 22);
            isDataXML = true;
            parseControl(XMLReader.getModelArchive(text.substring(index2 - 22)));           
        }
        else if(text.indexOf("$PROBLEM") != -1 && text.indexOf("$DATA") != -1 &&
                text.indexOf("$INPUT") != -1)
        {
            isDataXML = false;
            parseControl(text);
        }
        else
        {
            JOptionPane.showMessageDialog(null, "Either a SPK input file or a model\n" +
                                          "is not opened in the MDA editor window.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;   
        }
    }
  
    /** Parse a model (NONMEM control file).
     *  @param text The model text.
     */
    public void parseControl(String text)
    {
        StringTokenizer records = new StringTokenizer(text, "$");
        int nTokens = records.countTokens();
        reload = new Properties();
        int state = 0;
        for(int i = 0; i < nTokens; i++)
        {
            String previousValue = null;
            String value = records.nextToken();
            String key = value.split("\n")[0].trim().split(" ")[0];
            if((key.equals("TABLE") || key.equals("SCATTERPLOT")) && state == 1)
                key += "SIM";
            if((key.equals("TABLE") || key.equals("SCATTERPLOT")) && state == 2)
                key += "EST";
            if(key.equals("TABLESIM"))
                value = getAllValues("TABLESIM", value);
            if(key.equals("TABLEEST"))
                value = getAllValues("TABLEEST", value);
            if(key.equals("SCATTERPLOTSIM"))
                value = getAllValues("SCATTERPLOTSIM", value);
            if(key.equals("SCATTERPLOTEST"))
                value = getAllValues("SCATTERPLOTEST", value);          
            if(key.equals("OMEGA"))
                value = getAllValues("OMEGA", value);
            if(key.equals("SIGMA"))
                value = getAllValues("SIGMA", value);           
            reload.setProperty(key, value);
            if(key.equals("SIMULATION"))
                state = 1;
            if(key.equals("ESTIMATION"))
                state = 2;
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
