/*
 * MDAIterator.java
 *
 * Created on August 14, 2003, 9:35 AM
 */

package uw.rfpk.mda.nonmem.wizard;

import org.netbeans.ui.wizard.*;
import java.util.Vector;
import java.util.Properties;

/**
 * This class defines a iterator for the wizard
 * @author  jiaji
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
    private int advan = 0;
    private int nDataCol = 0;
    private int nTheta = 0;
    private int nEta = 0;
    private int nEps = 0;
    private String trans = null;
    private Vector beginningSteps = new Vector();
    private Continue cont = new Continue(); 
    private Pred pred = new Pred(this); 
    private Subroutines subroutines = new Subroutines(this);  
    private Model model = new Model(this); 
    private PK pk = new PK(this); 
    private Des des = new Des();
    private Aesinitial aesinitial = new Aesinitial();
    private Aes aes = new Aes();
    private Error error = new Error(this);
    private Theta theta = new Theta(this); 
    private Omega omega = new Omega(this); 
    private Sigma sigma = new Sigma(this); 
    private Covariance covariance = new Covariance();
    private Estimation estimation = new Estimation();
    private Simulation simulation = new Simulation();
    private Table tableEst = new Table(this); 
    private ScatterPlot scatterPlotEst = new ScatterPlot(this); 
    private Table tableSim = new Table(this); 
    private ScatterPlot scatterPlotSim = new ScatterPlot(this); 
    private Confirmation confirmation = new Confirmation();
    private int current = -1;
    private boolean isBack = false;
    
  /**
    * Constructor to create a MDAIterator object
    */  
    public MDAIterator()
    {
        beginningSteps.add(new GettingStarted(this));
        beginningSteps.add(new Problem()); 
        beginningSteps.add(new Data(this));
        beginningSteps.add(new Input(this)); 
        steps.addAll(beginningSteps);
        steps.add(cont);
    }     
    
    /** Set if it is an individual analysis
     * @param b A boolean, true for individual, false for population analysis
     */    
    public void setIsInd(boolean b) { isInd = b; }
    
    /** Set if using user predefined PK model
     * @param b A boolean, true for using user defined model, false for using ADVANs
     */    
    public void setIsPred(boolean b) { isPred = b; }
    
    /** Set if statistics of estimate step is included 
     * @param b A boolean, true for including statistics, false for otherwise
     */    
    public void setIsCov(boolean b) { isCov = b; }
    
    /** Set if the estimation step is included
     * @param b A boolean, true for including estimation, false for otherwise
     */    
    public void setIsEstimation(boolean b) { isEstimation = b; }    
    
    /** Set if the simulation step is included
     * @param b A boolean, true for including simulation, false for otherwise
     */    
    public void setIsSimulation(boolean b) { isSimulation = b; } 
    
    /** Set if the estimation step requires table output
     * @param b A boolean, true for requiring table output, false for otherwise
     */    
    public void setIsEstTable(boolean b) { isEstTable = b; }
    
    /** Set if the estimation step requires scatterplot output
     * @param b A boolean, true for requiring scatterplot output, false for otherwise
     */    
    public void setIsEstPlot(boolean b) { isEstPlot = b; } 
    
    /** Set if the simulation step requires table output
     * @param b A boolean, true for requiring table output, false for otherwise
     */     
    public void setIsSimTable(boolean b) { isSimTable = b; }
    
    /** Set if the simulation step requires scatterplot output
     * @param b A boolean, true for requiring scatterplot output, false for otherwise
     */    
    public void setIsSimPlot(boolean b) { isSimPlot = b; } 
    
    /** Set which ADVAN subroutine is used
     * @param i An int, the ADVAN number
     */    
    public void setAdvan(int i) { advan = i; }
    
    /** Set which TRANS subroutine is used
     * @param s An int, the TRANS number
     */    
    public void setTrans(String s) { trans = s; }
    
    /** Set the number of data column in the data file
     * @param i An int, number of data columns in the data file
     */    
    public void setNDataCol(int i) { nDataCol = i; }
    
    /** Set the number of THETAs in $PK or $PRED record
     * @param i An int, number of THETAs
     */    
    public void setNTheta(int i) { nTheta = i; }
    
    /** Set the number of ETAs in $PK or $PRED record
     * @param i An int, number of ETAs
     */    
    public void setNEta(int i) { nEta = i; }
    
    /** Set the number of EPSs in $ERROR or $PRED record
     * @param i An int, number of EPSs
     */    
    public void setNEps(int i) { nEps = i; }

    /** Get if it is an individual analysis
     * @return A boolean, true if individual analysis, false if otherwise
     */    
    public boolean getIsInd() { return isInd; };    
    
    /** Get if using user predefined PK model
     * @return A boolean, true if using user defined model, false if otherwise
     */    
    public boolean getIsPred() { return isPred; };    
        
    /** Get the ADVAN number
     * @return An int, the ADVAN number
     */    
    public int getAdvan() { return advan; };
    
    /** Get the TRANS number
     * @return An int, the TRANS number
     */    
    public String getTrans() { return trans; };
    
    /** Get the number of data columns in the data file
     * @return An int, the number of data columns 
     */    
    public int getNDataCol() { return nDataCol; }
    
    /** Get the number of THETAs in $PK or $PRED record
     * @return An int, the number of THETAs
     */    
    public int getNTheta() { return nTheta; }
    
    /** Get the number of ETAs in $PK or $PRED record
     * @return An int, the number of ETAs
     */    
    public int getNEta() { return nEta; }
    
    /** Get the number of EPSs in $ERROR record
     * @return An int, the number of EPSs
     */    
    public int getNEps() { return nEps; }
    
    /** Set the step sequence according to user input in Getting started step */
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
        if(isEstimation)
            steps.add(estimation);
        if(isCov)
            steps.add(covariance);
        if(isEstTable)
        {
            if(isSimTable)
                tableEst.isBoth(true);
            tableEst.setWhich("$ESTIMATION");
            steps.add(tableEst);            
        }
        if(isEstPlot)
        {
            if(isSimPlot)
                scatterPlotEst.isBoth(true);
            scatterPlotEst.setWhich("$ESTIMATION");
            steps.add(scatterPlotEst); 
        }
        if(isSimulation)
            steps.add(simulation);
        if(isSimTable)
        {
            if(isEstTable)
                tableSim.isBoth(true);
            tableSim.setWhich("$SIMULATION");
            steps.add(tableSim);
        }
        if(isSimPlot)
        {
            if(isEstPlot)
                scatterPlotSim.isBoth(true);
            scatterPlotSim.setWhich("$SIMULATION");
            steps.add(scatterPlotSim);
        }
        steps.add(confirmation);                        
    }

    /** Inform the iterator that the "Back" button has been clicked */
    public void setBack(){
        isBack = true;
    }
    
    /** Reset the iterator */
    public void reset(){
        actual = 0;
    }
    
    /** Get the current step
     * @return An WizardStep object, the current step object
     */    
    public WizardStep getCurrent(){
        if(current != -1 && isBack)
        {
            actual = current;
            current = -1;
        }
        isBack = false;
        return (WizardStep)steps.get(actual); 
    }
    
    /** Record the current step and set the actual step to the last step */
    public void lastStep(){
        current = actual;
        actual = steps.size() - 1;
    }
    
    /** Determine is finish ids allowed
     * @return A boolean, true for finish is allowed, false for otherwise
     */    
    public boolean canFinish(){
        return true;
    }
    
    /** Determine if the next step exists
     * @return A boolean, true for the next step exists, false for otherwise
     */    
    public boolean hasNext(){
        return (actual < steps.size() - 1);
    }
    
    /** Determine if the previous step exists
     * @return A boolean, true for the previous step exists, false for otherwise
     */    
    public boolean hasPrevious(){
        return (actual > 0);
    }
    
    /** Set the actual step to the next step */
    public void nextStep(){
        ++actual;
    }
    
    /** Set the actual step to the previous step */
    public void previousStep(){
        --actual;
    }
}
