/*
 * Continue.java
 *
 * Created on August 15, 2003, 1:17 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import java.util.Properties;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the Continue
 * @author  jiaji Du
 */

public class Continue extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private MDAIterator iterator = null;
    
    /** Creates new form Continue */
    public Continue(MDAIterator iter) { 
        iterator = iter;
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jTextPane1 = new javax.swing.JTextPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();

        setLayout(new java.awt.BorderLayout());

        setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(0, 12, 12, 12)));
        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setText("This is the NONMEM control file you have created.");
        add(jTextPane1, java.awt.BorderLayout.NORTH);

        jTextArea1.setEditable(false);
        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

    }//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    /**
     * This method is to return the StepDescriptor object
     * @return A StepDescriptor object
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{ 

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "......";
  	}

	public String getStepTitle(){
	    return "Confirmation";
	}

	public void showingStep(JWizardPane wizard){
            Properties records = ((MDAObject)wizard.getCustomizedObject()).getRecords();
	    String control = "";
            String[] names = {"Problem", "Data", "Input", "Pred", "Subroutines", "Aes",    
                              "Aesinitial", "Model", "PK", "Theta", "Omega", "Des", 
                              "Error", "Sigma", "Simulation", "TableSim", "ScatterPlotSim",
                              "Estimation", "Covariance", "TableEst", "ScatterPlotEst"}; 
            String ls = System.getProperty("line.separator");                  
            for(int i = 0; i < 21; i++)
            {
                if(!records.getProperty(names[i]).equals("")) 
                    control = control + records.getProperty(names[i]) + ls; 
            }
            jTextArea1.setText(control);            
	}

	public void hidingStep(JWizardPane wizard){

	}
        
	public boolean isValid(){
	    return true;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for Continue", 
                                 Continue.class.getResource("/uw/rfpk/mda/nonmem/help/Continue.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Continue.html");  
                }
            };
	}
    }
}
