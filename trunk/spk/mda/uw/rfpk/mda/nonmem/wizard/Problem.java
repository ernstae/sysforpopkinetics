/*
 * Problem.java
 *
 * Created on August 15, 2003, 1:17 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $PROBLEm record
 * @author  jiaji Du
 */
public class Problem extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private String PROBLEM = null;

    /** Creates new form PROBLEM */
    public Problem() {
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jDialog1 = new javax.swing.JDialog();
        jScrollPane1 = new javax.swing.JScrollPane();
        help = new javax.swing.JTextArea();
        jTextArea1 = new javax.swing.JTextArea();
        jTextPane1 = new javax.swing.JTextPane();

        jDialog1.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        help.setEditable(false);
        jScrollPane1.setViewportView(help);

        jDialog1.getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        setLayout(new java.awt.GridBagLayout());

        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(2);
        jTextArea1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextArea1KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(18, 12, 24, 12);
        add(jTextArea1, gridBagConstraints);

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setText("Enter the title of the problem into the following text area.  \nThe text becomes the heading of the NONMEM printout.\nOnly the first 72 characters of the text are used. ");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        add(jTextPane1, gridBagConstraints);

    }//GEN-END:initComponents

    private void jTextArea1KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextArea1KeyTyped
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());           
    }//GEN-LAST:event_jTextArea1KeyTyped
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextArea help;
    private javax.swing.JDialog jDialog1;
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
  	    return "$PROBLEM Record";
  	}

	public String getStepTitle(){
	    return "$PROBLEM Record";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard; 
            jTextArea1.requestFocusInWindow();
        }
	public void hidingStep(JWizardPane wizard){
            String record = jTextArea1.getText();
            if(Utility.checkTag(record, "Problem title")) 
                return;   
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            object.getRecords().setProperty("Problem", "$PROBLEM " + record);
            object.getSource().problem = record;            
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    jDialog1.setTitle("Help for " + getStepTitle());
                    jDialog1.setSize(600, 500);
                    jDialog1.show();
                }
            };
	}
    }
}
