/*
 * Pred.java
 *
 * Created on August 15, 2003, 1:17 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.text.DefaultEditorKit;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $PRED record
 * @author  jiaji Du
 */
public class Pred extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private boolean isValid = false;
    private JWizardPane wizardPane = null; 
    private MDAIterator iterator = null;

    /** Creates new form Pred 
     * @param iter A MDAIterator object to initialize the field iterator
     */
    public Pred(MDAIterator iter) {
        iterator = iter;
        initComponents();
        jButton1.addActionListener(new DefaultEditorKit.CutAction());
        jButton2.addActionListener(new DefaultEditorKit.CopyAction()); 
        jButton3.addActionListener(new DefaultEditorKit.PasteAction());        
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jDialog1 = new javax.swing.JDialog();
        jScrollPane2 = new javax.swing.JScrollPane();
        help = new javax.swing.JTextArea();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();

        jDialog1.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        help.setEditable(false);
        jScrollPane2.setViewportView(help);

        jDialog1.getContentPane().add(jScrollPane2, java.awt.BorderLayout.CENTER);

        setLayout(new java.awt.BorderLayout());

        setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(0, 12, 12, 12)));
        jTextArea1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextArea1KeyTyped(evt);
            }
        });

        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter abbreviated code for $PRED.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 39);
        jPanel1.add(jTextPane1, gridBagConstraints);

        jButton2.setText("Copy");
        jButton2.setPreferredSize(new java.awt.Dimension(67, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton2, gridBagConstraints);

        jButton3.setText("Paste");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton3, gridBagConstraints);

        jButton1.setText("Cut");
        jButton1.setPreferredSize(new java.awt.Dimension(67, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton1, gridBagConstraints);

        add(jPanel1, java.awt.BorderLayout.NORTH);

    }//GEN-END:initComponents

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());       
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jTextArea1KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextArea1KeyTyped
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
    }//GEN-LAST:event_jTextArea1KeyTyped
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextArea help;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JDialog jDialog1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
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
  	    return "$PRED Record";
  	}

	public String getStepTitle(){
	    return "$PRED Record";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            String value = ((MDAObject)wizard.getCustomizedObject()).getRecords().getProperty("Pred");
            if(!value.equals(""))
                jTextArea1.setText(value.substring(7));
	}

	public void hidingStep(JWizardPane wizard){
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            String record = jTextArea1.getText().trim();
            if(!record.equals("") && !Utility.checkTag(record, "PRED code"))
            {
                object.getRecords().setProperty("Pred", "$PRED \n" + record);
                object.getSource().pred = "\n" + record + "\n";
                // Eliminate comments
                record = Utility.eliminateComments(record); 
                // Find number of THETAs
                iterator.setNTheta(Utility.find(record.toUpperCase(), "THETA"));
                // Find number of ETAs
                iterator.setNEta(Utility.find(record.toUpperCase().replaceAll("THETA", ""), "ETA")); 
                // Find number of EPSs
                iterator.setNEps(Utility.find(record.toUpperCase(), "EPS"));
            }            
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    jDialog1.setTitle("Help for " + getStepTitle());
                    jDialog1.setSize(600, 500);
                    jDialog1.setVisible(true);
                    jDialog1.show();
                }
            };
	}
    }
}
