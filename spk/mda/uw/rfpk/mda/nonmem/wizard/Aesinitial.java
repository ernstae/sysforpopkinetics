/**
 * Aesinitial.java
 *
 * Created on August 15, 2003, 1:17 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.text.DefaultEditorKit;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $AESINITIAL record
 * @author  jiaji Du
 */
public class Aesinitial extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private boolean isValid = false;
    private JWizardPane wizardPane = null; 
    private MDAIterator iterator = null;

    /** Creates new form Aesinitial */
    public Aesinitial(MDAIterator iter) {
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

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();

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
        jTextPane1.setText("Enter abbreviated code for $AESINITIAL.   ");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jTextPane1, gridBagConstraints);

        jButton1.setText("Cut");
        jButton1.setPreferredSize(new java.awt.Dimension(67, 25));
        jPanel1.add(jButton1, new java.awt.GridBagConstraints());

        jButton2.setText("Copy");
        jButton2.setPreferredSize(new java.awt.Dimension(67, 25));
        jPanel1.add(jButton2, new java.awt.GridBagConstraints());

        jButton3.setText("Paste");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton3, new java.awt.GridBagConstraints());

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
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JPanel jPanel1;
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
  	    return "$AESINITIAL Record";
  	}

	public String getStepTitle(){
	    return "$AESINITIAL Record";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            jTextArea1.requestFocusInWindow(); 
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("AESINITIAL");
                if(text != null)
                {
                    jTextArea1.setText(text.substring(11).trim());
                    iterator.getReload().remove("AESINITIAL");
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                    
                }
            }            
	}

	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }            
            MDAObject object = (MDAObject)wizard.getCustomizedObject();                       
            String aesinitialCode = jTextArea1.getText().trim().replaceAll("\r", "").toUpperCase();
            if(!aesinitialCode.equals("") && !Utility.checkTag(aesinitialCode, getStepTitle()))
            {
                String record = "$AESINITIAL " + "\n" + aesinitialCode;
                object.getRecords().setProperty("Aesinitial", record);
                object.getSource().aesinitial = record.substring(12) + "\n";
            }
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $AESINITIAL Record", 
                                 Aesinitial.class.getResource("/uw/rfpk/mda/nonmem/help/Aesinitial.html"));                        
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Aesinitial.html");  
                }
            };
	}
    }
}
