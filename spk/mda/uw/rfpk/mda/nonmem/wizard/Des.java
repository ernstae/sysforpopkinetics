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

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.text.DefaultEditorKit;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * This class defines a step to create the $DES record.
 * @author  Jiaji Du
 */
public class Des extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private MDAIterator iterator = null;
    
    /** Creates new form Des.
     * @param iter a MDAIterator object to initialize the field iterator.     
     */
    public Des(MDAIterator iter) {
        iterator = iter;
        initComponents();
        jButton1.addActionListener(new DefaultEditorKit.CutAction());
        jButton2.addActionListener(new DefaultEditorKit.CopyAction()); 
        jButton3.addActionListener(new DefaultEditorKit.PasteAction());
        jTextArea1.getDocument().addDocumentListener(new MyDocumentListener());
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
        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter code for diff. eq. structure.");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 40);
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
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    private class MyDocumentListener implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {
            isValid = true;
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());         
        }
        public void removeUpdate(DocumentEvent e) {
            if(jTextArea1.getText().equals(""))
            {
                isValid = false;
                wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
            }
        }
        public void changedUpdate(DocumentEvent e) {}
    }
    
    /**
     * This method is to return the StepDescriptor object.
     * @return a StepDescriptor object.
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{ 

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "Differential Equation\nStructure";
  	}

	public String getStepTitle(){
	    return "Differential Equation Structure";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("DES");
                if(text != null)
                {
                    jTextArea1.setText(text.substring(4).trim());
                    iterator.getReload().remove("DES");
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                    
                }
            }
            else
            {
                String value = ((MDAObject)wizard.getCustomizedObject()).getRecords().getProperty("Des");
                if(value.equals(""))
                {
                    String ls = System.getProperty("line.separator");
                    jTextArea1.setText("DADT(1)=" + ls + "DADT(2)=");
                }
            }
            jTextArea1.requestFocusInWindow();
	}

	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }            
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            String desCode = jTextArea1.getText().trim().replaceAll("\r", "").toUpperCase();
            if(!desCode.equals(""))
            {
                String record = "$DES " + "\n" + desCode;
                object.getRecords().setProperty("Des", record);
                object.getSource().des = record.substring(5) + "\n";
            }
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $DES Record", 
                                 Des.class.getResource("/uw/rfpk/mda/nonmem/help/Des.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Des.html");  
                }
            };
	}
        
        public String getHelpID() {
            return "Des";
        }
        
    }
}
