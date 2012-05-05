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
import uw.rfpk.mda.nonmem.compartment.DesignTool;
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import java.util.Vector;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $SUBROUTINES record.
 * @author  Jiaji Du
 */
public class Subroutines extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private MDAIterator iterator = null;
    private MDAObject object = null;
    private boolean isValid = true;
    private int advan = 0;
    private boolean isInit = false;
    private JWizardPane wizardPane = null;
    
    /** Creates new form Subroutines.
     * @param iter a MDAIterator object to initialize the field iterator.
     */
    public Subroutines(MDAIterator iter) {
        iterator = iter;
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jTextPane1 = new javax.swing.JTextPane();
        jLabel1 = new javax.swing.JLabel();
        jComboBox2 = new javax.swing.JComboBox();
        jTextPane2 = new javax.swing.JTextPane();
        jLabel2 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jTextPane3 = new javax.swing.JTextPane();
        jSeparator1 = new javax.swing.JSeparator();
        jComboBox1 = new javax.swing.JComboBox();
        jPanel1 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(238, 238, 238));
        jTextPane1.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 8, 12);
        add(jTextPane1, gridBagConstraints);

        jLabel1.setText("SUBROUTINE ADVAN" + String.valueOf(iterator.getAdvan()));
        jLabel1.setText("Significant digits");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 60, 6, 18);
        add(jLabel1, gridBagConstraints);

        jComboBox2.setEnabled(false);
        jComboBox2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBox2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 18, 25, 84);
        add(jComboBox2, gridBagConstraints);

        jTextPane2.setBackground(new java.awt.Color(238, 238, 238));
        jTextPane2.setEditable(false);
        jTextPane2.setText("You also need to select a TRANS subriutine or use the default.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(13, 12, 12, 12);
        add(jTextPane2, gridBagConstraints);

        jLabel2.setText("TRANS Subroutines");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 60, 23, 18);
        add(jLabel2, gridBagConstraints);

        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        jTextArea1.setEditable(false);
        jTextArea1.setRows(2);
        jScrollPane1.setViewportView(jTextArea1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipady = 12;
        gridBagConstraints.insets = new java.awt.Insets(9, 12, 12, 12);
        add(jScrollPane1, gridBagConstraints);

        jTextPane3.setBackground(new java.awt.Color(238, 238, 238));
        jTextPane3.setEditable(false);
        jTextPane3.setText("The options you have selected in NONMEM syntax");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(15, 12, 0, 12);
        add(jTextPane3, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        add(jSeparator1, gridBagConstraints);

        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "1", "2", "3", "4", "5", "6", "7", "8" }));
        jComboBox1.setSelectedIndex(4);
        jComboBox1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBox1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 18, 0, 84);
        add(jComboBox1, gridBagConstraints);

        jLabel3.setText("Click this button to enter Graphical Model Editor");
        jPanel1.add(jLabel3);

        jButton1.setText("Enter");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 12, 12);
        add(jPanel1, gridBagConstraints);

    }// </editor-fold>//GEN-END:initComponents

    public void setValid()
    {
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());   
    }
    
    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        String[] subjects = {"1"};
        if(object.getDataLabels()[0].equals("ID"))
        {
            Vector data = object.getData();
            subjects = new String[data.size()];
            for(int i = 0; i < data.size(); i++)
                subjects[i] = ((String[])((Vector)data.get(i)).get(0))[0];
        }
        new DesignTool(subjects, "", object, iterator, this);
        isValid = false;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jComboBox1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBox1ActionPerformed
        setRecord();
    }//GEN-LAST:event_jComboBox1ActionPerformed

    private void jComboBox2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBox2ActionPerformed
        if(isInit)
            return;
        iterator.setTrans(((String)jComboBox2.getSelectedItem()));
        setRecord();
    }//GEN-LAST:event_jComboBox2ActionPerformed
    
    private void setRecord()
    {
        String record = "$SUBROUTINES ADVAN" + String.valueOf(advan);
        if(advan <= 4 || advan >= 10)
        {
            record += " " + ((String)jComboBox2.getSelectedItem()).trim();
        } 
        if(advan == 6 || advan == 8 || advan == 9 || advan == 10)
        {
            if(!iterator.analysis.equals("identifiability"))
                record += " TOL=" + ((String)jComboBox1.getSelectedItem()).trim();
        }
        jTextArea1.setText(record);
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JComboBox jComboBox1;
    private javax.swing.JComboBox jComboBox2;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JTextPane jTextPane3;
    // End of variables declaration//GEN-END:variables

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
  	    return "Model Library";
  	}

	public String getStepTitle(){
	    return "Model Library";
	}

	public void showingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }
            object = (MDAObject)wizard.getCustomizedObject();
            wizardPane = wizard;
            isValid = !(iterator.isGraphic && object.getSource().model == null);
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
            jLabel3.setEnabled(iterator.isGraphic);
            jButton1.setEnabled(iterator.isGraphic);
            int advanCurrent = iterator.getAdvan();
            String text = null;
            
            // If advan was not changed and no reload for subroutines, return
            if(iterator.getIsReload())
                text = iterator.getReload().getProperty("SUBROUTINES");           
            if(advan == advanCurrent && text == null)
                return;
            
            // Set advan to the current value
            advan = advanCurrent;
            
            // remove the reload for subroutines
            if(text != null)
            {
                iterator.getReload().remove("SUBROUTINES");
                text = text.trim().concat(" ");
            }

            // Initialize the GUI according to the advan and the reload for subroutines
            jTextPane1.setEnabled(true);
            if(advan == 6 || advan == 8 || advan == 9 || advan == 10)
            {
                jTextPane1.setText("You have selected to use SUBROUTINE ADVAN" + String.valueOf(advan) +
                                   ".  You need to specify the number of significant digits" +
                                   " in the computation for each compartment.");
                jComboBox1.setEnabled(true);
                jLabel1.setEnabled(true);
                if(text != null)
                {
                    int advanIndex = text.indexOf("ADVAN") + 5;
                    int advanReload = Integer.parseInt(text.substring(advanIndex, text.indexOf(" ", advanIndex)));
                    if(advanReload == advan)
                    {
                        int tolIndex = text.indexOf("TOL=") + 4;
                        if(tolIndex != -1)
                            jComboBox1.setSelectedItem(text.substring(tolIndex, tolIndex + 1));
                    }
                }
            }
            else
            {
                jTextPane1.setText("You have selected to use SUBROUTINE ADVAN" + String.valueOf(advan) +
                                   ".                                                     " +
                                   "                                        ");
                jComboBox1.setEnabled(false); 
                jLabel1.setEnabled(false);
            }
              
            // Initialize jComboBox2 according to the advan
            isInit = true;
            jComboBox2.removeAllItems();
            switch(advan)
            {
                case 1:
                case 2: 
                    jComboBox2.addItem("TRANS1");
                    jComboBox2.addItem("TRANS2");                 
                    break;
                case 3:
                case 4:
                    jComboBox2.addItem("TRANS1");
                    jComboBox2.addItem("TRANS3");
                    jComboBox2.addItem("TRANS4");
                    jComboBox2.addItem("TRANS5");                    
                    break;
                case 10:
                    jComboBox2.addItem("TRANS1");                    
                    break;
                case 11:
                case 12:
                    jComboBox2.addItem("TRANS1");
                    jComboBox2.addItem("TRANS4");
                    jComboBox2.addItem("TRANS6");                    
                    break;
            }
            isInit = false;
            if(advan <= 4 || advan >= 10)
            {
                jComboBox2.setEnabled(true); 
                jTextPane2.setText("You need to select a translator subroutine or use the default TRANS1.");
                jLabel2.setEnabled(true);
                if(text != null)
                {
                    int advanReload = Integer.parseInt(text.substring(18, text.indexOf(" ", 18)));
                    if(advanReload == advan)
                    {                    
                        int transIndex = text.indexOf("TRANS");
                        if(transIndex != -1)
                            jComboBox2.setSelectedItem(text.substring(transIndex, transIndex + 6));
                    }
                }                
            }
            else
            {
                jTextPane2.setText("                                                              ");
                jComboBox2.setEnabled(false); 
                jLabel2.setEnabled(false);
            }
            
            // Set text to the text area
            text = "$SUBROUTINES ADVAN" + String.valueOf(advan);
            if(jLabel2.isEnabled())
                text += " " + ((String)jComboBox2.getSelectedItem()).trim();
            if(jLabel1.isEnabled() && !iterator.analysis.equals("identifiability"))
                text += " TOL=" + ((String)jComboBox1.getSelectedItem()).trim();                    
            jTextArea1.setText(text);
            if(iterator.analysis.equals("identifiability"))
            {
                jLabel1.setEnabled(false);
                jTextPane1.setEnabled(false);
            }
	}

        public boolean checkingStep(JWizardPane wizard){
            return true;
        }
        
	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack()) return;
            String record = jTextArea1.getText();           
            object.getRecords().setProperty("Subroutines", record);
            String[] subroutines = new String[3];
            subroutines[0] = "advan" + String.valueOf(advan);
            int indexTol = record.indexOf("TOL=");
            if(indexTol != -1)
                subroutines[1] = record.substring(indexTol + 4, indexTol + 5);
            int indexTrans = record.indexOf("TRANS");
            if(indexTrans != -1)
                subroutines[2] = "trans" + record.substring(indexTrans + 5, indexTrans + 6);            
            object.getSource().subroutines = subroutines;
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){   
                }
            };
	}
        
        public String getHelpID() {
            return "Prepare_Input_Model_Numerics_from_the_Model_Library";
        }
        
    }
}