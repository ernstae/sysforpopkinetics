/*
 * Theta.java
 *
 * Created on August 15, 2003, 1:17 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.Utility; 
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.DefaultListModel;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $THETA record
 * @author  jiaji Du
 */
public class Theta extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this; 
    private int nTheta = 0;
    private int index = -1;
    private boolean isFixed = false;
    private boolean isUBInf = false;
    private boolean isLBInf = false;
    private boolean isValid = false;
    private MDAIterator iterator = null;
    private DefaultListModel model = null; 
    private JWizardPane wizardPane = null;


    /** Creates new form Theta 
     * @param iter A MDAIterator object to initialize the field iterator     
     */
    public Theta(MDAIterator iter) { 
        iterator = iter; 
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
        jScrollPane2 = new javax.swing.JScrollPane();
        help = new javax.swing.JTextArea();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        addButton = new javax.swing.JButton();
        upButton = new javax.swing.JButton();
        downButton = new javax.swing.JButton();
        jTextPane1 = new javax.swing.JTextPane();
        jTextPane2 = new javax.swing.JTextPane();
        changeButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JSeparator();
        jSeparator2 = new javax.swing.JSeparator();
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        jCheckBox3 = new javax.swing.JCheckBox();

        jDialog1.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        help.setEditable(false);
        jScrollPane2.setViewportView(help);

        jDialog1.getContentPane().add(jScrollPane2, java.awt.BorderLayout.CENTER);

        setLayout(new java.awt.GridBagLayout());

        jTextField1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField1KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 125;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(14, 0, 11, 10);
        add(jTextField1, gridBagConstraints);

        jTextField2.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField2KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 0, 9, 10);
        add(jTextField2, gridBagConstraints);

        addButton.setText("Add");
        addButton.setEnabled(false);
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 16, 7, 12);
        add(addButton, gridBagConstraints);

        upButton.setText("Up");
        upButton.setEnabled(false);
        upButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                upButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(16, 16, 7, 12);
        add(upButton, gridBagConstraints);

        downButton.setText("Down");
        downButton.setEnabled(false);
        downButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(5, 16, 56, 12);
        add(downButton, gridBagConstraints);

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter initial estimates and/or bounds for the fixed effects means.  If \nFIXED is selected, only the initial estimate is required.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        add(jTextPane1, gridBagConstraints);

        jTextPane2.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane2.setText("List of THETA\nvalues you \nhave entered.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(13, 12, 12, 0);
        add(jTextPane2, gridBagConstraints);

        changeButton.setText("Change");
        changeButton.setEnabled(false);
        changeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                changeButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 16, 6, 12);
        add(changeButton, gridBagConstraints);

        deleteButton.setText("Delete");
        deleteButton.setEnabled(false);
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(7, 16, 17, 12);
        add(deleteButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        add(jSeparator1, gridBagConstraints);

        jSeparator2.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        add(jSeparator2, gridBagConstraints);

        model = new DefaultListModel();
        jList1 = new javax.swing.JList(model);
        jList1.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jList1.setFixedCellHeight(15);
        jList1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jList1MouseClicked(evt);
            }
        });

        jScrollPane1.setViewportView(jList1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.ipady = 88;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 0, 12, 16);
        add(jScrollPane1, gridBagConstraints);

        jLabel1.setText("Lower Bound");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(14, 12, 11, 12);
        add(jLabel1, gridBagConstraints);

        jLabel2.setText("Initial Estimate");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 12, 9, 12);
        add(jLabel2, gridBagConstraints);

        jLabel3.setText("Upper Bound");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 12, 22, 12);
        add(jLabel3, gridBagConstraints);

        jTextField3.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField3KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 0, 20, 10);
        add(jTextField3, gridBagConstraints);

        jCheckBox1.setText("-INF");
        jCheckBox1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBox1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 6, 9, 10);
        add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setText("FIXED");
        jCheckBox2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBox2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 6, 7, 10);
        add(jCheckBox2, gridBagConstraints);

        jCheckBox3.setText("INF");
        jCheckBox3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBox3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 6, 18, 10);
        add(jCheckBox3, gridBagConstraints);

    }//GEN-END:initComponents

    private void jTextField2KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField2KeyTyped
        addButton.setEnabled(true);
    }//GEN-LAST:event_jTextField2KeyTyped

    private void jList1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jList1MouseClicked
        index = jList1.getSelectedIndex();  
        changeButton.setEnabled(true);
        deleteButton.setEnabled(true);
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_jList1MouseClicked

    private void jTextField3KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField3KeyTyped
        jCheckBox3.setSelected(false);
        isUBInf = false;
    }//GEN-LAST:event_jTextField3KeyTyped

    private void jTextField1KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField1KeyTyped
        jCheckBox1.setSelected(false);
        isLBInf = false;
    }//GEN-LAST:event_jTextField1KeyTyped

    private void jCheckBox3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox3ActionPerformed
        if(jCheckBox3.isSelected())
        {
            isUBInf = true;
            jTextField3.setText("1000000");
            jCheckBox2.setSelected(false);
            isFixed = false;
        }
        else
        {
            isUBInf = false;
            jTextField3.setText(""); 
        }
    }//GEN-LAST:event_jCheckBox3ActionPerformed

    private void jCheckBox1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox1ActionPerformed
        if(jCheckBox1.isSelected())
        {
            isLBInf = true;
            jTextField1.setText("-1000000");
            jCheckBox2.setSelected(false);
            isFixed = false;
        }
        else
        {
            isLBInf = false;
            jTextField1.setText(""); 
        }
    }//GEN-LAST:event_jCheckBox1ActionPerformed

    private void jCheckBox2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox2ActionPerformed
        if(jCheckBox2.isSelected())
        {
            isFixed = true;
            jTextField1.setText(jTextField2.getText());
            jTextField3.setText(jTextField2.getText());
            jCheckBox1.setSelected(false);
            jCheckBox3.setSelected(false);
        }
        else
        {
            isFixed = false;
        }
    }//GEN-LAST:event_jCheckBox2ActionPerformed

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
        // Remove the selected element 
        model.removeElement(jList1.getSelectedValue());
        jList1.setSelectedIndex(--index);

        // Set add, change, delete and left buttons
        addButton.setEnabled(true);
        isValid = false;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray()); 
        if(model.getSize() == 0 || index == -1)
        {
            changeButton.setEnabled(false);
            deleteButton.setEnabled(false); 
        }

        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void changeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_changeButtonActionPerformed
        // Remove the selected element
        model.removeElement(jList1.getSelectedValue());
        
        // Prepare to add
        String element = prepareToAdd();
        if(element == null) return;        

        // Add the element to the list
        model.add(index, element);     
        jList1.setSelectedIndex(index);
        
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_changeButtonActionPerformed

    private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
        jList1.setSelectedIndex(++index);
        if(index == 0)
        {
            changeButton.setEnabled(true);
            deleteButton.setEnabled(true);
        }
        
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_downButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
        // Prepare to add
        String element = prepareToAdd();
        if(element == null) return; 
        
        // add the element to the list
        model.add(++index, element);
        jList1.setSelectedIndex(index);
        
        // Set add and left options
        if(model.getSize() == nTheta && nTheta != 0)
        {
            addButton.setEnabled(false);
            isValid = true;
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray()); 
        }
        
        // Set change and delete buttons
        if(model.getSize() > 0)
        {
            changeButton.setEnabled(true);
            deleteButton.setEnabled(true);
        }
     
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_addButtonActionPerformed

    private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
        jList1.setSelectedIndex(--index);

        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_upButtonActionPerformed
    
    private String prepareToAdd()
    {
        // Check if addable
        if(model.getSize() == nTheta)
        {
            addButton.setEnabled(false); 
            JOptionPane.showMessageDialog(null, 
                                          "The number of THETAs is " + nTheta + 
                                          " found in $PK or $PRED record.",   
                                          "Input Error.",    
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        }
        
        // Pick up the text box content
        String text1 = jTextField1.getText().trim(); 
        String text2 = jTextField2.getText().trim(); 
        String text3 = jTextField3.getText().trim();  
        
        // Check if text1, text2 and text3 are numbers
        if(!isFixed && !Utility.isFloatNumber(text1) && !isFixed)
        {
            JOptionPane.showMessageDialog(null, 
                                          "The Lower Bound is not a floating " +
                                          "point number.",   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);           
            return null;
        }
        if(!Utility.isFloatNumber(text2))
        {
            JOptionPane.showMessageDialog(null, 
                                          "The Initial Estimate is not a floating " +
                                          "point number.",   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                
            return null;
        }
        if(!isFixed && !Utility.isFloatNumber(text3))
        { 
            JOptionPane.showMessageDialog(null, 
                                          "The Upper Bound is not a floating " +
                                          "point number.",   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                
            return null;
        }
        
        // Check if text1, text2 and text3 are in order
        if(!isFixed && new Double(text1).doubleValue() > new Double(text2).doubleValue())
        {
            JOptionPane.showMessageDialog(null, 
                                          "The Lower Bound is greater than " +
                                          "the Initial Estimate.",   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                
            return null;
        }
        if(!isFixed && new Double(text2).doubleValue() > new Double(text3).doubleValue())
        {
            JOptionPane.showMessageDialog(null, 
                                          "The Upper Bound is smaller than " +
                                          "the Initial Estimate.",   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                    
            return null;
        }
        
        // Construct the element for the list   
        String element = ""; 
        if(!isFixed)
        {
            String lowerBound = isLBInf? "-INF":text1;
            String upperBound = isUBInf? "INF":text3;
            element = "(" + lowerBound + "," + text2 + "," + upperBound + ")";
        }
        else
        {
            element = "(" + text2 + " FIXED)";           
        }
        return element;
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JButton changeButton;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton downButton;
    private javax.swing.JTextArea help;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JDialog jDialog1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JList jList1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JButton upButton;
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
  	    return "$THETA Record";
  	}

	public String getStepTitle(){
	    return "$THETA Record";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            if(nTheta != iterator.getNTheta())
            {
                nTheta = iterator.getNTheta();
                isValid = false;
                wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                
            }
 	}

	public void hidingStep(JWizardPane wizard){
            jTextField1.setText("");
            jTextField2.setText("");
            jTextField3.setText("");  
            jCheckBox1.setSelected(false);
            jCheckBox2.setSelected(false);
            jCheckBox3.setSelected(false);
            if(nTheta == 0 || model.getSize() == 0)
                return;
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            String record = "";
            String ls = System.getProperty("line.separator");
            for(int i = 0; i < nTheta; i++)
                record = record + ls + model.get(i);
            if(!record.equals(""))
            {
                object.getRecords().setProperty("Theta", "$THETA " + record);
                String[][] theta = new String[nTheta][4];
                for(int i = 0; i < nTheta; i++)
                {
                    String element = (String)model.get(i);
                    element = element.substring(1, element.length() - 1);
                    String[] values = element.split(",");
                    if(values.length == 1)
                    {
                        String value = values[0].split(" ")[0];
                        for(int j = 0; j < 3; j++)
                            theta[i][j] = value;
                        theta[i][3] = "yes";
                    }
                    else
                    {
                        for(int j = 0; j < 3; j++)
                            theta[i][j] = values[j];
                        theta[i][3] = "no";
                    }
                }
                object.getSource().theta = theta;
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
