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
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import java.awt.Color;
import java.util.Vector;
import javax.swing.JOptionPane;

/**
 * This class defines a step to create the $DES record.
 * @author  Jiaji Du
 */
public class Des extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private boolean isHighlighted = false;
    private MDAIterator iterator = null;
    private DefaultHighlighter highlighter = new DefaultHighlighter();
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter1 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,255,0));
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter2 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,0,0));
    
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
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();

        setLayout(new java.awt.BorderLayout());

        setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 12, 12, 12));
        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(238, 238, 238));
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

    }// </editor-fold>//GEN-END:initComponents

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
        
        private String record;
        private String code;

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
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }
            wizardPane = wizard;
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("DES");
                if(text != null)
                {
                    text = text.substring(5, text.length() - 1);
                    iterator.getReload().remove("DES");
                    jTextArea1.setText(text);
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
                }
                if(iterator.getAdvan() == 6 && iterator.initAdvan.contains("des"))
                {
                    text = initDes();
                    iterator.initAdvan.remove("des");
                    jTextArea1.setText(text);
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
                }
            }
            else
            {
                if(iterator.getAdvan() == 6 && iterator.initAdvan.contains("des"))
                {
                    jTextArea1.setText(initDes());
                    iterator.initAdvan.remove("des");
                }
                else
                {
                    String value = jTextArea1.getText();
                    if(value.equals(""))
                    {
                        String initCode = "";
                        for(int i = 0; i < iterator.getNComp(); i++)
                            initCode += "DADT(" + (i + 1) + ")\n";
                        jTextArea1.setText(initCode.trim());
                    }
                }
            }
            jTextArea1.requestFocusInWindow();
	}

        public boolean checkingStep(JWizardPane wizard){
            record = jTextArea1.getText().trim().replaceAll("\r", "").toUpperCase();
            // Correct IF conditions
            record = Utility.correctIFConditions(record);
            // Eliminate empty lines
            while(record.indexOf("\n\n") != -1)
                record = record.replaceAll("\n\n", "\n");
            String title = getStepTitle();
            if(!record.equals(""))
            {
                if(!Utility.checkCharacter(record, title)) return false;              
                // Eliminate comments
                code = Utility.eliminateComments(record);
                // Check P on left handside
                if(!Utility.checkPonLeft(code)) return false;
                // Check index
                if(!Utility.checkIndex(code, "A", "DADT")) return false;
                // Check ENDIF syntax
                if(!Utility.checkENDIF(code, title)) return false;
                // Check NONMEM compatibility
                Vector names = Utility.checkMathFunction(code, title);
                // Check parenthesis mismatch
                Vector lines = Utility.checkParenthesis(code, title);
                // Check expression left hand side
                Vector errors = Utility.checkLeftExpression(code, title);
                // Highlight the incompatible function names and mismatched parenthesis lines
                if(isHighlighted)
                {                
                    highlighter.removeAllHighlights();
                    isHighlighted = false;
                }
                if(names.size() > 0 || lines.size() > 0 || errors.size() > 0)
                {
                    jTextArea1.setHighlighter(highlighter);
                    Element paragraph = jTextArea1.getDocument().getDefaultRootElement();          
                    String[] text = jTextArea1.getText().split("\n");
                    try
                    {
                        for(int i = 0; i < text.length; i++)
                        {
                            for(int j = 0; j < names.size(); j++)
                            {
                                int comment = text[i].indexOf(";");
                                if(comment != 0)
                                {
                                    String line = text[i];
                                    if(comment > 0)
                                        line = text[i].substring(0, comment);
                                    int pos = 0;
                                    int offset = paragraph.getElement(i).getStartOffset();
                                    String name = (String)names.get(j);
                                    while ((pos = text[i].indexOf(name, pos)) >= 0) 
                                    {                
                                        highlighter.addHighlight(pos + offset, pos + offset + name.length(), highlight_painter1);
                                        pos += name.length();
                                        isHighlighted = true;
                                    }
                                }
                            }
                        }
                        for(int i = 0; i < lines.size(); i++)
                        {
                            int n = ((Integer)lines.get(i)).intValue(); 
                            highlighter.addHighlight(paragraph.getElement(n).getStartOffset(),
                                                     paragraph.getElement(n).getEndOffset() - 1,
                                                     highlight_painter2); 
                            isHighlighted = true;                    
                        }
                        for(int i = 0; i < errors.size(); i++)
                        {
                            int n = ((Integer)errors.get(i)).intValue(); 
                            highlighter.addHighlight(paragraph.getElement(n).getStartOffset(),
                                                     paragraph.getElement(n).getEndOffset() - 1,
                                                     highlight_painter2); 
                            isHighlighted = true;                    
                        }
                    }
                    catch(BadLocationException e) 
                    {
                        JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);
                    }
                    if(lines.size() == 0 && errors.size() == 0)
                        return true;
                    return false;
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "Code was missing.", "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            return true;
        }
        
	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack()) return;  
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            object.getRecords().setProperty("Des", "$DES \n" + record);
            object.getSource().des = "\n" + code.trim() + "\n";
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
            return "Prepare_Input_Differential_Equation_Structure";
        }
        
        private String initDes()
        {
            String des = "";
            int adn = iterator.adn;
            switch(adn)
            {
                case 1: 
                    des = "DADT(1)=-K*A(1)";
                    break;
                case 2: 
                    des = "DADT(1)=-KA*A(1)\nDADT(2)=KA*A(1)-K*A(2)";
                    break;
                case 3:
                    des = "DADT(1)=-(K+K12)*A(1)+K21*A(2)\nDADT(2)= K12*A(1)-K21*A(2)";
                    break;
                case 4:
                    des = "DADT(1)=-KA*A(1)\nDADT(2)=KA*A(1)-(K+K23)*A(2)+K32*A(3)\nDADT(3)= K23*A(2)-K32*A(3)";
                    break;
                case 6:
                    for(int i = 0; i < iterator.getNComp(); i++)
                        des += "DADT(" + (i + 1) + ")\n";
                    break;
                case 10:
                    des = "DADT(1)=-VM*A(1)/(KM+A(1))";
                    break;
                case 11:
                    des = "DADT(1)=-(K+K12+K13)*A(1)+K21*A(2)+K31*A(3)\nDADT(2)= K12*A(1)-K21*A(2)\nDADT(3)= K13*A(1)-K31*A(3)";
                    break;
                case 12:
                    des = "DADT(1)=-KA*A(1)\nDADT(2)= KA*A(1)-(K+K23+K24)*A(2)+K32*A(3)+K42*A(4)\nDADT(3)= K23*A(2)-K32*A(3)\nDADT(4)= K24*A(2)-K42*A(4)";
            }
            return des;
        }
    }
}
